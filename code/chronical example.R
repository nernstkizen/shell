#---------------------------------------------------------------------------------------------------------------------
### Setup
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
options(scipen=999)
# options(java.parameters = "-Xmx10000m")
options(java.parameters = "-Xmx1g")


#---------------------------------------------------------------------------------------------------------------------
### Library
#---------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(rgdal)
library(maps)
library(geoR)
library(fields)
library(splancs)
library(ggmap)
library(ggplot2)
library(randomForest)
library(gbm)
library(cvTools)
library(caret)
library(akima)
library(stats)
library(GGally)
#---------------------------------------------------------------------------------------------------------------------
### Project folder path
#---------------------------------------------------------------------------------------------------------------------
repo_path = "Z:/Eagle ford project"

#---------------------------------------------------------------------------------------------------------------------
### Data path
#---------------------------------------------------------------------------------------------------------------------
setwd(file.path(repo_path, "/data"))
#---------------------------------------------------------------------------------------------------------------------
### Load data
#---------------------------------------------------------------------------------------------------------------------
#@@corewell ShaleGas
data3<-read.csv('033_Core_ShaleGas.csv',header=TRUE,as.is=TRUE)
data3<-data3[-1,]

newdata3<-dplyr::select(data3,UWI=Unique.Private.Well.ID, latitude=Well.Latitude, longitude=Well.Longitude,
                        S2=Hydrocarbon...S2..mg.g., 
                        Tmax=Tmax..degrees.C., 
                        XrdClayChlorite=XRD.Clay.Fraction.Chlorite..weight.percent., 
                        Romeasured=Ro.Measured..percent.,
                        GriWaterFilledPorosity=GRI.Water.Filled.Porosity..percent.,
                        XrdClaylllite=XRD.Clay.Fraction.Illite..weight.percent.,
                        GscCombustibleGasContent=GSC.Combustible.Gas.Content,
                        S3=CO2...S3..mg.g.,
                        GriSaturationSo=GRI.Saturations...So..percent.Vp.,
                        XrdClayKaolinite=XRD.Clay.Fraction.Kaolinite..weight.percent.,
                        Toc=Leco.TOC..wt.percent.,
                        S1=Hydrocarbon...S1..mg.g.,
                        GriSaturationSg=GRI.Saturations...Sg..percent.,
                        NormalizedOil=Normalized.Oil.Content,
                        GriGrainDensity=GRI.Grain.Density..gm.cm.3.,
                        XrdDolomite=XRD.Bulk.Rock.Dolomite..weight.percent.,
                        CsgThoriumApi=CSG...Thorium..API.Units.,
                        XrdPlagioclase=XRD.Bulk.Rock.Plagioclase..weight.percent.,
                        StaticYoungsModulus=Static.Youngs.Modulus..10.6.psi.,
                        GriTotalPorosity=GRI.Total.Porosity..percent.,
                        GriGasFilledPorosity=GRI.Gas.Filled.Porosity..percent.,
                        GriBulkDensity=GRI.Bulk.Density..gm.cm.3.,
                        GriTypeParameter=GRI.Corey.Type.Parameter,
                        XrdMarcasite=XRD.Bulk.Rock.Marcasite..weight.percent.,
                        GriMatrixPermeabilityAbsolute=GRI.Matrix.Permeability...Absolute..md.
)
newdata3<-dplyr::arrange(newdata3,UWI)
newdata3[,2:28]<-sapply(newdata3[,2:28],FUN=as.numeric)



for (i in c(6,13,20))
{
  newdata3[,i][newdata3[,i]<0&!is.na(newdata3[,i])] <- NA
}
newdata3<-dplyr::group_by(newdata3,UWI,latitude,longitude)


#@@corewell SCAL
data4<-read.csv('034_Core_SCAL.csv',header=TRUE,as.is=TRUE)
data4<-data4[-1,]
newdata4<-dplyr::select(data4,UWI=Unique.Private.Well.ID, latitude=Well.Latitude, longitude=Well.Longitude,
                        ConfiningStressDynamic=Confining.Stress...Dynamic,
                        PoissonRatioDynamic=Poisson.s.Ratio...Dynamic,
                        BulkDensityDynamic=Bulk.Density...Dynamic,
                        ShearVelocityDynamic=Shear.Velocity...Dynamic
)
newdata4<-dplyr::arrange(newdata4,UWI)
newdata4[,2:7]<-sapply(newdata4[,2:7],FUN=as.numeric)
newdata4<-dplyr::group_by(newdata4,UWI,latitude,longitude)

#@@Production well location + Prod start time + true depth
a <- read.csv("012_Prod_Well.csv", as.is=T)
a <- a %>% dplyr::select(Entity, API, Surface.Latitude, Surface.Longitude) %>% dplyr::filter(!is.na(API),!is.na(Surface.Latitude),!is.na(Surface.Longitude))  # Location

b <- read.csv("013_Prod_Header.csv", as.is=T)
b <- b %>% dplyr::select(Entity, Date.Production.Start) %>% dplyr::filter(!is.na(Date.Production.Start))  # Prod start date

ab <- dplyr::left_join(b, a, by="Entity")
ab <- ab %>% dplyr::distinct(API) %>% dplyr::rename(Uwi=API, Latitude=Surface.Latitude, Longitude=Surface.Longitude)
ab$Date.Production.Start <- as.Date(ab$Date.Production.Start, format="%Y-%m-%d")
prod.date.loc <- ab

c<-read.csv("020_Well_Header.csv",as.is=T)
c<-c %>% dplyr::distinct(UWI) %>% dplyr::select (Uwi=UWI,Depth.True.Vertical)

abc<- dplyr::inner_join(ab,c,by='Uwi')

setwd(file.path(repo_path, "/data/Kaggle/Final/RulesBasedApproach Oct 8/RulesBasedApproach Oct 8"))
y <- read.csv("Rules features using recent and Jan 2014 data.csv")

y1 <- dplyr::select(y,Uwi)  # 2631 x 1
abc <- dplyr::inner_join(abc,y1,by='Uwi')

cord2.dec = SpatialPoints(cbind(abc$Longitude, abc$Latitude), proj4string=CRS("+proj=longlat"))
cord2.UTM <- spTransform(cord2.dec, CRS("+proj=utm +north +zone=14"))
abc$Longitude <- coordinates(cord2.UTM)[,1]
abc$Latitude <- coordinates(cord2.UTM)[,2]


y2 <- dplyr::select(y,Uwi,Target,Rules.Prediction, Kaggle.Prediction) #2632*4



#---------------------------------------------------------------------------------------------------------------------
### Reset working dir
#---------------------------------------------------------------------------------------------------------------------

setwd("Z:/Ayata project/code")
setwd(file.path(repo_path, "/data"))

#================================================================================================================================
# Universal Kriging Approach ###
#================================================================================================================================

##@Kriging using mean as aggregate method

##Aggregate data

Sumnewdata3<-dplyr::summarise(newdata3,
                              S2=mean(S2,na.rm=TRUE),
                              Tmax=mean(Tmax,na.rm=TRUE),
                              XrdClayChlorite=mean(XrdClayChlorite,na.rm=TRUE),
                              Romeasured=mean(Romeasured,na.rm=TRUE),
                              GriWaterFilledPorosity=mean(GriWaterFilledPorosity,na.rm=TRUE),
                              XrdClaylllite=mean(XrdClaylllite,na.rm=TRUE),
                              GscCombustibleGasContent=mean(GscCombustibleGasContent,na.rm=TRUE),
                              S3=mean(S3,na.rm=TRUE),
                              GriSaturationSo=mean(GriSaturationSo,na.rm=TRUE),
                              XrdClayKaolinite=mean(XrdClayKaolinite,na.rm=TRUE),
                              Toc=mean(Toc,na.rm=TRUE),
                              S1=mean(S1,na.rm=TRUE),
                              GriSaturationSg=mean(GriSaturationSg,na.rm=TRUE),
                              NormalizedOil=mean(NormalizedOil,na.rm=TRUE),
                              GriGrainDensity=mean(GriGrainDensity,na.rm=TRUE),
                              XrdDolomite=mean(XrdDolomite,na.rm=TRUE),
                              CsgThoriumApi=mean(CsgThoriumApi,na.rm=TRUE),
                              XrdPlagioclase=mean(XrdPlagioclase,na.rm=TRUE),
                              StaticYoungsModulus=mean(StaticYoungsModulus,na.rm=TRUE),
                              GriTotalPorosity=mean(GriTotalPorosity,na.rm=TRUE),
                              GriGasFilledPorosity=mean(GriGasFilledPorosity,na.rm=TRUE),
                              GriBulkDensity=mean(GriBulkDensity,na.rm=TRUE),
                              GriTypeParameter=mean(GriTypeParameter,na.rm=TRUE),
                              XrdMarcasite=mean(XrdMarcasite,na.rm=TRUE),
                              GriMatrixPermeabilityAbsolute=mean(GriMatrixPermeabilityAbsolute,na.rm=TRUE))

Sumnewdata4<-dplyr::summarise(newdata4,
                              ConfiningStressDynamic=mean(ConfiningStressDynamic,na.rm=TRUE),
                              PoissonRatioDynamic=mean(PoissonRatioDynamic,na.rm=TRUE),
                              BulkDensityDynamic=mean(BulkDensityDynamic,na.rm=TRUE),
                              ShearVelocityDynamic=mean(ShearVelocityDynamic,na.rm=TRUE))


sumnewdata<-dplyr::full_join(Sumnewdata3,Sumnewdata4,by=c('UWI','latitude','longitude'))

#Change NaN to NA
for (i in 2:32)
{
  index<-is.na(sumnewdata[,i])
  sumnewdata[index,i]<-NA
}

###Delete Outliers###########
sumnewdata$S2[60]<-NA
sumnewdata$NormalizedOil[77]<-NA
#####Check the trend#################



#Change UTM coordinate system

cord1.dec = SpatialPoints(cbind(sumnewdata$longitude, sumnewdata$latitude), proj4string=CRS("+proj=longlat"))
cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=14"))
sumnewdata$longitude <- coordinates(cord1.UTM)[,1]
sumnewdata$latitude <- coordinates(cord1.UTM)[,2]
sumnewdata<-as.data.frame(sumnewdata)


##Kriging for 29 variables (Different trends for different variables)


for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]  
  goodname<-paste('Krig',varr,sep='') 
  hhh<-!is.na(sumnewdata[,i+3])
  lookb=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.64)
  #lookc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',op='cloud',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #lookbc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',bin.cloud=TRUE,estimator.type = "modulus",max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #plot(lookc, main='Variogram cloud plot of Tmax',xlab='distance',ylab='variogram')
  #plot(lookbc, bin.cloud=TRUE, main="Binned variogram plot of Tmax",ylab='variogram',ylim=c(0,1000))  
  #plot(lookb)
  covpar<-variofit(lookb)#,cov.model='matern',fix.kappa = FALSE)
  if(covpar$cov.pars[2]==0) 
  {covpar$cov.pars[2]=0.001}
  #if(covpar$kappa>2)
  #{covpar$kappa=2}
  assign(goodname,Krig(x=sumnewdata[,c(3,2)], Y=sumnewdata[,i+3],theta=covpar$cov.pars[2],m=3))#,smoothness=covpar$kappa,Covariance="Matern"))
}

for (i in c(17,23))
{
  varr<-names(sumnewdata)[i+3]  
  goodname<-paste('Krig',varr,sep='') 
  hhh<-!is.na(sumnewdata[,i+3])
  lookb=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='1st',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.64)
  #lookc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',op='cloud',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #lookbc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',bin.cloud=TRUE,estimator.type = "modulus",max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #plot(lookc, main='Variogram cloud plot of Tmax',xlab='distance',ylab='variogram')
  #plot(lookbc, bin.cloud=TRUE, main="Binned variogram plot of Tmax",ylab='variogram',ylim=c(0,1000))  
  #plot(lookb)
  covpar<-variofit(lookb)#,cov.model='matern',fix.kappa = FALSE)
  if(covpar$cov.pars[2]==0) 
  {covpar$cov.pars[2]=0.001}
  #if(covpar$kappa>2)
  #{covpar$kappa=2}
  assign(goodname,Krig(x=sumnewdata[,c(3,2)], Y=sumnewdata[,i+3],theta=covpar$cov.pars[2],m=2))#,smoothness=covpar$kappa,Covariance="Matern"))}
}

for (i in c(1,12,16,18))
{
  varr<-names(sumnewdata)[i+3]  
  goodname<-paste('Krig',varr,sep='') 
  hhh<-!is.na(sumnewdata[,i+3])
  lookb=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='cte',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.64)
  #lookc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',op='cloud',max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #lookbc=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd',bin.cloud=TRUE,estimator.type = "modulus",max.dist=max(dist(sumnewdata[hhh,2:3]))*0.8)
  #plot(lookc, main='Variogram cloud plot of Tmax',xlab='distance',ylab='variogram')
  #plot(lookbc, bin.cloud=TRUE, main="Binned variogram plot of Tmax",ylab='variogram',ylim=c(0,1000))  
  #plot(lookb)
  covpar<-variofit(lookb)#,cov.model='matern',fix.kappa = FALSE)
  if(covpar$cov.pars[2]==0) 
  {covpar$cov.pars[2]=0.001}
  #if(covpar$kappa>2)
  #{covpar$kappa=2}
  assign(goodname,Krig(x=sumnewdata[,c(3,2)], Y=sumnewdata[,i+3],theta=covpar$cov.pars[2],m=1))#,smoothness=covpar$kappa,Covariance="Matern"))
}




##Prediction for production well

newabc<-abc
for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Krig',varr,sep='') 
  newabc<-cbind(newabc,predict(get(prename),as.matrix(abc[,c(5,4)])))
  names(newabc)[i+6]<-varr
}


##Easier way to plot####
#set.panel()
#surface(KrigS2, type="C",xlab='X',ylab='Y',main='Kriging results for S3') # look at the surface 
#points(KrigS3$x)

newabc$Longitude <- coordinates(cord2.dec)[,1]
newabc$Latitude <- coordinates(cord2.dec)[,2]

write.csv(newabc,file='Interpolation for top 29 variables for production well(no truncation).csv')


#================================================================================================================================
# Tree, RandomForest and Boosting Algorithm ###(my data)
#================================================================================================================================

#Introducint Target variable into newabc data set

newabcY<-dplyr::inner_join(newabc,y2,by='Uwi')
newabcY<-dplyr::arrange(newabcY,Uwi)

#ggscatmat(newabcY,columns=c(3:35))

write.csv(newabcY,file='Data preparing for Machine learning.csv')

ggscatmat(newabcY,columns=c(15:16,35))

###RandomForest

trainabcY<-newabcY[newabcY[,2]<='2010-07-01',]
testabcY<-dplyr::setdiff(newabcY,trainabcY)


runRFRegCV <- function(dat, m, no.tree, k ,ntrace=500){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- randomForest(Target~., data=train[,4:36], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(3,36)], Pred=predict(model,newdata=test[,4:35]), test[,c(37,38)])  # Uwi, Target, Pred, Latitude, Longitude
    
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  m <- model$mtry  # get default value of mtry
  sol <- data.frame(K=k,mse=mean(mse), rmse=mean(sqrt(mse)), m=m, n.Tree=no.tree)
  return(list(sol, pred))
}

#@@ 5-fold CV 
set.seed(666)
RF <- runRFRegCV(dat=trainabcY,  m=12, no.tree=1000, k=5)




runRFReg <- function(train, test, m, no.tree, ntrace=500){
  
  mse <- NULL;  pred <- NULL; sol <- NULL;
  # Split data into train/test set

  model <- randomForest(Target~., data=train[,4:36], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)
  #####################################################################################################
    
  # Predict test dataset and calculate mse
  test.pred <- cbind(test[,c(3,36)], Pred=predict(model,newdata=test[,4:35]), test[,c(37,38)])  # Uwi, Target, Pred, Latitude, Longitude
  mse <- sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred)
  sol <- data.frame(mse=mse, rmse=sqrt(mse), m=m, n.Tree=no.tree)
  return(list(sol, test.pred))
}

set.seed(666)
rf <- runRFReg(train=trainabcY, test=testabcY,  m=12, no.tree=1000)
 



ggplot()+geom_point(data=trainabcY, aes(x=Longitude, y=Latitude, colour = Target,size=Target))+scale_colour_gradientn(colours = jet.colors(7))
ggplot()+geom_point(data=testabcY, aes(x=Longitude, y=Latitude, colour = Target,size=Target))+scale_colour_gradientn(colours = jet.colors(7))



#####Direct Kriging##################


runKriCV <- function(dat, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  cord1.dec = SpatialPoints(cbind(dat$Longitude, dat$Latitude), proj4string=CRS("+proj=longlat"))
  cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=14"))
  dat$Longitude <- coordinates(cord1.UTM)[,1]
  dat$Latitude <- coordinates(cord1.UTM)[,2]
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    
    lookb=variog(coords=train[,c(5,4)],data=train[,36],trend='cte')
    
    #lookbc=variog(coords=train[,c(4,3)],data=train[,35],trend='2nd',bin.cloud=TRUE,estimator.type = "modulus")
    #par(mfrow=c(2,2))
    #plot(lookb, main="binned variogram") 
    #plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
    
    covpar<-variofit(lookb)#,cov.model='matern',fix.kappa = FALSE)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    #if(covpar$kappa>2)
    #{covpar$kappa=2}
    model <- Krig(x=train[,c(5,4)],Y=train[,36],theta=covpar$cov.pars[2],m=1)#,smoothness=covpar$kappa,Covariance="Matern") 
    test.pred <- cbind(test[,c(3,36)], Pred=predict(model,as.matrix(test[,c(5,4)])), test[,c(37,38)]) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=mean(sqrt(mse)))
  return(list(sol, pred))
  
}

KRI<-runKriCV(trainabcY,5)

runKri <- function(train,test){
  
  
  cord1.dec = SpatialPoints(cbind(train$Longitude, train$Latitude), proj4string=CRS("+proj=longlat"))
  cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=14"))
  train$Longitude <- coordinates(cord1.UTM)[,1]
  train$Latitude <- coordinates(cord1.UTM)[,2]
  
  cord2.dec = SpatialPoints(cbind(test$Longitude, test$Latitude), proj4string=CRS("+proj=longlat"))
  cord2.UTM <- spTransform(cord2.dec, CRS("+proj=utm +north +zone=14"))
  test$Longitude <- coordinates(cord2.UTM)[,1]
  test$Latitude <- coordinates(cord2.UTM)[,2] 

    #####################################################################################################
    
    # Predict test dataset and calculate mse
    
    lookb=variog(coords=train[,c(5,4)],data=train[,36],trend='cte')
    
    #lookbc=variog(coords=train[,c(4,3)],data=train[,35],trend='2nd',bin.cloud=TRUE,estimator.type = "modulus")
    #par(mfrow=c(2,2))
    #plot(lookb, main="binned variogram") 
    #plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
    
    covpar<-variofit(lookb)#,cov.model='matern',fix.kappa = FALSE)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    #if(covpar$kappa>2)
    #{covpar$kappa=2}
    model <- Krig(x=train[,c(5,4)],Y=train[,36],theta=covpar$cov.pars[2],m=1)#,smoothness=covpar$kappa,Covariance="Matern") 
    test.pred <- cbind(test[,c(3,36)], Pred=predict(model,as.matrix(test[,c(5,4)])), test[,c(37,38)]) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred)
     # save prediction results for fold i
  # CV results
  sol <- data.frame(mse=mean(mse), rmse=mean(sqrt(mse)))
  return(list(sol, test.pred))
  }

kri<-runKri(trainabcY,testabcY)









idw<-function(z,distance,k,num.neighs)
{
  idw.z<-rep(0,length(distance[,1]))
  for (i in 1:length(distance[,1]))
  {
    d<-sort(distance[i,],index.return=TRUE)
    w<-1/d$x[1:num.neighs]^k
    idw.z[i]<-sum(z[d$ix[1:num.neighs]]*w)/sum(w)
  }
  return(idw.z)
}  

aq.ch<-chull(newabcY$Longitude,newabcY$Latitude)
aq.ch<-c(aq.ch,aq.ch[1])
aq.border<-cbind(newabcY$Longitude[aq.ch],newabcY$Latitude[aq.ch])

aq.bbox<-sbox(as.points(newabcY$Longitude,newabcY$Latitude))
aq.grid<-gridpts(aq.bbox,npts=45000)
aq.grx<-unique(aq.grid[,1])
aq.gry<-unique(aq.grid[,2])
inside<-inout(aq.grid,aq.border,bound=TRUE)
aq.Grid<-aq.grid[inside,]
distmat<-rdist(aq.Grid,cbind(newabcY$Longitude,newabcY$Latitude))

TTarget<-idw(m,distmat,2,30)
M<-cbind(aq.Grid,TTarget)
M<-as.data.frame(M)
names(M)<-c('longitude','latitude','Target')
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
all_states <- map_data("county")
county <- subset(all_states, (long>=(-100.3))&(long<=(-96))&(lat>27.6)&(lat<31) )
county <- subset(all_states, (long>=(-101))&(long<=(-95))&(lat>27.6)&(lat<31.5) )
p <- ggplot() + geom_polygon( data=county, aes(x=long, y=lat, group = subregion),colour="grey", fill="white",size=1 )+
  geom_tile(data = M, aes(x = longitude, y = latitude, fill = Target),  alpha = 0.8)+scale_fill_gradientn(colours = jet.colors(7))+
  stat_contour(data = M,aes( x = longitude, y = latitude, z = Target))


ggplot()+geom_point(data=newabcY, aes(x=Longitude, y=Latitude, colour = Target,size=Target))+scale_colour_gradientn(colours = jet.colors(7))


lookb=variog(coords=newabcY[,c(4,3)],data=newabcY[,35],trend='2nd',max.dist=max(dist(newabcY[,4:3]))*0.64)
plot(lookb, main="binned variogram") 
covpar<-variofit(lookb)
if(covpar$cov.pars[2]==0) 
{covpar$cov.pars[2]=0.01}
model <- Krig(x=newabcY[,c(4,3)],Y=newabcY[,35],theta=covpar$cov.pars[2],m=1)
set.panel()
surface(model, type="C",xlab='X',ylab='Y',main='Kriging results for Toc') # look at the surface 
cbind(newabcY$Target,model$fitted.values)
rmse(newabcY$Target,model$fitted.values)




predKri<- Kri[[2]] 

mmm<-rep(0,25)
for (i in 1:25)
{
  print(i)
  M<-runKriCV(dat=newabcY, k=5)
  print(M[[1]])
  mmm[i]<-M[[1]][3]
}


linear trend
#RMSE 5.278(0.039)

no trend
#RMSE 5.288(0.037)




qRecCurv <- function(x) {
  
  x <- as.data.frame(na.omit(x))
  
  n.row.x <- nrow(x)  
  n.col.x <- ncol(x)  
  
  ranks <- x %>% dplyr::mutate_each(funs(row_number)) %>% dplyr::arrange(desc(Target))  # ranks for each col and then ordered by 1st col(true value)
  
  rec.q <- data.frame(matrix(-1, nrow = n.row.x , ncol = n.col.x))  # recover quantiles
  rec.q[1,] <- (ranks[1,] == n.row.x)
  for (i in 2:n.row.x)
  {
    #rec.q[i,] <- ranks %>% slice(1:i) %>% summarise_each (funs(sum(.<=i)/i))
    rec.q[i,] <- ranks %>% dplyr::slice(1:i) %>% dplyr::summarise_each (funs(sum(.>=(n.row.x-i+1))/i))
  }
  names(rec.q)[1]<- "True"
  rec.q[,1]<-1:n.row.x/n.row.x
  
  #row.names(rec.q) <- sapply(100*(1:n.row.x)/n.row.x,  FUN = function(x) paste("P",round(x,digits = 0),sep = ""))
  
  return(rec.q)
}  


QRecCurv <- function(x) {
  
  x <- as.data.frame(na.omit(x))
  
  n.row.x <- nrow(x)  
  n.col.x <- ncol(x)  
  
  ranks <- x %>% dplyr::mutate_each(funs(row_number)) %>% dplyr::arrange(Target)  # ranks for each col and then ordered by 1st col(true value)
  
  rec.q <- data.frame(matrix(-1, nrow = n.row.x , ncol = n.col.x))  # recover quantiles
  rec.q[1,] <- (ranks[1,] == 1)
  for (i in 2:n.row.x)
  {
    #rec.q[i,] <- ranks %>% slice(1:i) %>% summarise_each (funs(sum(.<=i)/i))
    rec.q[i,] <- ranks %>% dplyr::slice(1:i) %>% dplyr::summarise_each (funs(sum(.<=i)/i))
  }
  names(rec.q)[1]<- "True"
  rec.q[,1]<-1:n.row.x/n.row.x
  
  #row.names(rec.q) <- sapply(100*(1:n.row.x)/n.row.x,  FUN = function(x) paste("P",round(x,digits = 0),sep = ""))
  
  return(rec.q)
}  









#--------------------------------

date<-c('2010-01-01','2010-04-01','2010-07-01','2010-10-01','2011-01-01','2011-04-01','2011-07-01','2011-10-01',
        '2012-01-01','2012-04-01')


uprfs<-rep(0,5)
upkris<-rep(0,5)
lowrfs<-rep(0,5)
lowkris<-rep(0,5)


for (iter in 1:5)
{
  
trainabcY<-newabcY[newabcY[,2]<=date[iter],]
testabcY<-dplyr::setdiff(newabcY,trainabcY)  

#RF <- runRFRegCV(dat=trainabcY,  m=12, no.tree=1000, k=5)
rf <- runRFReg(train=trainabcY, test=testabcY,  m=12, no.tree=1000)
pred.rf<- rf[[2]]

#KRI<-runKriCV(trainabcY,5)
kri<-runKri(trainabcY,testabcY)
pred.kri<- kri[[2]]

#@@ Recovery Curve
pred.rf<-dplyr::select(pred.rf,Uwi, Target,rf=Pred)
pred.kri<-dplyr::select(pred.kri, Uwi, kriging=Pred)

jo <- dplyr::left_join(pred.rf, pred.kri, by="Uwi")
jo <- jo[,-1]  # rm Uwi
quan25<-ceiling(0.25*dim(testabcY)[1])


q.rec <- qRecCurv(jo) * 100
names(q.rec)=c('True','rf','kriging')
uprfs[iter]<-q.rec[quan25,2]
upkris[iter]<-q.rec[quan25,3]

Q.rec <- QRecCurv(jo) * 100
names(Q.rec)=c('True','rf','kriging')
lowrfs[iter]<-Q.rec[quan25,2]
lowkris[iter]<-Q.rec[quan25,3]
}

plot(uprfs,type='l',col='red',ylim=c(40,70),main='top quantile')
lines(upkris,type='l',col='blue')

plot(lowrfs,type='l',col='red',ylim=c(40,70),main='bottom quantile')
lines(lowkris,type='l',col='blue')
