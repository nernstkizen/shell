setwd("Z:/project/code")
source('header.R')
source("loadData.R")


setwd(file.path(repo_path, "/data"))


#================================================================================================================================
# Universal Kriging Approach ###
#================================================================================================================================

##@Kriging using mean as aggregate method

##Aggregate data


sumnewdata3<-dplyr::summarise(newdata3,
                       S2=mean(log(S2+0.1),na.rm=TRUE),
                       Tmax=mean(log(Tmax+0.1),na.rm=TRUE),
                       XrdClayChlorite=mean(log(XrdClayChlorite+0.1),na.rm=TRUE),
                       Romeasured=mean(log(Romeasured+0.1),na.rm=TRUE),
                       GriWaterFilledPorosity=mean(log(GriWaterFilledPorosity+0.1),na.rm=TRUE),
                       XrdClaylllite=mean(log(XrdClaylllite+0.1),na.rm=TRUE),
                       GscCombustibleGasContent=mean(log(GscCombustibleGasContent+0.1),na.rm=TRUE),
                       S3=mean(log(S3+0.1),na.rm=TRUE),
                       GriSaturationSo=mean(log(GriSaturationSo+0.1),na.rm=TRUE),
                       XrdClayKaolinite=mean(log(XrdClayKaolinite+1),na.rm=TRUE),
                       Toc=mean(log(Toc+0.1),na.rm=TRUE),
                       S1=mean(log(S1+0.1),na.rm=TRUE),
                       GriSaturationSg=mean(log(GriSaturationSg+0.1),na.rm=TRUE),
                       NormalizedOil=mean(log(NormalizedOil+0.1),na.rm=TRUE),
                       GriGrainDensity=mean(log(GriGrainDensity+0.1),na.rm=TRUE),
                       XrdDolomite=mean(log(XrdDolomite+0.1),na.rm=TRUE),
                       CsgThoriumApi=mean(log(CsgThoriumApi+0.1),na.rm=TRUE),
                       XrdPlagioclase=mean(log(XrdPlagioclase+0.1),na.rm=TRUE),
                       StaticYoungsModulus=mean(log(StaticYoungsModulus+0.1),na.rm=TRUE),
                       GriTotalPorosity=mean(log(GriTotalPorosity+0.1),na.rm=TRUE),
                       GriGasFilledPorosity=mean(log(GriGasFilledPorosity+0.1),na.rm=TRUE),
                       GriBulkDensity=mean(log(GriBulkDensity+0.1),na.rm=TRUE),
                       GriTypeParameter=mean(log(GriTypeParameter+0.1),na.rm=TRUE),
                       XrdMarcasite=mean(log(XrdMarcasite+0.1),na.rm=TRUE),
                       GriMatrixPermeabilityAbsolute=mean(log(GriMatrixPermeabilityAbsolute+0.1),na.rm=TRUE))



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





sumnewdata4<-dplyr::summarise(newdata4,
                       ConfiningStressDynamic=mean(log(ConfiningStressDynamic+0.1),na.rm=TRUE),
                       PoissonRatioDynamic=mean(log(PoissonRatioDynamic+0.1),na.rm=TRUE),
                       BulkDensityDynamic=mean(log(BulkDensityDynamic+0.1),na.rm=TRUE),
                       ShearVelocityDynamic=mean(log(ShearVelocityDynamic+0.1),na.rm=TRUE))





Sumnewdata4<-dplyr::summarise(newdata4,
                              ConfiningStressDynamic=mean(ConfiningStressDynamic,na.rm=TRUE),
                              PoissonRatioDynamic=mean(PoissonRatioDynamic,na.rm=TRUE),
                              BulkDensityDynamic=mean(BulkDensityDynamic,na.rm=TRUE),
                              ShearVelocityDynamic=mean(ShearVelocityDynamic,na.rm=TRUE))



sumnewdata<-dplyr::full_join(sumnewdata3,sumnewdata4,by=c('UWI','latitude','longitude'))

sumnewdata<-dplyr::full_join(Sumnewdata3,Sumnewdata4,by=c('UWI','latitude','longitude'))


cord1.dec = SpatialPoints(cbind(sumnewdata$longitude, sumnewdata$latitude), proj4string=CRS("+proj=longlat"))
cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=14"))
sumnewdata$longitude <- coordinates(cord1.UTM)[,1]
sumnewdata$latitude <- coordinates(cord1.UTM)[,2]



####Check the trend#################


hhh<-!is.na(sumnewdata$Tmax)
plot(sumnewdata[hhh,]$longitude,sumnewdata[hhh,]$latitude)

akima.li<-interp(x=sumnewdata[hhh,]$longitude,y=sumnewdata[hhh,]$latitude,z=sumnewdata[hhh,]$Tmax,linear=FALSE,duplicate = "median")


test<-matrix(0,1600,3)
{
  test[,1]<-rep(akima.li$x,40)
  test[,2]<-rep(akima.li$y,each=40)
  test[,3]<-as.vector(akima.li$z)
}
test<-as.data.frame(test)
names(test)[1:3]<-c("longitude","latitude",'Tmax')

ggplot(data=test,aes(x=longitude,y=latitude,z=Tmax))+geom_tile(aes(fill =Tmax))+ 
  scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))+geom_point(data=sumnewdata[hhh,],aes(x=longitude,y=latitude,colour=Tmax),size=8)+ 
  scale_colour_gradient(low = "white", high = "blue")



##Variogram check
hhh<-!is.na(sumnewdata$Tmax)

lookb=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata[hhh,]$Tmax,max.dist=max(dist(sumnewdata[,2:3]))*0.9,trend='2nd')
lookc=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata[hhh,]$Tmax,op='cloud',max.dist=max(dist(sumnewdata[,2:3]))*0.9,trend='2nd')
lookbc=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata[hhh,]$Tmax,bin.cloud=TRUE,max.dist=max(dist(sumnewdata[,2:3]))*0.9,trend='2nd')
looks=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata[hhh,]$Tmax,op='sm',band=8000,max.dist=max(dist(sumnewdata[,2:3]))*0.9,trend='2nd')

par(mfrow=c(2,2))
plot(lookb, main="binned variogram") 
plot(lookc, main="variogram cloud")
plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
plot(looks, main="smoothed variogram",ylim=c(0,0.0025)) 


#look4=variog4(coords=sumnewdata[hhh,2:3],data=resid)


##Kriging for 29 variables


for (i in 1:29)
{
varr<-names(sumnewdata)[i+3]  
goodname<-paste('Krig',varr,sep='') 
hhh<-!is.na(sumnewdata[,i+3])
lookb=variog(coords=sumnewdata[hhh,c(3,2)],data=sumnewdata[hhh,i+3],trend='2nd')
covpar<-variofit(lookb,kappa=0.5)
if(covpar$cov.pars[2]==0) 
{covpar$cov.pars[2]=0.01}
assign(goodname,Krig(x=sumnewdata[,c(3,2)], Y=sumnewdata[,i+3],theta=covpar$cov.pars[2],m=3))
}


##Prediction for production well

newabc<-abc
for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Krig',varr,sep='') 
  newabc<-cbind(newabc,predict(get(prename),as.matrix(abc[,c(4,3)])))
  names(newabc)[i+5]<-varr
}




for (i in c(1:29))
{
  newabc[,i+5]=exp(newabc[,i+5])-0.1
}


##Easier way to plot####
set.panel()
surface(KrigTmax, type="C",xlab='X',ylab='Y',main='Kriging results for Tmax') # look at the surface 
points(KrigTmax$x)




#library(maps)
#library(mapdata)
#library(mapproj)

#map(database= "county", ylim=c(45,90), xlim=c(-160,-50), col="grey80", fill=TRUE,add=TRUE)

newabc$Longitude <- coordinates(cord2.dec)[,1]
newabc$Latitude <- coordinates(cord2.dec)[,2]



write.csv(newabc,file='Interpolation for top 29 variables for production well(no truncation).csv')




##############################################
#Loess for 29 variables
##############################################


#Loess for 29 variables

for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]  
  goodname<-paste('Loess',varr,sep='')  
  assign(goodname,loess(get(varr)~latitude*longitude, data=sumnewdata,degree=1, span=0.5, normalize=F,control=loess.control(surface = "direct")))
}


##Prediction for production well

newabc<-abc
for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Loess',varr,sep='') 
  newabc<-cbind(newabc,predict(get(prename),as.matrix(abc[,3:4])))
  names(newabc)[i+5]<-varr
}



for (i in c(1:29))
{
  newabc[,i+5]=exp(newabc[,i+5])-0.1
}



newabc$Longitude <- coordinates(cord2.dec)[,1]
newabc$Latitude <- coordinates(cord2.dec)[,2]


write.csv(newabc,file='Interpolation for top 29 variables for production well(no truncation).csv')





##ggplot2

#@@Test Data for drawing kriging heatmap
testdata<-matrix(0,14400,2)
testdata[,1]<-rep(seq(from=min(sumnewdata$latitude),to=max(sumnewdata$latitude),length=120),each=120)
testdata[,2]<-rep(seq(from=min(sumnewdata$longitude),to=max(sumnewdata$longitude),length=120),120)

test<-as.data.frame(testdata)

for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Krig',varr,sep='') 
  test<-cbind(test,predict(get(prename),testdata))
  names(test)[i+2]<-varr  
}

for (i in c(1:29))
{
  test[,i+2]=exp(test[,i+2])-0.1
}



names(test)[1:2]<-c("Latitude","Longitude")


op<-par(ask=TRUE)

for (num in 1:29)

{
  varr<-names(test)[num+2]
  p<-ggplot(data=test,aes(x=Longitude,y=Latitude,z=get(varr)))+geom_tile(aes(fill =get(varr)))+ 
    scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))+geom_point(data=sumnewdata,aes(x=longitude,y=latitude,colour=get(varr)),size=8)+ 
  scale_colour_gradient(low = "white", high = "blue")+
    ggtitle(varr)
  
  plot(p)
  
}



par(op)







#================================================================================================================================
# Tree, RandomForest and Boosting Algorithm ###(my data)
#================================================================================================================================

#Introducint Target variable into newabc data set

newabcY<-dplyr::inner_join(newabc,y2,by='Uwi')
newabcY<-dplyr::arrange(newabcY,Uwi)


write.csv(newabcY,file='Data preparing for Machine learning.csv')


##boosting

runboostRegCV<- function(dat, no.tree, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    #model <- gbm(Target~., data=train[,5:35], n.trees=no.tree, shrinkage=0.01,distribution='gaussian',interaction.depth=4)  
    model <- gbm(Target~., data=train[,3:35], n.trees=no.tree, shrinkage=0.01,distribution='gaussian',interaction.depth=4) 
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    #test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34],n.trees<-no.tree), test[,c(36,37)])
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,3:34],n.trees<-no.tree), test[,c(36,37)])# Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)),n.Tree=no.tree)
  return(list(sol, pred))
}
#@@ 5-fold CV
boost5 <- runboostRegCV(dat=newabcY,  no.tree=5000, k=5)
predboost5<-boost5[[2]]







fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5)

gbmGrid <- expand.grid(interaction.depth=c(4),n.trees = 5000, shrinkage=(c(1))*0.01, n.minobsinnode=10)



gbmFit <- train(Target ~ ., data = newabcY[,3:35],
                method = "gbm",
                 trControl = fitControl,
                 tuneGrid=gbmGrid,
                 verbose = FALSE)











###RandomForest
runRFRegCV <- function(dat, m, no.tree, k ,ntrace=500){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    #model <- randomForest(Target~., data=train[,5:35], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)  
    model <- randomForest(Target~., data=train[,3:35], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    #test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34]), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,3:34]), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  m <- model$mtry  # get default value of mtry
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)), m=m, n.Tree=no.tree)
  return(list(sol, pred))
}

#@@ 5-fold CV 
set.seed(666)
rf <- runRFRegCV(dat=newabcY,  m=12, no.tree=1000, k=5)
predRF<- rf[[2]] 




fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=2)

rfGrid <- expand.grid(mtry=12)


rfFit <- train(Target ~ ., data = newabcY[,5:35],
                 method = "rf",
                 trControl = fitControl,
                 tuneGrid=rfGrid,
                 verbose = FALSE)

predict(rfFit,newabcY[,3:35])



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
    
    lookb=variog(coords=train[,c(4,3)],data=train[,35],trend='2nd')
    #lookbc=variog(coords=train[,c(4,3)],data=train[,35],trend='2nd',bin.cloud=TRUE)
    #par(mfrow=c(2,2))
    #plot(lookb, main="binned variogram") 
    #plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
    
    covpar<-variofit(lookb,kappa=0.5)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    model <- Krig(x=train[,c(4,3)],Y=train[,35],theta=covpar$cov.pars[2],m=3) 
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,as.matrix(test[,c(4,3)])), test[,c(36,37)]) 
    
     # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)))
  return(list(sol, pred))
  
}
set.seed(897)
Kri <- runKriCV(dat=newabcY, k=5)
predKri<- Kri[[2]] 



###New method########

fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5)

newGrid <- expand.grid(C=c(10,20,5,1),sigma=0.2)



newFit <- train(Target ~ ., data = newabcY[,5:35],
                method = "svmRadial",
                tuneGrid=newGrid,
                trControl = fitControl)


runRegSVMCV <- function(dat, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- svm(Target~., train[,5:35])  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34]), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)))
  return(list(sol, pred))
}
set.seed(897)
svm <- runRegSVMCV(dat=newabcY, k=5)
predsvm<- svm[[2]] 

tuneResult <- tune(svm, Target~.,  data = train[,5:35],
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)




#-------------------------------------------------------------------------------------------------------------------------
### Recover Curve
#-------------------------------------------------------------------------------------------------------------------------

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


#@@ Comparison of different model
# Prediction of  models (30 vars)
pred.boost<-dplyr::select(predboost5,Uwi, Target,boost=Pred)
pred.RF<-dplyr::select(predRF, Uwi, RF=Pred)
pred.Kri<-dplyr::select(predKri,Uwi,Kri=Pred)



jo <- dplyr::left_join(pred.boost, pred.RF, by="Uwi")
jo <- dplyr::left_join(jo,pred.Kri,by='Uwi')
jo <- jo[,-1]  # rm Uwi

q.rec <- qRecCurv(jo) * 100

# Round to integer percentage
index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
q.rec <- q.rec[index, ]

q.rec1 <- q.rec %>% dplyr::select(True) %>% dplyr::mutate(RecRate=True, Method="Baseline")
q.rec2 <- q.rec %>% dplyr::select(True, X2) %>% dplyr::rename(RecRate=X2) %>% dplyr::mutate(Method="boost")
q.rec3 <- q.rec %>% dplyr::select(True, X3) %>% dplyr::rename(RecRate=X3) %>% dplyr::mutate(Method="RandomForest")
q.rec4 <- q.rec %>% dplyr::select(True, X4) %>% dplyr::rename(RecRate=X4) %>% dplyr::mutate(Method="Kriging")


q.rec <- dplyr::union(q.rec1, q.rec2)
q.rec <- dplyr::union(q.rec, q.rec3)
q.rec <- dplyr::union(q.rec, q.rec4)


ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
  geom_line(lwd=1.2) +
  scale_color_manual(values=c("#fe506e", "black", "#228b22","#0099cc")) +
  xlab("Top Quantile Percentage") + ylab("Recover Rate") + 
  theme(#legend.position="none",
    axis.title.x = element_text(size=24),
    axis.title.y = element_text(size=24),
    axis.text.x = element_text(colour="grey20",size=15),
    axis.text.y = element_text(colour="grey20",size=15),
    legend.title=element_blank(),
    legend.text = element_text(size = 20),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  )
# plot(q.rec, type="l", xlab="Top Quantile Percentage", ylab="Recover Rate")
# lines(q.rec[,1],q.rec[,1], col="red")







#================================================================================================================================
# Tree, RandomForest and Adaboosting Algorithm ###(Kaggle data)
#================================================================================================================================

#Introducint Target variable into newabc data set

newbbcY<-dplyr::inner_join(bbc,y2,by='Uwi')

#compare three methods

##boosting

runboostRegCV<- function(dat, no.tree, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- gbm(Target~., data=train[,2:32], n.trees=no.tree, shrinkage=0.01,distribution='gaussian',interaction.depth=4)  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(1,32)], Pred=predict(model,newdata=test[,2:31],n.trees<-no.tree), test[,c(33,34)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)),n.Tree=no.tree)
  return(list(sol, pred))
}
#@@ 5-fold CV
set.seed(666)

boost5 <- runboostRegCV(dat=newbbcY,  no.tree=5000, k=5)


predboost5<-boost5[[2]]


`






###RandomForest
runRFRegCV <- function(dat, m, no.tree, k ,ntrace=500){

  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set

      test  <- dat[folds$subsets[folds$which==i],]
      train <- dplyr::setdiff(dat, test)
      model <- randomForest(Target~., data=train[,2:32], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(1,32)], Pred=predict(model,newdata=test[,2:31]), test[,c(33,34)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  m <- model$mtry  # get default value of mtry
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)), m=m, n.Tree=no.tree)
  return(list(sol, pred))
}

#@@ 5-fold CV 
set.seed(666)
rf <- runRFRegCV(dat=newbbcY,  m=12, no.tree=1000, k=5)
predRF<- rf[[2]] 


#-------------------------------------------------------------------------------------------------------------------------
### Recover Curve
#-------------------------------------------------------------------------------------------------------------------------
  
  
  #@@ Comparison of different model
  # Prediction of  models (30 vars)
  


  
  pred.boost<-dplyr::select(predboost5,Uwi, Target,boost=Pred)
  pred.RF<-dplyr::select(predRF, Uwi, RF=Pred)
  pred.kaggle <- dplyr::select(newbbcY, Uwi, Rules.Prediction, Kaggle.Prediction)
  
  
  
  jo <- dplyr::left_join(pred.boost, pred.RF, by="Uwi")
  jo <- dplyr::left_join(jo, pred.kaggle, by="Uwi")
  jo <- jo[,-1]  # rm Uwi
  
  q.rec <- qRecCurv(jo) * 100
  
  # Round to integer percentage
  index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
  q.rec <- q.rec[index, ]
  
  q.rec1 <- q.rec %>% dplyr::select(True) %>% dplyr::mutate(RecRate=True, Method="Baseline")
  q.rec2 <- q.rec %>% dplyr::select(True, X2) %>% dplyr::rename(RecRate=X2) %>% dplyr::mutate(Method="boost")
  q.rec3 <- q.rec %>% dplyr::select(True, X3) %>% dplyr::rename(RecRate=X3) %>% dplyr::mutate(Method="RandomForest")
  q.rec4 <- q.rec %>% dplyr::select(True, X4) %>% dplyr::rename(RecRate=X4) %>% dplyr::mutate(Method="Rule Based")
  q.rec5 <- q.rec %>% dplyr::select(True, X5) %>% dplyr::rename(RecRate=X5) %>% dplyr::mutate(Method="Kaggle")
  
  q.rec <- dplyr::union(q.rec1, q.rec2)
  q.rec <- dplyr::union(q.rec, q.rec3)
  q.rec <- dplyr::union(q.rec, q.rec4)
  q.rec <- dplyr::union(q.rec, q.rec5)



  ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
    geom_line(lwd=1.2) +
    scale_color_manual(values=c("#fe506e", "black", "#228b22", "#0099cc", "#e95d3c")) +
    xlab("Top Quantile Percentage") + ylab("Recover Rate") + 
    theme(#legend.position="none",
      axis.title.x = element_text(size=24),
      axis.title.y = element_text(size=24),
      axis.text.x = element_text(colour="grey20",size=15),
      axis.text.y = element_text(colour="grey20",size=15),
      legend.title=element_blank(),
      legend.text = element_text(size = 20),
      legend.justification=c(1,0), legend.position=c(1,0),
      legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
    )
  # plot(q.rec, type="l", xlab="Top Quantile Percentage", ylab="Recover Rate")
  # lines(q.rec[,1],q.rec[,1], col="red")
  






