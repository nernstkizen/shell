setwd("Z:/project/code")
source('header.R')
source("loadData.R")


setwd(file.path(repo_path, "/data"))


#================================================================================================================================
# Universal Kriging Approach ###
#================================================================================================================================

##@Kriging using mean as aggregate method

##Aggregate data



sumnewdata3<-summarise(newdata3,
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






sumnewdata4<-summarise(newdata4,
                       ConfiningStressDynamic=mean(ConfiningStressDynamic,na.rm=TRUE),
                       PoissonRatioDynamic=mean(PoissonRatioDynamic,na.rm=TRUE),
                       BulkDensityDynamic=mean( BulkDensityDynamic,na.rm=TRUE),
                       ShearVelocityDynamic=mean(ShearVelocityDynamic,na.rm=TRUE))


sumnewdata<-full_join(sumnewdata3,sumnewdata4,by=c('UWI','latitude','longitude'))




##Variogram check
##hhh<-!is.na(sumnewdata$Tmax)
##lookb=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata$Tmax[hhh])
##lookc=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata$Tmax[hhh],op='cloud')
##lookbc=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata$Tmax[hhh],bin.cloud=TRUE)
##looks=variog(coords=sumnewdata[hhh,2:3],data=sumnewdata$Tmax[hhh],op='sm',band=1)

##par(mfrow=c(2,2))
##plot(lookb, main="binned variogram") 
##plot(lookc, main="variogram cloud")
##plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
##plot(looks, main="smoothed variogram") 

#look4=variog4(coords=sumnewdata[,2:3],data=sumnewdata$Tmax)


##Kriging for 29 variables


for (i in 1:29)
{
varr<-names(sumnewdata)[i+3]  
goodname<-paste('Krig',varr,sep='')  
assign(goodname,Krig(x=sumnewdata[,2:3], Y=sumnewdata[,i+3]))
}


##Prediction for production well

newabc<-abc
for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Krig',varr,sep='') 
  newabc<-cbind(newabc,predict(get(prename),as.matrix(abc[,3:4])))
  names(newabc)[i+5]<-varr
}


write.csv(newabc,file='Interpolation for top 29 variables for production well(no truncation).csv')





##ggplot2

#@@Test Data for drawing kriging heatmap
testdata<-matrix(0,14400,2)
testdata[,1]<-rep(seq(from=27,to=32,length=120),each=120)
testdata[,2]<-rep(seq(from=-101,to=-96,length=120),120)

test<-as.data.frame(testdata)

for (i in 1:29)
{
  varr<-names(sumnewdata)[i+3]
  prename<-paste('Krig',varr,sep='') 
  test<-cbind(test,predict(get(prename),testdata))
  names(test)[i+2]<-varr  
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





NOTNEEDRIGNTNOW<-function{


##@Kriging using trimmed mean (10%) as aggregate method

##Aggregate data

Trunsumnewdata1<-summarise(newdata1,Tmax=mean(Tmax,na.rm=TRUE,trim=0.1),S2=mean(S2,na.rm=TRUE,trim=0.1),Romeasure=mean(Romeasure,na.rm=TRUE,trim=0.1))
#Trunsumnewdata1<-filter(Trunsumnewdata1,!is.na(Tmax))

Trunsumnewdata3<-summarise(newdata3,ClayChlo=mean(ClayChlo,na.rm=TRUE,trim=0.1),waterporosity=mean(waterporosity,na.rm=TRUE,trim=0.1))
#Trunsumnewdata3<-filter(Trunsumnewdata1,!is.na(Tmax))


##Kriging for Five variables


TrunKrigTmax<-Krig(x=Trunsumnewdata1[,2:3], Y=Trunsumnewdata1$Tmax)
TrunKrigS2<-Krig(x=Trunsumnewdata1[,2:3], Y=Trunsumnewdata1$S2)
TrunKrigRomeasure<-Krig(x=Trunsumnewdata1[,2:3], Y=Trunsumnewdata1$Romeasure)
TrunKrigClayChlo<-Krig(x=Trunsumnewdata3[,2:3], Y=Trunsumnewdata3$ClayChlo)
TrunKrigwaterporosity<-Krig(x=Trunsumnewdata3[,2:3], Y=Trunsumnewdata3$waterporosity)



##Predict for production well


Trunpredic.Tmax<-predict(TrunKrigTmax,as.matrix(abc[,3:4]))
Trunpredic.S2<-predict(TrunKrigS2,as.matrix(abc[,3:4]))
Trunpredic.Romeasure<-predict(TrunKrigRomeasure,as.matrix(abc[,3:4]))
Trunpredic.ClayChlo<-predict(TrunKrigClayChlo,as.matrix(abc[,3:4]))
Trunpredic.waterporosity<-predict(TrunKrigwaterporosity,as.matrix(abc[,3:4]))

Trunnewabc<-cbind(abc,Tmax=Trunpredic.Tmax,S2=Trunpredic.S2,Romeasure=Trunpredic.Romeasure,ClayChlo=Trunpredic.ClayChlo,waterporosity=Trunpredic.waterporosity)



write.csv(Trunnewabc,file='Interpolation for top five variables for production well(10% truncation).csv')

##ggplot2

Truntest<-cbind(testdata,Tmax=predict(TrunKrigTmax,testdata),S2=predict(TrunKrigS2,testdata),Romeasure=predict(TrunKrigRomeasure,testdata),
               ClayChlo<-predict(TrunKrigClayChlo,testdata),waterporosity<-predict(TrunKrigwaterporosity,testdata))
Truntest<-as.data.frame(Truntest)
names(Truntest)<-c("Latitude","Longitude","Tmax","S2","Romeasure","ClayChlo","waterporosity")


TrunpTmax<-ggplot(data=Truntest,aes(x=Longitude,y=Latitude,z=Tmax))+geom_tile(aes(fill = Tmax)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))
direct.label(TrunpTmax)


TrunpS2<-ggplot(data=Truntest,aes(x=Longitude,y=Latitude,z=S2))+geom_tile(aes(fill = S2)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(TrunpS2)

TrunpRomeasure<-ggplot(data=Truntest,aes(x=Longitude,y=Latitude,z=Romeasure))+geom_tile(aes(fill = Romeasure)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(TrunpRomeasure)

TrunpClayChlo<-ggplot(data=Truntest,aes(x=Longitude,y=Latitude,z=ClayChlo))+geom_tile(aes(fill = ClayChlo)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(TrunpClayChlo)

Trunpwaterporosity<-ggplot(data=Truntest,aes(x=Longitude,y=Latitude,z=waterporosity))+geom_tile(aes(fill = waterporosity)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(Trunpwaterporosity)










##@Kriging using mean calcaulated without outliers as aggregate method


##Define a function to calculate mean without outliers
Normal_mean <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75),na.rm=TRUE)
  H <- 1.5 * IQR(x,na.rm=TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(mean(y,na.rm=TRUE))
}

##Aggregate data

Norsumnewdata1<-summarise(newdata1,Tmax=Normal_mean(Tmax),S2=Normal_mean(S2),Romeasure=Normal_mean(Romeasure))
#Norsumnewdata1<-filter(Norsumnewdata1,!is.na(Tmax))

Norsumnewdata3<-summarise(newdata3,ClayChlo=Normal_mean(ClayChlo),waterporosity=Normal_mean(waterporosity))
#Norsumnewdata3<-filter(Norsumnewdata1,!is.na(Tmax))


#Kriging for Five variables


NorKrigTmax<-Krig(x=Norsumnewdata1[,2:3], Y=Norsumnewdata1$Tmax)
NorKrigS2<-Krig(x=Norsumnewdata1[,2:3], Y=Norsumnewdata1$S2)
NorKrigRomeasure<-Krig(x=Norsumnewdata1[,2:3], Y=Norsumnewdata1$Romeasure)
NorKrigClayChlo<-Krig(x=Norsumnewdata3[,2:3], Y=Norsumnewdata3$ClayChlo)
NorKrigwaterporosity<-Krig(x=Norsumnewdata3[,2:3], Y=Norsumnewdata3$waterporosity)



##Predict for production well


Norpredic.Tmax<-predict(NorKrigTmax,as.matrix(abc[,3:4]))
Norpredic.S2<-predict(NorKrigS2,as.matrix(abc[,3:4]))
Norpredic.Romeasure<-predict(NorKrigRomeasure,as.matrix(abc[,3:4]))
Norpredic.ClayChlo<-predict(NorKrigClayChlo,as.matrix(abc[,3:4]))
Norpredic.waterporosity<-predict(NorKrigwaterporosity,as.matrix(abc[,3:4]))

Nornewabc<-cbind(abc,Tmax=Norpredic.Tmax,S2=Norpredic.S2,Romeasure=Norpredic.Romeasure,ClayChlo=Norpredic.ClayChlo,waterporosity=Norpredic.waterporosity)

write.csv(Nornewabc,file='Interpolation for top five variables for production well(outlier removed).csv')





##ggplot2

Nortest<-cbind(testdata,Tmax=predict(NorKrigTmax,testdata),S2=predict(NorKrigS2,testdata),Romeasure=predict(NorKrigRomeasure,testdata),
            ClayChlo<-predict(NorKrigClayChlo,testdata),waterporosity<-predict(NorKrigwaterporosity,testdata))
Nortest<-as.data.frame(Nortest)
names(Nortest)<-c("Latitude","Longitude","Tmax","S2","Romeasure","ClayChlo","waterporosity")


NorpTmax<-ggplot(data=Nortest,aes(x=Longitude,y=Latitude,z=Tmax))+geom_tile(aes(fill = Tmax)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))
direct.label(NorpTmax)


NorpS2<-ggplot(data=Nortest,aes(x=Longitude,y=Latitude,z=S2))+geom_tile(aes(fill = S2)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(NorpS2)

NorpRomeasure<-ggplot(data=Nortest,aes(x=Longitude,y=Latitude,z=Romeasure))+geom_tile(aes(fill = Romeasure)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(NorpRomeasure)

NorpClayChlo<-ggplot(data=Nortest,aes(x=Longitude,y=Latitude,z=ClayChlo))+geom_tile(aes(fill = ClayChlo)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(NorpClayChlo)

Norpwaterporosity<-ggplot(data=Nortest,aes(x=Longitude,y=Latitude,z=waterporosity))+geom_tile(aes(fill = waterporosity)) + scale_fill_gradient(low = "white", high = "red")+
  stat_contour(size=1,aes(colour=..level..))

direct.label(Norpwaterporosity)

}


#================================================================================================================================
# Adaboosting Algorithm ###
#================================================================================================================================

#Introducint Target variable into newabc data set

newabcY<-inner_join(newabc,y2,by='Uwi')

TrainY<-newabcY[500:2500,]
TestY<-newabcY[-(500:2500),]

#compare three methods

boost.Target<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=5000,shrinkage=0.01)
boost.predict<-predict(boost.Target,newdata=TestY[,5:34],n.trees=5000)
boost.error<-mean((boost.predict-TestY$Target)^2)



Tree.Target<-tree(Target~.,data=TrainY[,5:35])
Tree.predict<-predict(Tree.Target,newdata=TestY[,5:34])
Tree.error<-mean((Tree.predict-TestY$Target)^2)
 


RF.Target<-randomForest(Target~.,data=TrainY[,5:35],ntree=5000)
RF.predict<-predict(RF.Target,newdata=TestY[,5:34])
RF.error<-mean((RF.predict-TestY$Target)^2)


c(boost.error,Tree.error,RF.error)



cbind(True=TestY$Target[1:50],boost=boost.predict[1:50],Tree=as.vector(Tree.predict[1:50]),RF=as.vector(RF.predict[1:50]))













