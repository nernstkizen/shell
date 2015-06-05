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
                       S2=mean(log(S2+0.01),na.rm=TRUE),
                       Tmax=mean(log(Tmax+0.01),na.rm=TRUE),
                       XrdClayChlorite=mean(log(XrdClayChlorite+0.01),na.rm=TRUE),
                       Romeasured=mean(log(Romeasured+0.01),na.rm=TRUE),
                       GriWaterFilledPorosity=mean(log(GriWaterFilledPorosity+0.01),na.rm=TRUE),
                       XrdClaylllite=mean(log(XrdClaylllite+0.01),na.rm=TRUE),
                       GscCombustibleGasContent=mean(log(GscCombustibleGasContent+0.01),na.rm=TRUE),
                       S3=mean(log(S3+0.01),na.rm=TRUE),
                       GriSaturationSo=mean(log(GriSaturationSo+0.01),na.rm=TRUE),
                       XrdClayKaolinite=mean(log(XrdClayKaolinite+1),na.rm=TRUE),
                       Toc=mean(log(Toc+0.01),na.rm=TRUE),
                       S1=mean(log(S1+0.01),na.rm=TRUE),
                       GriSaturationSg=mean(log(GriSaturationSg+0.01),na.rm=TRUE),
                       NormalizedOil=mean(log(NormalizedOil+0.01),na.rm=TRUE),
                       GriGrainDensity=mean(log(GriGrainDensity+0.01),na.rm=TRUE),
                       XrdDolomite=mean(log(XrdDolomite+0.01),na.rm=TRUE),
                       CsgThoriumApi=mean(log(CsgThoriumApi+0.01),na.rm=TRUE),
                       XrdPlagioclase=mean(log(XrdPlagioclase+0.01),na.rm=TRUE),
                       StaticYoungsModulus=mean(log(StaticYoungsModulus+0.01),na.rm=TRUE),
                       GriTotalPorosity=mean(log(GriTotalPorosity+0.01),na.rm=TRUE),
                       GriGasFilledPorosity=mean(log(GriGasFilledPorosity+0.01),na.rm=TRUE),
                       GriBulkDensity=mean(log(GriBulkDensity+0.01),na.rm=TRUE),
                       GriTypeParameter=mean(log(GriTypeParameter+0.01),na.rm=TRUE),
                       XrdMarcasite=mean(log(XrdMarcasite+0.01),na.rm=TRUE),
                       GriMatrixPermeabilityAbsolute=mean(log(GriMatrixPermeabilityAbsolute+0.01),na.rm=TRUE))






sumnewdata4<-summarise(newdata4,
                       ConfiningStressDynamic=mean(log(ConfiningStressDynamic+0.01),na.rm=TRUE),
                       PoissonRatioDynamic=mean(log(PoissonRatioDynamic+0.01),na.rm=TRUE),
                       BulkDensityDynamic=mean(log(BulkDensityDynamic+0.01),na.rm=TRUE),
                       ShearVelocityDynamic=mean(log(ShearVelocityDynamic+0.01),na.rm=TRUE))


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



for (i in c(1:29))
{
  newabc[,i+5]=exp(newabc[,i+5])-0.01
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

for (i in c(1:29))
{
  test[,i+2]=exp(test[,i+2])-0.01
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
# Tree, RandomForest and Adaboosting Algorithm ###(my data)
#================================================================================================================================

#Introducint Target variable into newabc data set

newabcY<-inner_join(newabc,y2,by='Uwi')

xiaoshiba<-function{
#compare three methods

boost.Target1<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=3000,shrinkage=0.01)
boost.predict1<-predict(boost.Target1,newdata=TestY[,5:34],n.trees=3000)
boost.error1<-mean((boost.predict1-TestY$Target)^2)

boost.Target2<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=5000,shrinkage=0.01)
boost.predict2<-predict(boost.Target2,newdata=TestY[,5:34],n.trees=5000)
boost.error2<-mean((boost.predict2-TestY$Target)^2)

boost.Target3<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=7000,shrinkage=0.01)
boost.predict3<-predict(boost.Target3,newdata=TestY[,5:34],n.trees=7000)
boost.error3<-mean((boost.predict3-TestY$Target)^2)

boost.Target4<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=9000,shrinkage=0.01)
boost.predict4<-predict(boost.Target4,newdata=TestY[,5:34],n.trees=9000)
boost.error4<-mean((boost.predict4-TestY$Target)^2)

boost.Target5<-gbm(Target~.,data=TrainY[,5:35], distribution='gaussian',n.trees=15000,shrinkage=0.01)
boost.predict5<-predict(boost.Target5,newdata=TestY[,5:34],n.trees=15000)
boost.error5<-mean((boost.predict5-TestY$Target)^2)


Tree.Target<-tree(Target~.,data=TrainY[,5:35])
Tree.predict<-predict(Tree.Target,newdata=TestY[,5:34])
Tree.error<-mean((Tree.predict-TestY$Target)^2)
 


RF.Target<-randomForest(Target~.,data=TrainY[,5:35],ntree=1000)
RF.predict<-predict(RF.Target,newdata=TestY[,5:34])
RF.error<-mean((RF.predict-TestY$Target)^2)


Kaggle.error<-mean((TestY$Kaggle.Prediction-TestY$Target)^2)

c(boost.error1,boost.error2,boost.error3,boost.error4,boost.error5,Tree.error,RF.error,Kaggle.error)
}








##boosting

runboostRegCV<- function(dat, no.tree, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- gbm(Target~., data=train[,5:35], n.trees=no.tree, shrinkage=0.01,distribution='gaussian',interaction.depth=4)  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34],n.trees<-no.tree), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  rmse <- sqrt(mse)
  sol <- data.frame(K=k,mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse),n.Tree=no.tree)
  return(list(sol, pred))
}
#@@ 5-fold CV
set.seed(666)
boost1 <- runboostRegCV(dat=newabcY,  no.tree=1000, k=5)
boost2 <- runboostRegCV(dat=newabcY,  no.tree=3000, k=5)
boost3 <- runboostRegCV(dat=newabcY,  no.tree=6000, k=5)
boost4 <- runboostRegCV(dat=newabcY,  no.tree=10000, k=5)
boost5 <- runboostRegCV(dat=newabcY,  no.tree=15000, k=5)

predboost1<-boost1[[2]]
predboost2<-boost2[[2]]
predboost3<-boost3[[2]]
predboost4<-boost4[[2]]
predboost5<-boost5[[2]]


##Tree

runTreeRegCV<- function(dat, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- tree(Target~., data=train[,5:35])  
    
    #####################################################################################################
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34]), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  rmse <- sqrt(mse)
  sol <- data.frame(K=k,mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse))
  return(list(sol, pred))
}
#@@ 5-fold CV
set.seed(666)
Tree <- runTreeRegCV(dat=newabcY, k=5)

predTree <- Tree[[2]]







###RandomForest
runRFRegCV <- function(dat, m, no.tree, k ,ntrace=500){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- randomForest(Target~., data=train[,5:35], importance=T, mtry=m, do.trace=ntrace, ntree=no.tree)  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(2,35)], Pred=predict(model,newdata=test[,5:34]), test[,c(36,37)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  m <- model$mtry  # get default value of mtry
  rmse <- sqrt(mse)
  sol <- data.frame(K=k, mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse), m=m, n.Tree=no.tree)
  return(list(sol, pred))
}

#@@ 5-fold CV 
set.seed(666)
rf <- runRFRegCV(dat=newabcY,  m=12, no.tree=1000, k=5)
predRF<- rf[[2]] 





#-------------------------------------------------------------------------------------------------------------------------
### Recover Curve
#-------------------------------------------------------------------------------------------------------------------------

qRecCurv <- function(x) {
  
  x <- as.data.frame(na.omit(x))
  
  n.row.x <- nrow(x)  
  n.col.x <- ncol(x)  
  
  ranks <- x %>% mutate_each(funs(row_number)) %>% arrange(desc(Target))  # ranks for each col and then ordered by 1st col(true value)
  
  rec.q <- data.frame(matrix(-1, nrow = n.row.x , ncol = n.col.x))  # recover quantiles
  rec.q[1,] <- (ranks[1,] == n.row.x)
  for (i in 2:n.row.x)
  {
    #rec.q[i,] <- ranks %>% slice(1:i) %>% summarise_each (funs(sum(.<=i)/i))
    rec.q[i,] <- ranks %>% slice(1:i) %>% summarise_each (funs(sum(.>=(n.row.x-i+1))/i))
  }
  names(rec.q)[1]<- "True"
  rec.q[,1]<-1:n.row.x/n.row.x
  
  #row.names(rec.q) <- sapply(100*(1:n.row.x)/n.row.x,  FUN = function(x) paste("P",round(x,digits = 0),sep = ""))
  
  return(rec.q)
}  


#@@ Comparison of different model
# Prediction of  models (30 vars)
pred.boost<-select(predboost5,Uwi, Target,boost=Pred)
pred.Tree <- select(predTree, Uwi, Tree=Pred)
pred.RF<-select(predRF, Uwi, RF=Pred)




jo <- left_join(pred.boost, pred.Tree, by="Uwi")
jo <- left_join(jo, pred.RF,by='Uwi')
jo <- jo[,-1]  # rm Uwi

q.rec <- qRecCurv(jo) * 100

# Round to integer percentage
index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
q.rec <- q.rec[index, ]

q.rec1 <- q.rec %>% select(True) %>% mutate(RecRate=True, Method="Baseline")
q.rec2 <- q.rec %>% select(True, X2) %>% rename(RecRate=X2) %>% mutate(Method="boost")
q.rec3 <- q.rec %>% select(True, X3) %>% rename(RecRate=X3) %>% mutate(Method="Tree")
q.rec4 <- q.rec %>% select(True, X4) %>% rename(RecRate=X4) %>% mutate(Method="RandomForest")


q.rec <- union(q.rec1, q.rec2)
q.rec <- union(q.rec, q.rec3)
q.rec <- union(q.rec, q.rec4)



ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
  geom_line(lwd=1.2) +
  scale_color_manual(values=c("#fe506e", "black", "#228b22", "#0099cc")) +
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

newbbcY<-inner_join(bbc,y2,by='Uwi')

#compare three methods

xiaoshiba<-function{
boost.Target1<-gbm(Target~.,data=TrainY[,2:32], distribution='gaussian',n.trees=5000,shrinkage=0.01)
boost.predict1<-predict(boost.Target1,newdata=TestY[,2:31],n.trees=5000)
boost.error1<-mean((boost.predict1-TestY$Target)^2)

boost.Target2<-gbm(Target~.,data=TrainY[,2:32], distribution='gaussian',n.trees=7000,shrinkage=0.01)
boost.predict2<-predict(boost.Target2,newdata=TestY[,2:31],n.trees=7000)
boost.error2<-mean((boost.predict2-TestY$Target)^2)

boost.Target3<-gbm(Target~.,data=TrainY[,2:32], distribution='gaussian',n.trees=9000,shrinkage=0.01)
boost.predict3<-predict(boost.Target3,newdata=TestY[,2:31],n.trees=9000)
boost.error3<-mean((boost.predict3-TestY$Target)^2)

boost.Target4<-gbm(Target~.,data=TrainY[,2:32], distribution='gaussian',n.trees=12000,shrinkage=0.01)
boost.predict4<-predict(boost.Target4,newdata=TestY[,2:31],n.trees=12000)
boost.error4<-mean((boost.predict4-TestY$Target)^2)

boost.Target5<-gbm(Target~.,data=TrainY[,2:32], distribution='gaussian',n.trees=15000,shrinkage=0.01)
boost.predict5<-predict(boost.Target5,newdata=TestY[,2:31],n.trees=15000)
boost.error5<-mean((boost.predict5-TestY$Target)^2)


Tree.Target<-tree(Target~.,data=TrainY[,2:32])
Tree.predict<-predict(Tree.Target,newdata=TestY[,2:31])
Tree.error<-mean((Tree.predict-TestY$Target)^2)

RF.Target<-randomForest(Target~.,data=TrainY[,2:32],ntree=1000,mtry=12)
RF.predict<-predict(RF.Target,newdata=TestY[,2:31])
RF.error<-mean((RF.predict-TestY$Target)^2)


Kaggle.error<-mean((TestY$Kaggle.Prediction-TestY$Target)^2)

c(boost.error1,boost.error2,boost.error3,boost.error4,boost.error5,Tree.error,RF.error,Kaggle.error)
}




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
  rmse <- sqrt(mse)
  sol <- data.frame(K=k,mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse),n.Tree=no.tree)
  return(list(sol, pred))
}
#@@ 5-fold CV
set.seed(666)
boost1 <- runboostRegCV(dat=newbbcY,  no.tree=1000, k=5)
boost2 <- runboostRegCV(dat=newbbcY,  no.tree=3000, k=5)
boost3 <- runboostRegCV(dat=newbbcY,  no.tree=6000, k=5)
boost4 <- runboostRegCV(dat=newbbcY,  no.tree=10000, k=5)
boost5 <- runboostRegCV(dat=newbbcY,  no.tree=15000, k=5)

predboost1<-boost1[[2]]
predboost2<-boost2[[2]]
predboost3<-boost3[[2]]
predboost4<-boost4[[2]]
predboost5<-boost5[[2]]


##Tree

runTreeRegCV<- function(dat, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- tree(Target~., data=train[,2:32])  
    
    #####################################################################################################
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(1,32)], Pred=predict(model,newdata=test[,2:31]), test[,c(33,34)])  # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  rmse <- sqrt(mse)
  sol <- data.frame(K=k,mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse))
  return(list(sol, pred))
}
#@@ 5-fold CV
set.seed(666)
Tree <- runTreeRegCV(dat=newbbcY, k=5)

predTree <- Tree[[2]]







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
  rmse <- sqrt(mse)
  sol <- data.frame(K=k, mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse), m=m, n.Tree=no.tree)
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
  


  
  pred.boost<-select(predboost5,Uwi, Target,boost=Pred)
  pred.Tree <- select(predTree, Uwi, Tree=Pred)
  pred.RF<-select(predRF, Uwi, RF=Pred)
  pred.kaggle <- select(newbbcY, Uwi, Rules.Prediction, Kaggle.Prediction)
  
  
  
  jo <- left_join(pred.boost, pred.Tree, by="Uwi")
  jo <- left_join(jo, pred.RF,by='Uwi')
  jo <- left_join(jo, pred.kaggle, by="Uwi")
  jo <- jo[,-1]  # rm Uwi
  
  q.rec <- qRecCurv(jo) * 100
  
  # Round to integer percentage
  index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
  q.rec <- q.rec[index, ]
  
  q.rec1 <- q.rec %>% select(True) %>% mutate(RecRate=True, Method="Baseline")
  q.rec2 <- q.rec %>% select(True, X2) %>% rename(RecRate=X2) %>% mutate(Method="boost")
  q.rec3 <- q.rec %>% select(True, X3) %>% rename(RecRate=X3) %>% mutate(Method="Tree")
  q.rec4 <- q.rec %>% select(True, X4) %>% rename(RecRate=X4) %>% mutate(Method="RandomForest")
  q.rec5 <- q.rec %>% select(True, X5) %>% rename(RecRate=X5) %>% mutate(Method="Rule Based")
  q.rec6 <- q.rec %>% select(True, X6) %>% rename(RecRate=X6) %>% mutate(Method="Kaggle")
  
  q.rec <- union(q.rec1, q.rec2)
  q.rec <- union(q.rec, q.rec3)
  q.rec <- union(q.rec, q.rec4)
  q.rec <- union(q.rec, q.rec5)
  q.rec <- union(q.rec, q.rec6)


  ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
    geom_line(lwd=1.2) +
    scale_color_manual(values=c("#fe506e", "black", "#228b22", "#0099cc", "#e95d3c","blue")) +
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
  
  
  
  
  
  
  
  
  
