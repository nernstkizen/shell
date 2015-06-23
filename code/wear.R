######################################################################################################################
# Header file
######################################################################################################################

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
library(geoR)
library(fields)
library(ggplot2)
library(directlabels)
library(randomForest)
library(gbm)
library(cvTools)
library(caret)
library(MASS)
library(rpart2)
#---------------------------------------------------------------------------------------------------------------------
### Project folder path
#---------------------------------------------------------------------------------------------------------------------
#repo_path = "C:/Apps/projects/xxx"
repo_path = "Z:/project"



#---------------------------------------------------------------------------------------------------------------------
### Data path
#---------------------------------------------------------------------------------------------------------------------
wd <- getwd()
setwd(file.path(repo_path, "/data"))




#---------------------------------------------------------------------------------------------------------------------
### Load data
#---------------------------------------------------------------------------------------------------------------------
#@@Wear data
weardata<-read.csv('wear.csv',header=TRUE,as.is=TRUE)


#---------------------------------------------------------------------------------------------------------------------
### Reset working dir
#---------------------------------------------------------------------------------------------------------------------
setwd(wd)



#Just for convenience
#SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P     

#RF


#Wear 600

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 2,
  repeats=1)


rfGrid <- expand.grid(mtry=c(6,7))

rfFit.600<- train(wear600 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                  data = weardata,
                  method = "rf",
                  tuneGrid=rfGrid,
                  trControl = fitControl,
                  verbose = FALSE,
                  importance=TRUE,
                  ntree=200)
plot(varImp(rfFit.600),lwd=20)



predict(rfFit.600,weardata[100,])
#Wear 630


fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=2)


rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7))

rfFit.630<- train(wear630 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "rf",
                    tuneGrid=rfGrid,
                    trControl = fitControl,
                    verbose = FALSE,
                    importance=TRUE)
plot(varImp(rfFit.630))



#wear 700

fitControl <- trainControl(## 5-fold CV
  method ="repeatedcv",
  number = 5,
  repeats=2)
 

rfGrid <- expand.grid(mtry=c(6,7))

rfFit.700<- train(wear700 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "rf",
                    tuneGrid=rfGrid,
                    trControl = fitControl,
                    verbose = FALSE,
                    importance=TRUE)

plot(varImp(rfFit.700))





#Boosting


#Wear 600

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=2)

gbmGrid <- expand.grid(interaction.depth=4,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.600<- train(wear600 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)
plot(varImp(gbmFit.600))

#Wear 630

fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=2)

gbmGrid <- expand.grid(interaction.depth=4,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.630<- train(wear630 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)


plot(varImp(gbmFit.630))





#wear 700


fitControl <- trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  repeats=2)

gbmGrid <- expand.grid(interaction.depth=4,n.trees = 2000, shrinkage=0.01, n.minobsinnode=10)

gbmFit.700<- train(wear700 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
                    data = weardata,
                    method = "gbm",
                    tuneGrid=gbmGrid,
                    trControl = fitControl,
                    verbose = FALSE)


plot(varImp(gbmFit.700))


gbm(wear700 ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P, 
    data=weardata, n.trees=2000, shrinkage=0.01,distribution='gaussian',interaction.depth=4,
    class.stratify.cv=TRUE)  





#Multivariate Regression



#Wear 600

set.seed(777)
n<-dim(weardata)[1]
R2<-rep(0,100)
result600<-rep(0,100)
for (i in 1:100)
{

n1<-base::sample(1:n,size=ceiling(n*0.8))
lm600<-lm(log(wear600+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
          data = weardata[n1,])
lm600 <- stepAIC(lm600, direction="both")
R2[i]<-summary(lm600)$r.squared
prelm600<-exp(predict(lm600,newdata=weardata[-n1,6:12]))-1
trulm600<-weardata[-n1,3]
result600[i]<-sqrt(mean((prelm600-trulm600)^2))
}
mean(R2)
sd(R2)
mean(result600)
sd(result600)
#Calculate the final expression
lm600<-lm(log(wear600+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
          data = weardata)

lm600 <- stepAIC(lm600, direction="both")






#Wear 600  (Stratified Cross validation)


n<-dim(weardata)[1]
result600<-rep(0,100)
for (i in 1:100)
{
  
  
  n11<-base::sample(which(weardata$wear600<0.5),size=ceiling(0.8*length(which(weardata$wear600<0.5))))
  n12<-base::sample(which(weardata$wear600>0.5&weardata$wear600<1.5),size=ceiling(0.8*length(which(weardata$wear600>0.5&weardata$wear600<1.5))))
  n13<-base::sample(which(weardata$wear600>1.5),size=ceiling(0.8*length(which(weardata$wear600>1.5))))
  n1<-c(n11,n12,n13)
  lm600<-lm(log(wear600+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
            data = weardata[n1,])
  lm600 <- stepAIC(lm600, direction="both")
  prelm600<-exp(predict(lm600,newdata=weardata[-n1,6:12]))-1
  trulm600<-weardata[-n1,3]
  result600[i]<-sqrt(mean((prelm600-trulm600)^2))
  
}



#Wear 630(Adapted)(not good)
wearnewdata<-weardata
wearnewdata[,6:10]<-log(weardata[,6:10]+0.1)
n<-dim(wearnewdata)[1]
result630<-rep(0,100)
for (i in 1:100)
{
  
  
  n1<-base::sample(1:n,size=ceiling(n*0.8))
  lm630<-lm(log(wear630+0.1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P,
            data = wearnewdata[n1,])
  lm630 <- stepAIC(lm630, direction="both")
  prelm630<-exp(predict(lm630,newdata=wearnewdata[-n1,6:12]))-0.1
  trulm630<-wearnewdata[-n1,3]
  result630[i]<-sqrt(mean((prelm630-trulm630)^2))
}





#wear 630

set.seed(777)
n<-dim(weardata)[1]
result630<-rep(0,100)
R2<-rep(0,100)
for (i in 1:100)
{
  
  n1<-base::sample(1:n,size=ceiling(n*0.8))
  lm630<-lm(log(wear630+2) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
            data = weardata[n1,])
  lm630 <- stepAIC(lm630, direction="both")
  R2[i]<-summary(lm630)$r.squared
  prelm630<-exp(predict(lm630,newdata=weardata[-n1,6:12]))-2
  trulm630<-weardata[-n1,3]
  result630[i]<-sqrt(mean((prelm630-trulm630)^2))

}

mean(R2)
sd(R2)
mean(result630)
sd(result630)




#Calculate the final expression
lm630<-lm(log(wear630+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
          data = weardata)

lm630 <- stepAIC(lm630, direction="both")





#wear 700

set.seed(777)
n<-dim(weardata)[1]
result700<-rep(0,100)
R2<-rep(0,100)
for (i in 1:100)
{
  
  n1<-base::sample(1:n,size=ceiling(n*0.8))
  lm700<-lm(log(wear700+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
            data = weardata[n1,])
  lm700 <- stepAIC(lm700, direction="both")
  R2[i]<-summary(lm700)$r.squared
  prelm700<-exp(predict(lm700,newdata=weardata[-n1,6:12]))-1
  trulm700<-weardata[-n1,3]
  result700[i]<-sqrt(mean((prelm700-trulm700)^2))
}

plot(log(weardata$wear700+1)~weardata$SlurryVelocity)



mean(R2)
sd(R2)
mean(result700)
sd(result700)

#Calculate the final expression
lm700<-lm(log(wear700+1) ~ SlurryVelocity+BitumenWtPct+SolidsConcWt+UltraFinesPct+FinesPct+d50+P ,
          data = weardata)
lm700 <- stepAIC(lm700, direction="both")



