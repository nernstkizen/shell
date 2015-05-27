######################################################################################################################
# Load Data
######################################################################################################################

#---------------------------------------------------------------------------------------------------------------------
### Data path
#---------------------------------------------------------------------------------------------------------------------
wd <- getwd()
setwd(file.path(repo_path, "/data"))
#---------------------------------------------------------------------------------------------------------------------
### Load data
#---------------------------------------------------------------------------------------------------------------------
#@@corewell GeoChem
data1<-read.csv('031_Core_GeoChem.csv',header=TRUE,as.is=TRUE)
data1<-data1[-1,]

newdata1<-select(data1,UWI=Unique.Private.Well.ID, latitude=Well.Latitude, longitude=Well.Longitude
                 ,Tmax=Tmax..degrees.C., S2=Hydrocarbon...S2..mg.g., Romeasure=Ro.Measured..percent.)
newdata1<-arrange(newdata1,UWI)
#newdata1[, 2:6] <- lapply(newdata[,2:6], as.numeric)
newdata1[,2]<-as.numeric(newdata1[,2])
newdata1[,3]<-as.numeric(newdata1[,3])
newdata1[,4]<-as.numeric(newdata1[,4])
newdata1[,5]<-as.numeric(newdata1[,5])
newdata1[,6]<-as.numeric(newdata1[,6])
newdata1<-group_by(newdata1,UWI,latitude,longitude)



#@@corewell RCA
#data2<-read.csv('032_Core_RCA.csv',header=TRUE)



#@@corewell ShaleGas
data3<-read.csv('033_Core_ShaleGas.csv',header=TRUE,as.is=TRUE)
data3<-data3[-1,]



newdata3<-select(data3,UWI=Unique.Private.Well.ID, latitude=Well.Latitude, longitude=Well.Longitude,
                 ClayChlo=XRD.Clay.Fraction.Chlorite..weight.percent.,
                 waterporosity=GRI.Water.Filled.Porosity..percent.)
newdata3<-arrange(newdata3,UWI)
newdata3[,2]<-as.numeric(newdata3[,2])
newdata3[,3]<-as.numeric(newdata3[,3])
newdata3[,4]<-as.numeric(newdata3[,4])
newdata3[,5]<-as.numeric(newdata3[,5])
newdata3<-group_by(newdata3,UWI,latitude,longitude)


#@@corewell SCAL
#data4<-read.csv('034_Core_SCAL.csv',header=TRUE)


#@@Production well location + Prod start time + true depth
a <- read.csv("012_Prod_Well.csv", as.is=T)
a <- a %>% select(Entity, API, Surface.Latitude, Surface.Longitude) %>% filter(!is.na(API),!is.na(Surface.Latitude),!is.na(Surface.Longitude))  # Location

b <- read.csv("013_Prod_Header.csv", as.is=T)
b <- b %>% select(Entity, Date.Production.Start) %>% filter(!is.na(Date.Production.Start))  # Prod start date

ab <- left_join(a, b, by="Entity")
ab <- ab %>% distinct(API) %>% rename(Uwi=API, Latitude=Surface.Latitude, Longitude=Surface.Longitude)
ab$Date.Production.Start <- as.Date(ab$Date.Production.Start, format="%Y-%m-%d")
prod.date.loc <- ab

c<-read.csv("020_Well_Header.csv",as.is=T)
c<-c %>% distinct(UWI) %>% select (Uwi=UWI,Depth.True.Vertical)

abc<- inner_join(ab,c,by='Uwi')
abc<-abc[,-5]

setwd(file.path(repo_path, "/data/Kaggle/Final/RulesBasedApproach Oct 8/RulesBasedApproach Oct 8"))
y <- read.csv("Rules features using recent and Jan 2014 data.csv")
y <- select(y,Uwi)  # 2631 x 1
abc<-inner_join(abc,y,by='Uwi')










#---------------------------------------------------------------------------------------------------------------------
### Reset working dir
#---------------------------------------------------------------------------------------------------------------------
setwd(wd)

