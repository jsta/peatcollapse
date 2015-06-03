library(zoo)

#list.files("inst/extdata/Raw/")

eden<-read.csv("inst/extdata/Raw/eden_1431030671_water_level.csv",header=T,skip=4)[,1:2]
names(eden)<-c("datetime","depth")
eden[,"datetime"]<-as.POSIXct(eden[,"datetime"])
eden<-eden[!duplicated(eden$datetime),]

aqhi<-read.csv("inst/extdata/Raw/AquatrollBW_ShallowWellCorrected_2015-10-30_2015-04-20.csv",header=F,skip=73)[,c(1,9)]
aqlow<-read.csv("inst/extdata/Raw/Aquatroll_Deep Well_10302014_04202015.csv",header=F,skip=73)[,c(1,3)]
names(aqhi)<-names(aqlow)<-c("datetime","depth")

aqhi$datetime<-as.POSIXct(strptime(aqhi$datetime,format="%m/%d/%y %H:%M"))
aqlow$datetime<-as.POSIXct(strptime(aqlow$datetime,format="%m/%d/%y %H:%M"))
aqhi<-aqhi[!duplicated(aqhi$datetime),]
aqlow<-aqlow[!duplicated(aqlow$datetime),]

aqlow$datetime<-as.POSIXct(strftime(aqlow$datetime,format="%Y-%m-%d %H:00"))
aqhi$datetime<-as.POSIXct(strftime(aqhi$datetime,format="%Y-%m-%d %H:00"))

aqlow<-aggregate(aqlow,by=list(aqlow$datetime),mean)
aqhi<-aggregate(aqhi,by=list(aqhi$datetime),mean)

#units in ft
onsite<-read.table(text="
2014-10-15 12:00,0.65
2014-11-19 12:00,0.5
2015-01-13 12:00,0
2014-12-16 12:00,0.16",sep=",")

names(onsite)<-c("datetime","depth")
onsite$datetime<-as.POSIXct(strptime(onsite$datetime,format="%Y-%m-%d %H:%M"))
onsitezoo<-zoo(onsite$depth,onsite$datetime)
aqhizoo<-zoo(aqhi$depth,aqhi$datetime)
onsitezoo<-merge(aqhizoo,onsitezoo)
onsitezoo<-onsitezoo[!is.na(onsitezoo$onsitezoo)]
fit<-lm(onsitezoo$onsitezoo~onsitezoo$aqhizoo)$coefficients

aqhi$depth<-aqhi$depth*fit[2]+fit[1]


edenzoo<-zoo(eden$depth,eden$datetime)
aqhizoo<-zoo(aqhi$depth,aqhi$datetime)
#aqlowzoo<-zoo(aqlow$depth,aqlow$datetime)

allzoo<-merge(edenzoo,aqhizoo,all=FALSE)
fit<-lm(allzoo$aqhizoo~allzoo$edenzoo)$coefficients

edenzoo<-edenzoo*fit[2]+fit[1]-0.32

