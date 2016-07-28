ddir<-"inst/extdata/Raw/aquatroll/"
deep<-read.csv(paste(ddir,"AquatrollBS_DeepWellCorrected_2015-10-30_2015-04-20.csv",sep=""),skip=72)[,c(1,8)]

shallow<-read.csv(paste(ddir,"AquatrollBW_ShallowWellCorrected_2014-10-30_2015-04-20.csv",sep=""),skip=72)[,c(1,9)]

houragg<-function(x){
  #x<-deep
  names(x)<-c("datetime","depth")
  x$datetime<-strptime(x$datetime,format="%m/%d/%y %H:%M")
  x$datetime<-strftime(x$datetime,format="%Y-%m-%d %H:00:00")
  #x$datetime<-as.factor(x$datetime)
  x<-aggregate(. ~ datetime, data=x, mean)
  x[,1]<-as.POSIXct(x[,1])
  x
}

deep<-houragg(deep)
shallow<-houragg(shallow)

edent<-read.csv(paste(ddir,"eden_1431030671_water_level.csv",sep=""),header=T,skip=4)[,1:2]
names(edent)<-c("datetime","depth")
edent$depth<-edent$depth*30.48#ft to cm
edent[,"datetime"]<-as.POSIXct(edent[,"datetime"])
edent<-edent[!duplicated(edent$datetime),]

##read in manual onsite measurements (units in ft)####
# onsite<-read.table(text="
# 2014-10-15 12:00,0.65
# 2014-11-19 12:00,0.5
# 2015-01-13 12:00,0
# 2014-12-16 12:00,0.16",sep=",")
#names(onsite)<-c("datetime","depth")
#onsite$datetime<-as.POSIXct(strptime(onsite$datetime,format="%Y-%m-%d %H:%M"))
#onsitezoo<-zoo(onsite$depth,onsite$datetime)

#fit aquatroll to eden####
shallow<-zoo(shallow$depth,shallow$datetime)
edent<-zoo(edent$depth,edent$datetime)
allzoo<-merge(edent,shallow,all=FALSE)
fit<-lm(allzoo$shallow~allzoo$edent)

plot(predict(fit))
lines(as.numeric(allzoo$shallow),col="red")

#forecast aquatroll depth based on more recent eden####
#aqhi$depth<-aqhi$depth*fit[2]+fit[1]
