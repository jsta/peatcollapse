#list.files()
#dt<-read.csv("20141226_SGPeatCollapse_BrackishWFieldData.csv")[,1:7]
dt<-read.csv("20141226_SGPeatCollapse_FreshWFieldData.csv")[,1:7]
library(reshape2)
dt.melt<-melt(dt,id=c("Chamber","Sipper"))

test.list<-unique(dt.melt$variable)[3:5]
test.list.cl<-c("Temperature (C)","Conductivity","Salinity")

for(i in 1:(length(test.list))){
  test<-as.character(test.list[i])
  curdata<-dt.melt[dt.melt$variable==test,]
  curdata$value<-as.numeric(curdata$value)
  #if(i>6){
    #par(mar=c(4,4.2,0.5,1))
    boxplot(curdata$value~curdata$Chamber,ylab=test.list.cl[i],xlab="Chamber #",outline=FALSE)
}