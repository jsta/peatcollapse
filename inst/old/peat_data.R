#graph project data

site<-"brackish"
dt<-read.csv(paste("peatcollapse_",site,".csv",sep=""))

boxplot(dt$Salinity~dt$Chamber,outline=FALSE,xlab="Chamber",ylab="Salinity",main=paste(site,"water ","site ","porewater",sep=""))
boxplot(dt$Temp~dt$Chamber,outline=FALSE,xlab="Chamber",ylab="Salinity",main=site)
plot(dt$Cond,dt$Salinity)