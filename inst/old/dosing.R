dt<-read.csv("inst/extdata/Raw/dosingdata.csv")[,-8]
library(zoo)

yr<-substring(dt$date,1,4)
mon<-substring(dt$date,5,6)
dt$datetime<-paste(yr,"-",mon,"-01",sep="")


dt$datetime<-as.POSIXct(dt$datetime)

bw<-dt[dt$site=="bw",]
par(mar=c(5,4,4,5)+.1)
plot(bw$datetime,na.locf(bw[,3]),type="b",col="red",cex=2,xlab="",ylab="Water Level (m)",main="Brackish Site")
par(new=TRUE)
plot(bw$datetime,na.approx(bw[,7]),type="b",cex=2,yaxt="n",xaxt="n",xlab="",ylab="")
axis(4)
mtext("Dose Salt Mass",side=4,line=3)
legend("topright",col=c("red","black"),lty=1,legend=c("WL","DS"))

fw<-dt[dt$site=="fw",]
plot(fw$datetime,na.approx(fw[,4]),type="b",col="red",cex=2,xlab="",ylab="Water Level (m)",main="Freshwater Site")


par(mfrow=c(1,2))

plot(as.POSIXct(bw$datetime),cumsum(bw[,"salt_c"]),ylab="Cumulative Salt Added (cups)",type="b",xlab="",ylim=c(30,170),main="Brackish")

plot(as.POSIXct(fw$datetime),cumsum(fw[,"salt_c"]),ylab="Cumulative Salt Added (cups)",type="b",xlab="",ylim=c(30,170),main="Freshwater")





