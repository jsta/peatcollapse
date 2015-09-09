##read data from fieldpilot1_data.xlsx####
#the T2 field was renumbered for the freshwater site to go from 0-8 instead of repeating 1-3
#uncomment below to install the lattice package
#install.packages("lattice")

dt<-read.table("clipboard",header=T,na.strings="NA")
brackish<-dt[dt$site=="brackish",]
freshwater<-dt[dt$site=="freshwater",]

#remove surface water and outside mesocosm (X) data
brackishpw<-brackish[brackish$sampletype!="surfacewater",]
freshwaterpw<-freshwater[freshwater$sampletype!="surfacewater",]

brackishout<-brackish[brackish$quad=="outside",]
freshwaterout<-freshwater[freshwater$quad=="outside",]

freshwatersw<-freshwater[freshwater$quad=="inside",]
brackishsw<-brackish[brackish$quad=="inside",]
           
#boxplots of pw data           
boxplot(brackishpw$sal~brackishpw$T,xlab="Time",ylab="Salinity",main="Brackish Site PW")
boxplot(freshwaterpw$sal~freshwaterpw$T,xlab="Time",ylab="Salinity",main="Freshwater Site PW")

#histograms of outside pw
hist(freshwaterout$sal,xlab="Salinity",main="freshwater site Outside")
hist(brackishout$sal,xlab="Salinity",main="brackish site Outside")

#time series of pw
library(lattice)
xyplot(brackishpw$sal~brackishpw$T|brackishpw$quad,xlab="Time",ylab="Salinity",main="Brackish Site PW",group=brackishpw$num)
xyplot(freshwaterpw$sal~freshwaterpw$T|freshwaterpw$quad,xlab="Time",ylab="Salinity",main="Freshwater Site PW",group=freshwaterpw$num)
#sw
xyplot(freshwaterout$sal~freshwaterout$T|freshwaterout$quad,xlab="Time",ylab="Salinity",main="freshwater Site Outside",group=freshwaterout$num)
xyplot(brackishsw$sal~brackishsw$T|brackishsw$quad,xlab="Time",ylab="Salinity",main="brackish Site",group=brackishsw$num)
xyplot(freshwatersw$sal~freshwatersw$T,xlab="Time",ylab="Salinity",main="freshwater Site Surface Water",group=freshwatersw$num)
#boxplot(freshwatersw$sal~freshwatersw$num,main="freshwater site Surface Water",ylab="Salinity")
#boxplot(brackishsw$sal~brackishsw$num,main="freshwater site Surface Water",ylab="Salinity")

