#get porewater data from xlsx
#system("xlsx2csv -s 2 PeatCollapse_LabData_Thru_Nov2014_clean.xlsx porewater_pc.csv")
dt<-read.table("labdata_field_pc.csv",header=TRUE,sep=",")[,1:7]
pw<-dt[dt$MATRIX=="PW",]
bw.pw<-pw[pw$SITE=="BW",]
fw.pw<-pw[pw$SITE=="FW",]
sw<-dt[dt$MATRIX=="SW",]
bw.sw<-sw[sw$SITE=="BW",]
fw.sw<-sw[sw$SITE=="FW",]
test.list<-unique(dt$TEST_NAME)
unit.list<-dt[match(test.list,dt$TEST_NAME),"UNITS"]
test.list.cl<-c("Alkalinity","Chloride","Dissolved Organic Carbon","pH","NH4","NOX","SO4","Total Dissolved Nitrogen")

#create a sheet of plots####
plot.pc.lab<-function(site.pwsw){
nm<-deparse(substitute(site.pwsw))
bmp(paste(getwd(),"/fig/",nm,".bmp",sep=""),width=760,height=800)
par(mfrow=c(4,2),mar=c(1,4.2,0.5,1),cex.axis=1.5,cex.lab=1.5)
layout(matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=T),widths=c(1,1),heights=c(1,1,1,1.2))
for(i in 1:(length(test.list))){
  test<-as.character(test.list[i])
  unit<-as.character(unit.list[i])
  curdata<-site.pwsw[site.pwsw$TEST_NAME==test,]
  if(i>6){
    par(mar=c(4,4.2,0.5,1))
    boxplot(curdata$VALUE~curdata$CHAMBER,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="Chamber #")
  }else{
  boxplot(curdata$VALUE~curdata$CHAMBER,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="Chamber #",xaxt="n")
  }
}
dev.off()
}
plot.pc.lab(bw.pw)
plot.pc.lab(fw.pw)
#plot.pc.lab(fw.sw)
#plot.pc.lab(bw.sw)

# create individual plots####
# #plot fw.pw
# for(i in 1:(length(test.list)-1)){
#   test<-as.character(test.list[i])
#   bmp(paste(getwd(),"/fig/","fw.pw",test,".bmp",sep=""))
#   curdata<-fw.pw[fw.pw$TEST_NAME==test,]
#   boxplot(curdata$VALUE~curdata$CHAMBER,ylab=test,xlab="Chamber #",main="Oct-Nov 2014")
#   dev.off()
# }
# #plot bw.sw
# for(i in 1:(length(test.list)-1)){
#   test<-as.character(test.list[i])
#   bmp(paste(getwd(),"/fig/","bw.sw",test,".bmp",sep=""))
#   curdata<-bw.sw[bw.sw$TEST_NAME==test,]
#   boxplot(curdata$VALUE~curdata$CHAMBER,ylab=test,xlab="Chamber #",main="Oct-Nov 2014")
#   dev.off()
# }
# #plot fw.sw
# for(i in 1:(length(test.list)-1)){
#   test<-as.character(test.list[i])
#   bmp(paste(getwd(),"/fig/","fw.sw",test,".bmp",sep=""))
#   curdata<-fw.sw[fw.sw$TEST_NAME==test,]
#   boxplot(curdata$VALUE~curdata$CHAMBER,ylab=test,xlab="Chamber #",main="Oct-Nov 2014")
#   dev.off()
# }