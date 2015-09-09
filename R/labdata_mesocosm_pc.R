#get porewater data from xlsx
#system("xlsx2csv -s 1 PeatCollapse_LabData_Thru_Nov2014_clean.xlsx labdata_mesocosm_pc.csv")
#system("xlsx2csv -s 2 PeatCollapse_LabData_Thru_Nov2014_clean.xlsx labdata_field_pc.csv")
dt<-read.table("inst/extdata/Raw/old/labdata_mesocosm_pc.csv",header=TRUE,sep=",")[,1:8]
#dt<-read.table("labdata_field_pc.csv",header=TRUE,sep=",")[,1:7]
pw<-dt[dt$SWPW=="PW",]
sw<-dt[dt$SWPW=="SW",]

#load treatment keys
key<-read.table(header=FALSE,text="
1 elevinun                    
3 elevinun
10 elevinun
12 elevinun
17 elevinun
19 elevinun
2 elevcont
4 elevcont
9 elevcont
11 elevcont
18 elevcont
20 elevcont
6 ambinun
8 ambinun
14 ambinun
16 ambinun
22 ambinun
24 ambinun
5 ambcont
7 ambcont
13 ambcont
15 ambcont
21 ambcont
23 ambcont
")

swkey<-read.table(header=FALSE,text="
1 elev
2 amb
3 elev
4 amb
5 elev
6 amb
")

#####
names(key)<-c("CORE","TRT")
names(swkey)<-c("CRYPT","TRT")
pw<-merge(key,pw)
sw<-merge(swkey,sw)

test.list<-unique(dt$TEST_NAME)[c(1:6,8:9)]
unit.list<-dt[match(test.list,dt$TEST_NAME),"UNITS"]
test.list.cl<-c("Alkalinity","Chloride","Dissolved Organic Carbon","pH","NH4","NOX","SO4","Total Dissolved Nitrogen")
#site.pwsw<-pw


#time series plots
plot.ts.lab<-function(site.pwsw,unit.list,test.list.cl){
  #site.pwsw<-pw
  treat.list<-unique(site.pwsw$TRT)
  test.list<-unique(site.pwsw$TEST_NAME)
  unit.list<-site.pwsw[match(test.list,site.pwsw$TEST_NAME),"UNITS"]
  test.list.cl<-c("Alkalinity","NOX","Dissolved Organic Carbon","Total Dissolved Nitrogen","NH4","SO4","pH","Salinity","Chloride")
  
  #format dates
  site.pwsw$COLLECT_DATE<-as.POSIXct(strptime(site.pwsw$COLLECT_DATE,"%m/%d/%Y %H:%M"))
  site.pwsw$COLLECT_DATE<-as.POSIXct(strptime(strftime(site.pwsw$COLLECT_DATE,"%Y/%m"),"%Y/%m"))
    site.pwsw$COLLECT_DATE.fac<-strftime(site.pwsw$COLLECT_DATE,"%Y/%m")
  site.pwsw$COLLECT_DATE.fac<-factor(site.pwsw$COLLECT_DATE.fac,ordered=TRUE)

  #loop through parameters
  for(i in 1:(length(test.list))){
    #i<-5
    bmp(paste(getwd(),"/fig/",test.list[i],".bmp",sep=""),width=760,height=760)
    par(mfrow=c(2,2),mar=c(1,4.2,0.5,1),cex.axis=1.5,cex.lab=1.5)
    layout(matrix(c(1,2,3,4),2,2,byrow=T),widths=c(1,1),heights=c(1,1.2))
    test<-as.character(test.list[i])
    unit<-as.character(unit.list[i])
    curdata<-site.pwsw[site.pwsw$TEST_NAME==test,]
    tick.space<-signif(((range(curdata$VALUE)[2]-range(curdata$VALUE)[1])/5),1)
    tick.start<-signif(range(curdata$VALUE)[1],2)
    tick.end<-signif(range(curdata$VALUE)[2],3)
    tick.loc<-seq(from=tick.start,to=tick.end,by=tick.space)
    for(j in 1:length(unique(treat.list))){
        treat<-treat.list[j]
        curdata2<-curdata[curdata$TRT==treat,]
        sampling.key<-data.frame(cbind(seq(from=1,to=length(levels(unique(curdata2$COLLECT_DATE.fac))),by=1),levels(unique(curdata2$COLLECT_DATE.fac))))
        names(sampling.key)<-c("Sampling","COLLECT_DATE.fac")
        curdata3<-merge(sampling.key,curdata2)
        curdata3$Sampling<-as.numeric(curdata3$Sampling)
        
        
        curdata.mean<-aggregate(curdata3$VALUE,by=list(curdata3$Sampling),mean)[,2]
        curdata.sd<-aggregate(curdata3$VALUE,by=list(curdata3$Sampling),sd)[,2]
        n<-aggregate(curdata3$VALUE,by=list(curdata3$Sampling),length)[,2]
        
        x<-1:length(curdata.mean)
        
        #start plotting####
        if(j%%2==0&&j<3){
          plot(x,curdata.mean,axes=FALSE,ylim=c(tick.start,tick.end),main=treat,ylab="",xlab="",col="black",pch=16,cex=1.5)
          epsilon=0.08
          segments(x,curdata.mean-curdata.sd,x,curdata.mean+curdata.sd)
          segments(x-epsilon,curdata.mean-curdata.sd,x+epsilon,curdata.mean-curdata.sd)
          segments(x-epsilon,curdata.mean+curdata.sd,x+epsilon,curdata.mean+curdata.sd)
          axis(2,at=tick.loc)#chart 2
          
        }else{
          if(j%%2==0&&j>2){
            par(mar=c(7.5,4.2,0.5,1))
            plot(x,curdata.mean,axes=FALSE,ylim=c(tick.start,tick.end),main=treat,ylab="",xlab="",col="black",pch=16,cex=1.5)
            epsilon=0.08
            segments(x,curdata.mean-curdata.sd,x,curdata.mean+curdata.sd)
            segments(x-epsilon,curdata.mean-curdata.sd,x+epsilon,curdata.mean-curdata.sd)
            segments(x-epsilon,curdata.mean+curdata.sd,x+epsilon,curdata.mean+curdata.sd)
            axis(1,at=1:length(levels(unique(curdata3$COLLECT_DATE.fac))),labels=levels(unique(curdata3$COLLECT_DATE.fac)),las=2)#chart 4
                                  }else{
            if(j==1){
              plot(x,curdata.mean,axes=FALSE,ylim=c(tick.start,tick.end),main=treat,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="",col="black",pch=16,cex=1.5)
              epsilon=0.08
              segments(x,curdata.mean-curdata.sd,x,curdata.mean+curdata.sd)
              segments(x-epsilon,curdata.mean-curdata.sd,x+epsilon,curdata.mean-curdata.sd)
              segments(x-epsilon,curdata.mean+curdata.sd,x+epsilon,curdata.mean+curdata.sd)
              axis(2,at=tick.loc,tick.loc)#chart 1
              }else{
              par(mar=c(7.5,4.2,0.5,1))
              plot(x,curdata.mean,axes=FALSE,ylim=c(tick.start,tick.end),main=treat,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="",col="black",pch=16,cex=1.5)
              epsilon=0.08
              segments(x,curdata.mean-curdata.sd,x,curdata.mean+curdata.sd)
              segments(x-epsilon,curdata.mean-curdata.sd,x+epsilon,curdata.mean-curdata.sd)
              segments(x-epsilon,curdata.mean+curdata.sd,x+epsilon,curdata.mean+curdata.sd)
              axis(1,at=1:length(levels(unique(curdata3$COLLECT_DATE.fac))),labels=levels(unique(curdata3$COLLECT_DATE.fac)),las=2)
              axis(2,at=tick.loc,tick.loc)#chart 3
            
            }
            }
        }
    }
  dev.off()
  }
}
  
#plot.ts.lab(pw)

#create a sheet of boxplots####
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
    boxplot(curdata$VALUE~curdata$TRT,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="Chamber #",outline=FALSE)
  }else{
  boxplot(curdata$VALUE~curdata$TRT,ylab=paste(test.list.cl[i]," (",unit,")",sep=""),xlab="Chamber #",xaxt="n",outline=FALSE)
  }
}
dev.off()
}
plot.pc.lab(pw)
plot.pc.lab(sw)


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