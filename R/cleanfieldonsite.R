#'@name cfieldonsite
#'@title Clean onsite data files
#'@param sumpathlist list of file paths
#'@param pwsw return only pw, sw, or all?
#'@description Removes entries tagged in the "timing.of.sample.with.dosing" field of anything other than "1 day post"

cfieldonsite<-function(sumpathlist,pwsw="all"){
  library(jsta)
  full<-list()
  
  for(i in 1:length(sumpathlist)){
    #i=1
    dt<-read.csv(sumpathlist[i],skip=1,stringsAsFactors=F)
    names(dt)<-tolower(names(dt))
    
    if(any(names(dt)=="timing.of.sample.with.dosing")){
      dt<-dt[dt$timing.of.sample.with.dosing=="1 day post",]
      dt<-dt[,-which(names(dt)=="timing.of.sample.with.dosing")]
    }
    
    dt<-dt[nchar(as.character(dt$chamber))<3,]
    names(dt)[c(1,4,5)]<-c("date","pwsw","trt")
    
    if(mean(nchar(dt$date))<8){
      dt$date <- sapply(dt$date,jsta::mdy2mmyyyy)
    }
    
    dt$date<-as.POSIXct(strptime(dt$date,"%m/%d/%Y"))
    if(pwsw=="pw"){
      dt<-dt[dt$pwsw=="PW",]
    }
    if(pwsw=="sw"){
      dt<-dt[dt$pwsw=="SW",]
    }
    suppressWarnings(dt[,11:14]<-apply(dt[,11:14],2,function(x) as.numeric(x)))
    suppressWarnings(dt[,"chamber"]<-sapply(dt[,"chamber"],function(x) as.numeric(x)))
  
    dtagg<-aggregate(dt[,11:14],by=list(dt$date,dt$chamber,dt$pwsw),function(x) mean(x,na.rm=T))
    #dtagg<-aggregate(dt,list(dt$date,dt$chamber,dt$pwsw),function(x) mean(x,na.rm=T))
    names(dtagg)[1:3]<-c("date","chamber","pwsw")
    names(dtagg)<-tolower(names(dtagg))
    dtagg<-dtagg[order(dtagg$date),]
    dtagg$site<-paste(substring(sumpathlist[i],49,49),"W",sep="")
    full[[i]]<-dtagg    
  }
  do.call(rbind,full)
  
  
}