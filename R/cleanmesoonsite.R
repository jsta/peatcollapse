#cleanmesoonsite
#'@name cmesoonsite
#'@param sumpathlist list of file paths
#'@param pwsw return only pw, sw, or all?
#'@description Removes entries tagged in the "timing.of.sample.with.dosing" field of anything other than "1 day post"

cmesoonsite<-function(sumpathlist,pwsw="all"){
  library(jsta)
  full<-list()
  
  for(i in 1:length(sumpathlist)){
    #i=1
    dt<-read.csv(sumpathlist[i],skip=1,stringsAsFactors=F)
    names(dt)<-tolower(names(dt))
    
    if(any(names(dt)=="exp")){
      dt<-dt[dt$exp!="Source",]
    }
    
    dt$sampling.date <- sapply(dt$sampling.date,jsta::mdy2mmyyyy)
    
    names(dt)[names(dt)=="sampling.date"]<-"date"
    dt$date<-as.POSIXct(strptime(dt$date,"%m/%d/%Y"))
    
    names(dt)[names(dt)=="core.."]<-"chamber"
    dt$chamber<-suppressWarnings(as.integer(gsub("/","",dt$chamber)))
    names(dt)[names(dt)=="sample.source"]<-"pwsw"
    names(dt)[names(dt)=="crypt.tank.."]<-"site"
    
    if(pwsw=="pw"){
      dt<-dt[dt$pwsw=="PW",]
    }
    if(pwsw=="sw"){
      dt<-dt[dt$pwsw=="SW",]
    }
    
    
    
    suppressWarnings(dt[,11:14]<-apply(dt[,11:14],2,function(x) as.numeric(x)))
    
    #head(clab[,c("site","pwsw","date","chamber")])
    key<-read.table(text="
PW,1
SW,2",sep=",")
    
    names(key)<-c("pwsw","pwswc")
    dt<-merge(key,dt)
    dt$chamber[is.na(dt$chamber)]<-999
    
    dtagg<-aggregate(dt[,12:15],by=list(dt$date,dt$chamber,dt$pwswc,dt$site),function(x) mean(x,na.rm=T))
    
    names(dtagg)[1:4]<-c("date","chamber","pwswc","site")
    dtagg<-merge(key,dtagg)
    dtagg$chamber[dtagg$chamber==999]<-NA
    dtagg<-dtagg[,-1]
    
    dtagg<-dtagg[with(dtagg,order(pwsw,date,chamber,site)),]
#     dtagg$site<-paste(substring(sumpathlist[i],49,49),"W",sep="")
     full[[i]]<-dtagg    
  }
  do.call(rbind,full)
  
  
}
  
  
