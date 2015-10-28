#'@name get_mesoonsite
#'@title Get mesocosm onsite data
#'@param experiment character choice of "soilplant" or "soil"
#'@param onsitepath character file.path to onsite raw data
#'@export
get_mesoonsite <- function(onsitepath = file.path("Raw", "onsite"), experiment = "SoilPlant"){
  
    flist <- list.files(onsitepath, full.names = TRUE, include.dirs = TRUE)
    flist <- flist[grep(tolower(experiment), tolower(flist))]
    
    dtpath <- flist[which.max(as.numeric(substring(unlist(lapply(strsplit(flist,"/"), function(x) unlist(x[3]))), 1, 8)))]
    
  #'@name cmesoonsite
  #'@param sumpathlist list of file paths
  #'@param pwsw return only pw, sw, or all?
  #'@description Removes entries tagged in the "timing.of.sample.with.dosing" field of anything other than "1 day post"
  cmesoonsite<-function(sumpathlist, pwsw = "all"){
    
    full<-list()
    
    for(i in 1:length(sumpathlist)){
      #i=1
      dt<-read.csv(sumpathlist[i],skip=1,stringsAsFactors=F)
      names(dt)<-tolower(names(dt))
      
      if(any(names(dt)=="exp")){
        dt<-dt[dt$exp!="Source",]
      }
      
      dt$sampling.date <- sapply(dt$sampling.date, mdy2mmyyyy)
      
      names(dt)[names(dt)=="sampling.date"]<-"date"
      dt$date<-as.POSIXct(strptime(dt$date,"%m/%d/%Y"))
      
      names(dt)[names(dt)=="core.."]<-"core"
      dt$core<-suppressWarnings(as.integer(gsub("/","",dt$core)))
      names(dt)[names(dt)=="sample.source"]<-"pwsw"
      names(dt)[names(dt)=="crypt.tank.."]<-"crypt"
      
      if(pwsw=="pw"){
        dt<-dt[dt$pwsw=="PW",]
      }
      if(pwsw=="sw"){
        dt<-dt[dt$pwsw=="SW",]
      }
      
      suppressWarnings(dt[,11:14]<-apply(dt[,11:14],2,function(x) as.numeric(x)))
      
      key<-read.table(text="
PW,1
SW,2",sep=",", stringsAsFactors = FALSE)
      
      names(key) <- c("pwsw", "pwswc")
      dt<-merge(key, dt)
      dt$core[is.na(dt$core)] <- 999
      
      dtagg<-aggregate(dt[,12:15],by=list(dt$date,dt$core,dt$pwswc,dt$crypt),function(x) mean(x,na.rm=T))
      
      names(dtagg)[1:4]<-c("date","core","pwswc","crypt")
      dtagg<-merge(key,dtagg)
      dtagg$core[dtagg$core==999]<-NA
      dtagg<-dtagg[,-1]
      
      dtagg<-dtagg[with(dtagg,order(pwsw,date,core,crypt)),]
      full[[i]]<-dtagg    
    }
    do.call(rbind,full)
  }
  
  cmesoonsite(dtpath)
}

#'@name get_fieldonsite
#'@title Get field onsite data
#'@param onsitepath character file.path to folder containing raw onsite data files
#'@export
#'@examples \dontrun{
#'fieldonsite <- get_fieldonsite(onsitepath = file.path("Raw", "onsite"))
#'}
get_fieldonsite <- function(onsitepath = file.path("Raw", "onsite")){
  
  flist <- list.files(onsitepath, full.names = TRUE, include.dirs = TRUE)
  fwpath <- flist[grep(tolower("FreshWFieldData"), tolower(flist))]
  fwpath <- fwpath[which.max(as.numeric(substring(unlist(lapply(strsplit(fwpath,"/"), function(x) unlist(x[3]))), 1, 8)))]
    
  bwpath <- flist[grep(tolower("BrackishWFieldData"), tolower(flist))]
  bwpath <- bwpath[which.max(as.numeric(substring(unlist(lapply(strsplit(bwpath,"/"), function(x) unlist(x[3]))), 1, 8)))]
  
  #browser()
    
  cfieldonsite<-function(sumpathlist, pwsw="all"){
      full<-list()
      
      for(i in 1:length(sumpathlist)){
        dt <- read.csv(sumpathlist[i], skip = 1, stringsAsFactors = F)
        names(dt) <- tolower(names(dt))
        
        if(any(names(dt) == "timing.of.sample.with.dosing")){
          dt <- dt[dt$timing.of.sample.with.dosing == "1 day post"|dt$timing.of.sample.with.dosing=="",]
          dt<-dt[,-which(names(dt)=="timing.of.sample.with.dosing")]
        }
        
        dt<-dt[nchar(as.character(dt$chamber))<3,]
        names(dt)[c(1,4,5)]<-c("date","pwsw","trt")
        
        if(mean(nchar(dt$date))<8){
          dt$date <- sapply(dt$date, mdy2mmyyyy)
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
        
        dt$inout <- NA
        dt[dt$sipper < 3, "inout"] <- "out"
        dt[dt$sipper >= 3, "inout"] <- "in"
        
        dtagg<-aggregate(dt[,11:14],by=list(dt$date,dt$chamber,dt$pwsw,dt$inout),function(x) mean(x,na.rm=T))
        #dtagg<-aggregate(dt,list(dt$date,dt$chamber,dt$pwsw),function(x) mean(x,na.rm=T))
        names(dtagg)[1:4]<-c("date","chamber","pwsw","inout")
        names(dtagg)<-tolower(names(dtagg))
        dtagg[,5:ncol(dtagg)] <- round(dtagg[,5:ncol(dtagg)], 3)
        
        dtagg<-dtagg[order(dtagg$inout,dtagg$date),]
        dtagg$site<-paste(substring(sumpathlist[i],36,36),"W",sep="")
        full[[i]]<-dtagg    
      }
      do.call(rbind,full)
    }
  
  fw <- cfieldonsite(fwpath)
  bw <- cfieldonsite(bwpath)
  
  dt <- rbind(fw, bw)
  names(dt)[names(dt) == "date"] <- "collect_date"
  dt$location <- paste(dt$site, substring(dt$pwsw, 1, 1), dt$chamber, sep = "-")

  dt
}