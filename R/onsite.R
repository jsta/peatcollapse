#'@name get_mesoonsite
#'@title Get mesocosm onsite data
#'@description Get mesocosm onsite data
#'@param experiment character choice of "soilplant" or "soil"
#'@param onsitepath character file.path to onsite raw data
#'@export
#'@examples
#'\dontrun{
#'mesoonsite <- get_mesoonsite(onsitepath = file.path("Raw", "onsite"), experiment = "SoilPlant")
#'}
get_mesoonsite <- function(onsitepath = file.path("Raw", "onsite"), experiment = "SoilPlant"){
  
    flist <- list.files(onsitepath, full.names = TRUE, include.dirs = TRUE)
    flist <- flist[grep(tolower(experiment), tolower(flist))]
    
    dtpath <- flist[which.max(as.numeric(substring(unlist(lapply(strsplit(flist,"/"), function(x) unlist(x[3]))), 1, 8)))]
    
#name cmesoonsite
#param sumpathlist list of file paths
#param pwsw return only pw, sw, or all?
#description Removes entries tagged in the "timing.of.sample.with.dosing" field of anything other than "1 day post"
  cmesoonsite <- function(sumpathlist, pwsw = "all"){
    
    full<-list()
    
    for(i in 1:length(sumpathlist)){
      #i=1
      dt <- read.csv(sumpathlist[i], skip = 1, stringsAsFactors = F)
      names(dt)<-tolower(names(dt))
      
#       if(any(names(dt)=="exp")){
#         dt<-dt[dt$exp!="Source",]
#       }
      
      dt$sampling.date <- sapply(dt$sampling.date, mdy2mmyyyy)
      
      names(dt)[names(dt) == "sampling.date"] <- "date"
      dt$date <- as.POSIXct(strptime(dt$date, "%m/%d/%Y"))
      
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
      
      names(dtagg)[1:4] <- c("date", "core", "pwswc", "crypt")
      dtagg <- merge(key, dtagg)
      dtagg$core[dtagg$core == 999] <- NA
      dtagg <- dtagg[,-1]
      
      construct_station <- function(x){
        if(nchar(x["crypt"]) < 2){
          if(!is.na(x["core"])){
          gsub(" ", "", paste0("C", x["crypt"], "C", x["core"]))
          }else{
            paste0("C", x["crypt"])
          }
        }else{
          x["crypt"]
        }
      }
      
      dtagg$station <- apply(dtagg, 1, function(x) construct_station(x))
      
      dtagg<-dtagg[with(dtagg,order(pwsw,date,core,crypt)),]
      full[[i]]<-dtagg    
    }
    do.call(rbind,full)
  }
  
  cmesoonsite(dtpath)
}

#'@name get_fieldonsite
#'@title Get field onsite data
#'@description Get field onsite data
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
  
  cfieldonsite<-function(sumpathlist, pwsw="all"){
    
      full<-list()
      
      for(i in 1:length(sumpathlist)){
        #browser()
        #i <- 1
        dt <- read.csv(sumpathlist[i], skip = 1, stringsAsFactors = F)
        names(dt) <- tolower(names(dt))
        
        if(any(names(dt) == "timing.of.sample.with.dosing")){
          dt <- dt[dt$timing.of.sample.with.dosing == "1 day post"|dt$timing.of.sample.with.dosing == "" | dt$timing.of.sample.with.dosing == "0 day post",]
          dt <- dt[,-which(names(dt) == "timing.of.sample.with.dosing")]
        }
        
        #dt <- dt[nchar(as.character(dt$chamber)) < 3,]
        names(dt)[c(1,4,5)] <- c("date", "pwsw", "trt")
        
        if(mean(nchar(dt$date)) < 8){
          dt$date <- sapply(dt$date, mdy2mmyyyy)
        }
        dt$date <- as.POSIXct(strptime(dt$date, "%m/%d/%Y"))
        
        #unique(dt[dt$date > "2015-06-17",]$chamber)
        
        #==========================================================#
        if(pwsw == "pw"){
          dt <- dt[dt$pwsw == "PW",]
        }
        if(pwsw == "sw"){
          dt<-dt[dt$pwsw == "SW",]
        }
        #==========================================================#

        suppressWarnings(dt[,11:14] <- apply(dt[,11:14], 2, function(x) as.numeric(x)))
        
        dt$site <- paste(substring(sumpathlist[i], 36, 36), "W", sep = "")
        
        dt[,"chamber"] <- gsub(" ", "", dt$chamber)
        dt[dt$chamber == "S+AC0-199", "chamber"] <- "S-199"
        dt[dt$chamber == "S+AC0-199 ", "chamber"] <- "S-199"
        
        dt[dt$chamber == "S-199", "site"] <- "S-199"
              
        #dt[dt$site == "S-199",]
        suppressWarnings(dt[,"chamber"] <- sapply(dt[,"chamber"], function(x) as.numeric(x)))
        
        dt$inout <- NA
        dt[dt$sipper < 3, "inout"] <- "out"
        dt[dt$sipper >= 3, "inout"] <- "in"
        
        #adjust specific lines/dates
        if(any(dt$site == "S-199") & any(dt$site == "BW")){
          dt[dt$site == "S-199" & dt$date == "2015-02-12", "date"] <- "2015-02-11"
        }
        
        
        if(any(dt$site == "S-199") & any(dt$site == "BW")){
          dt[dt$site == "S-199" & dt$date == "2015-07-14", "date"] <- "2015-07-15"
        }
        
        if(any(dt$site == "S-199") & any(dt$site == "BW")){
          dt[dt$site == "S-199" & dt$date == "2015-08-18", "date"] <- "2015-08-19"
        }
        
        
        
        dtagg <- aggregate(dt[,11:14], by = list(dt$date, dt$chamber, dt$pwsw, dt$inout, dt$site), function(x) mean(x, na.rm = T))
        names(dtagg)[1:5]<-c("date", "chamber", "pwsw", "inout", "site")
        
        
        
        if(any(!is.na(unlist(dt[dt$site == "S-199", 11:14])))){ 
          s199agg <- aggregate(dt[dt$site == "S-199", 11:14], by = list(dt[dt$site == "S-199", "date"], dt[dt$site == "S-199", "pwsw"], dt[dt$site == "S-199", "site"]), function(x) mean(x, na.rm = T))
          names(s199agg)[1:3] <- c("date", "pwsw", "site")
          
          s199agg_pad <- data.frame(matrix(NA, nrow = nrow(s199agg), ncol = 2))
          names(s199agg_pad) <- c("inout", "chamber")
          s199agg <- cbind(s199agg, s199agg_pad)
          
          s199agg <- s199agg[,match(names(dtagg), names(s199agg))]
          
          dtagg <- rbind(dtagg, s199agg)
        }
        
        names(dtagg)<-tolower(names(dtagg))
        dtagg[,6:ncol(dtagg)] <- round(dtagg[,6:ncol(dtagg)], 3)
        
        dtagg <- dtagg[order(dtagg$inout, dtagg$date, dtagg$site),]
        #dtagg$site<-paste(substring(sumpathlist[i],36,36),"W",sep="")
        full[[i]] <- dtagg    
      }
      do.call(rbind, full)
    }
  
  fw <- cfieldonsite(fwpath)
  bw <- cfieldonsite(bwpath)
  
  construct_location <-function(x, fpath){
    if(x["site"] == "S-199"){
      
      paste0(paste(substring(fpath, 36, 36), "W", sep = ""), "-", x["site"])  
    }else{
      gsub(" ", "", paste(x["site"], substring(x["pwsw"], 1, 1), x["chamber"], sep = "-"))
    }
  }
  
  fw$location <- apply(fw, 1, function(x) construct_location(x, fwpath))
  bw$location <- apply(bw, 1, function(x) construct_location(x, bwpath))
  
  dt <- rbind(fw, bw)
  names(dt)[names(dt) == "date"] <- "collect_date"
  
  dt
}