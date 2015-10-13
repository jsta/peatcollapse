#'@name create_labdb
#'@title Create sqlite database of peatcollapse lab data
#'@import jsta
#'@import RSQLite
#'@param eddpath character file.path to edd data folder
#'@param dbname character file name of output SQLite database
#'@param tablename character name of internal database table
#'@export
#'@examples \dontrun{
#'create_labdb()
#'}
create_labdb <- function(eddpath = file.path("Raw", "lab", "EDD"), dbname  = "pc_eddlab.db", tablename = "eddlab"){
  
  clean_csv <- function(eddpath){
    flist <- list.files(eddpath, full.names = TRUE, include.dirs = TRUE, pattern = "csv")
    
    dt <- read.csv(flist[1], stringsAsFactors = FALSE)
    
    for(i in 2:length(flist)){
      newdt <- read.csv(flist[i], stringsAsFactors = FALSE)
      
      missing_col <- names(dt)[!(names(dt) %in% names(newdt))]
      if(length(missing_col) > 0){
        missing_na <- matrix(NA, nrow = nrow(newdt), ncol = length(missing_col))
        newdt <- cbind(newdt, missing_na)
        names(newdt)[(ncol(newdt) - length(missing_col) + 1):ncol(newdt)] <- missing_col
      }
      
      missing_col <- names(newdt)[!(names(newdt) %in% names(dt))]
      if(length(missing_col) > 0){
        missing_na <- matrix(NA, nrow = nrow(dt), ncol = length(missing_col))
        dt <- cbind(dt, missing_na)
        names(dt)[(ncol(dt) - length(missing_col) + 1):ncol(dt)] <- missing_col
      }
      
      dt <- rbind(dt, newdt)
    }
    
    dt[,apply(dt, 2, function(x) sum(!is.na(x)) > 0)]
    
  }
  
  edd <- clean_csv(eddpath = eddpath)
  
  fullcentury <- gsub("/", "", sapply(edd$collect_date, function(x) strsplit(x, " ")[[1]][1]))
  fullcentury <- sapply(fullcentury, function(x) gsub("20", "", x))
  edd$collect_date <- jsta::date456posix(fullcentury, century = 20)
  
  eddlab <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), dbname)
  invisible(RSQLite::dbWriteTable(conn = eddlab, name = tablename, value = edd, overwrite = TRUE))
  invisible(RSQLite::dbDisconnect(eddlab))
}

#'@name clean_lab
#'@title Clean lab data
#'@import RSQLite
#'@param begindate character date in YYYY-MM-DD format specifying a lower limit on the date of measurements to return
#'@param dbname character file.path of edd database
#'@param tablename character name of internal database table
#'@export
#'@details mdl flags are removed from result field
#'@return An updated SQLite database with seperate tables for "meso" and "field" studies. Data remains in long rather than wide format?
#'@examples \dontrun{
#'clean_lab()
#'}
clean_lab <- function(dbname = "pc_eddlab.db", tablename = "eddlab", begindate = "2014-09-07"){
  eddlab <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), dbname)
  
  q <- paste("SELECT matrix, location, sample_type, collect_date, acode, result, result_units_desc, mdl_final, cust_sample_id FROM", tablename, "WHERE sample_type LIKE 'SAMP' OR sample_type LIKE 'FD'")
  dt <- RSQLite::dbGetQuery(eddlab, q)
  
  #remove mdl flags from result field=================================#
  dt$result[which(is.na(suppressWarnings(as.numeric(dt$result))))] <- sapply(dt$result[which(is.na(suppressWarnings(as.numeric(dt$result))))], function(x) substring(x, 1, nchar(x)-1))
  dt$result <- as.numeric(dt$result)
  
  dt <- dt[dt$collect_date > as.POSIXct(begindate),]
  
  #split meso and field into sql tables===============================#
  mesolocations <- c(unique(dt$location[grep("C", dt$location)]), unique(dt$location[grep("H", dt$location)]), unique(dt$location[grep("M", dt$location)]))
  fieldlocations <- unique(dt$location)[!(unique(dt$location) %in% mesolocations)]
  
  meso <- dt[dt$location %in% mesolocations,]
  field <- dt[dt$location %in% fieldlocations,]
  
  invisible(RSQLite::dbWriteTable(conn = eddlab, name = "meso", value = meso, overwrite = TRUE))
  invisible(RSQLite::dbWriteTable(conn = eddlab, name = "field", value = field, overwrite = TRUE))
  invisible(RSQLite::dbDisconnect(eddlab))
}

#'@name get_mesolab
#'@title Get mesocosm lab data
#'@import RSQLite
#'@import reshape2
#'@import DBI
#'@param project character choice of "soil" or "soilplant"
#'@param dbname character file.path of edd database
#'@export
#'@examples \dontrun{
#'clab <- get_mesolab(project = "soilplant")
#'}
get_mesolab <- function(project = "soilplant", dbname = "pc_eddlab.db"){
  
  #update database and format data====================================#
  create_labdb()
  clean_lab()
  
  eddlab <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), dbname)
  q <- "SELECT * FROM meso"
  dt <- RSQLite::dbGetQuery(eddlab, q)
  dt$collect_date <- as.POSIXct(dt$collect_date, origin = "1970-01-01", tz = "EST")
  dt$collect_date <- as.POSIXct(strftime(dt$collect_date, format = "%Y-%m-%d"))
  invisible(RSQLite::dbDisconnect(eddlab))
  
  dt <- reshape2::dcast(dt, collect_date + location + matrix ~ acode,  value.var="result", fun.aggregate=mean)
  
  #add calculated fields==============================================#
  
  dt$crypt[substring(dt$location, 1, 1) == "C"] <- substring(dt$location, 2, 2)[substring(dt$location, 1, 1) == "C"]
  dt$core <- sapply(dt$location, function(x) substring(x, 4, nchar(x)))
  
  #swkey is the same for both meso experiments
  swkey<-read.table(header=FALSE,text="
                    1 elev
                    2 amb
                    3 elev
                    4 amb
                    5 elev
                    6 amb
                    ")
  
  if(project == "soilplant"){
    key <- read.table(header = FALSE, text = "
                      1 elevcont                    
                      3 elevcont
                      10 elevinun
                      12 elevinun
                      17 elevcont
                      19 elevcont
                      2 elevinun
                      4 elevinun
                      9 elevcont
                      11 elevcont
                      18 elevinun
                      20 elevinun
                      6 ambcont
                      8 ambcont
                      14 ambcont
                      16 ambcont
                      22 ambinun
                      24 ambinun
                      5 ambinun
                      7 ambinun
                      13 ambinun
                      15 ambinun
                      21 ambcont
                      23 ambcont
                      ")
  }else{
    key <- read.table(header = FALSE,text = "
                      1 elevcont                    
                      3 elevcont
                      10 elevinun
                      12 elevinun
                      17 elevcont
                      19 elevcont
                      2 elevinun
                      4 elevinun
                      9 elevcont
                      11 elevcont
                      18 elevinun
                      20 elevinun
                      6 ambcont
                      8 ambcont
                      14 ambcont
                      16 ambcont
                      22 ambinun
                      24 ambinun
                      5 ambinun
                      7 ambinun
                      13 ambinun
                      15 ambinun
                      21 ambcont
                      23 ambcont
                      ")
  }
  
  names(swkey)<-c("crypt","trt")
  names(key)<-c("core","trt")
  
  dt <- rbind(merge(dt[dt$matrix == "PW",], key), merge(dt[dt$matrix == "SW",], swkey))
  dt$trt[c(grep("H", dt$location), grep("M", dt$location))] <- NA
  
  dt$core <- as.numeric(dt$core)
  
  #fix column names===================================================#
  names(dt)[which(names(dt)=="collect_date")] <- "date"
  names(dt)[which(names(dt)=="location")] <- "station"
  names(dt)[which(names(dt)=="matrix")] <- "pwsw"
  
  #choose project time interval=======================================#
  soilinterval <- c("2014-09-12", "2014-12-01")
  plantinterval <- c("2015-02-03", strftime(Sys.Date(), format = "%Y-%m-%d"))
  if(project == "soil"){
    dt <- dt[dt$date >= soilinterval[1] & dt$date <= soilinterval[2],]
  }
  if(project == "soilplant"){
    dt <- dt[dt$date >= plantinterval[1] & dt$date <= plantinterval[2],]
  }

  #browser()  
  #merge sulfide by approximating to the nearest wq date===============#
  sulfide <- clean_sulfide()$mesodt[,c("date","core","crypt","sulfide.mm", "datesulfide")]

  
  align_sulfide_dates <- function(x, dates = dt$date){
    if(any(abs(difftime(x, dates)) < 12)){
      dates[which.min(abs(difftime(x, dates)))]
    }else{
      x
    }
  }
  
  sulfide$date <- do.call(c,mapply(align_sulfide_dates, sulfide$date, SIMPLIFY = FALSE))
    
  dt <- merge(dt, sulfide, by = c("date","core", "crypt"), all.x = TRUE, all.y = FALSE)
  
  #return=============================================================#
  dt[order(dt$date, dt$pwsw, dt$station, dt$crypt, dt$core),]
  }

#'@name get_fieldlab
#'@import jsta
#'@title Get lab results from Field Study
#'@param addlims logical integrate lims data with edd data?
#'@param limspath character folder.path to folder containing lims data
#'@param fieldonsite data.frame output of get_fieldonsite
#'@export
#'@examples
#'\dontrun{
#'dt <- get_fieldlab(fieldonsite = fieldonsite)
#'}
get_fieldlab <- function(fieldonsite, addlims = TRUE, limspath = "inst/extdata/Raw/lab"){
  create_labdb()
  clean_lab()
  
  eddlab <- RSQLite::dbConnect(DBI::dbDriver("SQLite"), "pc_eddlab.db")
  q <- "SELECT * FROM field"
  dt <- RSQLite::dbGetQuery(eddlab, q)
  dt$collect_date <- as.POSIXct(dt$collect_date, origin = "1970-01-01", tz = "EST")
  dt$collect_date <- as.POSIXct(strftime(dt$collect_date, format = "%Y-%m-%d"))
  invisible(RSQLite::dbDisconnect(eddlab))
  
  
  #==================================================================#
  align_dates <- function(x, dates = fieldonsite$collect_date){
    dates[which.min(abs(difftime(x, dates)))]
  }
  
  dt$collect_date <- do.call(c, mapply(align_dates, dt$collect_date, SIMPLIFY = FALSE))
  
  
  #==================================================================#
  #dt[which(dt$collect_date == "2015-04-13" & dt$location == "BW-P-16"),]
  #
  
  dt <- reshape2::dcast(dt, collect_date + location + matrix + cust_sample_id ~ acode,  value.var="result", fun.aggregate=mean)
  dt$site <- suppressWarnings(do.call(rbind, strsplit(dt$location, "-"))[,1])
  dt$trt <- NA
  
  dt$chamber <- suppressWarnings(do.call(rbind, strsplit(dt$location, "-"))[,3])
  dt <- dt[nchar(dt$chamber) < 3,]
  
  #dt[nchar(dt$chamber) == 3,] #is S-199 samples
  
  dt$chamber <- suppressWarnings(as.numeric(as.character(dt$chamber)))
  dt[dt$chamber > 9 & dt$site == "BW", "trt"] <- "treatment"
  dt[is.na(dt$trt), "trt"] <- "control"  
  dt[dt$chamber > 10 & dt$site == "FW", "trt"] <- "treatment"
  dt[is.na(dt$trt), "trt"] <- "control"
  
  dt <- dt[-(grep("GT", dt$location)),]
  
  if(addlims == TRUE){
    limspaths <- list.files(limspath, pattern = "csv", include.dirs = TRUE, full.names = TRUE)
    
    source("R/old/cleanlab.R")
    lims <- cleanlab(limspaths)
    names(lims)[names(lims) %in% c("date", "station", "pwsw", "sample_id")] <- c("collect_date", "location", "cust_sample_id", "matrix")
    lims <- jsta::align_dfcol(target = lims, template = dt)
   
    align_dates <- function(x, dates = fieldonsite$collect_date){
      dates[which.min(abs(difftime(x, dates)))]
    }
    lims$collect_date <- do.call(c, mapply(align_dates, lims$collect_date, SIMPLIFY = FALSE))
    
    dt <- rbind(lims, dt)
    dt <- dt[!duplicated(paste(dt$acode, dt$collect_date, dt$sample_type, dt$location, dt$matrix), fromLast = TRUE),]
  }
  dt$inout <- "in"

#merge phosphorus======================================================#
  phosdt <- clean_p()
  dt <- merge(dt, phosdt, by = c("collect_date", "site", "matrix","chamber", "inout"), all.x = TRUE)
  
#   phosdt[phosdt$collect_date == "2015-04-15" & phosdt$site == "BW" & phosdt$matrix == "PW" & phosdt$chamber == "16",]
#   
#   dt[dt$collect_date == "2015-04-15" & dt$site == "BW" & dt$matrix == "PW" & dt$chamber == "16",]
#   
#   test[test$collect_date == "2015-04-15" & test$site == "BW" & test$matrix == "PW" & test$chamber == "16",]
#

#merge sulfide=========================================================#
  #merge sulfide by approximating to the nearest wq date===============#
  sulfide <- clean_sulfide()$fielddt
  
  
  align_sulfide_dates <- function(x, df = dt){
    
    dfsub <- df[df$site == x["site"],]
    dfsub$collect_date[which.min(abs(difftime(x["collect_date"], dfsub$collect_date)))]
  }
  
  
  
  sulfide$collect_date <- strftime(as.POSIXct(apply(sulfide, 1, function(x) align_sulfide_dates(x)), origin = "1970-01-01", tz = "EST"), format = "%Y-%m-%d")
    
#   sulfide$collect_date <- do.call(c,mapply(align_sulfide_dates, sulfide$date, SIMPLIFY = FALSE))

  sulfide$inout <- "in"
  sulfide$matrix <- "PW"
  
  
  
  dt <- merge(dt, sulfide, by = c("site", "chamber", "collect_date", "matrix", "inout"), all.x = TRUE)
  
  
  return(dt)
}

#'@name clean_sulfide
#'@title Clean sulfide data
#'@import gdata
#'@param sulfpath character file path to an .xlsx file
#'@param sheet_nums numeric sheet indices containing raw data
#'@examples \dontrun{
#'dt <- clean_sulfide()
#'}
clean_sulfide <- function(sulfpath = NA, sheet_nums = NA){
  
  #check for fxn inputs================================================#
  if(is.na(sulfpath)){
    flist <- list.files(file.path("inst", "extdata", "Raw", "lab"), pattern = "Sulfide*", full.names = TRUE, include.dirs = TRUE)
    sulfpath <- flist[which.max(suppressWarnings(as.numeric(substring(unlist(lapply(flist, function(x) unlist(strsplit(x, "/"))[5])), 1, 8))))]
  }
  
  if(is.na(sheet_nums)){
    sheet_nums <- which(unlist(lapply(gdata::sheetNames(sulfpath), function(x) length(unlist(strsplit(x, "\\."))))) == 3)
  }

  #get calibration coef================================================#
  get_calibration <- function(sulfpath, sheet_nums){
  
    dates <- vector("list", length(sheet_nums))
    as <- vector("list", length(sheet_nums))
    bs <- vector("list", length(sheet_nums))
    
    for(i in sheet_nums){
      ab <- as.character(gdata::read.xls(xls = sulfpath, sheet = i)[9:12, 3])
      ab <- ab[nchar(ab) > 0]
      
      b <- as.numeric(ab[2])
      a <- as.numeric(paste(strsplit(ab[1], ".E")[[1]][1], strsplit(ab[1], ".E")[[1]][2], sep = "e"))
        
      date <- gsub("X", "", names(gdata::read.xls(xls = sulfpath, sheet = i, blank.lines.skip =FALSE))[1])
      
      sheet_pos <- which(sheet_nums %in% i)
      dates[[sheet_pos]] <- date
      as[[sheet_pos]] <- a
      bs[[sheet_pos]] <- b
      
      result <- data.frame(date = do.call(unlist, list(dates)), a = unlist(as), b = unlist(bs))
    }
  result
  }
  
  calibration <- get_calibration(sulfpath = sulfpath, sheet_nums = sheet_nums)
  calibration$date <- as.POSIXct(strptime(calibration$date, format = "%Y.%m.%d"))
  
  fielddt <- gdata::read.xls(xls = sulfpath, stringsAsFactors = FALSE)[,1:6]
  mesodt <- gdata::read.xls(xls = sulfpath, sheet = grep("meso", tolower(gdata::sheetNames(sulfpath))) , stringsAsFactors = FALSE)[,1:6]
  
  clean_sulfdt <- function(dt){
  
  names(dt) <- tolower(names(dt))
  dt$date <- as.POSIXct(dt$date)
  dt$mv <- suppressWarnings(as.numeric(dt$mv))
  
  dt <- merge(dt, calibration, by.x = "date", by.y = "date")
  dt$ppm <- dt$a * exp(dt$b * dt$mv)
  dt$sulfide.mm <- dt$ppm / 32 #molecular weight
  
  dt
  }
  
  mesodt <- clean_sulfdt(mesodt)
  mesodt$datesulfide <- mesodt$date
  
  #browser()
  
  #==================================================================#
  fielddt <- clean_sulfdt(fielddt)
  fielddt$sipper <- substring(fielddt$sipper, 1, 1)
  
  fielddt <- aggregate(fielddt[, "sulfide.mm"], by = list(fielddt$date, fielddt$site, fielddt$chamber, fielddt$sipper), function(x) round(mean(x,na.rm=T), 4))
  
  fielddt <- reshape2::dcast(fielddt, Group.1 + Group.2 + Group.3 ~ Group.4, value.var="x", fun.aggregate=mean)
  names(fielddt) <- c("collect_date", "site", "chamber", "deepsulfide.mm", "shallowsulfide.mm")
  fielddt$datesulfide <- fielddt$collect_date

  #==================================================================#
  list(mesodt = mesodt, fielddt = fielddt)
}

#'@name clean_p
#'@title Clean phosphorus data
#'@description averages FDs, strips EBs
#'@param sumpath character file.path to raw phosphorus data
#'@examples \dontrun{
#'dt <- clean_p()
#'}
clean_p <- function(sumpath = NA){
  
  if(is.na(sumpath)){
    flist <- list.files(file.path("inst", "extdata", "Raw", "lab", "phosphorus"), full.names = TRUE, include.dirs = TRUE)
    sumpath <- flist[grep("labp", tolower(flist))]
  }
  
  dt1 <- read.csv(sumpath, stringsAsFactors=F)
  names(dt1)<-tolower(names(dt1))
  names(dt1)[3]<-"pwsw"
  
  dt1 <- dt1[nchar(dt1[, "date"]) > 0,]
  dsplit <- matrix(unlist(strsplit(dt1$date,"/",fixed=T)),ncol=3,byrow=3)
  
  pad0 <- function(x){
    if(nchar(x) < 2){
      paste("0", x, sep="")
    }else{
      x  
    }
  }
  
  for(i in 1:2){
    dsplit[,i]<-unlist(lapply(dsplit[,i],function(x) pad0(x)),use.names=F)
  }
  
  dt1$datetime<-paste(dsplit[,1],"/",dsplit[,2],"/",dsplit[,3],sep="")
  dt1$datetime<-as.POSIXct(strptime(dt1$datetime,"%m/%d/%Y"))
  dt1$date<-strftime(dt1$datetime,"%m/%d/%Y")
  dt1$date<-as.POSIXct(strptime(dt1$date,"%m/%d/%Y"))
  dt1<-dt1[dt1$chamber>0 & dt1$chamber<199,] #strips ebs
  dt1<-dt1[,c(1:3,6,7,10:13)]
  
  
  uid<-paste(dt1[,"date"],dt1[,"site"],dt1[,"pwsw"],dt1[,"chamber"],sep="")
  
  if(any(duplicated(uid))){#any FDs?
    duplist<-unique(uid[duplicated(uid)])
    
    for(j in duplist){
      curdup <- which(uid == j)
      dt1[curdup[1], 5:ncol(dt1)] <- apply(dt1[curdup, 5:ncol(dt1)], 2, function(x) mean(x, na.rm = T))
      dt1 <- dt1[-curdup[2:length(curdup)],]
      uid <- paste(dt1[, "date"], dt1[,"site"], dt1[,"pwsw"], dt1[,"chamber"], sep = "")
    }
  }
  dt1 <- dt1[order(dt1$date),]
  
  names(dt1)[names(dt1) %in% c("date", "pwsw")] <- c("collect_date", "matrix")
  
  dt1$inout <- "in"
  dt1
  
}

#'@name cleanlab
#'@title Clean LIMS lab data files
#'@param proj string options are field, fieldbw, fieldfw, and meso
#'@param pwsw string choice of all, sw, pw
#'@param sumpathlist list of file.paths to lims raw data files
#'@import reshape2
#'@details EBs are discarded, only meso data has FDs
cleanlab <- function(sumpathlist, proj = "field", pwsw = "all"){
  
  fulldt <- list()
  
  for(j in sumpathlist){
    
    dt1 <- read.csv(j, stringsAsFactors=F)
    
    dt1 <- dt1[dt1$SAMPLE_TYPE == "SAMP" | dt1$SAMPLE_TYPE == "FD",][,c(1, 3, 4, 8, 19, 21, 22)]
    
    if(pwsw=="sw"){
      dt1<-dt1[dt1$MATRIX=="SW",]
    }
    if(pwsw=="pw"){
      dt1<-dt1[dt1$MATRIX=="PW",]
    }
    
    names(dt1) <- tolower(names(dt1))
    names(dt1)[which(names(dt1) %in% c("matrix", "collect_date"))] <- c("pwsw", "datetime")
    dsplit <- matrix(unlist(strsplit(dt1$datetime, "/", fixed = T)), ncol = 3, byrow = 3)
    
    pad0<-function(x){
      if(nchar(x)<2){
        paste("0",x,sep="")
      }else{
        x  
      }
    }
    
    dsplit[,1] <- unlist(lapply(dsplit[,1], function(x) pad0(x)), use.names = FALSE)
    dsplit[,2] <- unlist(lapply(dsplit[,2], function(x) pad0(x)), use.names = FALSE)
    
    dt1$datetime<-paste(dsplit[,1],"/",dsplit[,2],"/",dsplit[,3],sep="")
    dt1$datetime<-as.POSIXct(strptime(dt1$datetime,"%m/%d/%Y %H:%M"))
    dt1$date<-strftime(dt1$datetime,"%m/%d/%Y")
    
    dwide <- reshape2::dcast(dt1, date + station + pwsw + sample_id ~ test_name, value.var="value", fun.aggregate = mean)
    
    #test<-paste(dt1[,"date"],dt1[,"station"],dt1[,"pwsw"])
    #dt1[which(paste(dt1[,"date"],dt1[,"station"],dt1[,"pwsw"])==test[1]),]
    #dwide[which(paste(dwide[,"date"],dwide[,"station"],dwide[,"pwsw"])==test[1]),]
    
    #choose project
    #proj="field"
    statsplit <- strsplit(dwide$station,"-")
    dwide$date <- as.POSIXct(strptime(dwide$date, "%m/%d/%Y"))
    ###########################################################  
    if(proj=="field"){
      
      dproj<-dwide[nchar(dwide$station)>5,]
      
      dproj$site <- do.call(rbind,strsplit(dproj$station,"-"))[,1]
      dproj$chamber <- do.call(rbind,strsplit(dproj$station,"-"))[,3]
      dproj$trt<-NA
      dproj<-dproj[nchar(dproj$chamber)<3,]
      dproj$chamber<-as.numeric(as.character(dproj$chamber))
      if(any(dproj$site=="BW")){
        dproj[dproj$chamber>9&dproj$site=="BW","trt"]<-"treatment"
        dproj[is.na(dproj$trt),"trt"]<-"control"  
      }
      if(any(dproj$site=="FW")){
        dproj[dproj$chamber>10&dproj$site=="FW","trt"]<-"treatment"
        dproj[is.na(dproj$trt),"trt"]<-"control"  
      }
      
      namestemp <- c("date","station","sample_id","pwsw","ALKA","CL","DOC","LCOND","LPH","NH4","NOX","Salinity","SO4","TDN","TN","site","chamber","trt")
      namesmiss <- which(is.na(match(namestemp, names(dproj))))
      paddt <- data.frame(matrix(NA, ncol = length(namestemp[namesmiss]), nrow = nrow(dproj)))
      names(paddt) <- namestemp[namesmiss]
      dproj <- cbind(dproj, paddt)
      dproj <- dproj[,match(namestemp, names(dproj))]
      
      fulldt[[which(j==sumpathlist)]]<-dproj
    }
    ###################################################  
    if(proj=="meso"){
      dprojsw<-dwide[nchar(dwide$station)==2&substring(dwide$station,1,1)=="C",]
      dproj<-dwide[nchar(dwide$station)<=5&nchar(dwide$station)>=4,]
      dproj<-dproj[dproj$station!="S-199",]
      dprojsw$site<-substring(dprojsw$station,2,2) #site = crypt
      dproj$site<-substring(dproj$station,2,2) #site = crypt
      dproj$chamber<-sapply(dproj$station,function(x) substring(x,4,nchar(x))) #chamber = core number
      
      #load treatment keys
      #     #plants plus soil
      #     key<-read.table(header=FALSE,text="
      #                     1 elevcont                    
      #                     3 elevcont
      #                     10 elevinun
      #                     12 elevinun
      #                     17 elevcont
      #                     19 elevcont
      #                     2 elevinun
      #                     4 elevinun
      #                     9 elevcont
      #                     11 elevcont
      #                     18 elevinun
      #                     20 elevinun
      #                     6 ambcont
      #                     8 ambcont
      #                     14 ambcont
      #                     16 ambcont
      #                     22 ambinun
      #                     24 ambinun
      #                     5 ambinun
      #                     7 ambinun
      #                     13 ambinun
      #                     15 ambinun
      #                     21 ambcont
      #                     23 ambcont
      #                     ")
      
      #soil only
      key<-read.table(header=FALSE,text="
                      1 elevcont                    
                      3 elevcont
                      10 elevinun
                      12 elevinun
                      17 elevcont
                      19 elevcont
                      2 elevinun
                      4 elevinun
                      9 elevcont
                      11 elevcont
                      18 elevinun
                      20 elevinun
                      6 ambcont
                      8 ambcont
                      14 ambcont
                      16 ambcont
                      22 ambinun
                      24 ambinun
                      5 ambinun
                      7 ambinun
                      13 ambinun
                      15 ambinun
                      21 ambcont
                      23 ambcont
                      ")
      
      #swkey is the same for both meso experiments
      swkey<-read.table(header=FALSE,text="
                        1 elev
                        2 amb
                        3 elev
                        4 amb
                        5 elev
                        6 amb
                        ")
      
      #not sure what the point of this code block is...
      # dproj$trt<-NA
      # for(i in 1:nrow(dproj)){
      #   #i<-2
      #   if(nchar(dproj[i,"station"]))
      #   dproj[i,"trt"]<-key[match(dproj[i,"chamber"],key[,1]),2]
      # }
      
      names(key)<-c("chamber","trt")
      names(swkey)<-c("site","trt")
      
      pw<-merge(key,dproj)
      sw<-merge(swkey,dprojsw)
      sw$chamber<-NA
      
      #pwsave<-pw
      pw<-pw[,match(names(pw),names(sw))]
      
      dproj<-rbind(pw,sw)
      
      fullvar<-c("date","station","pwsw","site","chamber","trt","ALKA","CL","DOC","NH4","NOX","Salinity","SO4","TDN")
      if(length(fullvar[is.na(match(fullvar,names(dproj)))])>0){
        paddt<-data.frame(matrix(99999999,ncol=length(fullvar[is.na(match(fullvar,names(dproj)))]),nrow=nrow(dproj)))
        names(paddt)<-fullvar[is.na(match(fullvar,names(dproj)))]
        dproj<-cbind(dproj,paddt)
      }
      
      #savedproj<-dproj
      #dproj<-savedproj
      dproj<-aggregate(cbind(ALKA,CL,DOC,NH4,NOX,Salinity,SO4,TDN) ~ date + station + pwsw + site + chamber + trt,FUN=mean,data=dproj,na.action=na.pass)
      names(dproj)<-c("date","station","pwsw","site","chamber","trt","ALKA","CL","DOC","NH4","NOX","Salinity","SO4","TDN")
      
      
      dproj[,which(colMeans(dproj[,7:ncol(dproj)])==99999999)+6]<-NA
      
      #fix column names
      namestemp<-c("date","station","pwsw","ALKA","CL","DOC","LCOND","LPH","NH4","NOX","Salinity","SO4","TDN","TN","site","chamber","trt")
      namesmiss<-which(is.na(match(namestemp,names(dproj))))
      paddt<-data.frame(matrix(NA,ncol=length(namestemp[namesmiss]),nrow=nrow(dproj)))
      names(paddt)<-namestemp[namesmiss]
      dproj<-cbind(dproj,paddt)
      dproj<-dproj[,match(namestemp,names(dproj))]
      
      fulldt[[which(j==sumpathlist)]]<-dproj
    }
  }
  
  do.call(rbind,fulldt)
}