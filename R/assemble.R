#'@name assemble_meso
#'@title Assemble mesocosm data file from onsite and lab data
#'@description Assemble mesocosm data file from onsite and lab data
#'@param experiment character choice of "soilplant" or "soil"
#'@param onsitepath character folder.path to folder containing onsite data
#'@param sulfpath character folder.path to folder containing sulfide data
#'@param eddpath character folder.path to folder containing edd data
#'@param tofile logical print results to file?
#'@export
#'@examples \dontrun{
#'meso <- assemble_meso(eddpath = file.path("Raw", "lab", "EDD"),
#' sulfpath = file.path("Raw", "lab"), tofile = FALSE)
#' 
#' meso <- assemble_meso(experiment = "soil")
#' }
assemble_meso <- function(experiment = "soilplant", onsitepath = file.path("Raw", "onsite"), sulfpath =  file.path("Raw", "lab"), eddpath = file.path("Raw", "lab", "EDD"), tofile = FALSE){
 
  mesolab <- get_mesolab(eddpath = eddpath, sulfpath = sulfpath, project = experiment)
  mesoonsite <- get_mesoonsite(onsitepath = onsitepath, experiment = experiment)

  align_dates <- function(x, dates = mesoonsite$date){
    #x <- mesolab$date[1]
    if(any(abs(difftime(x, dates)) < 9)){ #adjust numeric tolerance
      dates[which.min(abs(difftime(x, dates)))]
    }else{
      #test for month offset
      daycandidate <- unique(dates[strftime(x, format = "%Y-%m") == strftime(dates, format = "%Y-%m")])
      daycandidate <- daycandidate[(as.numeric(strftime(x, format = "%d")) %% as.numeric(strftime(daycandidate, format = "%d"))) == 0]
      if(length(daycandidate) > 0){
        daycandidate
      }else{
        x
      }
    }
  }
                                                                          mesolab$date <- do.call(c,mapply(align_dates, mesolab$date, SIMPLIFY = FALSE))
 
  cmesoall <- merge(mesoonsite, mesolab, by=c("station", "date", "pwsw"), all.y = TRUE, all.x = TRUE)
  cmesoall <- cmesoall[,!(names(cmesoall) %in% c("core.x", "crypt.x"))]
  names(cmesoall)[names(cmesoall) %in% c("core.y", "crypt.y")] <- c("core", "crypt")
  
  #fill-in missing station entries
  stationmiss <- cmesoall[is.na(cmesoall$station),]
  if(nrow(stationmiss) > 0){
  stationmiss$station <- paste0("C", stationmiss$crypt, "C", stationmiss$core)
  if(length(stationmiss[is.na(stationmiss$core),]$station) > 0){
    stationmiss[is.na(stationmiss$core),]$station <- paste0("C", stationmiss[is.na(stationmiss$core),]$crypt)
  }
  
  cmesoall[is.na(cmesoall$station),]$station <- stationmiss$station 
  }
  
  #cmesoall <- cmesoall[with(cmesoall, order(crypt, pwsw, date, core)),]

  cmesoall <- mesokey(cmesoall)
  cmesoall <- cmesoall[order(cmesoall$pwsw, cmesoall$date, cmesoall$core, cmesoall$station, cmesoall$trt),]
  
  #add unit labels to column names=====================================#
  unitkey <- read.table(header = FALSE, text = "
temp-temp.degC 
cond-cond.mscm 
ALKA-ALKA.mgL 
CL-CL.mglCaCO3
DOC-DOC.mgL
NH4-NH4.mgL
SO4-SO4.mgl
TDN-TDN.mgl", sep = "-", stringsAsFactors = FALSE)
  
  #names(cmesoall)[!is.na(match(names(cmesoall), unitkey[,1]))]
  
  names(cmesoall)[names(cmesoall) %in% unitkey[,1]] <- unitkey[na.omit(match(names(cmesoall), unitkey[,1])), 2]
  
  if(tofile == TRUE){
    write.csv(cmesoall,file.path(paste0("mesoall_", experiment, ".csv")), row.names = FALSE)
  }
  
  cmesoall
  
}

#'@name assemble_field
#'@title Assemble field data file from onsite and lab data
#'@description Assemble field data file from onsite and lab data
#'@param onsitepath character folder.path to folder containing onsite data
#'@param eddpath character folder.path to folder containing edd data
#'@param limspath character folder.path to folder containing lims data
#'@param ppath character folder.path to folder containing phosphorus data
#'@param sulfpath character folder.path to folder containing sulfide data
#'@param tofile logical print results to file?
#'@details QUESTION: WHY ARE THERE NA LOCATIONS AT THEEND OF assemble_field
#'         ANSWER:   BW-S-199 needs to be averaged with FW-S-199
#'@export
#'@examples \dontrun{
#'field <- assemble_field(tofile = FALSE)
#'}
assemble_field <- function(onsitepath = file.path("Raw", "onsite"), eddpath = file.path("Raw", "lab", "EDD") , limspath = file.path("Raw", "lab"), ppath = file.path("Raw", "lab", "phosphorus") , sulfpath = file.path("Raw", "lab"), tofile = TRUE){

  fieldonsite <- get_fieldonsite(onsitepath = onsitepath)
  fieldlab <- get_fieldlab(fieldonsite = fieldonsite, eddpath = eddpath, limspath = limspath, ppath = ppath, sulfpath = sulfpath, addlims = TRUE)
  names(fieldlab)[names(fieldlab) == "matrix"] <- "pwsw"
  
#   align_dates <- function(x, fieldonsite){
#     dates <- fieldonsite[fieldonsite$site %in% x["site"], "collect_date"]
#     diff_dates <- abs(difftime(x["collect_date"], dates, units = "days"))
#     dates <- data.frame(dates = dates, diff_dates = diff_dates)
#     dates <- dates[order(dates$diff_dates),]
#     if(as.numeric(dates[1,2]) > 10){
#       
#       x["collect_date"]
#     }else{
#       strftime(as.POSIXct(dates[1,1], origin = "1970-01-01", tz = "NewYork"), format = "%Y-%m-%d")
#     }
#   }
#   
#   fieldlab$collect_date <- apply(fieldlab, 1, function(x) align_dates(x, fieldonsite =  fieldonsite))

  #==================================================================#
  
  
#   fieldlab[fieldlab$location == "FW-S-199",]
#   unique(fieldlab$location)
  
#   s199_fieldlab <- fieldlab[fieldlab$site == "S-199",]
#   fieldlab <- fieldlab[fieldlab$site != "S-199",]
#   
#   c_s199_pnumbers <- function(x, s199_fieldlab){
#     if(nchar(x["cust_sample_id"]) < 20 & nrow(s199_fieldlab[s199_fieldlab$collect_date %in% x["collect_date"],]) > 1){
#       s199_fieldlab_sub <- s199_fieldlab[s199_fieldlab$collect_date %in% x["collect_date"][1],]
#       paste(unique(s199_fieldlab_sub$cust_sample_id), collapse = "_")
#     }else{
#       x["cust_sample_id"]
#     }
#   }
#   
#   s199_fieldlab$cust_sample_id <- apply(s199_fieldlab, 1, function(x) c_s199_pnumbers(x, s199_fieldlab))
  
  
  
#   s199_fieldlab <- suppressWarnings(aggregate(s199_fieldlab[,8:25], by = list(s199_fieldlab$site, s199_fieldlab$collect_date, s199_fieldlab$pwsw, s199_fieldlab$location, s199_fieldlab$cust_sample_id), mean))
#   names(s199_fieldlab)[1:5] <- c("site", "collect_date", "pwsw", "location", "cust_sample_id")
  
  
  
#   padna_names <- c(names(fieldlab)[!(names(fieldlab) %in% names(s199_fieldlab))])
#   padna <- data.frame(matrix(NA,nrow = nrow(s199_fieldlab), ncol = length(padna_names)))
#   names(padna) <- padna_names
#   s199_fieldlab <- cbind(s199_fieldlab, padna)
#   s199_fieldlab <- s199_fieldlab[,match(names(s199_fieldlab), names(fieldlab))]
#   fieldlab <- rbind(fieldlab, s199_fieldlab)
  #==================================================================#
  cfieldall <- merge(fieldonsite, fieldlab, by = c("site", "chamber", "collect_date", "pwsw", "inout", "location"), all.y = TRUE, all.x = TRUE)
  cfieldall <- cfieldall[order(cfieldall$inout, cfieldall$site, cfieldall$pwsw, cfieldall$collect_date, cfieldall$chamber),]
  
  cfieldall[!is.na(cfieldall$chamber) & cfieldall$chamber > 9 & cfieldall$site == "BW" & cfieldall$site != "S-199", "trt"] <- "treatment"
  cfieldall[!is.na(cfieldall$chamber) & cfieldall$chamber > 10 & cfieldall$site == "FW" & cfieldall$site != "S-199", "trt"] <- "treatment"
  cfieldall[is.na(cfieldall$trt) & cfieldall$site != "S-199", "trt"] <- "control"
  
  #remove rows and columns of na
  cfieldall <- cfieldall[,apply(cfieldall, 2, function(x) sum(!is.na(x))) > 1]
  cfieldall <- cfieldall[apply(cfieldall[,7:18], 1, function(x) sum(!is.na(x))) > 0,]
  
  #add unit labels to column names=====================================#
  unitkey <- read.table(header = FALSE, text = "
temp-temp.degC 
cond-cond.mscm 
ALKA-ALKA.mgL 
CL-CL.mglCaCO3
DOC-DOC.mgL
NH4-NH4.mgL
SO4-SO4.mgl
TDN-TDN.mgl", sep = "-", stringsAsFactors = FALSE)
  
  names(cfieldall)[names(cfieldall) %in% unitkey[,1]] <- unitkey[na.omit(match(names(cfieldall), unitkey[,1])), 2] 

#==================================================================#
  #print(paste(nrow(cfieldall), "rows output"))
  if(tofile == TRUE){
    write.csv(cfieldall, file.path("fieldall.csv"), row.names = FALSE)
  }
  
  cfieldall
  
}