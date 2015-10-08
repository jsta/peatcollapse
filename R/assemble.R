#'@name assemble_meso
#'@title Assemble mesocosm data file from onsite and lab data
#'@param project character choice of "soilplant" or "soil"
#'@param tofile logical print results to file?
#'@export
#'@examples \dontrun{
#' dt <- assemble_meso(tofile = TRUE)
#' }
assemble_meso <- function(project = "soilplant", tofile = FALSE){
  source("R/lab.R")
  source("R/onsite.R")
  
  mesolab <- get_mesolab(project = project)
  mesoonsite <- get_mesoonsite(experiment = project)
  
  align_dates <- function(x, dates = mesoonsite$date){
    #x <- mesolab$date[1]
    if(any(abs(difftime(x, dates)) < 4)){
      dates[which.min(abs(difftime(x, dates)))]
    }else{
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
 
  cmesoall <- merge(mesoonsite, mesolab, by=c("crypt", "core", "date", "pwsw"), all.y = TRUE, all.x = TRUE)
  
  #fill-in missing station entries
  stationmiss <- cmesoall[is.na(cmesoall$station),]
  stationmiss$station <- paste0("C", stationmiss$crypt, "C", stationmiss$core)
  stationmiss[is.na(stationmiss$core),]$station <- paste0("C", stationmiss[is.na(stationmiss$core),]$crypt)
  cmesoall[is.na(cmesoall$station),]$station <- stationmiss$station 
  
  #cmesoall <- cmesoall[with(cmesoall, order(crypt, pwsw, date, core)),]
  source("R/misc.R")
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
  
  names(cmesoall)[!is.na(match(names(cmesoall), unitkey[,1]))]
  names(cmesoall)[names(cmesoall) %in% unitkey[,1]] <- unitkey[na.omit(match(names(cmesoall), unitkey[,1])), 2]
  
  if(tofile == TRUE){
    write.csv(cmesoall,file.path("inst", "extdata", paste0("mesoall_", project, ".csv")), row.names = FALSE)
  }
}

#'@name assemble_field
#'@title Assemble field data file from onsite and lab data
#'@param tofile logical print results to file?
#'@export
#'@examples \dontrun{
#'dt <- assemble_field(tofile = TRUE)
#'}
assemble_field <- function(tofile = TRUE){
  source("R/lab.R")
  source("R/onsite.R")
  
  fieldonsite <- get_fieldonsite()
  fieldlab <- get_fieldlab(fieldonsite = fieldonsite, addlims = TRUE)
  names(fieldlab)[names(fieldlab) == "matrix"] <- "pwsw"
  
  align_dates <- function(x, dates = fieldonsite$collect_date){
    dates[which.min(abs(difftime(x, dates)))]
  }
  
  fieldlab$collect_date <- do.call(c, mapply(align_dates, fieldlab$collect_date, SIMPLIFY = FALSE))
  
  cfieldall <- merge(fieldonsite, fieldlab, by=c("site", "chamber", "collect_date", "pwsw", "inout", "location"), all.y = TRUE, all.x = TRUE)
  cfieldall <- cfieldall[order(cfieldall$inout, cfieldall$site, cfieldall$pwsw, cfieldall$collect_date, cfieldall$chamber),]
  
  cfieldall[cfieldall$chamber > 9 & cfieldall$site == "BW", "trt"] <- "treatment"
  cfieldall[cfieldall$chamber > 10 & cfieldall$site == "FW", "trt"] <- "treatment"
  cfieldall[is.na(cfieldall$trt), "trt"] <- "control"
  
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
  
  names(cfieldall)[!is.na(match(names(cfieldall), unitkey[,1]))]
  names(cfieldall)[names(cfieldall) %in% unitkey[,1]] <- unitkey[na.omit(match(names(cfieldall), unitkey[,1])), 2] 
   
#==================================================================#
  #print(paste(nrow(cfieldall), "rows output"))
  if(tofile == TRUE){
    write.csv(cfieldall, file.path("inst", "extdata", "fieldall.csv"), row.names = FALSE)
  }
  
  cfieldall
  
}