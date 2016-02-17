
#'@name mesokey
#'@title Key mesocosm treatment labels to an input data.frame
#'@description Key mesocosm treatment labels to an input data.frame
#'@param dt data.frame
#'@export
mesokey <- function(dt){
  #load treatment keys
    #plants plus soil
  key<-read.table(header=FALSE,text="
                1 1 elevcont                    
                1 3 elevcont
                3 10 elevinun
                3 12 elevinun
                5 17 elevcont
                5 19 elevcont
                1 2 elevinun
                1 4 elevinun
                3 9 elevcont
                3 11 elevcont
                5 18 elevinun
                5 20 elevinun
                2 6 ambcont
                2 8 ambcont
                4 14 ambcont
                4 16 ambcont
                6 22 ambinun
                6 24 ambinun
                2 5 ambinun
                2 7 ambinun
                4 13 ambinun
                4 15 ambinun
                6 21 ambcont
                6 23 ambcont
                1 NA elev
                2 NA amb
                3 NA elev
                4 NA amb
                5 NA elev
                6 NA amb
                  ")
  
    #soil only
#     key<-read.table(header=FALSE,text="
#                   1 1 elevinun                    
#                   1 3 elevinun
#                   3 10 elevinun
#                   3 12 elevinun
#                   5 17 elevinun
#                   5 19 elevinun
#                   1 2 elevcont
#                   1 4 elevcont
#                   3 9 elevcont
#                   3 11 elevcont
#                   5 18 elevcont
#                   5 20 elevcont
#                   2 6 ambinun
#                   2 8 ambinun
#                   4 14 ambinun
#                   4 16 ambinun
#                   6 22 ambinun
#                   6 24 ambinun
#                   2 5 ambcont
#                   2 7 ambcont
#                   4 13 ambcont
#                   4 15 ambcont
#                   6 21 ambcont
#                   6 23 ambcont
#                   1 NA elev
#                   2 NA amb
#                   3 NA elev
#                   4 NA amb
#                   5 NA elev
#                   6 NA amb
#                     ")
  
  names(key)<-c("crypt","core","trt")
  
  misstrt <- which(is.na(dt$trt) & nchar(dt$station) > 2)
  cdt <- dt[misstrt,]
  cdt <- cdt[, -which(names(cdt) == "trt")]
  cdt <- merge(key, cdt)
  cdt <- cdt[, match(names(dt), names(cdt))]
  
  if(nrow(cdt) > 0){
    dt[misstrt,] <- cdt
  }
  
  dt
}

#'@name mdy2mmyyyy
#'@title convert m/d/yy to mm/dd/yyyy
#'@description Pads dates in preparation for POSIX coercion
#'@param x character date to be formatted
#'@export
#'@examples
#' x <- "5/5/15"
#' mdy2mmyyyy(x)
mdy2mmyyyy <- function(x){
  
  #strsplit based on "/"
  month <- strsplit(x, "/")[[1]][1]
  if(nchar(month) < 2){
    month <- paste("0", month, sep="")
  }
  day <- strsplit(x, "/")[[1]][2]
  if(nchar(day) < 2){
    day <- paste("0", day ,sep="")
  }
  year <- strsplit(x, "/")[[1]][3]
  if(nchar(year) < 3 & as.numeric(year) < 80){
    year <- paste("20", year, sep="")
  }else{
    if(nchar(year) < 3){
      year <- paste("19", year, sep="")
    }
  }
  
  paste(month, "/", day, "/", year, sep="")
}

#'@name date456posix
#'@title Convert numeric dates in mddyy to POSIXct
#'@description Convert numeric dates in mddyy to POSIXct
#'@param x numeric where the first 1-2 digits specify the month attribute because leading zeros have been stripped
#'@param century numeric century recommended choice of "19" or "20"
#'@export
#'@examples
#'dates <- c("51514", "101214", "8714", "1214", "81412")
#'date456posix(dates, century = "20")
date456posix <- function(x, century){
  year <- paste0(century, substring(x, (nchar(x) - 1), nchar(x)))
  day <- substring(x, (nchar(x) - 3), nchar(x) - 2)
  
  if(any(as.numeric(day) > 31)){
    day <- as.character(sapply(day, function(x){
      if(as.numeric(x) > 31){
        x <- substring(x, 2, 2)
      }
      x
    }))
  }
  
  
  mon <- substring(x, 1, nchar(x) - 4)
  
  if(any(nchar(mon) == 0)){
    mon[which(nchar(mon) == 0)] <- substring(x[which(nchar(mon) == 0)], 1, 1)
  }
  
  mon <- as.character(sapply(mon, function(x){
    if(as.numeric(x) < 10){
      x <- paste0("0", x)
    }else{
      x
    }
  }))
  
  date <- paste0(year, "-", mon, "-", day)
  return(as.POSIXct(strptime(date, format="%Y-%m-%d")))
}

#'@name align_dfcol
#'@title Align a data.frame to a template
#'@description Align a data.frame to a template
#'@param template data.frame
#'@param target data.frame
#'@export
#'@examples
#'target <- data.frame(matrix(NA, ncol = 5))
#'template <- data.frame(matrix(NA, ncol = 6))
#'names(target) <- c(letters[c(3, 1, 2, 4, 8)])
#'names(template) <- c(letters[1:6])
#'align_dfcol(target =  target, template = template)

align_dfcol <- function(target, template){
  
  target <- target[,names(target) %in% names(template)]
  
  missing.names <- names(template)[!(names(template) %in% names(target))]
  pad.na <- data.frame(matrix(NA, nrow = nrow(target), ncol = length(missing.names)))
  names(pad.na) <- missing.names
  
  target <- cbind(target, pad.na)
  target[,match(names(template), names(target))]
  
}