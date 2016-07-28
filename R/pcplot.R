
#plot label key
labelkey <- read.table(text = "
ph,Onsite pH
LPH,Lab pH
salinity,Salinity
CL,Chloride (mg/L)
SO4,Sulfate (mg/L)
temp,Temperature (deg C)
cond,Conductivity (mS/cm) 
ALKA,Alkalinity (mg/L)
DOC,Dissolved Organic Carbon (mg/L)
LCOND,Lab Conductivity
NH4,Ammonium (mg/L)
NOX,Nitrate (mg/L)
TDN,Total Dissolved Nitrogen (mg/L)
TN,Total Nitrogen (mg/L)
srp.um.l,SRP (um/l)
tdp.um.l,Total Dissolved Phosphorus (um/l)
srp.ppb,SRP (ppb)
tdp.ppb,TDP (ppb)
mean.salinity,Lab Salinity",sep=",")

#'@name scplot
#'@title scatter plot of peat collapse data
#'@description Create a scatter plot of peat collapse data
#'@param rangecor provide a list of logical arguments to trim data outliers
#'@param params provide a list of variables to plot. This list is parsed two at a time for scplot. Otherwise can be provided as a subset of data.frame names
#'@param dt data.frame
#'@param bwfw character choice of "bw" or "fw"
#'@param pwsw character choice of "pw" or "sw"
#'@importFrom graphics plot
#'@examples \dontrun{
#'dt <- read.csv(list.files("/home/jose/Documents/Science/Data/peatcollapse/",
#' pattern = "fieldall*")[1], stringsAsFactors = FALSE)
#'scplot(dt)
#'}
scplot <- function(dt, params = c("ph", "LPH", "salinity", "CL.mglCaCO3"), rangecor = c("LPH > 6"), bwfw = "all", pwsw = "pw"){
  #dt<-cfieldall
  if(pwsw=="pw"){
    dt<-dt[dt$pwsw=="PW",]
  }
  if(pwsw=="sw"){
    dt<-dt[dt$pwsw=="SW",]
  }
  #dt<-cfieldall
  for(i in seq(1,length(params),2)){
    curdt<-dt[,params]
    rngcor<-do.call(rbind,strsplit(rangecor," "))
    if(any(!is.na(match(params,rngcor)))){
      var<-which(!is.na(match(params,rngcor)))#variable to trim
      rngrow<-which(!is.na(match(rngcor,params)))#row of trim key
      rngstring<-paste('curdt[curdt[,"',params[var],'"]',rngcor[rngrow,2],rngcor[rngrow,3],',]',sep="")
      curdt<-eval(parse(text=rngstring))
      }
    xlab<-as.character(labelkey[match(params[i],labelkey[,1]),2])
    ylab<-as.character(labelkey[match(params[i+1],labelkey[,1]),2])
    plot(curdt[,params[i]],curdt[,params[i+1]],xlab=xlab,ylab=ylab,main=paste(bwfw,pwsw))
  }
}

bxplot<-function(dt,params=names(dt)[c(5:8,10:15,17:20,22:25)],bwfw="bw",pwsw="pw",notch=FALSE){
  #dt<-cfieldall
  parampos<-match(params,names(dt))
  dt$chamber<-as.numeric(as.character(dt$chamber))
#   dt$trt<-NA
#   dt[dt$chamber>9,"trt"]<-"treatment"
#   dt[is.na(dt$trt),"trt"]<-"control"
  if(pwsw=="pw"){
    dt<-dt[dt$pwsw=="PW",]
  }
  if(pwsw=="sw"){
    dt<-dt[dt$pwsw=="SW",]
  }

  if(bwfw=="bw"){
    dt<-dt[dt$site=="BW",]
  }
  if(bwfw=="fw"){
    dt<-dt[dt$site=="FW",]
  }
  
  allna<-which(sapply(parampos,function(x) !any(!is.na(dt[,x]))))
  if(length(allna)>0){
    parampos<-parampos[-allna]
    message(names(dt)[parampos[allna]]," is all na and has been removed")
  }
  
  for(i in parampos){
    print(i)
    #i<-parampos[8]
  ylab<-as.character(labelkey[match(names(dt)[i],labelkey[,1]),2])
  boxplot(dt[,names(dt)[i]]~dt$trt,ylab=ylab,xlab="",main=toupper(paste(bwfw,pwsw," ",min(dt$date)," - ",max(dt$date),sep="")),outline=F,notch=notch)
  }
}

#'@name tsplot
#'@title Time-series plot
#'@description Time-series plot
#'@export
#'@import dplyr
#'@importFrom graphics arrows legend points
#'@importFrom grDevices dev.off png
#'@importFrom stats sd time
#'@param dt data.frame
#'@param params character vector of column names
#'@param bwfw character choice if "fw" or "bw"
#'@param pwsw character choice of "pw" or "sw"
#'@param inout character choice of "in" or "out"
#'@param inclegend logical include legend?
#'@param tofile logical save plot to disk?
#'@param print_xaxis logical print xaxis?
#'@param print_main logical print main label?
#'@examples \dontrun{
#'cfieldall <- read.csv("/home/jose/Documents/Science/Data/peatcollapse/fieldallv9.csv",
#' stringsAsFactors = FALSE)
#'cfieldall$collect_date <- as.POSIXct(cfieldall$collect_date)
#'tsplot(cfieldall, params = names(cfieldall)[c(9,12,13,14,16,18,19,22)],
#' bwfw = "bw", pwsw = "pw", tofile = FALSE, inout = "in", inclegend = FALSE)
#' }

tsplot <- function(dt, params, bwfw, pwsw, inout, inclegend = TRUE, tofile = FALSE, print_xaxis = TRUE, print_main = TRUE){
  
  parampos <- match(params, names(dt))
  dt$chamber <- as.numeric(as.character(dt$chamber))
  
  dt <- dplyr::filter(dt, site == toupper(bwfw) & pwsw == toupper(get("pwsw")) & inout == get("inout"))
  
  allna <- which(sapply(parampos, function(x) !any(!is.na(dt[,x]))))
  if(length(allna) > 0){
    message(names(dt)[parampos[allna]], " is all na and has been removed")
    parampos <- parampos[-allna]
  }
  
  for(i in parampos){
    curdt <- dt[,c("collect_date", "chamber", "trt", names(dt)[i])]
    curdt[,4] <- as.numeric(curdt[,4])
    ylim <- c(min(curdt[,4], na.rm = T) - (sd(curdt[,4], na.rm = T) * 0.5), max(curdt[,4], na.rm = T) + (sd(curdt[,4], na.rm = T) * 0.5))
    if(ylim[1] < 0){
      ylim[1] <- 0
    }
    
    means <- aggregate(curdt[,4], by = list(curdt$collect_date, curdt$trt), function(x) mean(x, na.rm = T))
    sds <- aggregate(curdt[,4], by = list(curdt$collect_date, curdt$trt), function(x) sd(x, na.rm = T))
    names(means) <- names(sds) <- c("date", "trt", "value")
    
    if(tofile == TRUE){
      outname <- toupper(paste(bwfw,pwsw, "_", names(dt)[i], sep = ""))
      png(file.path(paste(outname, ".png", sep = "")), width = 537, height = 401)
    }
    
    for(j in unique(means[,"trt"])){
        cmean <- means[means[,"trt"]==j,]
        csd <- sds[sds[,"trt"] == j,]
        cmean$date <- as.POSIXct(cmean$date)
        
        #clean up x-axis labels
        czoo <- zoo::zoo(cmean[,3], cmean$date)
        times <- time(czoo)
        ticks <- seq(times[1], times[length(times)], by = "months")
                
      if(j == unique(means[,"trt"])[1]){
        
        main_label <- toupper(paste(bwfw, pwsw, " ", min(cmean$date), " - ", max(cmean$date), sep = ""))
        
        if(print_main == TRUE){
          plot(cmean[,"date"], cmean[,"value"], pch = 19, ylim = ylim, ylab = names(dt)[i], xaxt = "n", xlab = "", main = main_label)
          }else{
            plot(cmean[,"date"], cmean[,"value"], pch = 19, ylim = ylim, ylab = names(dt)[i], xaxt = "n", xlab = "")          
        }
        
        suppressWarnings(arrows(cmean[,"date"], cmean[,"value"] - csd[,"value"], cmean[,"date"], cmean[,"value"] + csd[,"value"], length = 0.05, angle = 90, code = 3))
        
        if(print_xaxis == TRUE){
          #format_xaxis <- "%b-%y"
          format_xaxis <- "%m-%Y"
          axis(1, at = ticks, labels = strftime(ticks, format_xaxis), tcl = -0.3, las = 2)
        }
        if(inclegend == TRUE){
          legend("topleft", c("control", "treatment"), col = c("black", "red"), pch = 19)
        }
      }else{
        points(cmean[,"date"], cmean[,"value"], pch = 19, ylim = ylim, ylab = names(dt)[i], xaxt = "n", col = "red")
        suppressWarnings(arrows(cmean[,"date"], cmean[,"value"] - csd[,"value"], cmean[,"date"], cmean[,"value"] + csd[,"value"], length = 0.05, angle = 90, code = 3, col = "red"))
      }
    }
    if(tofile == TRUE){
      dev.off()
    }
  }
}

hstplot<-function(dt,params=names(dt)[c(15)],bwfw="bw",pwsw="pw"){
  parampos<-match(params,names(dt))
  dt$chamber<-as.numeric(as.character(dt$chamber))
  #dt$trt<-NA
  #dt[dt$chamber>9,"trt"]<-"treatment"
  #dt[is.na(dt$trt),"trt"]<-"control"
  
  if(bwfw=="bw"){
    dt<-dt[dt$site=="BW",]
  }
  if(bwfw=="fw"){
    dt<-dt[dt$site=="FW",]
  }
  
  if(pwsw=="pw"){
    dt<-dt[dt$pwsw=="PW",]
  }
  if(pwsw=="sw"){
    dt<-dt[dt$pwsw=="SW",]
  }
  
  allna<-which(sapply(parampos,function(x) !any(!is.na(dt[,x]))))
  if(length(allna)>0){
    parampos<-parampos[-allna]
    message(names(dt)[parampos[allna]]," is all na and has been removed")
  }
  
  for(i in parampos){
    #i<-parampos[1]  
    print(names(dt)[i])
    curdt<-dt[,c("date","chamber","trt",names(dt)[i])]
    xlab<-as.character(labelkey[match(names(dt)[i],labelkey[,1]),2])
    
    adt<-curdt[curdt$trt==unique(curdt$trt)[1],]#control
    ahist<-hist(adt[,4],plot=F)
    bdt<-curdt[curdt$trt==unique(curdt$trt)[2],]
    bhist<-hist(bdt[,4],plot=F)
    
    dist<-ahist$breaks[2]-ahist$breaks[1]
    breaks<-seq(min(ahist$breaks,bhist$breaks)-dist,max(ahist$breaks,bhist$breaks)+dist,dist)
    ahist<-hist(adt[,4],breaks=breaks,plot=F)
    bhist<-hist(bdt[,4],breaks=breaks,plot=F)
    
    #if(is.null(xlim)){
      xlim<-c(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks))
    #}
    #if(is.null(ylim)){
      ylim<-c(0,max(ahist$counts,bhist$counts))
    #}
    
    overlap<-ahist
    for(j in 1:length(overlap$counts)){
      if(ahist$counts[j]>0 & bhist$counts[j]>0){
        overlap$counts[j]<-min(ahist$counts[j],bhist$counts[j])
      }else{
        overlap$counts[j]<-0
      }
    }
    
    plot(ahist,xlim=xlim,ylim=ylim,col="black",xlab=xlab,main=toupper(paste(bwfw,pwsw," ",min(adt$date)," - ",max(adt$date),sep="")))
    plot(bhist,xlim=xlim,ylim=ylim,add=T,col="white")
    plot(overlap,xlim=xlim,ylim=ylim,add=T,col="darkgrey")
    legend("topright",c("treatment","control"),col=c("white","black"),fill=c("white","black"))
  }
}

#'@name mesotsplot
#'@title plot mesocosm data
#'@description Plot mesocosm data as a time-series
#'@import zoo
#'@importFrom graphics axis
#'@param dt data.frame
#'@param params character vector of column names
#'@param pwsw character choice of "pw" or "sw"
#'@param tofile logical save plot to file?
#'@param addlegend logical add legend?
#'@examples \dontrun{
#'dt <- read.csv("inst/extdata/mesoall.csv")
#'mesotsplot(dt, params = names(dt)[c(6, 8, 11, 12, 13, 16, 19, 20, 21)], pwsw = "pw", tofile = FALSE)
#'}
    
mesotsplot <- function(dt,params=names(dt)[c(11)],pwsw="pw",tofile=FALSE,addlegend=TRUE){
  #dt<-read.csv("inst/extdata/mesoall.csv")
  parampos<-match(params,names(dt))
  if(any(class(dt$date) == "factor")){
    dt$date<-as.POSIXct(dt$date)
  }
  
  if(pwsw=="pw"){
    dt<-dt[dt$pwsw=="PW",]
  }
  if(pwsw=="sw"){
    dt<-dt[dt$pwsw=="SW",]
  }
  
  allna<-which(sapply(parampos,function(x) !any(!is.na(dt[,x]))))
  if(length(allna)>0){
    message(names(dt)[parampos[allna]]," is all na and has been removed")
    parampos<-parampos[-allna]
  }
  
  #library(zoo, quietly = TRUE)
  for(i in parampos){
    #i<-parampos[1]  
    #print(names(dt)[i])
    curdt<-dt[,c("date","chamber","trt",names(dt)[i])]
    ylab<-as.character(labelkey[match(names(dt)[i],labelkey[,1]),2])
    ylim<-c(min(curdt[,4],na.rm=T)-(sd(curdt[,4],na.rm=T)*2),max(curdt[,4],na.rm=T)+(sd(curdt[,4],na.rm=T)*2))
    if(ylim[1]<0){
      ylim[1]=0
    }
    
    means<-aggregate(curdt[,4],by=list(curdt$date,curdt$trt),function(x) mean(x,na.rm=T))
    sds<-aggregate(curdt[,4],by=list(curdt$date,curdt$trt),function(x) sd(x,na.rm=T))
    ns<-aggregate(curdt[,4],by=list(curdt$date,curdt$trt),function(x) NROW(x))
    sds[,3] <- sds[,3]/sqrt(ns[,3]) 
    names(means)<-names(sds)<-c("date","trt","value")
    
    #browser()
    if(tofile==TRUE){
      outname<-toupper(paste("meso",pwsw,"_",names(dt)[i],sep=""))
      png(file.path("fig","meso",paste(outname,".png",sep="")),width=489,height=386)
    }
    
    collist<-c("black","red","darkgreen","gray")
    for(j in unique(means[,"trt"])){
      #j<-unique(means[,"trt"])[2]
      cmean<-means[means[,"trt"]==j,]
      csd<-sds[sds[,"trt"]==j,]
      
      #clean up x-axis labels
      czoo<-zoo::zoo(cmean[,3],cmean$date)
      times<-time(czoo)
      ticks<-seq(times[1],times[length(times)],by="months")
      
      if(j==unique(means[,"trt"])[1]){
        plot(cmean[,"date"],cmean[,"value"],pch=19,ylim=ylim,ylab=ylab,xaxt="n",xlab="",main=toupper(paste(pwsw," ",min(cmean$date)," - ",max(cmean$date),sep="")))
        suppressMessages(arrows(cmean[,"date"],cmean[,"value"]-csd[,"value"],cmean[,"date"],cmean[,"value"]+csd[,"value"],length=0.05,angle=90,code=3))
        axis(1,at=ticks,labels=strftime(ticks,"%b-%y"),tcl=-0.3,las=2)
        if(addlegend==TRUE){
        legend("topleft",legend=c("ambcont","ambinun","elevcont","elevinun"),col=c("black","red","darkgreen","gray"),pch=19,horiz=TRUE)
        }
      }else{
        points(cmean[,"date"],cmean[,"value"],pch=19,ylim=ylim,ylab=ylab,xaxt="n",col=collist[match(j,unique(means[,"trt"]))])
        suppressMessages(arrows(cmean[,"date"],cmean[,"value"]-csd[,"value"],cmean[,"date"],cmean[,"value"]+csd[,"value"],length=0.05,angle=90,code=3,col=collist[match(j,unique(means[,"trt"]))]))
      }
    }
    if(tofile==TRUE){
      dev.off()
    }
  }
}

#                 
#     for(j in unique(curdt[,"trt"])){
#       ctrt<-curdt[curdt$trt==j,]
#       if(j==unique(curdt[,"trt"])[1]){
#         hist(ctrt[,4],xlim=xlim,ylim=ylim,xlab=xlab,main=toupper(paste(bwfw,pwsw," ",min(ctrt$date)," - ",max(ctrt$date),sep="")),col=rgb(0,0,1,1/4))
#         }else{
#           hist(ctrt[,4],xlim=xlim,ylim=ylim,xlab=xlab,main=toupper(paste(bwfw,pwsw," ",min(ctrt$date)," - ",max(ctrt$date),sep="")),add=T,col=rgb(1,0,0,1/4))
#       }
#     }

  #initial plotting
#   chm<-unique(dt$chamber)[order(unique(dt$chamber))]
#   curdt2<-curdt[curdt$chamber==chm[1],]
#   
#   plot(curdt2$date,curdt2[,4],type="l",ylab=ylab,xlab="",ylim=ylim,xaxt="n")
#   #clean up x-axis labels
#   dtzoo<-zoo(curdt2[,4],curdt2$date)
#   times<-time(dtzoo)
#   ticks<-seq(times[1],times[length(times)],by="months")
#   axis(1,at=ticks,labels=strftime(ticks,"%b-%y"),tcl=-0.3)
#   #add remaining chambers
#   for(j in chm[-1]){
#     curdt2<-curdt[curdt$chamber==j,]
#     if(j>9){
#       lines(curdt2$date,curdt2[,4],col="red")
#     }else{
#       lines(curdt2$date,curdt2[,4],col="black")
#     }
#   }
#   legend("topleft",c("control","treatment"),col=c("black","red"),lty=1)
#   
#   }
# }