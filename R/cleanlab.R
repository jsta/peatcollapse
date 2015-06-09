#'@name cleanlab
#'@title Clean lab data files
#'@param proj string options are field, fieldbw, fieldfw, and meso
#'@param pwsw string choice of all, sw, pw
#'@details EBs are discarded, only meso data has FDs
#'

cleanlab<-function(sumpathlist,proj="field",pwsw="all"){
  #sumpath<-flist[9]
  fulldt<-list()
  for(j in sumpathlist){
    #j<-sumpathlist[1]
    
  dt1<-read.csv(j,stringsAsFactors=F)
  dt1<-dt1[dt1$SAMPLE_TYPE=="SAMP"|dt1$SAMPLE_TYPE=="FD",][,c(3,4,8,19,21,22)]
  if(pwsw=="sw"){
    dt1<-dt1[dt1$MATRIX=="SW",]
  }
  if(pwsw=="pw"){
    dt1<-dt1[dt1$MATRIX=="PW",]
  }
  
  names(dt1)<-tolower(names(dt1))
  names(dt1)[2:3]<-c("pwsw","datetime")
  dsplit<-matrix(unlist(strsplit(dt1$datetime,"/",fixed=T)),ncol=3,byrow=3)
  
  pad0<-function(x){
    if(nchar(x)<2){
      paste("0",x,sep="")
    }else{
      x  
    }
  }
  
  for(i in 1:2){
    dsplit[,i]<-unlist(lapply(dsplit[,i],function(x) pad0(x)),use.names=F)
  }
  
  dt1$datetime<-paste(dsplit[,1],"/",dsplit[,2],"/",dsplit[,3],sep="")
  dt1$datetime<-as.POSIXct(strptime(dt1$datetime,"%m/%d/%Y %H:%M"))
  dt1$date<-strftime(dt1$datetime,"%m/%d/%Y")
  
  library(reshape2)
    dwide<-dcast(dt1,date + station + pwsw ~ test_name, value.var="value",fun.aggregate=mean)
    
    #test<-paste(dt1[,"date"],dt1[,"station"],dt1[,"pwsw"])
    #dt1[which(paste(dt1[,"date"],dt1[,"station"],dt1[,"pwsw"])==test[1]),]
    #dwide[which(paste(dwide[,"date"],dwide[,"station"],dwide[,"pwsw"])==test[1]),]
  
  #choose project
  #proj="field"
  statsplit<-strsplit(dwide$station,"-")
  dwide$date<-as.POSIXct(strptime(dwide$date,"%m/%d/%Y"))
###########################################################  
  if(proj=="field"){
    dproj<-dwide[nchar(dwide$station)>5,]
    
    dproj$site<-do.call(rbind,strsplit(dproj$station,"-"))[,1]
    dproj$chamber<-do.call(rbind,strsplit(dproj$station,"-"))[,3]
    dproj$trt<-NA
    dproj<-dproj[nchar(dproj$chamber)<3,]
    dproj$chamber<-as.numeric(as.character(dproj$chamber))
    if(any(dproj$site=="BW")){
      dproj[dproj$chamber>9&dproj$site=="BW","trt"]<-"treatment"
      dproj[is.na(dproj$trt),"trt"]<-"control"  
    }
    if(any(dproj$site=="BW")){
      dproj[dproj$chamber>10&dproj$site=="FW","trt"]<-"treatment"
      dproj[is.na(dproj$trt),"trt"]<-"control"  
    }
    
    namestemp<-c("date","station","pwsw","ALKA","CL","DOC","LCOND","LPH","NH4","NOX","Salinity","SO4","TDN","TN","site","chamber","trt")
    namesmiss<-which(is.na(match(namestemp,names(dproj))))
    paddt<-data.frame(matrix(NA,ncol=length(namestemp[namesmiss]),nrow=nrow(dproj)))
    names(paddt)<-namestemp[namesmiss]
    dproj<-cbind(dproj,paddt)
    dproj<-dproj[,match(namestemp,names(dproj))]
    
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
    key<-read.table(header=FALSE,text="
                    1 elevcont                    
                    3 elevcont
                    10 elevcont
                    12 elevcont
                    17 elevcont
                    19 elevcont
                    2 elevinun
                    4 elevinun
                    9 elevinun
                    11 elevinun
                    18 elevinun
                    20 elevinun
                    6 ambcont
                    8 ambcont
                    14 ambcont
                    16 ambcont
                    22 ambcont
                    24 ambcont
                    5 ambinun
                    7 ambinun
                    13 ambinun
                    15 ambinun
                    21 ambinun
                    23 ambinun
                    ")

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
#     
#   pw<-dt[dt$MATRIX=="PW",]
#   bw.pw<-pw[pw$SITE=="BW",]
#   fw.pw<-pw[pw$SITE=="FW",]
#   sw<-dt[dt$MATRIX=="SW",]
#   bw.sw<-sw[sw$SITE=="BW",]
#   fw.sw<-sw[sw$SITE=="FW",]
#   test.list<-unique(dt$TEST_NAME)
#   unit.list<-dt[match(test.list,dt$TEST_NAME),"UNITS"]
#   test.list.cl<-c("Alkalinity","Chloride","Dissolved Organic Carbon","pH","NH4","NOX","SO4","Total Dissolved Nitrogen")
#   
#   
#   
#   dt
}

#'@name cleanp
#'@title clean phosphorus data
#'@description averages FDs, strips EBs

cleanp<-function(sumpath,pwsw="all"){
  dt1<-read.csv(sumpath,stringsAsFactors=F)
  if(pwsw=="sw"){
    dt1<-dt1[dt1$type=="SW",]
  }
  if(pwsw=="pw"){
    dt1<-dt1[dt1$type=="PW",]
  }
  
  names(dt1)<-tolower(names(dt1))
  names(dt1)[3]<-"pwsw"
  
  #test<-paste(")
  
  dt1<-dt1[nchar(dt1[,"date"])>0,]
  dsplit<-matrix(unlist(strsplit(dt1$date,"/",fixed=T)),ncol=3,byrow=3)
  
  pad0<-function(x){
    if(nchar(x)<2){
      paste("0",x,sep="")
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
  dt1<-dt1[dt1$chamber>0 & dt1$chamber<199,]
  dt1<-dt1[,c(1:3,6,7,10:13)]
  
  uid<-paste(dt1[,"date"],dt1[,"site"],dt1[,"pwsw"],dt1[,"chamber"],sep="")
  #savedt1<-dt1
  if(any(duplicated(uid))){
    duplist<-unique(uid[duplicated(uid)])
    
    for(j in duplist){
      #j<-duplist[2]
      curdup<-which(uid==j)
      dt1[curdup[1],5:ncol(dt1)]<-apply(dt1[curdup,5:ncol(dt1)],2,function(x) mean(x,na.rm=T))
      dt1<-dt1[-curdup[2:length(curdup)],]
      uid<-paste(dt1[,"date"],dt1[,"site"],dt1[,"pwsw"],dt1[,"chamber"],sep="")
      }
  }
  dt1[order(dt1$date),]
}