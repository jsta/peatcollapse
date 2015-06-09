#'@name cleanlab
#'@title Clean lab data files
#'@param proj string options are field, fieldbw, fieldfw, and meso
#'@param pwpw string choice of all, sw, pw
#'

cleanlab<-function(sumpath,proj="field",pwsw="all"){
  #sumpath<-flist[9]
    
  dt1<-read.csv(sumpath,stringsAsFactors=F)
  dt1<-dt1[dt1$SAMPLE_TYPE=="SAMP",][,c(3,4,8,19,21,22)]
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
  dwide<-dcast(dt1,date + station + pwsw ~ test_name, value.var="value",add.missing=T,fun.aggregate=mean)
  
  #choose project
  #proj="field"
  statsplit<-strsplit(dwide$station,"-")
  dwide$date<-as.POSIXct(strptime(dwide$date,"%m/%d/%Y"))
  
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
  }
  
  if(proj=="meso"){
    dprojsw<-dwide[nchar(dwide$station)==2&substring(dwide$station,1,1)=="C",]
    dproj<-dwide[nchar(dwide$station)<=5&nchar(dwide$station)>=4,]
    dproj<-dproj[dproj$station!="S-199",]
    dprojsw$site<-substring(dprojsw$station,2,2) #site = crypt
    dproj$site<-substring(dproj$station,2,2) #site = crypt
    dproj$chamber<-sapply(dproj$station,function(x) substring(x,4,nchar(x))) #chamber = core number
   
    #load treatment keys
    key<-read.table(header=FALSE,text="
                    1 elevinun                    
                    3 elevinun
                    10 elevinun
                    12 elevinun
                    17 elevinun
                    19 elevinun
                    2 elevcont
                    4 elevcont
                    9 elevcont
                    11 elevcont
                    18 elevcont
                    20 elevcont
                    6 ambinun
                    8 ambinun
                    14 ambinun
                    16 ambinun
                    22 ambinun
                    24 ambinun
                    5 ambcont
                    7 ambcont
                    13 ambcont
                    15 ambcont
                    21 ambcont
                    23 ambcont
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
  }
      
  dproj
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
  dt1[,c(1:3,6,7,10:13)]
}