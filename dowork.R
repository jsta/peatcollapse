#Peat Collapse Data Cleaning and Analysis
#1. clean field-lab data
#2. clean field-onsite data
#3. merge cleaned lab data and cleaned onsite-field data
#4. plot field-lab/onsite data
#4a. statistical tests for treatment effect (in development)

#5. clean mesocosm-lab data
#6. clean mescocosm-onsite data
#7. merge cleaned mesocosm-lab data and cleaned onsite mesocosm data
#8. plot mesocosm-lab/onsite data

#9. join aquatroll and eden data
#10. fit model and training set and compare with test set

#1. clean field-lab data####
source("R/cleanlab.R")
#clean inputs
flist<-list.files("inst/extdata/Raw/lab","*.csv",include.dirs=T,full.names=T)
#sumpathlist<-flist[1]
#names(cleanlab(flist[1],proj="field",pwsw="all"))
#names(cleanlab(flist[2],proj="field",pwsw="all"))
sumpathlist<-flist[1:2]
clab<-cleanlab(sumpathlist,proj="field",pwsw="all")

#sumpath<-flist[1]
#clab<-cleanlab(sumpath=sumpath,proj="field",pwsw="all")
#sumpath<-flist[2]
#clab2<-cleanlab(sumpath=sumpath,proj="field",pwsw="all")
#clab3<-clab

#merge inputs
# if((ncol(clab2)-ncol(clab3))>0){
#   numdiff<-ncol(clab2)-ncol(clab3)
#   clab3[,(ncol(clab3)+1):(ncol(clab3)+numdiff)]<-NA
#   names(clab3)[(ncol(clab3)-(numdiff-1)):(ncol(clab3))]<-names(clab2)[is.na(match(names(clab2),names(clab3)))]
#   clab3<-clab3[,match(names(clab2),names(clab3))]
#   clab<-rbind(clab2,clab3)
# }else{
#   clab<-rbind(clab,clab2)
# }

#merge P data
source("R/cleanlab.R")
sumpath<-flist[3]
cp<-cleanp(sumpath)
claball<-merge(clab,cp,by=c("site","chamber","date","pwsw"),all.x=TRUE)


#2. clean field-onsite data####
source("R/cleanfieldonsite.R")
flist<-list.files("inst/extdata/Raw/onsite","*.csv",include.dirs=T,full.names=T)
#basename(flist)[6:7]
sumpathlist<-flist[c(6:7)]
cfonsite<-cfieldonsite(sumpathlist,pwsw="all")#ignore na warnings

#test#
test<-paste(cfonsite[,1],cfonsite[,2],cfonsite[,3])
if(length(test[duplicated(test)])>0){message("Duplicated rows. Ensure that only the latest data files are being globbed")}

#3. merge cleaned field-lab/onsite data####
cfieldall<-merge(cfonsite,claball,by=c("site","chamber","date","pwsw"),all.y=TRUE,all.x=TRUE)
cfieldall<-cfieldall[with(cfieldall,order(site,pwsw,date,chamber)),]
if(any(cfieldall$site=="BW")){
  cfieldall[cfieldall$chamber>9&cfieldall$site=="BW","trt"]<-"treatment"
  cfieldall[is.na(cfieldall$trt),"trt"]<-"control"  
}
if(any(cfieldall$site=="BW")){
  cfieldall[cfieldall$chamber>10&cfieldall$site=="FW","trt"]<-"treatment"
  cfieldall[is.na(cfieldall$trt),"trt"]<-"control"  
}

#write.csv(cfieldall,"inst/extdata/fieldallv4.csv")

#4. plot field-lab/onsite data####
#cfieldall<-read.csv("inst/extdata/fieldallv4.csv")[,-1]
#cfieldall$date<-as.POSIXct(cfieldall$date)
source("R/pcplot.R")
scplot(cfieldall,params=c("ph","LPH","salinity","CL"),rangecor=c("LPH > 6"),bwfw="FW and BW",pwsw="sw")#scatterplots
bxplot(cfieldall,params=names(cfieldall)[c(5:8,11:13,15:17,19:20,22:25)],bwfw="bw",pwsw="pw",notch=T)#boxplots
tsplot(cfieldall,params=names(cfieldall)[c(7)],bwfw="fw",pwsw="pw",tofile=FALSE)#timeseries
hstplot(cfieldall,params=names(cfieldall)[c(5:8,11:13,15:17,19:20)],bwfw="bw",pwsw="pw")#overlapping histograms

#4a. statistical tests for treatment effect (in development)####
#oneway.test
oneway.test(TDN~trt,data=cfieldall)
#nonparametric kruskal.test
cfieldall$trt<-as.factor(cfieldall$trt)
kruskal.test(TDN~trt,data=cfieldall[!is.na(cfieldall$TDN),])
#aov
aov.out<-aov(TDN~trt,data=cfieldall)
summary(aov.out)
#TuckeyHSD
TukeyHSD(aov.out)

#cfieldall$chamber<-droplevels(cfieldall$chamber)
#levels(cfieldall$chamber)<-seq(1,16,1)
cfieldall<-droplevels(cfieldall)
dfMod<-subset(cfieldall, trt %in% c(""))
factor(cfieldall$chamber)
b<-with(cfieldall,boxplot(TDN~chamber + trt))

lablist.x<-paste(unique(cfieldall$chamber),unique(cfieldall$trt),sep=".")
axis(1,at=c(3,6,8,10,12,14,16,18,21,23,27,29,31,33,35),labels=F)
text(x=c(3,6,8,10,12,14,16,18,21,23,27,29,31,33,35),par("usr")[3]-1.2,labels=lablist.x,srt=90,pos=1,xpd=T)

#5. clean mesocosm-lab data
source("R/cleanlab.R")
flist<-list.files("inst/extdata/Raw/lab","*.csv",include.dirs=T,full.names=T)
sumpath<-flist[2]
clab<-cleanlab(sumpath=sumpath,proj="meso",pwsw="all")
sumpath<-flist[4]
clab2<-cleanlab(sumpath=sumpath,proj="meso",pwsw="all")
if(length(names(clab))>length(names(clab2))){
  clab2[,names(clab)[which(is.na(match(names(clab),names(clab2))))]]<-NA
  clab2<-clab2[,match(names(clab),names(clab2))]
}
clab<-rbind(clab,clab2)

#6. clean mescocosm-onsite data
source("R/cleanmesoonsite.R")
flist<-list.files("inst/extdata/Raw/onsite","*.csv",include.dirs=T,full.names=T)
sumpathlist<-flist[c(5)]
cfonsite<-cmesoonsite(sumpathlist,pwsw="all")#ignore na warnings


#7. merge cleaned mesocosm-lab data and cleaned onsite mesocosm data
cmesoall<-merge(cfonsite,clab,by=c("site","chamber","date","pwsw"),all.y=FALSE,all.x=TRUE)
cmesoall<-cmesoall[with(cmesoall,order(site,pwsw,date,chamber)),]
#write.csv(cmesoall,"inst/extdata/mesoall.csv")

#8. plot mesocosm-lab/onsite data
