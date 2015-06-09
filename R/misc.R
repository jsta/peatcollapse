mesokey<-function(dt){
#dt<-cmesoall
  #load treatment keys
  key<-read.table(header=FALSE,text="
                1 1 elevcont                    
                1 3 elevcont
                3 10 elevcont
                3 12 elevcont
                5 17 elevcont
                5 19 elevcont
                1 2 elevinun
                1 4 elevinun
                3 9 elevinun
                3 11 elevinun
                5 18 elevinun
                5 20 elevinun
                2 6 ambcont
                2 8 ambcont
                4 14 ambcont
                4 16 ambcont
                6 22 ambcont
                6 24 ambcont
                2 5 ambinun
                2 7 ambinun
                4 13 ambinun
                4 15 ambinun
                6 21 ambinun
                6 23 ambinun
                1 NA elev
                2 NA amb
                3 NA elev
                4 NA amb
                5 NA elev
                6 NA amb
                  ")
  
  names(key)<-c("site","chamber","trt")
  
  misstrt<-which(is.na(dt$trt))
  cdt<-dt[misstrt,]
  cdt<-cdt[,-which(names(cdt)=="trt")]
  cdt<-merge(key,cdt)
  cdt<-cdt[,match(names(dt),names(cdt))]
  dt[misstrt,]<-cdt
  dt
}