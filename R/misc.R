mesokey<-function(dt){
#dt<-cmesoall
  #load treatment keys
#   #plants plus soil
#   key<-read.table(header=FALSE,text="
#                 1 1 elevcont                    
#                 1 3 elevcont
#                 3 10 elevinun
#                 3 12 elevinun
#                 5 17 elevcont
#                 5 19 elevcont
#                 1 2 elevinun
#                 1 4 elevinun
#                 3 9 elevcont
#                 3 11 elevcont
#                 5 18 elevinun
#                 5 20 elevinun
#                 2 6 ambcont
#                 2 8 ambcont
#                 4 14 ambcont
#                 4 16 ambcont
#                 6 22 ambinun
#                 6 24 ambinun
#                 2 5 ambinun
#                 2 7 ambinun
#                 4 13 ambinun
#                 4 15 ambinun
#                 6 21 ambcont
#                 6 23 ambcont
#                 1 NA elev
#                 2 NA amb
#                 3 NA elev
#                 4 NA amb
#                 5 NA elev
#                 6 NA amb
#                   ")
  
    #soil only
    key<-read.table(header=FALSE,text="
                  1 1 elevinun                    
                  1 3 elevinun
                  3 10 elevinun
                  3 12 elevinun
                  5 17 elevinun
                  5 19 elevinun
                  1 2 elevcont
                  1 4 elevcont
                  3 9 elevcont
                  3 11 elevcont
                  5 18 elevcont
                  5 20 elevcont
                  2 6 ambinun
                  2 8 ambinun
                  4 14 ambinun
                  4 16 ambinun
                  6 22 ambinun
                  6 24 ambinun
                  2 5 ambcont
                  2 7 ambcont
                  4 13 ambcont
                  4 15 ambcont
                  6 21 ambcont
                  6 23 ambcont
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