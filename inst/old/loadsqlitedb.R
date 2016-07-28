#READ IN ALL EDD FILES AND LOAD TO SQLITE DATABASE

#==================================================================#
flist<-list.files("inst/extdata/Raw/lab/EDD", full.names = TRUE, include.dirs = TRUE)

dt <- read.csv(flist[1], stringsAsFactors = FALSE)

for(i in 2:length(flist)){
  print(i)
  newdt <- read.csv(flist[i], stringsAsFactors = FALSE)
  
  missing_col <- names(dt)[!(names(dt) %in% names(newdt))]
  if(length(missing_col) > 0){
    missing_na <- matrix(NA, nrow = nrow(newdt), ncol = length(missing_col))
    newdt <- cbind(newdt, missing_na)
    names(newdt)[(ncol(newdt)-length(missing_col)+1):ncol(newdt)] <- missing_col
  }
  
  missing_col <- names(newdt)[!(names(newdt) %in% names(dt))]
  if(length(missing_col) > 0){
    missing_na <- matrix(NA, nrow = nrow(dt), ncol = length(missing_col))
    dt <- cbind(dt, missing_na)
    names(dt)[(ncol(dt)-length(missing_col)+1):ncol(dt)] <- missing_col
  }
  
  dt <- rbind(dt, newdt)
}

#drop columns of all NA
dt <- dt[,apply(dt, 2, function(x) sum(!is.na(x)) > 0)]

#==================================================================#
library(RSQLite)

eddlab <- dbConnect(dbDriver("SQLite"), "pc_eddlab.db")
#dbWriteTable(eddlab, "eddlab", dt)
#dbListTables(eddlab)
dbDisconnect(eddlab)
dbListFields(eddlab, "eddlab")

pwsw <- "'SW'"
q <- paste("
SELECT *
FROM eddlab
WHERE matrix =", pwsw
)

dbGetQuery(eddlab, q)

eddlab <- dbConnect(dbDriver("SQLite"), "pc_eddlab.db")
cl <- dbGetQuery(eddlab, "select * from eddlab where acode = 'CL'")
hist(as.numeric(cl$result))

#==================================================================#


db <- datasetsDb()
dbListTables(db)
dbListFields(db, "CO2")
dbGetQuery(db, "select * from CO2 where Treatment = 'chilled'")





