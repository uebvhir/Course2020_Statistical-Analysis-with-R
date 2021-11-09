###################################################
### chunk number 1: libs
###################################################
library(tools)
library("RBioinf")
library("DBI")
library("RSQLite")
dbPath <- "hgu95av2-sqlite.db"


###################################################
### chunk number 2: checkForDB
###################################################
## Verify that the database is found
if (!file.exists(dbPath))
  stop("SQLite database ", dbPath, " was not found")


###################################################
### chunk number 3: connecting
###################################################
drv <- dbDriver("SQLite")
db <- dbConnect(drv, dbPath)
db


###################################################
### chunk number 4: disconnecting
###################################################
dbDisconnect(db)


###################################################
### chunk number 5: exploring
###################################################
db <- dbConnect(drv, dbPath)
dbListTables(db)
dbListFields(db, "go_probe")


###################################################
### chunk number 6: queries1
###################################################
sql <- "select affy_id, evi from go_probe"
rs <- dbSendQuery(db, sql)
rs


###################################################
### chunk number 7: queries2
###################################################
dbColumnInfo(rs)
dbGetStatement(rs)
dbGetRowsAffected(rs)


###################################################
### chunk number 8: queries3
###################################################
chunk <- fetch(rs, n=5)
chunk


###################################################
### chunk number 9: queries4
###################################################
dbGetRowCount(rs)


###################################################
### chunk number 10: queries5
###################################################
dbClearResult(rs)
rs


###################################################
### chunk number 11: buildingRstructures
###################################################
sql <- "select * from go_probe"
rs <- dbSendQuery(db, sql)
dat <- fetch(rs, n=500)
goList <- split(dat$go_id, dat$affy_id)


###################################################
### chunk number 12: basicODBCWindows eval=FALSE
###################################################
## library("RODBC")
## if (.Platform$OS.type == "windows") {
##     pcomplexXls <- system.file("extdata/pcomplex.xls", package="RBioinf")
##     if (pcomplexXls == "")
##       stop("couldn't find pcomplex.xls")
##     db <- odbcConnectExcel(pcomplexXls)
## }


###################################################
### chunk number 13: proteinComplexData eval=FALSE
###################################################
## if (!is.null(db)) {
##     sqlTables(db)
##     res <- sqlQuery(db, "select * from [Sheet1$]")
##     res[1:5, ]
## }
## 


