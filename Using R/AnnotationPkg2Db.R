###################################################
### chunk number 1: dumpVect
###################################################
makeDf <- function(...) {
    ## Create a data.frame from a list of vectors by 
    ## manipulating attributes.
    ## This is cheating, but convenient because it avoids
    ## conversion to factor.
    df <- list(...)
    if (length(df) < 1)
      stop("Invalid args")
    lens <- sapply(df, length)
    if (length(unique(lens)) > 1)
      stop("Invalid args: all must be vectors of the same length")
      
    attributes(df) <- list(class="data.frame",
                           row.names=seq(length=lens[1]),
                           names=paste("V", 1:length(df), sep=""))
    df
}

dumpVects <- function(fname, ...) {
    ## Write vectors passed in ... to file fname.
    ## Format is appropriate for SQLite import.
    df <- makeDf(...)
    write.table(df, row.names=FALSE, col.names=FALSE, 
                sep="|", quote=FALSE, file=fname)
}



###################################################
### chunk number 2: hgu95av2-dataACC
###################################################
library("hgu95av2")
acc <- as.list(hgu95av2ACCNUM)
## affy_id acc_num
dumpVects(fname="hgu95av2-acc.txt", names(acc), unlist(acc))


###################################################
### chunk number 3: exampleUnroll1
###################################################
dat <- list(a=1:4, b=10:11, c=111)
dat


###################################################
### chunk number 4: exampleUnroll2
###################################################
lens <- sapply(dat, length)
keys <- rep(names(dat), lens)
vals <- unlist(dat)
data.frame(key=I(keys), value=I(vals), row.names=1:length(vals))


###################################################
### chunk number 5: hgu95av2-dataPMID
###################################################
pmid <- as.list(hgu95av2PMID)
affyId <- rep(names(pmid), sapply(pmid, length))
## affy_id pm_id
dumpVects(fname="hgu95av2-pmid.txt", affyId, unlist(pmid))


###################################################
### chunk number 6: hgu95av2-dataGOontology
###################################################
## ontology codes
codes <- c("BP", "CC", "MF")
desc <- c("Biological Process", "Cell Cycle", "Molecular Function")
dumpVects(fname="hgu95av2-goOntName.txt", codes, desc)


###################################################
### chunk number 7: hgu95av2-dataGO1
###################################################
go <- as.list(hgu95av2GO)
affyId <- names(go)
affyId <- rep(affyId, sapply(go, length))
goId <- unlist(sapply(go, function(x) {
    if (length(x) == 1 && is.na(x))
      return(x)
    else
      return(names(x))
}))

evi <- unlist(sapply(go, function(x) {
    if (length(x) == 1 && is.na(x))
      return(x)
    sapply(x, function(y) y$Evidence)
}))

## affy_id go_id evidence_code
dumpVects(fname="hgu95av2-go.txt", affyId, goId, evi)


###################################################
### chunk number 8: hgu95av2-dataGO2
###################################################
ontol <- unlist(sapply(go, function(x) {
    if (length(x) == 1 && is.na(x))
      return(x)
    sapply(x, function(y) y$Ontology)
}))

goIdx <- !is.na(goId)
uGo <- goId[goIdx]
uOnt <- ontol[goIdx]
names(uOnt) <- uGo
uGo <- unique(uGo)
uOnt <- uOnt[uGo]

## go_id ontology_code
dumpVects(fname="hgu95av2-goId2Ontol.txt", uGo, uOnt)


###################################################
### chunk number 9: hgu95av2-dataGO-evi
###################################################
## Hard-coded since this data is not available in the hgu95av2 
## package except from the man page
codes <- c("IMP", "IGI", "IPI", "ISS", "IDA", "IEP", "IEA", "TAS", 
           "NAS", "ND", "IC")
descs <- c("inferred from mutant phenotype", 
           "inferred from genetic interaction", 
           "inferred from physical interaction", 
           "inferred from sequence similarity", 
           "inferred from direct assay", 
           "inferred from expression pattern", 
           "inferred from electronic annotation", 
           "traceable author statement", 
           "non-traceable author statement", 
           "no biological data available", 
           "inferred by curator")
dumpVects(fname="hgu95av2-goEvidence.txt", codes, descs)


###################################################
### chunk number 10: createSqliteDb
###################################################
library("RBioinf")
## This chunk creates the SQLite database using flat files assumed to be in the
## current working directory.  The schema is part of the RBioinf package and is
## the only reason for the dependency.
schemaFile <- file.path("schema", "affy-annotation-schema.sql")
schemaFile <- system.file(schemaFile, package="RBioinf")
if (schemaFile == "")
  stop("the database schema file was not found.")

sqlite <- system.file("sqlite/bin/sqlite3", package="RSQLite")
if (identical(sqlite, "")) 
  sqlite <- "sqlite3"

## Need to do this via system call because we want to use the SQLite specific
## .import directive which is not understood by DBI, AFAIK.
dbName <- "hgu95av2-sqlite.db"
if (file.exists(dbName))
  file.remove(dbName)
importCmd <- paste(sqlite, dbName, "<", schemaFile)
res <- system(importCmd) 
if (res != 0)
  stop("SQLite DB import failed.  This is the command that failed:\n", 
       importCmd)


