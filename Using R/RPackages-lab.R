###################################################
### chunk number 1: chartrEx
###################################################
dnaSeq <- "AAGGTTCC"
comSeq <- "TTCCAAGG"
ans <- chartr("ATGC", "TACG", dnaSeq)
ans
comSeq == ans


###################################################
### chunk number 2: ans1
###################################################
dnaComplement <- function(s) {
    chartr("ATGC", "TACG", s)
}


###################################################
### chunk number 3: skeletonExample
###################################################
user <- Sys.info()["user"]
pkgName <- paste("DNAhelper", user, sep="")
if (interactive()) {
    package.skeleton(name=pkgName, list="dnaComplement")
}


