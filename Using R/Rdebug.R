###################################################
### chunk number 1: setup
###################################################
library("RBioinf")
require("methods")


###################################################
### chunk number 2: convertMode with mode list eval=FALSE
###################################################
## x <- convertMode(1:4, list())


###################################################
### chunk number 3: traceback convertMode call eval=FALSE
###################################################
## traceback()


###################################################
### chunk number 4: warning to error
###################################################
saveopt <- options(warn=2)


###################################################
### chunk number 5: warning to error
###################################################
options(saveopt)


###################################################
### chunk number 6: setVNames with browser eval=FALSE
###################################################
## setVNames <- function(x, nm)
## {
##     browser()
##     names(x) <- nm
##     asSimpleVector(x, "numeric")
## }


###################################################
### chunk number 7: call setVNames with browser
###################################################
x <- 1:10
x <- setVNames(x, letters[1:10])


###################################################
### chunk number 8: set option error to recover
###################################################
 options(error=recover)


###################################################
### chunk number 9: call convertMode with recover eval=FALSE
###################################################
## x <- convertMode(1:4, list())


###################################################
### chunk number 10: setVNames without browser eval=FALSE
###################################################
## rm("setVNames")


###################################################
### chunk number 11: call to setVNames for a matrix
###################################################
x <- matrix(1:4, nrow=2)
names(setVNames(x, letters[1:4]))


###################################################
### chunk number 12: debug asSimpleVector
###################################################
debug(asSimpleVector)


###################################################
### chunk number 13: call setVNames with debug eval=FALSE
###################################################
## names(setVNames(x, letters[1:4]))


###################################################
### chunk number 14: undebug asSimpleVector
###################################################
undebug(asSimpleVector)


###################################################
### chunk number 15: trace asSimpleVector
###################################################
trace(asSimpleVector)
x <- list(1:3, 4:5)
for (i in seq(along=x)) {
    x[[i]] <- asSimpleVector(x[[i]], "complex")
}
untrace(asSimpleVector)


###################################################
### chunk number 16: asSimpleVector
###################################################
 printWithNumbers(asSimpleVector)


###################################################
### chunk number 17: call to trace for asSimpleVector
###################################################
trace(asSimpleVector, tracer=browser, at=9)


###################################################
### chunk number 18: call asSimpleVector with trace eval=FALSE
###################################################
## names(setVNames(1:4, letters[1:4]))


###################################################
### chunk number 19: call to untrace for asSimpleVector
###################################################
untrace(asSimpleVector)


###################################################
### chunk number 20: subsetAsCharacter generic
###################################################
setGeneric("subsetAsCharacter")


###################################################
### chunk number 21: subsetAsCharacter method
###################################################
setMethod("subsetAsCharacter",
          signature(x="character", i="missing",
                    j="missing"), function(x, i, j) x)


###################################################
### chunk number 22: trace subsetAsCharacter method
###################################################
trace("subsetAsCharacter", tracer=browser,
      signature=c(x="numeric"))


###################################################
### chunk number 23: call to subsetAsCharacter that is traced eval=FALSE
###################################################
## subsetAsCharacter(1.5, 1:2)


###################################################
### chunk number 24: calls to subsetAsCharacter that are not traced
###################################################
subsetAsCharacter(1+0i, 1:2)
subsetAsCharacter("x")
untrace("subsetAsCharacter")


###################################################
### chunk number 25: codetoolsex
###################################################
library("codetools")
 findGlobals(asSimpleVector)


###################################################
### chunk number 26: codet2
###################################################
findLocals(asSimpleVector)


###################################################
### chunk number 27: codet3
###################################################
> foo=function(x, y) {
   x = 10
   z = 20
  baz(100)
 }
 checkUsage(foo, name="foo")


###################################################
### chunk number 28: profEx
###################################################
  Rprof()
  mad(runif(1000000))
  Rprof(NULL)


###################################################
### chunk number 29: summaryRprof
###################################################
  summaryRprof()


###################################################
### chunk number 30: profEx2
###################################################
  Rprof()
  mad(runif(1000000), na.rm=TRUE)
  Rprof(NULL)
  summaryRprof()


