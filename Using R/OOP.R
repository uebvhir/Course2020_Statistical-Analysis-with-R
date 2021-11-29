###################################################
### chunk number 1: loadLibs
###################################################
library("RBioinf")
library("graph")
library("Rgraphviz")


###################################################
### chunk number 2: ffex
###################################################
setClass("passenger", representation(name="character", phone="character"))
setClass("freqflyer", representation(ffnumber = "integer"),
   contains = "passenger")
getClass("freqflyer")
subClassNames("passenger")
superClassNames("freqflyer")


###################################################
### chunk number 3: rectEx
###################################################
setClass("rectangle", 
   representation(h="numeric", w="numeric", area="numeric"))

myr = new("rectangle", h=10, w=20, area=200)

setGeneric("area", function(rect) standardGeneric("area"))

setMethod("area", "rectangle", function(rect) rect@area)

myr@area

area(myr)


###################################################
### chunk number 4: oddS3
###################################################
 x = 1:10
 class(x)
 dim(x) = c(2,5)
 class(x)
 attr(x, "class")


###################################################
### chunk number 5: S3ex1
###################################################

 x=1:10
 class(x) = "apple"

 y = mean
 class(y) = c("apple", "fruit")

 inherits(x, "apple")
 inherits(y, "fruit")

 z=factor(1:3, ordered=TRUE)
 class(z)


###################################################
### chunk number 6: S3ex2
###################################################
 x = matrix(1:10)
 inherits(x, "matrix")
 inherits(x, "integer")


###################################################
### chunk number 7: glmEX
###################################################
   counts <- c(18,17,15,20,10,20,25,13,12)
   outcome <- gl(3,1,9)
   treatment <- gl(3,3)
   d.AD <- data.frame(treatment, outcome, counts)
   glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
   class(glm.D93)
   is.list(glm.D93)
   attr(glm.D93, "class")



###################################################
### chunk number 8: glmEX2
###################################################
names(glm.D93)



###################################################
### chunk number 9: VARLS3
###################################################

 ex1VL = c("Sex, M=MALE, F=FEMALE", "Age in years")
 names(ex1VL) = c("Sex", "Age")
 class(ex1VL) = "VARLS3"


###################################################
### chunk number 10: simData
###################################################
 set.seed(123)
 simExprs = matrix(rgamma(10000, 500), nc=10, nr=100)
 simS = sample(c("M", "F"), 10, rep=TRUE)
 simA = sample(30:45, 10, rep=TRUE)
 simPD = data.frame(Sex=simS, Age=simA)


###################################################
### chunk number 11: newS3PD
###################################################

 new.EXPRS3 = function(Class, eData, pData, cDesc) {
     if(!is.matrix(eData) )
         stop("invalid expression data")
     if(!is.data.frame(pData) )
         stop("invalid phenotypic data")
     if(!inherits(cDesc, "VARLS3"))
         stop("invalid cov description")
     ncE = ncol(eData)
     nrP = nrow(pData)
     if( ncE != nrP )
         stop("incorrect dimensions")
     pD = list(pData=pData, varLabels=cDesc)
     class(pD) = "PHENODS3"
     ans = list(exprs=eData, phenoData = pD)
     class(ans) = class(Class)
     ans
}


###################################################
### chunk number 12: makeNewS3
###################################################

 myES3 = new.EXPRS3("EXPRS3", simExprs, simPD, ex1VL)



###################################################
### chunk number 13: ES3printmethod
###################################################
  print.PHENODS3 = function(object) {
      dm = dim(object$pData)
      cat("instance of PHENODS3 with ", dm[2], " variables", sep="")
      cat(" and ", dm[1], " cases\n", sep="")
      vL <- object$varLabels
      cat("\t varLabels\n")
      nm <- names(vL)
      for(i in seq(along=vL) )
         cat("\t\t", nm[[i]], ": ", vL[[i]], "\n", sep="")
   }

  print.EXPRS3 = function(object) {
      dm = dim(object$exprs)
      cat("instance of EXPRS3\n")
      cat("number of genes:", dm[1], "\n")
      cat("number of samples:", dm[2], "\n")
      print(object$phenoData)
   }


###################################################
### chunk number 14: S3genericEx
###################################################
 myfun = function(x, ...) UseMethod("myfun", x)
 myfun.foo = function(x, ...) print(ls(all=TRUE))

 y=1; class(y) = c("foo", "zip", "zoom")
 myfun(y)



###################################################
### chunk number 15: S3generic2
###################################################
myfun.foo = function(x, ...) {
  print(paste(".Generic =", .Generic))
  print(paste(".Class =", paste(.Class, collapse=", ")))
  print(paste(".Method =", .Method))
}
myfun(y)


###################################################
### chunk number 16: S3methods
###################################################
methods("mean")


###################################################
### chunk number 17: S3glmmethods
###################################################

 methods(class="glm")



###################################################
### chunk number 18: findMethods
###################################################
apropos("\\$<-")



###################################################
### chunk number 19: setClass
###################################################
  setClass("a", representation(s1="numeric"), prototype=list(s1=0))

  myA <- new("a")
  myA

  m2 = new("a", s1=10)
  m2


###################################################
### chunk number 20: superClass
###################################################
   setClass("b", contains="a", representation(s2="character"),
          prototype=list(s2="a"))
   myB <- new("b")
   myB


###################################################
### chunk number 21: removeClass
###################################################
setClass("ohno", representation(y="numeric"))
getClass("ohno")
removeClass("ohno")
tryCatch(getClass("ohno"), error=function(x) "ohno is gone")


###################################################
### chunk number 22: aSlots
###################################################
getSlots("a")
slotNames("a")


###################################################
### chunk number 23: showExtends
###################################################
extends("b")
extends("b", "a")
extends("a", "b")
superClassNames("b")
subClassNames("a")


###################################################
### chunk number 24: simpleUse
###################################################
getClass("factor")
extends("factor")


###################################################
### chunk number 25: unclassEx
###################################################
y = new("a", s1=10)
unclass(y)


###################################################
### chunk number 26: proto1
###################################################
 setClass("a", representation=list(d="numeric"), prototype=list(d=10))
 new("a")
 setClass("b", representation=list(d="numeric"), prototype=prototype(d=10))
 new("b")


###################################################
### chunk number 27: getproto
###################################################
 getPrototype(getClass("a"))
 getPrototype(getClass("b"))


###################################################
### chunk number 28: protofun
###################################################
 pr1 = prototype(d=10)
 class(pr1)
 pr1


###################################################
### chunk number 29: initialize
###################################################

  setClass("ex1", representation(s1="numeric"), prototype=prototype(s1=rnorm(10)))
  b <- new("ex1")
  b



###################################################
### chunk number 30: init2
###################################################

setClass("xx", representation(a="numeric", b="character"),
  prototype(a=3, b="hi there"))

new("xx")

setMethod("initialize", "xx", function(.Object, b ) {
   .Object@b <- b
   .Object@a <- nchar(b)
   .Object
})

 new("xx", b="yowser")



###################################################
### chunk number 31: initializeNM
###################################################

  setClass("aX", representation(s1="numeric"), prototype=list(s1=1))
  setClass("cX", contains="aX", representation(s2="numeric"))

  ax1 = new("aX", s1=10)
  ax1
  new("cX")
  new("cX", ax1)
  new("cX", ax1, s1=20)


###################################################
### chunk number 32: initStuff
###################################################

   setMethod("initialize", "aX", function(.Object, s1) {
      print("init aX")
      if( !missing(s1) )
           .Object@s1 = s1 
      .Object} 
     )

   new("aX")
   
   new("aX", s1=100)


###################################################
### chunk number 33: init2
###################################################

   setMethod("initialize", "cX", function(.Object, s2, s1) {
      print("init c")
      if( !missing(s1) )
        .Object = callNextMethod(.Object, s1=s1)
      if(!missing(s2) )
        .Objec@s2 = s2
      .Object} 
   )



###################################################
### chunk number 34: noslots
###################################################
setClass("seq", prototype=numeric(3))
s1 = new("seq")
s1
attributes(s1)
slotNames(s1)



###################################################
### chunk number 35: initNoSlot
###################################################
 setMethod("initialize", "seq", function(.Object) {
   .Object[1]=10; .Object})

 new("seq")


###################################################
### chunk number 36: funExt
###################################################

setClass("DBFunc", "function")
setMethod("$", signature = c("DBFunc", "character"),
  function(x, name) x(name))

mytestFun = function(arg) print(arg)

mtF = new("DBFunc", mytestFun)
mtF$y



###################################################
### chunk number 37: cUex
###################################################
setClassUnion("lorN", c("list", "NULL"))
subClassNames("lorN")
superClassNames("lorN")

isVirtualClass("lorN")
isClassUnion("lorN")


###################################################
### chunk number 38: 
###################################################
setClass("foo", representation(a="ANY"))
setGeneric("a", function(object) standardGeneric("a"))
setMethod("a", "foo", function(object) object@a)
b<-new("foo", a=10)
a(b)


###################################################
### chunk number 39: simpleGeneric
###################################################

 setGeneric("foo", 
   function(object, x) standardGeneric("foo") )

 setMethod("foo", signature("numeric", "character"),
   function(object, x) print("Hi, I'm method one"))



###################################################
### chunk number 40: genericReturns
###################################################
 setGeneric("foo", function(x,y,...) {
  y = standardGeneric("foo")
   print("I'm back")
  y
 })

 setMethod("foo", "numeric", function(x,y,...) {print("I'm gone")}
 )

 foo(1)


###################################################
### chunk number 41: Rdots
###################################################
 setGeneric("bar", function(x, y, ...) standardGeneric("bar"))

 setMethod("bar", signature("numeric", "numeric"),
    function(x, y, d) print("Method1"))

 ##removes the method above
 setMethod("bar", signature("numeric", "numeric"),
    function(x,y, z) print("Method2"))

 bar(1,1,z=20)
 bar(2,2,30)
 tryCatch(bar(2,4,d=20), error=function(e) print("no method1"))



###################################################
### chunk number 42: replacement
###################################################

 setGeneric("a<-", function(x, value)
            standardGeneric("a<-"))

 setReplaceMethod("a", "foo",
  function(x, value) {
    x@a <- value
    x
  })

  a(b) <- 32

  a


###################################################
### chunk number 43: ldots
###################################################

 cnew = function(x, ...) {
   if(nargs() < 3)
    c2(x, ...)
   else
    c2(x, cnew(...))
 }


###################################################
### chunk number 44: ldots2
###################################################

 c2 = function(x, y) standardGeneric("c2")



###################################################
### chunk number 45: ex1
###################################################
setClass("object")
setClass("grid-layout", contains="object")
setClass("horizontal-grid", contains="grid-layout")
setClass("vertical-grid", contains="grid-layout")
setClass("hv-grid", contains=c("horizontal-grid", "vertical-grid"))

LPO("hv-grid")



###################################################
### chunk number 46: cpotest
###################################################
computeClassLinearization("object")
computeClassLinearization("grid-layout")
computeClassLinearization("vertical-grid")


###################################################
### chunk number 47: s1
###################################################
setClass("vh-grid", contains=c("vertical-grid", "horizontal-grid"))

setClass("confused", contains=c("hv-grid", "vh-grid"))

LPO("vh-grid")
tryCatch(LPO("confused"), error=function(x) "this one failed")



###################################################
### chunk number 48: plotconfG
###################################################

 confG = class2Graph("confused")

 cGa = makeNodeAttrs(confG, shape="ellipse", fill="grey", width=4)

 plot(confG, nodeAttrs=cGa)



###################################################
### chunk number 49: showExtends
###################################################

 setClass("a")
 setClass("b")
 setClass("c", contains = c("a", "b"))
 setClass("d", contains = c("b", "a"))

 extends("c")
 extends("d")

 setClass("e", contains=c("c", "d"))



###################################################
### chunk number 50: sCdemo
###################################################

getAllSuperClasses(getClass("e"))

cD = superClassDepth(getClass("e"))
cD$label
cD$depth

superClasses(getClass("e"))



###################################################
### chunk number 51: c2G
###################################################

cH = class2Graph("e")



###################################################
### chunk number 52: editWin
###################################################

 setClass("pane", contains="object")
 setClass("editing-mixin", contains="object")
 setClass("scrolling-mixin", contains="object")

 setClass("scrollable-pane", contains=c("pane", "scrolling-mixin"))
 setClass("editable-pane", contains=c("pane", "editing-mixin"))

 setClass("editable-scrollable-pane",
         contains=c("scrollable-pane", "editable-pane"))



###################################################
### chunk number 53: LPOseW
###################################################

LPO("editable-scrollable-pane")
LPO("editable-scrollable-pane", C3=TRUE)



###################################################
### chunk number 54: eWgraph
###################################################
 eWG = class2Graph("editable-scrollable-pane")

 eWGattrs = makeNodeAttrs(eWG, shape="ellipse", fill="grey", width=4)

 plot(eWG, nodeAttrs=eWGattrs)



