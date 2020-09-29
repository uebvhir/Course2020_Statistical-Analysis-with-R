###################################################
### chunk number 1: NULL
###################################################
length(NULL)


###################################################
### chunk number 2: NAtypes
###################################################
typeof(NA)
as.character(NA)
as.integer(NA)


###################################################
### chunk number 3: infandnan
###################################################
y = 1/0
y
-y
y-y



###################################################
### chunk number 4: types
###################################################
typeof(y)
mode(NA)
storage.mode(letters[1:4])


###################################################
### chunk number 5: testis
###################################################
is.integer(y)
is.character(y)
is.double(y)
is.numeric(y)


###################################################
### chunk number 6: colon
###################################################
1:3
1.3:3.2

x=11:20
x[4]
x[4:5]



###################################################
### chunk number 7: avec
###################################################
 x = c(1,2,3,4)
 x
 dim(x) = c(2,2)
 x
 typeof(x)

 y = letters[1:10]
 y
 dim(y) = c(2,5)
 y
 typeof(y)


###################################################
### chunk number 8: createVecs
###################################################

 c(1, 3:5)
 c(1, "c")
 numeric(2)
 character(2)
 seq(1, 10, by = 2)
 sample(1:100, 5)



###################################################
### chunk number 9: simpleFuns
###################################################

 sum(numeric())
 prod(numeric())



###################################################
### chunk number 10: numericalProb
###################################################
 a <- sqrt(2)
 a * a == 2
 a * a - 2


###################################################
### chunk number 11: factorEx1
###################################################
set.seed(123)
x=sample(letters[1:5], 10, replace=TRUE)
y=factor(x)
y
attributes(y)



###################################################
### chunk number 12: factorEx
###################################################
 y=sample(letters[1:5], 20, rep=T)
 v = as.factor(y)
 xx=list(I=c("a","e"), II=c("b", "c", "d"))
 levels(v) = xx
 v


###################################################
### chunk number 13: ordFact
###################################################
 z= ordered(y)
 class(z)



###################################################
### chunk number 14: listex
###################################################

 y = list(a=1, 17, b=4:5, c="a")
 y
 names(y)



###################################################
### chunk number 15: listex2
###################################################

l2 = list(mn = mean, var = var)

l3 = list(l2, y)


###################################################
### chunk number 16: envEx
###################################################

 e1 = new.env(hash=TRUE)

 e1$a = 10
 ls(e1)

 e1[["a"]]



###################################################
### chunk number 17: simpleFun
###################################################
median


###################################################
### chunk number 18: simpFun2
###################################################
get("+")


###################################################
### chunk number 19: add
###################################################

 x=1:4
 x+5
 myP = get("+")
 myP
 myP(x, 5)


###################################################
### chunk number 20: sqbisafun
###################################################
 myl = list(a1 = 10, b=20, c=30)
 myl[c(2,3)]

 myl$a
 myl["a"]
 f = "b"
 myl[[f]]
 myl$f


###################################################
### chunk number 21: subpos
###################################################
 x= 11:20
 x[c(1,3,5)]


###################################################
### chunk number 22: posind
###################################################
  x= 1:10
  x[1:3]
  x[9:11]
  x[0:1]
x[c(1,2,NA)]


###################################################
### chunk number 23: charSub
###################################################
x=1:5
names(x) = letters[1:5]
x[c("a", "d")]


###################################################
### chunk number 24: logicalSubs
###################################################
(letters[1:10])[c(TRUE, FALSE, NA)]
(1:5)[rep(NA,6)]


###################################################
### chunk number 25: emptySub
###################################################
x=matrix(1:9,nc=3)
x[,1]
x[1,]


###################################################
### chunk number 26: dropSubs
###################################################
x[,1,drop=FALSE]
x[1,,drop=FALSE]


###################################################
### chunk number 27: arrayMatrixSub
###################################################
x=array(1:27, dim=c(3,3,3))
y= matrix(c(1,2,3,2,2,2,3,2,1), byrow=TRUE, ncol=3)
x[y]


###################################################
### chunk number 28: posassign
###################################################
 x[1:3] <- 10
 x


###################################################
### chunk number 29: negassignb
###################################################
 x=1:10
 x[-(2:4)] = 10
 x


###################################################
### chunk number 30: subassignall
###################################################
x=matrix(1:10, nc=2)
x[] = sort(x)


###################################################
### chunk number 31: forloop
###################################################

for(i in 1:3) print(i)

for(i in 1:5) if(i > 3 ) break
i



###################################################
### chunk number 32: ifelse
###################################################

 x = matrix(1:10, nc=2)
 ifelse( x < 2, x, c(10, 11, 12))



###################################################
### chunk number 33: simpleVect
###################################################

 x= 11:15
 x + 3


###################################################
### chunk number 34: Vect2
###################################################

 nchar(month.name)



###################################################
### chunk number 35: simpRecyc
###################################################

 1:10 + 1:3



###################################################
### chunk number 36: recyc2
###################################################

 1:3 + numeric()

 x = matrix(1:10, nc=2)
 x+(1:2)



###################################################
### chunk number 37: search
###################################################
search()


###################################################
### chunk number 38: findandget
###################################################
find("+")
get("+")


###################################################
### chunk number 39: 
###################################################
assign("+", function(e1, e2) print("howdy"))
1+10
rm("+")
1+10


###################################################
### chunk number 40: simpleEx
###################################################
library(tools)


###################################################
### chunk number 41: printex
###################################################

print("a")
v = print("a")
v



###################################################
### chunk number 42: evalEx
###################################################
x=expression(1:10)
x
eval(x)
evalq(x)
eval(quote(x))


###################################################
### chunk number 43: evalEx2
###################################################

 e = new.env()
 e$x = 10
 evalq(x, envir=e)



###################################################
### chunk number 44: usesoflocal
###################################################
     gg <- local({
         k <- function(y) f(y)
         f <- function(x) if(x) x*k(x-1) else 1
     })
     gg
     ls(environment(gg))
     for (i in 1:5) print( gg(i) )


###################################################
### chunk number 45: namespaces
###################################################
loadedNamespaces()
MASS::lda
loadedNamespaces()
search()


###################################################
### chunk number 46: simpleEx
###################################################

foo <- function() {
  y <- 10
  function(x) x+y
}

bar <- foo()
bar
is.function(bar)
bar(3)


###################################################
### chunk number 47: ex2
###################################################
 bar2 = function(x) x + z
 e1 = new.env()
 e1$z = 20
 tryCatch(bar2(11), error=function(x) "bar2 failed")
 environment(bar2) = e1
 tryCatch(bar2(11), error=function(x) "bar2 failed")


###################################################
### chunk number 48: mlfun
###################################################
Rmlfun <-
  function(x) {
    sumx <- sum(x)
    n <- length(x)
    function(mu)
      n * log(mu) - mu * sumx
  }


###################################################
### chunk number 49: mleval
###################################################
 efun <- Rmlfun(1:10)   # efun is a function!
 efun(3)

 efun2 <- Rmlfun(20:30)
 efun2(3)
 efun(3)                # nothing has changed for efun


###################################################
### chunk number 50: mklike
###################################################
Rmklike <-
  function(data) {
    n <- length(data)
    sumx <- sum(data)
    lfun <- function(mu) n * log(mu) - mu * sumx
    score <- function(mu) n / mu - sumx
    d2 <- function(mu) -n / mu^2
    list(lfun = lfun, score = score, d2 = d2)
  }


###################################################
### chunk number 51: newton
###################################################
newton <- function(lfun, est, tol = 1e-7, niter = 500) {
    cscore <- lfun$score(est)
    if (abs(cscore) < tol)
      return(est)
    for (i in 1:niter) {
      new <- est - cscore / lfun$d2(est)
      cscore <- lfun$score(new)
      if (abs(cscore) < tol)
        return(new)
      est <- new
    }
    stop("exceeded allowed number of iterations")
  }


###################################################
### chunk number 52: lexEx2
###################################################

e1 <- new.env()
e1$a <- 10
foo <- function(x) x+a
environment(foo) <- e1
foo(4)



###################################################
### chunk number 53: lexEx2cont
###################################################
e1$a <- 20
foo(4)
e1[["a"]]


###################################################
### chunk number 54: getEx
###################################################
b <- get("foo")
b(23)


###################################################
### chunk number 55: tryCatchEx
###################################################
foo <- function(x) {
    if( x < 3 ) list() + x
    else {
      if(x < 10 )  warning("ouch")
      else 33
   }
}

tryCatch(foo(2), error=function(e) "an error", warning = function(e)
"a warning")

tryCatch(foo(5), error=function(e) "an error", warning = function(e)
"a warning")

tryCatch(foo(29))



###################################################
### chunk number 56: filenotfound
###################################################

FNFcondition <- function (message, call = NULL)
{
    class <- c("fileNotFound", "error", "condition")
    structure(list(message = as.character(message), call = call),
        class = class)
}

v1 <- FNFcondition("file not found")

tryCatch( signalCondition(v1), fileNotFound = function(e) e )
tryCatch( signalCondition(v1), condition = function(e) "condition" )



###################################################
### chunk number 57: Ex1
###################################################

 odd <- function(x) (x %% 2) > 0


###################################################
### chunk number 58: Ex2
###################################################
runmax <- function(x, width, na.rm=FALSE) {
    if( na.rm ) {
        na.code <- ifelse(na.rm,-1,+1)*1e347 ### recode NA as very small/large
        x[is.na(x)]<-na.code
    }
    nsub<-length(x)-width+1              ### number of subsequence blocks
    xcol<-col(matrix(0, width, nsub))
    xord<-xsub<-xcol
    xsub[]<-x[c(xcol+row(xcol))-1]   ### one block per col
    xord[]<-order(xcol,xsub)         ### order by block and by value within block
    xmax<-xsub[xord[width,]]             ### pick out last of each block
    if( na.rm )
        xmax[xmax==na.code]<-NA
    xmax
}



