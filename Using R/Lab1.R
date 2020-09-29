###################################################
### chunk number 1: loadlib
###################################################
library("Biobase")


###################################################
### chunk number 2: showexSet
###################################################
data(exSet)
exSet


###################################################
### chunk number 3: exploringclass
###################################################

class(exSet)
slotNames(exSet)
exSet$cov1
exSet[1,]
exSet[,1]



###################################################
### chunk number 4: envEx
###################################################

 e1 = new.env(hash=TRUE)
 e1$a = rnorm(10)
 e1$b = runif(20)
 ls(e1)
 xx = as.list(e1)
 names(xx)
 rm(a, envir=e1)



###################################################
### chunk number 5: knn
###################################################
library("class")
apropos("knn")


###################################################
### chunk number 6: knnE1
###################################################

exprsExSet = exprs(exSet)
classExSet = exSet$cov2
esub = exSet[,-1]
pred1 = knn(t(exprs(esub)), exprs(exSet)[,1], esub$type)

classExSet[1]


###################################################
### chunk number 7: GOexample
###################################################
library("GO")
library("hgu95av2")
affyGO = as.list(hgu95av2GO)
#find the MF terms
affyMF = lapply(affyGO, function(x) {
   onts = sapply(x, function(z) z$Ontology)
   if( is.null(unlist(onts)) || is.na(unlist(onts)) )
     NA
   else
     unique(names(onts)[onts=="MF"])
})



###################################################
### chunk number 8: aprop
###################################################
apropos(mean)
find(mean)


###################################################
### chunk number 9: helpsearch eval=FALSE
###################################################
## help.search("mean")


###################################################
### chunk number 10: libPath
###################################################

.libPaths()



###################################################
### chunk number 11: Q1Data
###################################################
x1 <- exprs(exSet)["31340_at",]
x2 <- exprs(exSet)["31653_at",]
x <- c(x1, x2)
y <- as.factor(c(rep(hgu95av2SYMBOL$"31340_at", length(x1)), 
                  rep(hgu95av2SYMBOL$"31653_at", length(x1)))) 


###################################################
### chunk number 12: fig1
###################################################

plot(x, y, ylim=c(0.5, 2.5), axes=FALSE)
box()
axis(1)
axis(2, at=sort(as.numeric(unique(y))), labels=levels(y))


###################################################
### chunk number 13: chroms
###################################################
 whCHR = unlist(mget(geneNames(exSet), hgu95av2CHR))
 chrGenes <- table(whCHR)
 chrGenes


###################################################
### chunk number 14: Q2 Data
###################################################
mean.8 <- rep(0, length(chrGenes) - 7)
for (i in 1:length(mean.8))
  mean.8[i] <- mean(chrGenes[i:(i+7)])


###################################################
### chunk number 15: fig2
###################################################
mp <- barplot(chrGenes, density=10, xlab="Chromosomes", 
              ylab="Number of Genes", col=1)
lines(mp[8:length(chrGenes)], mean.8,   lty=1, lwd=2, col=2)
points(mp[8:length(chrGenes)], mean.8, pch=16, col=2)


###################################################
### chunk number 16: 
###################################################
sessionInfo()


