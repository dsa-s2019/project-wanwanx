require(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

require(data.table)
require(ggplot2)
require(plotrix)
require(RBGL)
require(graph)
require(pcalg)
require(Rgraphviz)

dir_raw<-"../ACIC_data/"
dir_out<-"../memo/plot/"

data("gmG")

## skeleton
suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x))
skel.gmG <- skeleton(suffStat, indepTest = gaussCItest,
                       p = ncol(gmG8$x), alpha = 0.01)
par(mfrow = c(1,2))
plot(gmG8$g, main = ""); plot(skel.gmG, main = "")


## pc
n <- nrow (gmG8$ x)
V <- colnames(gmG8$ x) # labels aka node names
## estimate CPDAG
pc.fit <- pc(suffStat = list(C = cor(gmG8$x), n = n),
             indepTest = gaussCItest, ## indep.test: partial correlations
             alpha=0.01, labels = V, verbose = FALSE)
if (require(Rgraphviz)) {
  ## show estimated CPDAG
  par(mfrow=c(1,2))
  plot(pc.fit, main = "Estimated CPDAG")
  plot(gmG8$g, main = "True DAG")
}


## ges
score <- new("GaussL0penObsScore", gmG8$x)
## Estimate the essential graph
ges.fit <- ges(score)
## Plot the estimated essential graph and the true DAG
if (require(Rgraphviz)) {
  par(mfrow=c(1,2))
  plot(ges.fit$essgraph, main = "Estimated CPDAG")
  plot(gmG8$g, main = "True DAG")
} else { ## alternative:
  str(ges.fit, max=2)
  as(as(ges.fit$essgraph,"graphNEL"),"Matrix")
}

## fci
res <- fci(suffStat = list(C = cor(gmG8$x), n = n),
           indepTest=gaussCItest,
           alpha = 0.01, labels = V, doPdsep = FALSE)
par(mfrow=c(1,2))
plot(res, main = "Estimated CPDAG")
plot(gmG8$g, main = "True DAG")


## rfci
res <- rfci(suffStat = list(C = cor(gmG8$x), n = n),
           indepTest=gaussCItest,
           alpha = 0.01, labels = V)
par(mfrow=c(1,2))
plot(res, main = "Estimated CPDAG")
plot(gmG8$g, main = "True DAG")



## gies
score <- new("GaussL0penObsScore", gmG8$x)
## Estimate the essential graph
gies.fit <- gies(score)
## Plot the estimated essential graph and the true DAG
if (require(Rgraphviz)) {
  par(mfrow=c(1,2))
  plot(gies.fit$essgraph, main = "Estimated CPDAG")
  plot(gmG8$g, main = "True DAG")
} else { ## alternative:
  str(gies.fit, max=2)
  as(as(gies.fit$essgraph,"graphNEL"),"Matrix")
}