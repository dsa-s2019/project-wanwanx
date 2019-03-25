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
require(Counterfactual)

dir_raw<-"../../ACIC_data/"
dir_out<-"../memo/plot/"

# ## skeleton
# suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x))
# skel.gmG <- skeleton(suffStat, indepTest = gaussCItest,
#                        p = ncol(gmG8$x), alpha = 0.01)
# par(mfrow = c(1,2))
# plot(gmG8$g, main = ""); plot(skel.gmG, main = "")
# 
# 
# ## pc
# n <- nrow (gmG8$ x)
# V <- colnames(gmG8$ x) # labels aka node names
# ## estimate CPDAG
# pc.fit <- pc(suffStat = list(C = cor(gmG8$x), n = n),
#              indepTest = gaussCItest, ## indep.test: partial correlations
#              alpha=0.01, labels = V, verbose = FALSE)
# if (require(Rgraphviz)) {
#   ## show estimated CPDAG
#   par(mfrow=c(1,2))
#   plot(pc.fit, main = "Estimated CPDAG")
#   plot(gmG8$g, main = "True DAG")
# }
# 
# 
# ## ges
# score <- new("GaussL0penObsScore", gmG8$x)
# ## Estimate the essential graph
# ges.fit <- ges(score)
# ## Plot the estimated essential graph and the true DAG
# if (require(Rgraphviz)) {
#   par(mfrow=c(1,2))
#   plot(ges.fit$essgraph, main = "Estimated CPDAG")
#   plot(gmG8$g, main = "True DAG")
# } else { ## alternative:
#   str(ges.fit, max=2)
#   as(as(ges.fit$essgraph,"graphNEL"),"Matrix")
# }
# 
# ## fci
# res <- fci(suffStat = list(C = cor(gmG8$x), n = n),
#            indepTest=gaussCItest,
#            alpha = 0.01, labels = V, doPdsep = FALSE)
# par(mfrow=c(1,2))
# plot(res, main = "Estimated CPDAG")
# plot(gmG8$g, main = "True DAG")
# 
# 
# ## rfci
# res <- rfci(suffStat = list(C = cor(gmG8$x), n = n),
#            indepTest=gaussCItest,
#            alpha = 0.01, labels = V)
# par(mfrow=c(1,2))
# plot(res, main = "Estimated CPDAG")
# plot(gmG8$g, main = "True DAG")
# 
# 
# 
# ## gies
# score <- new("GaussL0penObsScore", gmG8$x)
# ## Estimate the essential graph
# gies.fit <- gies(score)
# ## Plot the estimated essential graph and the true DAG
# if (require(Rgraphviz)) {
#   par(mfrow=c(1,2))
#   plot(gies.fit$essgraph, main = "Estimated CPDAG")
#   plot(gmG8$g, main = "True DAG")
# } else { ## alternative:
#   str(gies.fit, max=2)
#   as(as(gies.fit$essgraph,"graphNEL"),"Matrix")
# }
# 
# ## counterfactual
# 
# data(engel)
# attach(engel)
# counter_income <- mean(income)+0.75*(income-mean(income))
# cdfx <- c(1:length(income))/length(income)
# plot(c(0,4000),range(c(0,1)), xlim =c(0, 4000), type="n", xlab = "Income",ylab="Probability")
# lines(sort(income), cdfx)
# lines(sort(counter_income), cdfx, lwd = 2, col = 'grey70')
# legend(1500, .2, c("Original", "Counterfactual"), lwd=c(1,2),bty="n",col=c(1,'grey70'))


## navy example 
data(nlsw88)
attach(nlsw88)
lwage <- log(wage)
logitres <- counterfactual(lwage~tenure+ttl_exp+grade,
                            group = union, treatment=TRUE,
                            decomposition=TRUE, method = "logit",
                            weightedboot = TRUE, sepcore = TRUE, ncore=2)
detach(nlsw88)

## dat1
dat1 <- read.csv(paste0(dir_raw,"low_dimensional_datasets/low1.csv"))
tY <- as.numeric(dat1$Y)
cf1 <- counterfactual(tY~ V1+V2+V3 , data=dat1,
                           group = A, treatment=TRUE,
                           decomposition=TRUE, method = "probit",
                           weightedboot = TRUE, sepcore = TRUE, ncore=2)

## dat2
dat2 <- read.csv(paste0(dir_raw,"low_dimensional_datasets/low2.csv"))
lY <- log(Y)
cf1 <- counterfactual(Y~ V1+V2+V3 , data=dat2,
                      group = A, treatment=TRUE,
                      decomposition=TRUE, method = "logit",
                      weightedboot = TRUE, sepcore = TRUE, ncore=2)
cf2 <- counterfactual(lY~ V1+V2+V3 , data=dat2,
                      group = A, treatment=TRUE,
                      decomposition=TRUE, method = "logit",
                      weightedboot = TRUE, sepcore = TRUE, ncore=4)
cf3 <- counterfactual(lY~ . , data=dat2,
                      group = A, treatment=TRUE,
                      decomposition=TRUE, method = "logit",
                      weightedboot = TRUE, sepcore = TRUE, ncore=4)
sapply(dat2,length)
cf4 <- counterfactual(Y~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10, data=dat2,
                      group = A, treatment=TRUE,
                      decomposition=TRUE, method = "logit",
                      weightedboot = TRUE, sepcore = TRUE, ncore=2)
cf5 <- counterfactual(Y~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+
                        V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+
                        V21+V22+V23+V24+V25+V26+V27+V28+V29, data=dat2,
                      group = A, treatment=TRUE,
                      decomposition=TRUE, method = "logit",
                      weightedboot = TRUE, sepcore = TRUE, ncore=2)

