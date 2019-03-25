require(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

require(data.table)
require(ggplot2)
require(plotrix)
# require(RBGL)
# require(graph)
# require(pcalg)
# require(Rgraphviz)
# require(Counterfactual)
require(ATE)
require(CausalGAM)

dir_raw<-"../../ACIC_data/"
dir_out<-"../plot/"


## dat1(true is 0.191469)
dat1 <- read.csv(paste0(dir_raw,"TestDatasets_lowD/testdataset1.csv"))
Y <- dat1$Y
treat <- dat1$A
X <- as.matrix(subset(dat1, select = -c(Y,A)))

##ATE function
fit <- ATE(Y,treat,X)
#plot function is empirical of every covaraite V

##GAM function
ATE.out <- estimate.ATE(pscore.formula = A ~ s(V2),
                        pscore.family = binomial,
                        outcome.formula.t = Y ~ s(V1) + s(V2) + s(V3) + s(V4)+s(V5) + s(V6) + s(V7) + s(V8),
                        outcome.formula.c = Y ~ s(V1) + s(V2) + s(V3) + s(V4)+s(V5) + s(V6) + s(V7) + s(V8),
                        outcome.family = binomial,
                        treatment.var = "A",
                        data=dat1,
                        divby0.action="t",
                        divby0.tol=0.001,
                        var.gam.plot=FALSE,
                        nboot=50)
## print summary of estimates
print(ATE.out)



## Not run:
## a simulated data example with Gaussian outcomes
##
## number of units in sample
n <- 200
## measured potential confounders
z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)
z4 <- rnorm(n)
## treatment assignment
prob.treated <-pnorm(-0.5 + 0.75*z2)
x <- rbinom(n, 1, prob.treated)
## potential outcomes
y0 <- z4 + rnorm(n)
y1 <- z1 + z2 + z3 + cos(z3*2) + rnorm(n)
## observed outcomes
y <- y0
y[x==1] <- y1[x==1]
## put everything in a data frame
examp.data <- data.frame(z1, z2, z3, z4, x, y)
## estimate ATE
##
## in a real example one would want to use a larger number of
## bootstrap replications
##
ATE.out <- estimate.ATE(pscore.formula = x ~ s(z2),
                        pscore.family = binomial,
                        outcome.formula.t = y ~ s(z1) + s(z2) + s(z3) + s(z4),
                        outcome.formula.c = y ~ s(z1) + s(z2) + s(z3) + s(z4),
                        outcome.family = gaussian,
                        treatment.var = "x",
                        data=examp.data,
                        divby0.action="t",
                        divby0.tol=0.001,
                        var.gam.plot=FALSE,
                        nboot=50)
## print summary of estimates
print(ATE.out)
## a simulated data example with Bernoulli outcomes
##
## number of units in sample
n <- 200
## measured potential confounders
z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)
z4 <- rnorm(n)
## treatment assignment
prob.treated <-pnorm(-0.5 + 0.75*z2)
x <- rbinom(n, 1, prob.treated)
## potential outcomes
p0 <- pnorm(z4)
p1 <- pnorm(z1 + z2 + z3 + cos(z3*2))
y0 <- rbinom(n, 1, p0)
y1 <- rbinom(n, 1, p1)
## observed outcomes
y <- y0
y[x==1] <- y1[x==1]
## put everything in a data frame
examp.data <- data.frame(z1, z2, z3, z4, x, y)
## estimate ATE
##
## in a real example one would want to use a larger number of
## bootstrap replications
##
ATE.out <- estimate.ATE(pscore.formula = x ~ s(z2),
                        pscore.family = binomial,
                        outcome.formula.t = y ~ s(z1) + s(z2) + s(z3) + s(z4),
                        outcome.formula.c = y ~ s(z1) + s(z2) + s(z3) + s(z4),
                        outcome.family = binomial,
                        treatment.var = "x",
                        data=examp.data,
                        divby0.action="t",
                        divby0.tol=0.001,
                        var.gam.plot=FALSE,
                        nboot=50)
## print summary of estimates
print(ATE.out)
## End(Not run)


