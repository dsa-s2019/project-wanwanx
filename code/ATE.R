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
require("glmnet")
require("ncvreg")

dir_raw<-"../../ACIC_data/"
dir_out<-"../plot/"



## data prepration
# raw_list <- list()
# for (i in 1:8){
#   raw_list[[i]]<- read.csv(paste0("../../ACIC_data/TestDatasets_lowD/testdataset",
#                                   i,"_cf.csv"))
#   print(raw_list[[i]]$ATE[1])
# }

# #TRUE ATE 
# [1] 0.1914691
# [1] 2.5
# [1] 2.090099
# [1] 0.1696688
# [1] 0.1914691
# [1] 2.5
# [1] 2.090099
# [1] 0.1696688

# ## dat1(true is 0.191469)
# dat1 <- read.csv(paste0(dir_raw,"TestDatasets_lowD/testdataset2.csv"))
# Y <- dat1$Y
# treat <- dat1$A
# X <- as.matrix(subset(dat1, select = -c(Y,A)))



# fit <- ATE(Y,treat,X,ATT = FALSE)
# print(fit$est)


# ##ATE function (CAL)
# fit <- ATE(Y,treat,X,ATT = TRUE)
# print(fit$est)
# fit <- ATE(Y,treat,X,ATT = FALSE,theta = -1)
# print(fit$est)
# 
# fit <- ATE(Y,treat,X,ATT = TRUE,theta = 1)
# print(fit$est)
#plot function is empirical of every covaraite V


##GAM function set up
# for (i in 1:(ncol(dat1)-3)){
#   name <- paste0("V",i)
#   print(length(unique(dat1[[name]])))
# }


# pxnam <- paste0("s(",colnames(dat1)[3:ncol(dat1)],",df=3)")
# pflm <- as.formula(paste("A ~ ", paste(pxnam, collapse= "+")))
# 
# oxnam <- paste0("s(",colnames(dat1)[3:ncol(dat1)],",df=3)")
# oflm <- as.formula(paste("Y ~ ", paste(pxnam, collapse= "+")))
# 
# t1 <- gam(pflm, family = binomial, data=dat1)
# t2 <- gam(oflm, family = gaussian, data=dat1)

# ATE_lin <- data.frame(NumVar = c(1:(ncol(dat1)-2)))
# for (i in 1:(ncol(dat1)-2)){
#   #pxnam <- paste0("s(",colnames(dat1)[3:i],")")
#   pxnam <- colnames(dat1)[3:i+2]
#   #pxnam <- paste0("s(",c("V4","V5","V11","V15","V20","V22"),")")
#   oflm <- as.formula(paste("Y ~ ", paste(pxnam, collapse= "+")))
#   pflm <- as.formula(paste("A ~ ", paste(pxnam, collapse= "+")))
#   ## ATE calculation
#   lin <- estimate.ATE(pscore.formula = pflm,
#                         pscore.family = binomial,
#                         outcome.formula.t = oflm,
#                         outcome.formula.c = oflm,
#                         outcome.family = gaussian,
#                         treatment.var = "A",
#                         data=dat1,
#                         divby0.action="t",
#                         divby0.tol=0.001,
#                         var.gam.plot=FALSE,
#                         nboot=50)
#   ATE_lin$AIPW[i] <- lin$ATE.AIPW.hat
#   ATE_lin$IPW[i] <- lin$ATE.IPW.hat
#   ATE_lin$REG[i] <- lin$ATE.reg.hat
#   print(i)
# }
# 
# png(paste0(dir_out,"test1_linear_ATE.png"), width = 4, height = 4, units = 'in', res = 1000)
# plot(ATE_lin$AIPW,col=2,type="s")
# points(ATE_lin$IPW,col=3)
# points(ATE_lin$REG,col=4)
# dev.off()
# 
# 
# ATE_s <- data.frame(NumVar = c(1:(ncol(dat1)-2)))
# for (i in 1:(ncol(dat1)-2)){
#   pxnam <- paste0("lo(",colnames(dat1)[3:i+2],")")
#   #pxnam <- colnames(dat1)[3:i+2]
#   #pxnam <- paste0("s(",c("V4","V5","V11","V15","V20","V22"),")")
#   oflm <- as.formula(paste("Y ~ ", paste(pxnam, collapse= "+")))
#   pflm <- as.formula(paste("A ~ ", paste(pxnam, collapse= "+")))
#   ## ATE calculation
#   tryCatch({
#     s <- estimate.ATE(pscore.formula = pflm,
#                       pscore.family = binomial,
#                       outcome.formula.t = oflm,
#                       outcome.formula.c = oflm,
#                       outcome.family = binomial,
#                       treatment.var = "A",
#                       data=dat1,
#                       divby0.action="t",
#                       #divby0.tol=0.001,
#                       var.gam.plot=FALSE,
#                       nboot=50)
#     ATE_s$AIPW[i] <- s$ATE.AIPW.hat
#     ATE_s$IPW[i] <- s$ATE.IPW.hat
#     ATE_s$REG[i] <- s$ATE.reg.hat
#   }, error=function(e){})
#   print(i)
# }
# png(paste0(dir_out,"test1_s_ATE.png"), width = 4, height = 4, units = 'in', res = 1000)
# plot(ATE_s$AIPW,col=2,type="s")
# points(ATE_s$IPW,col=3)
# points(ATE_s$REG,col=4)
# dev.off()
# 
# ## causal structure
# source("https://bioconductor.org/biocLite.R")
# biocLite("RBGL")
# biocLite("graph")
# biocLite("Rgraphviz")
# require(RBGL)
# require(graph)
# require(pcalg)
# require(Rgraphviz)
# ## GIES 
# 
# score <- new("GaussL0penObsScore", dat1)
# gies.fit <- gies(score)
# png(paste0(dir_out,"test1_causal_gies.png"), width = 8, height = 8, units = 'in', res = 2000)
# plot(gies.fit$essgraph, main = "Estimated CPDAG")
# dev.off()
# 
# ## RFCI
# res <- rfci(suffStat = list(C = cor(dat1), n = nrow(gmG8$x)),
#             indepTest=gaussCItest,
#             alpha = 0.05,labels = colnames(dat1))
# png(paste0(dir_out,"test1_causal_rfci.png"), width = 8, height = 8, units = 'in', res = 2000)
# plot(res)
# dev.off()
# 
# 
# 
# #############test and training split up###################
# dat <- read.csv(paste0(dir_raw,"TestDatasets_lowD/testdataset1.csv"))
# set.seed(101) # Set Seed so that same sample can be reproduced in future also
# # Now Selecting 75% of data as sample from total 'n' rows of the data  
# sample <- sample.int(n = nrow(dat), size = floor(.80*nrow(dat)), replace = F)
# dat1 <- dat[sample, ]
# dat2  <- dat[-sample, ]
# 
# ## ATE ##
# Y <- dat2$Y
# treat <- dat2$A
# X <- dat2[,-c(1,2)]
# fit <- ATE(Y,treat,X,ATT = FALSE)
# print(fit$est)
# 
# 
# #####stepwise add in (linear)####
# max.length <- 10
# nvar <- 22
# true_ATE <- 0.191469
# full_var <- temp_var <- paste0("V",c(1:nvar))
# temp_ate <- 0
# in_var <- NULL
# step_ATE <- data.frame(length=c(1:max.length),
#                        ATE=rep(0,max.length),
#                        ATE_diff=rep(0,max.length))
# for (length in 1:max.length){
#   #length <- 2
#   temp_ATE <- rep(0,length(temp_var))
#   for(i in 1:length(temp_var)){
#     #i <- 19
#     oflm <- as.formula(paste("Y ~ ", paste(c(in_var,temp_var[i]), collapse= "+")))
#     pflm <- as.formula(paste("A ~ ", paste(c(in_var,temp_var[i]), collapse= "+")))
#     #print(oflm)
#     ## ATE calculation
#     tryCatch({
#       lin <- estimate.ATE(pscore.formula = pflm,
#                           pscore.family = binomial,
#                           outcome.formula.t = oflm,
#                           outcome.formula.c = oflm,
#                           outcome.family = binomial,
#                           treatment.var = "A",
#                           data=dat1,
#                           divby0.action="t",
#                           divby0.tol=0.001,
#                           var.gam.plot=FALSE,
#                           nboot=50)
#       temp_ATE[i] <- lin$ATE.AIPW.hat
#     }, error=function(e){})
#   }
#   ind <- which.min(abs(temp_ATE-true_ATE))
#   
#   step_ATE$addin[length] <- temp_var[ind]
#   step_ATE$ATE[length] <- temp_ATE[ind]
#   step_ATE$ATE_diff[length] <- abs(temp_ATE[ind]-true_ATE)
#   
#   in_var <- c(in_var,temp_var[ind])
#   temp_var <- temp_var[-ind]
#   
#   print(length)
# }
# png(paste0(dir_out,"test1_step_lin_ATE.png"), 
#     width = 4, height = 4, units = 'in', res = 1000)
# plot(step_ATE$ATE ~ step_ATE$length)
# abline(h=true_ATE,col=2)
# dev.off()
# 
# step_lin_final <- step_ATE$addin[1:max.length] 
# oflm <- as.formula(    paste("Y ~ ", paste(step_lin_final, collapse= "+")))
# pflm <- as.formula(paste("A ~ ", paste(step_lin_final, collapse= "+")))
# print(oflm)
# lin_final <- estimate.ATE(pscore.formula = pflm,
#                       pscore.family = binomial,
#                       outcome.formula.t = oflm,
#                       outcome.formula.c = oflm,
#                       outcome.family = binomial,
#                       treatment.var = "A",
#                       data=dat2,
#                       divby0.action="t",
#                       divby0.tol=0.001,
#                       var.gam.plot=FALSE,
#                       nboot=50)
# print(lin_final$ATE.AIPW.hat)
#  
# 
# #####stepwise add in (smooth)####
# max.length <- 10
# nvar <- 22
# true_ATE <- 0.191469
# full_var <- temp_var <- paste0("V",c(1:nvar))
# temp_ate <- 0
# in_var <- NULL
# step_ATE <- data.frame(length=c(1:max.length),
#                        ATE=rep(0,max.length),
#                        ATE_diff=rep(0,max.length))
# for (length in 1:max.length){
#   #length <- 1
#   temp_ATE <- rep(0,length(temp_var))
#   for(i in 1:length(temp_var)){
#     #i <- 19
#     oflm <- as.formula(paste("Y ~ ", paste0("s(",c(in_var,temp_var[i]),")", collapse= "+")))
#     pflm <- as.formula(paste("A ~ ", paste(c(in_var,temp_var[i]), collapse= "+")))
#     print(oflm)
#     ## ATE calculation
#     tryCatch({
#       lin <- estimate.ATE(pscore.formula = pflm,
#                           pscore.family = binomial,
#                           outcome.formula.t = oflm,
#                           outcome.formula.c = oflm,
#                           outcome.family = binomial,
#                           treatment.var = "A",
#                           data=dat1,
#                           divby0.action="t",
#                           #divby0.tol=0.001,
#                           var.gam.plot=FALSE,
#                           nboot=50)
#       temp_ATE[i] <- lin$ATE.AIPW.hat
#     }, error=function(e){})
#   }
#   ind <- which.min(abs(temp_ATE-true_ATE))
#   in_var <- c(in_var,temp_var[ind])
#   temp_var <- temp_var[-ind]
#   
#   step_ATE$addin[length] <- temp_var[ind]
#   step_ATE$ATE[length] <- temp_ATE[ind]
#   step_ATE$ATE_diff[length] <- abs(temp_ATE[ind]-true_ATE)
#   
#   print(length)
# }
# png(paste0(dir_out,"test1_step_s_ATE.png"), 
#     width = 4, height = 4, units = 'in', res = 1000)
# plot(step_ATE$ATE ~ step_ATE$length)
# abline(h=true_ATE,col=2)
# dev.off()
# 
# step_s_final <- step_ATE$addin[1:4] 
# oflm <- as.formula(paste("Y ~ ", paste0("s(",step_s_final,",df=2)",collapse= "+")))
# pflm <- as.formula(paste("A ~ ", paste0("s(",step_s_final,",df=2)",collapse= "+")))
# print(oflm)
# s_final <- estimate.ATE(pscore.formula = pflm,
#                           pscore.family = binomial,
#                           outcome.formula.t = oflm,
#                           outcome.formula.c = oflm,
#                           outcome.family = binomial,
#                           treatment.var = "A",
#                           data=dat2,
#                           divby0.action="t",
#                           divby0.tol=0.001,
#                           var.gam.plot=FALSE,
#                           nboot=50)
# print(s_final$ATE.AIPW.hat)


## propensity score model using correlation


## use LASSO to select covariates into model(propensity and outcome)
true_ATE <- c(0.1914691,2.5,2.090099,0.1696688,
              0.1914691,2.5,2.090099,0.1696688)
file_size=8
result <- data.frame(num = seq(1,file_size),true =true_ATE,
                     CAL=rep(0,file_size),CAL_sle = rep(0,file_size),
                     AIPW =rep(0,file_size),IPW=rep(0,file_size),REG=rep(0,file_size),
                     CAL_se=rep(0,file_size),CAL_sle_se=rep(0,file_size),
                     AIPW_sand_se=rep(0,file_size),
                     AIPW_asym_se=rep(0,file_size),AIPW_bs_se=rep(0,file_size),
                     IPW_asym_se=rep(0,file_size),IPW_bs_se=rep(0,file_size),
                     REG_asym_se=rep(0,file_size),REG_bs_se=rep(0,file_size))
outcome_ind_list <- list()
pscore_ind_list <- list()
for (i in 1:8){
  #i=4
  dat1 <- read.csv(paste0(dir_raw,"TestDatasets_lowD/testdataset",i,".csv"))
  #set.seed(101) 
  #sample <- sample.int(n = nrow(dat), size = floor(.80*nrow(dat)), replace = F)
  #dat1 <- dat[sample, ]
  #dat2  <- dat[-sample, ]
  
  ## detect type of Y
  pscore_type <- outcome_type <- "binomial"
  if(length(unique(dat1$Y)) >2){
    outcome_type <- "gaussian"
  }
  
  ## select vairable for outcome/propensity score by MCP
  X <- as.matrix(dat1[,-c(1,2)])
  Y <- dat1$Y
  A <- dat1$A
  varname <- colnames(X)

  #CAL using all variables
  tryCatch({
    fit <- ATE(Y,A,X,ATT = FALSE)
    fit.summary <- summary(fit)
    result$CAL[i] <- fit.summary$Estimate["ATE","Estimate"]
    result$CAL_se[i] <- fit.summary$Estimate["ATE","StdErr"]
  }, error=function(e){})
  
  #LASSO selection
  test1=cv.glmnet(X,Y,family=outcome_type)
  min_cvm <- min(test1$cvm[which(test1$nzero >= 3)])
  lambda_outcome <- test1$lambda[which(test1$cvm == min_cvm)]
  outcome_mcp <- glmnet(X,Y,lambda=lambda_outcome,family=outcome_type,
                    alpha = 1)
  outcome_ind <- (which(outcome_mcp$beta != 0) -1)[-1]
  outcome_ind_list[[i]] <- outcome_ind
  
  test2 <- cv.glmnet(X,A,family=pscore_type)
  min_cvm <- min(test2$cvm[which(test2$nzero >= 3)])
  lambda_pscore <- test2$lambda[which(test2$cvm == min_cvm)]
  pscore_mcp <- glmnet(X,A,lambda=lambda_pscore,family=pscore_type,
                        alpha = 1)
  pscore_ind <- (which(pscore_mcp$beta != 0) -1)[-1]
  pscore_ind_list[[i]]<- pscore_ind
  
  #CAL using selected variable
  tryCatch({
  X_sig <- X[,outcome_ind]
  fit <- ATE(Y,A,X_sig,ATT = FALSE)
  fit.summary <- summary(fit)
  result$CAL_sle[i] <- fit.summary$Estimate["ATE","Estimate"]
  result$CAL_sle_se[i] <- fit.summary$Estimate["ATE","StdErr"]
  }, error=function(e){})
  
  ## select vairable for outcome by MCP
  oflm <- as.formula(paste("Y ~ ", paste(varname[outcome_ind], collapse= "+")))
  pflm <- as.formula(paste("A ~ ", paste(varname[pscore_ind], collapse= "+")))
  print(oflm)
  print(pflm)
  
  lin <- estimate.ATE(pscore.formula = pflm,
                        #pscore.family = pscore_type,
                      pscore.family = binomial,
                        outcome.formula.t = oflm,
                        outcome.formula.c = oflm,
                        outcome.family = outcome_type,
                      #outcome.family = gaussian,
                        treatment.var = "A",
                        data=dat1,
                        divby0.action="t",
                        #divby0.tol=0.001,
                        var.gam.plot=FALSE,
                        nboot=500)
  result$AIPW[i] <- lin$ATE.AIPW.hat
  result$AIPW_sand_se[i] <- lin$ATE.AIPW.sand.SE
  result$AIPW_asym_se[i] <- lin$ATE.AIPW.asymp.SE
  result$AIPW_bs_se[i] <- lin$ATE.AIPW.bs.SE
  
  result$IPW[i] <- lin$ATE.IPW.hat
  result$IPW_asym_se[i] <- lin$ATE.IPW.asymp.SE
  result$IPW_bs_se[i] <- lin$ATE.IPW.bs.SE
  
  result$REG[i] <- lin$ATE.reg.hat
  result$REG_asym_se[i] <- lin$ATE.reg.asymp.SE
  result$REG_bs_se[i] <- lin$ATE.reg.bs.SE
  
  print(i)
}

result$CAL_sum <- result$CAL_sle_sum <- result$AIPW_sum <- 
  result$IPW_sum <- result$REG_sum <- "N"
for (i in 1:file_size){
  if ((result$true[i] >= (result$CAL[i] - 1.95*result$CAL_se[i])) & (result$true[i] <= result$CAL[i] + 1.95*result$CAL_se[i])){
    result$CAL_sum[i] <- "Y"
  }
  if ((result$true[i] >= (result$CAL_sle[i] - 1.95*result$CAL_sle_se[i])) & (result$true[i] <= result$CAL_sle[i] + 1.95*result$CAL_sle_se[i])){
    result$CAL_sle_sum[i] <- "Y"
  }
  if ((result$true[i] >= result$AIPW[i] - 1.95*result$AIPW_asym_se[i]) & (result$true[i] <= result$AIPW[i] + 1.95*result$AIPW_asym_se[i])){
    result$AIPW_sum[i] <- "Y"
  }
  if ((result$true[i] >= result$IPW[i] - 1.95*result$IPW_asym_se[i]) & (result$true[i] <= result$IPW[i] + 1.95*result$IPW_asym_se[i])){
    result$IPW_sum[i] <- "Y"
  }
  if ((result$true[i] >= result$REG[i] - 1.95*result$REG_asym_se[i]) & (result$true[i] <= result$REG[i] + 1.95*result$REG_asym_se[i])){
    result$REG_sum[i] <- "Y"
  }
}

result$CAL_diff <- result$CAL - result$true
result$CAL_sle_diff <- result$CAL_sle - result$true
result$AIPW_diff <- result$AIPW - result$true
result$IPW_diff <- result$IPW - result$true
result$REG_diff <- result$REG - result$true



write.csv(result,file=paste0(dir_out,"method_comparison_lasso.csv"))
save(outcome_ind_list,file=paste0(dir_out,"lasso_outcome_ind.RData"))
save(pscore_ind_list,file=paste0(dir_out,"lasso_pscore_ind.RData"))



