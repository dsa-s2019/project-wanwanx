require(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

require(data.table)
require(ggplot2)
require(plotrix)
require(ATE)
require(CausalGAM)
require("glmnet")
require("ncvreg")
require("grf")


dir_raw<-"../../ACIC_data/"
dir_out<-"../plot/"


## use LASSO to select covariates into model(propensity and outcome)
file_size=3200
result <- data.frame(num = seq(1,file_size))


start_time <- proc.time()

outcome_ind_list <- list()
pscore_ind_list <- list()
for (i in 1:50){
  #i=1
  dat1 <- fread(paste0(dir_raw,"low_dimensional_datasets/low",i,".csv"))
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
    result$LASSO_CAL[i] <- fit.summary$Estimate["ATE","Estimate"]
    result$LASSO_CAL_se[i] <- fit.summary$Estimate["ATE","StdErr"]
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
  result$LASSO_AIPW[i] <- lin$ATE.AIPW.hat
  result$LASSO_AIPW_sand_se[i] <- lin$ATE.AIPW.sand.SE
  result$LASSO_AIPW_se[i] <- lin$ATE.AIPW.asymp.SE
  result$LASSO_AIPW_bs_se[i] <- lin$ATE.AIPW.bs.SE
  
  
  
  Y <- dat1$Y
  W <- dat1$A
  X <- as.data.frame(dat1[,-c(1,2)])
  
  
  ## create causal tree
  c.forest = causal_forest(X, Y, W)
  
  ## calculate ATE(AIPW)
  rf_AIPW_fit <- average_treatment_effect(c.forest, target.sample = "all",method="AIPW")
  result$RF_AIPW[i] <- rf_AIPW_fit["estimate"]
  result$RF_AIPW_se[i] <- rf_AIPW_fit["std.err"]
  
  forest.W = regression_forest(X, W, tune.parameters = TRUE)
  W.hat = predict(forest.W)$predictions
  forest.Y = regression_forest(X, Y, tune.parameters = TRUE)
  Y.hat = predict(forest.Y)$predictions
  forest.Y.varimp = variable_importance(forest.Y)
  selected.vars = which(forest.Y.varimp / mean(forest.Y.varimp) > 0.2)
  tau.forest = causal_forest(X[,selected.vars], Y, W,
                             W.hat = W.hat, Y.hat = Y.hat,
                             tune.parameters = TRUE)
  
  ## calculate ATE(AIPW)
  rf_AIPW2_fit <- average_treatment_effect(tau.forest, target.sample = "all",method="AIPW")
  result$RF_AIPW_DUAL[i] <- rf_AIPW2_fit["estimate"]
  result$RF_AIPW_DUAL_se[i] <- rf_AIPW2_fit["std.err"]
  
  print(i)
}

proc.time()-start_time

write.csv(result,file=paste0(dir_out,"method_comparison_full.csv"))
save(outcome_ind_list,file=paste0(dir_out,"full_outcome_ind.RData"))
save(pscore_ind_list,file=paste0(dir_out,"full_pscore_ind.RData"))




