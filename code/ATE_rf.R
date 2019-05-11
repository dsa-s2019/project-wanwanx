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


# #TRUE ATE 
# [1] 0.1914691
# [1] 2.5
# [1] 2.090099
# [1] 0.1696688
# [1] 0.1914691
# [1] 2.5
# [1] 2.090099
# [1] 0.1696688


## use LASSO to select covariates into model(propensity and outcome)
true_ATE <- c(0.1914691,2.5,2.090099,0.1696688,
              0.1914691,2.5,2.090099,0.1696688)
file_size=8
result <- data.frame(num = seq(1,file_size),true =true_ATE,
                    RF_AIPW =rep(0,file_size),RF_TMLE=rep(0,file_size),
                    RF_AIPW_se=rep(0,file_size),RF_TMLE_se=rep(0,file_size),
                    RF_AIPW2 =rep(0,file_size),RF_TMLE2=rep(0,file_size),
                    RF_AIPW2_se=rep(0,file_size),RF_TMLE2_se=rep(0,file_size))
for (i in 1:8){
  #i=1
  dat1 <- read.csv(paste0(dir_raw,"TestDatasets_lowD/testdataset",i,".csv"))
  
  # ## detect type of Y
  # pscore_type <- outcome_type <- "binomial"
  # if(length(unique(dat1$Y)) >2){
  #   outcome_type <- "gaussian"
  # }
  
  Y <- dat1$Y
  W <- dat1$A
  X <- dat1[,-c(1,2)]
  
  
  ## create causal tree
  c.forest = causal_forest(X, Y, W)
  
  ## calculate ATE(AIPW)
  rf_AIPW_fit <- average_treatment_effect(c.forest, target.sample = "all",method="AIPW")
  result$RF_AIPW[i] <- rf_AIPW_fit["estimate"]
  result$RF_AIPW_se[i] <- rf_AIPW_fit["std.err"]
  
  
  ## calculate ATE(TMLE)
  rf_TMLE_fit <- average_treatment_effect(c.forest, target.sample = "all",method="TMLE")
  result$RF_TMLE[i] <- rf_TMLE_fit["estimate.1"]
  result$RF_TMLE_se[i] <- rf_TMLE_fit["std.err"]
  
  
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
  result$RF_AIPW2[i] <- rf_AIPW2_fit["estimate"]
  result$RF_AIPW2_se[i] <- rf_AIPW2_fit["std.err"]
  
  
  ## calculate ATE(TMLE)
  rf_TMLE2_fit <- average_treatment_effect(tau.forest, target.sample = "all",method="TMLE")
  result$RF_TMLE2[i] <- rf_TMLE2_fit["estimate.1"]
  result$RF_TMLE2_se[i] <- rf_TMLE2_fit["std.err"]
  
  print(i)
}

result$RF_AIPW_sum <- result$RF_TMLE_sum <- 
  result$RF_AIPW2_sum <- result$RF_TLME2_sum <- "N"
for (i in 1:file_size){
  if ((result$true[i] >= (result$RF_AIPW[i] - 1.95*result$RF_AIPW_se[i])) & (result$true[i] <= result$RF_AIPW[i] + 1.95*result$RF_AIPW_se[i])){
    result$RF_AIPW_sum[i] <- "Y"
  }
  if ((result$true[i] >= (result$RF_TMLE[i] - 1.95*result$RF_TMLE_se[i])) & (result$true[i] <= result$RF_TMLE[i] + 1.95*result$RF_TMLE_se[i])){
    result$RF_TMLE_sum[i] <- "Y"
  }
  if ((result$true[i] >= (result$RF_AIPW2[i] - 1.95*result$RF_AIPW2_se[i])) & (result$true[i] <= result$RF_AIPW2[i] + 1.95*result$RF_AIPW2_se[i])){
    result$RF_AIPW2_sum[i] <- "Y"
  }
  if ((result$true[i] >= (result$RF_TMLE2[i] - 1.95*result$RF_TMLE2_se[i])) & (result$true[i] <= result$RF_TMLE2[i] + 1.95*result$RF_TMLE2_se[i])){
    result$RF_TMLE2_sum[i] <- "Y"
  }
}

result$RF_AIPW_diff <- result$RF_AIPW - result$true
result$RF_TMLE_diff <- result$RF_TMLE - result$true
result$RF_AIPW2_diff <- result$RF_AIPW2 - result$true
result$RF_TMLE2_diff <- result$RF_TMLE2 - result$true

write.csv(result,file=paste0(dir_out,"method_comparison_rf.csv"))





