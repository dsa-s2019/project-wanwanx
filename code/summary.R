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

result_lasso <- read.csv(paste0(dir_out,"method_comparison_lasso.csv"))
result_rf <- read.csv(paste0(dir_out,"method_comparison_rf.csv"))

result <- merge(result_lasso,result_rf,by=c("num","X","true"))

result$LASSO_CAL <- result$CAL_sle
result$LASSO_AIPW <- result$AIPW
result$LASSO_IPW <- result$IPW
result$LASSO_REG <- result$REG

result$LASSO_CAL_se <- result$CAL_sle_se
result$LASSO_AIPW_se <- result$AIPW_asym_se
result$LASSO_IPW_se <- result$IPW_asym_se
result$LASSO_REG_se <- result$REG_asym_se

result$RF_AIPW_DUAL <- result$RF_AIPW2
result$RF_TMLE_DUAL <- result$RF_TMLE2

result$RF_AIPW_DUAL_se <- result$RF_AIPW2_se
result$RF_TMLE_DUAL_se <- result$RF_TMLE2_se


file_size <- 8
#error bar plot
method_list <- c("CAL","LASSO_AIPW","LASSO_IPW","LASSO_REG","LASSO_CAL",
            "RF_AIPW","RF_AIPW_DUAL")
df3 <- NULL
for (i in 1:length(method_list)){
  #i=1
  method <- method_list[i]
  result_temp <- result[,c(method,paste0(method,"_se"))]
  df3_temp <- data.frame(method=rep(method,file_size))
  df3_temp$file <- as.factor(c(1:file_size))
  df3_temp$est <- result_temp[[method]]
  df3_temp$se <- result_temp[[paste0(method,"_se")]]
  df3 <- rbind(df3,df3_temp)
}

for (j in 1:file_size){
  #j=1
  
  true <- result$true[j]
  p <- ggplot(df3[df3$file == j, ], aes(x=method, y=est)) + 
    geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=.1, 
                  position=position_dodge(0.05)) +
    geom_line() + geom_point()+
    scale_color_brewer(palette="Paired")+theme_minimal()+
    geom_hline(yintercept=true, linetype="dashed", color = "red")+
    theme(axis.text.x = element_text(face = "bold", color = "black", 
                                     size = 8, angle = 70),
          axis.text.y = element_text(face = "bold", color = "black", 
                                     size = 8),
          axis.title = element_text(face="bold",size = 12))
  p
  ggsave(p,file=paste0(dir_out,"method_comparison_",j,".png"),width=5,height=3)
}


## difference 


# selected order
#low track
raw_list_low <- list()
for (i in 1:8){
  raw_list_low[[i]]<- read.csv(paste0("../../ACIC_data/TestDatasets_lowD/testdataset",
                                  i,".csv"))
  #print(raw_list_low[[i]]$ATE[1])
}
raw_var <- sapply(raw_list_low,length) - 2

load(paste0(dir_out,"lasso_outcome_ind.RData"))
low_outcome <- sapply(outcome_ind_list,length)

load(paste0(dir_out,"lasso_pscore_ind.RData"))
low_pscore <- sapply(pscore_ind_list,length)

mydata <- data.frame(file=c(1:8),raw=raw_var,lasso_outcome = low_outcome)
my_data_long <- melt(mydata, id.vars = c("file"))
p <-ggplot(data=my_data_long,aes(x=file, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(color="black",size=14,face="bold"),
        axis.title.y = element_text(color="black",size=14,face="bold"))
ggsave(p,file=paste0(dir_out,"lasso_outcome.png"),width=4,height=3)

mydata <- data.frame(file=c(1:8),raw=raw_var,lasso_pscore = low_pscore)
my_data_long <- melt(mydata, id.vars = c("file"))
p <- ggplot(data=my_data_long,aes(x=file, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(color="black",size=14,face="bold"),
        axis.title.y = element_text(color="black",size=14,face="bold"))
ggsave(p,file=paste0(dir_out,"lasso_pscore.png"),width=4,height=3)


#high track
raw_list_low <- list()
for (i in 1:8){
  raw_list_low[[i]]<- read.csv(paste0("../../ACIC_data/TestDatasets_highD/highDim_testdataset",
                                      i,".csv"))
  #print(raw_list_low[[i]]$ATE[1])
}
raw_var <- sapply(raw_list_low,length) - 2

load(paste0(dir_out,"high_outcome_ind.RData"))
low_outcome <- sapply(outcome_ind_list,length)

load(paste0(dir_out,"high_pscore_ind.RData"))
low_pscore <- sapply(pscore_ind_list,length)

mydata <- data.frame(file=c(1:8),raw=raw_var,lasso_outcome = low_outcome)
my_data_long <- melt(mydata, id.vars = c("file"))
p <-ggplot(data=my_data_long,aes(x=file, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(color="black",size=14,face="bold"),
        axis.title.y = element_text(color="black",size=14,face="bold"))
ggsave(p,file=paste0(dir_out,"high_lasso_outcome.png"),width=4,height=3)

mydata <- data.frame(file=c(1:8),raw=raw_var,lasso_pscore = low_pscore)
my_data_long <- melt(mydata, id.vars = c("file"))
p <- ggplot(data=my_data_long,aes(x=file, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))+
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(color="black",size=14,face="bold"),
        axis.title.y = element_text(color="black",size=14,face="bold"))
ggsave(p,file=paste0(dir_out,"high_lasso_pscore.png"),width=4,height=3)


## difference
result_diff <- result[,grep("diff",colnames(result))] 
result_diff$RF_TMLE2_diff <- NULL
result_diff$RF_TMLE_diff <- NULL
colnames(result_diff) <- c("CAL","LASSO_CAL","LASSO_AIPW","LASSO_IPW",
                           "LASSO_REG","RF_AIPW","RF_AIPW_DUAL")
result_diff$file <- c(1:8)
result <- melt(result_diff, id.vars = c("file"))
result$file <- as.factor(result$file)

# Change color by groups
dp <-ggplot(result, aes(x=variable, y=value, fill=file)) + 
  geom_dotplot(binaxis='y', stackdir='center')+
  labs(x="methods", y = "difference")
  
p <-dp + theme_classic()+
  theme(axis.title.x = element_text(color="black",size=14,face="bold"),
        axis.title.y = element_text(color="black",size=14,face="bold"),
        legend.title=element_text(size=14), 
        legend.text=element_text(size=13),
        legend.position = "bottom")
ggsave(p,file=paste0(dir_out,"comparison_diff.png"),width=7,height=5)


## all files
# setwd("../../clusterV1/")
# temp = list.files(pattern="*.csv")
# full_list <- lapply(temp, read.csv)
# saveRDS(full_list,file="../project/plot/full_list.Rds")

full_list <- readRDS("../plot/full_list.Rds")

full_result <- rbindlist(full_list, use.names=TRUE)

require(tidyverse)  # data manipulation
require(cluster)    # clustering algorithms
require(factoextra) # clustering algorithms & visualization


mydata <- full_result[,c("CAL","LASSO_CAL","LASSO_AIPW","RF_AIPW","RF_AIPW_DUAL")]
mydata <- as.matrix(na.omit(mydata))
detect_k <- NULL
for (i in 1:5){
  res <- Ckmeans.1d.dp(t, k=c(1:40), mydata[,i])
  detect_k[i] <- max(res$cluster)
}



#mydata <- full_result[,c("CAL")]
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata)


k2 <- kmeans(mydata, centers = 32,nstart = 25)
p <-fviz_cluster(k2, data = mydata,ggtheme = theme_minimal(),geom = "points")
p
ggsave(p,file=paste0(dir_out,"cluster_CAL.png"),width=6,height=6)

require(Ckmeans.1d.dp)
mydata <- full_result[,c("CAL","LASSO_CAL")]
mydata <- na.omit(mydata) # listwise deletion of missing
#mydata <- scale(mydata)
result <- Ckmeans.1d.dp(mydata$CAL,k=32)
#plot(result)

k=32
plot(mydata$CAL, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Optimal univariate clustering given k",
     sub=paste("Number of clusters given:", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("bottomright", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")

t <- c(1:2249)
w <- mydata$LASSO_CAL^10  # stress the peaks 
res <- Ckmeans.1d.dp(t, k=c(1:40), w)
plot(t, w,
     col=res$cluster, pch=res$cluster, type="h", 
     xlab="File Index", ylab="Estimated ATE",
     xaxt='n',yaxt='n',cex.lab = 1.5)



