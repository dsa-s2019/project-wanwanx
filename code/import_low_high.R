require(data.table)
require(ggplot2)
require(plotrix)
require(scales)

dir_raw<-"C:/Users/Wanwan/Dropbox/Courses/6494_dsa_Yan/ACIC_data/"
dir_out<-"C:/Users/Wanwan/Dropbox/Courses/6494_dsa_Yan/project/plot/"
M <- 3200

## import both tracks
dat_low <- list()
#dat_high <- list()
for (i in 1:M){
  templ<-paste(paste("low_dimensional_datasets/low",i,sep=""),".csv",sep="")
  dat_low[[i]] <- fread(paste(dir_raw,templ,sep=""))
  if (i %% 100 == 0){print(i)}
}
# for (i in 1:M){
#   temph<-paste(paste("high_dimensional_datasets/high",i,sep=""),".csv",sep="")
#   dat_high[[i]] <- fread(paste(dir_raw,temph,sep=""))
#   if (i %% 100 == 0){print(i)}
# }
#save(dat_low,file="dat_low.RData")
#save(dat_high,file="dat_high.RData")



## descriptive stat (low)
#Y cts or discrete, 1 discrete, 2 cts
y_type_low <- rep("continuous",M)
for (i in 1:M){
  temp <- typeof(dat_low[[i]]$Y)
  if (temp == "integer"){
    y_type_low[i] <- "discrete"}
}
bp_obj<-as.data.frame(table(y_type_low))
bp <- ggplot(bp_obj, aes(x="", y=Freq, fill=y_type_low))+
      geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)
ggsave(filename = paste(dir_out,"freq_y_low.png"), plot = last_plot())  

#type of X variables, 0 is cts, 1 is discrete
x_type_low <- list()
for (i in 1:M){
  x_type_low[[i]] <- as.vector(sapply(dat_low[[i]], typeof))
}



#dimensions
row_num_low<-col_num_low<-c()
for (i in 1:M){
  row_num_low[i] <- nrow(dat_low[[i]])
  col_num_low[i] <- ncol(dat_low[[i]])
}
fp_obj1<-as.data.frame(table(row_num_low))
fp_obj2<-as.data.frame(table(col_num_low))

# Pieplot
pie1 <- ggplot(fp_obj1, aes(x="", y=Freq, fill=row_num_low)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq/3200*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="bottom")
ggsave(pie1,file=paste0(dir_out,"obs_freq.eps"))


pie2 <- ggplot(fp_obj2, aes(x="", y=Freq, fill=col_num_low)) +
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(Freq/3200*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#ffd700", "#bcbcbc", "#ffa500", "#254290","#D25353")) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="bottom")
ggsave(pie2,file=paste0(dir_out,"var_freq.eps"))
