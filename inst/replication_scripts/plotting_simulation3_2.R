# This script processes the Section 3.2 simulation files and generates the plots in our paper
rm(list=ls())
start_time<-Sys.time()
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("ggplot2", "ggpubr", "ggpattern", "reshape2"))
library(ggplot2) #3.4.2
library(reshape2) #1.4.4
library(ggpattern) #1.0.1
library(ggpubr) #0.6.0

#use this command set the directory in which the plots will be saved
#setwd("your/path/here")

#if you want to plot simulations saved in a local folder, load_sim_from_local_folder should be set to TRUE, and the path set above MUST be the folder where the simulation outputs were stored
#if load_sim_from_local_folder is FALSE, it will load our simulation results and calibration parameters saved in the package 
load_sim_from_local_folder <- TRUE

if(load_sim_from_local_folder){
  local_path<-getwd()
  for(setup in c("pc_OLS", "sWF_lasso")){
    assign(paste0(setup,"_sim"), readRDS(paste0(local_path,"/",setup,"_sim.RData")))
    assign(paste0(setup,"_DFM"), readRDS(paste0(local_path,"/",setup,"_DFM.RData")))
    assign(paste0(setup,"_IRF"), readRDS(paste0(local_path,"/",setup,"_IRF.RData")))
  }
}else{
  internal_path<-system.file("extdata", package="HDLPrepro", mustWork = TRUE)
  for(setup in c("pc_OLS", "sWF_lasso")){
    assign(paste0(setup,"_sim"), readRDS(paste0(internal_path,"/",setup,"_sim.RData")))
    assign(paste0(setup,"_DFM"), readRDS(paste0(internal_path,"/",setup,"_DFM.RData")))
    assign(paste0(setup,"_IRF"), readRDS(paste0(internal_path,"/",setup,"_IRF.RData")))
  }
}

# plotting the "dense" DFM  -----------------------------------------------
# code for Figure S.4 in the supplementary appendix
figS4<-plot_processed_sim(process_sim(pc_OLS_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_models
ggsave(filename="figS4.pdf", plot=figS4, device="pdf",width=19, height = 12, units="cm",dpi=1000)
#code for Figure S.5 in the supplementary appendix
figS5<-plot_processed_sim(process_sim(pc_OLS_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_lrvs
ggsave(filename="figS5.pdf", plot=figS5, device="pdf",width=19, height = 12, units="cm",dpi=1000)

# plotting the "sparse" DFM -----------------------------------------------
# code for Figure 2
fig2<-plot_processed_sim(process_sim(sWF_lasso_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_models
ggsave(filename="fig2.pdf", plot=fig2, device="pdf",width=19, height = 12, units="cm",dpi=1000)
# code for Figure S.6 in the supplementary appendix
figS6<-plot_processed_sim(process_sim(sWF_lasso_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_lrvs
ggsave(filename="figS6.pdf", plot=figS6, device="pdf",width=19, height = 12, units="cm",dpi=1000)



# code for Figure S.3 in the supplementary appendix

pc_scaling<-sqrt(crossprod(pc_OLS_DFM$factors$F_hat)[1,1])
dense_nonzeros<-apply(X=pc_OLS_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(x!=0)})
dense_l1<-apply(X=pc_OLS_DFM$factors$Lambda*pc_scaling, MARGIN=2, FUN=function(x){sum(abs(x))})

sparse_nonzeros<-apply(X=sWF_lasso_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(x!=0)})
sparse_l1<-apply(X=sWF_lasso_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(abs(x))})

# plotting the summary statistics comparing the dense and sparse DFM --------
nonz<-data.frame(factor=paste0("F",1:6), dense=dense_nonzeros, sparse=sparse_nonzeros)
nonzero<-data.frame(factor=NULL, type=NULL)
for(i in 1:nrow(nonz)){
  for(j in 1:nonz$dense[i]){
    nonzero<-rbind(nonzero,c(nonz$factor[i], "PC"))
  }
  for(j in 1:nonz$sparse[i]){
    nonzero<-rbind(nonzero,c(nonz$factor[i], "WF"))
  }
}
colnames(nonzero)<-c("Factor", "Lambda")
#nonzeros<-melt(nonz, id="factor")
p1<-ggplot(data=nonzero, mapping=aes(x=Factor,  pattern=Lambda, fill=Lambda))+
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6)+
  scale_fill_manual(values = c("black", "white")) +
  scale_pattern_manual(values = c(PC = "none", WF = "stripe")) +
  theme_bw()+
  ylab("Number of nonzeros")+
  labs(title="Nonzeros in factor loadings")+
  theme(plot.title = element_text(hjust=0.5))

l1<-data.frame(factor=paste0("F",1:6), PC=dense_l1, WF=sparse_l1)
l1_melt<-melt(l1,id="factor")
colnames(l1_melt)<-c("Factor", "Lambda", "value")
#
p2<-ggplot(data=l1_melt, mapping=aes(x=Factor, y=value, pattern=Lambda, fill=Lambda))+
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   stat="identity")+
  scale_fill_manual(values = c("black", "white")) +
  scale_pattern_manual(values = c(PC = "none", WF = "stripe")) +
  theme_bw()+
  ylab(expression(L[1]-norm))+
  labs(title=expression(L[1]-norms~of~factor~loadings))+
  theme(plot.title = element_text(hjust=0.5))


Phi_dense<-pc_OLS_DFM$factors$Phi
df_dense<-melt(Phi_dense); colnames(df_dense)<-c("Regressor", "Dependent", "Phi")
df_dense$Regressor<-paste0("F",df_dense$Regressor)
df_dense$Dependent<-paste0("F",df_dense$Dependent)
p3<-ggplot(data=df_dense, aes(x=Regressor, y=Dependent))+
  geom_tile(aes(fill=Phi))+
  scale_y_discrete(limits = rev)+
  scale_fill_gradient2(low="blue", high="darkred", guide="colorbar",  limits=c(-0.2, 0.9), breaks=c(-0.2,0,0.3,0.6,0.9))+
  theme_bw()+
  labs(title="Dense VAR")+
  theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Phi_sparse<-sWF_lasso_DFM$factors$Phi
df_sparse<-melt(Phi_sparse); colnames(df_sparse)<-c("Regressor", "Dependent", "Phi")
df_sparse$Regressor<-paste0("F",df_sparse$Regressor)
df_sparse$Dependent<-paste0("F",df_sparse$Dependent)
p4<-ggplot(data=df_sparse, aes(x=Regressor, y=Dependent))+
  geom_tile(aes(fill=Phi))+
  scale_y_discrete(limits = rev)+
  scale_fill_gradient2(low="blue", high="darkred", guide="colorbar", limits=c(-0.2, 0.9), breaks=c(-0.2,0,0.3,0.6,0.9))+
  theme_bw()+
  labs(title="Sparse VAR")+
  theme(plot.title = element_text(hjust=0.5), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

bars<-ggarrange(p1, p2, common.legend = TRUE, legend="right")
heatmaps<-ggarrange(p3,p4, common.legend=TRUE, legend="right")

combined<-ggarrange(bars, heatmaps, ncol=1)
ggsave(filename="figS3.pdf", plot=combined, device="pdf", width=18, height = 18, units="cm", dpi=1000)

# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_plotting_simulation3_2.txt")