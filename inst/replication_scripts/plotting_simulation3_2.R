library(HDLPrepro)
# This sets the working directory to where our simulation results are stored in the package.
# If you ran the simulations using the file "running_simulation3_1.R", you should set the directory to wherever you saved the simulation outputs then
setwd(system.file("extdata", package="HDLPrepro", mustWork = TRUE))
for(setup in c("pc_OLS", "sWF_lasso")){
  assign(paste0(setup,"_sim"), readRDS(paste0(setup,"_sim.RData")))
}

# plotting the "dense" DFM  -----------------------------------------------
plot_processed_sim(process_sim(pc_OLS_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_models
#ggsave(filename="pc_OLS_models.pdf",device="pdf",width=19, height = 12, units="cm",dpi=1000)
plot_processed_sim(process_sim(pc_OLS_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_lrvs
#ggsave(filename="pc_OLS_lrvs.pdf",device="pdf",width=19, height = 12, units="cm",dpi=1000)

# plotting the "sparse" DFM -----------------------------------------------
plot_processed_sim(process_sim(sWF_lasso_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_models
#ggsave(filename="sWF_lasso_models.pdf",device="pdf",width=19, height = 12, units="cm",dpi=1000)
plot_processed_sim(process_sim(sWF_lasso_sim), models=c("HDLP_04", "FALP", "LP"))$combined_only_lrvs
#ggsave(filename="sWF_lasso_lrvs.pdf",device="pdf",width=19, height = 12, units="cm",dpi=1000)


# plotting the summary statistics comparing the dense and sparse D --------

library(ggplot2)
library(reshape2)
library(ggpattern)
library(ggpubr)

pc_scaling<-sqrt(crossprod(pc_OLS_DFM$factors$F_hat)[1,1])
dense_nonzeros<-apply(X=pc_OLS_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(x!=0)})
dense_l1<-apply(X=pc_OLS_DFM$factors$Lambda*pc_scaling, MARGIN=2, FUN=function(x){sum(abs(x))})

sparse_nonzeros<-apply(X=sWF_lasso_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(x!=0)})
sparse_l1<-apply(X=sWF_lasso_DFM$factors$Lambda, MARGIN=2, FUN=function(x){sum(abs(x))})

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

ggarrange(bars, heatmaps, ncol=1)
#ggsave(filename="dense_vs_sparse_summary.pdf",device="pdf",width=18, height = 18, units="cm",dpi=1000)