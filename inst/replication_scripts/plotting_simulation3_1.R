# This script processes the files of the simulation in Section 3.1 and generates the plots in our paper
rm(list=ls())
start_time<-Sys.time()
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("ggplot2", "ggpubr", "reshape2"))
library(reshape2) #1.4.4
library(ggplot2) #3.4.2
library(ggpubr) #0.6.0

#use this command set the directory in which the plots will be saved
#setwd("your/path/here")
#if you want to plot simulations saved in a local folder, load_sim_from_local_folder should be set to TRUE, and the path set above MUST be the folder where the simulation outputs were stored
#if load_sim_from_local_folder is FALSE, it will load our simulation results and calibration parameters saved in the package 
load_sim_from_local_folder <- TRUE

if(load_sim_from_local_folder){
  sim_folder<-getwd()
}else{
  sim_folder<-system.file("extdata", package="HDLPrepro", mustWork = TRUE) 
}

# THE FOLLOWING SETTINGS SHOULD MATCH THOSE IN THE SCRIPT running_simulations3_1.R 
################################ settings ######################################
Ns<-c(20,40,100) # numbers of variables 
Ts<-c(100,200,500) # sample sizes
PIs<-0.8 # values of the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008. 
#If you want to plot the results for other values than 0.8, change this to e.g. PIs=c(0.4,0.5,0.6,0.7,0.8) (this needs to match the setting in running_simulation3_1.R). which value is being plotted is then controlled by the index variable pi below. 
M=1000 # number of replications in the simulation
hmax=10 #  maximum horizon - the impulse response function is evaluated from horizon 0 to hmax
VAR_lags=4 # number of VAR lags in the DGP
LP_lags=4 # number of lags included in the local projection estimation equations
################################################################################

# loading and processing these can take up to a minute
coverage<-sims_to_coverage(path=sim_folder, M, Ns, Ts, PIs, hmax, partial=FALSE, switching=FALSE)
coverage_partial<-sims_to_coverage(path=sim_folder, M, Ns,Ts,PIs,hmax,partial=TRUE, switching=FALSE)
coverage_switching<-sims_to_coverage(path=sim_folder, M, Ns,Ts,PIs,hmax,partial=FALSE, switching=TRUE)
coverage_partial_switching<-sims_to_coverage(path=sim_folder, M, Ns,Ts,PIs,hmax,partial=TRUE, switching=TRUE)

width<-sims_to_width(path=sim_folder, M,  Ns, Ts, PIs, hmax, partial=FALSE, switching=FALSE)
width_partial<-sims_to_width(path=sim_folder, M, Ns, Ts, PIs, hmax,partial=TRUE, switching=FALSE)
width_switching<-sims_to_width(path=sim_folder, M, Ns, Ts, PIs, hmax, partial=FALSE, switching=TRUE)
width_partial_switching<-sims_to_width(path=sim_folder, M, Ns, Ts, PIs, hmax, partial=TRUE, switching=TRUE)


# plot coverages ----------------------------------------------------------
# code for Figure S.1 in the supplementary appendix and Figure 1
pi=1 # pi=1 plots the simulation results for plug-in constant = 0.8, which is what we include in the paper. If more values were simulated, this needs to be manually adjusted
count<-0
P<-list()
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    count<-count+1
    df<-data.frame(horizon=0:hmax,
                   not_switching=coverage[n,t,pi,], switching=coverage_switching[n,t,pi,],
                   not_switching_partial=coverage_partial[n,t,pi,], switching_partial=coverage_partial_switching[n,t,pi,])
    df_melt<-melt(df, id="horizon")
    P[[count]]<-ggplot(data=df,
                       aes(x=horizon, y=value, colour=variable)) +
      geom_hline(yintercept=0.95, color="black", linewidth=0.5)+
      geom_line(aes(x=horizon,y=not_switching), colour="red", linewidth=0.75)+ #regular DL
      geom_line(aes(x=horizon,y=switching), colour="red", linewidth=0.75, linetype="twodash")+ #regular DL switching
      geom_line(aes(x=horizon,y=not_switching_partial), colour="blue", linewidth=0.75)+ #partial DL
      geom_line(aes(x=horizon,y=switching_partial), colour="blue",linewidth=0.75,  linetype="twodash")+ #partial DL switching
      ylim(c(0,1.0001))+
      ylab("Coverage")+
      xlab("Horizon")+
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ggtitle(paste0("P=",Ns[n], ", T=",Ts[t]))+theme_bw()
  }
}

ggarrange(P[[1]],P[[2]],P[[3]],P[[4]],P[[5]],P[[6]],P[[7]],P[[8]],P[[9]],
          ncol=3, nrow=3, legend="right",common.legend = TRUE,legend.grob = get_legend(P[[1]]))
ggsave(filename="figS1.pdf",device="pdf",width=18, height = 18, units="cm",dpi=1000)
ggarrange(P[[4]],P[[5]],P[[6]],
          ncol=3, nrow=1, legend="right",common.legend = TRUE,legend.grob = get_legend(P[[1]]))
ggsave(filename="fig1.pdf",device="pdf",width=18, height = 6, units="cm",dpi=1000)

# plot interval widths ----------------------------------------------------
# code for Figure S.2 in the supplementary appendix
pi=1 # pi=1 plots the simulation results for plug-in constant = 0.8, which is what we include in the paper. If more values were simulated, this needs to be manually adjusted
count<-0
P<-list()
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    count<-count+1
    df<-data.frame(horizon=0:hmax,
                   not_switching=width[n,t,pi,], switching=width_switching[n,t,pi,],
                   not_switching_partial=width_partial[n,t,pi,], switching_partial=width_partial_switching[n,t,pi,])
    df_melt<-melt(df, id="horizon")
    P[[count]]<-ggplot(data=df,
                       aes(x=horizon, y=value, colour=variable)) +
      geom_line(aes(x=horizon,y=not_switching), colour="red", linewidth=0.75)+ #regular DL
      geom_line(aes(x=horizon,y=switching), colour="red", linewidth=0.75,  linetype="twodash")+ #regular DL switching
      geom_line(aes(x=horizon,y=not_switching_partial), colour="blue", linewidth=0.75)+ #partial DL
      geom_line(aes(x=horizon,y=switching_partial), colour="blue", linewidth=0.75, linetype="twodash")+ #partial DL switching
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      ggtitle(paste0("P=",Ns[n], ", T=",Ts[t]))+
      ylab("Width")+
      xlab("Horizon")+theme_bw()
  }
}
ggarrange(P[[1]],P[[2]],P[[3]],P[[4]],P[[5]],P[[6]],P[[7]],P[[8]],P[[9]],
          ncol=3, nrow=3, legend="right",common.legend = TRUE)
ggsave(filename="figS2.pdf",device="pdf",width=18, height = 18, units="cm",dpi=1000)

# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_plotting_simulation3_1.txt")