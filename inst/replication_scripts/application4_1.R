# This script estimates the models and plots the results of our application in Section 4.1, using the same FRED-MD processed data as that used in Section 3.2
rm(list=ls())
start_time<-Sys.time()
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("ggplot2", "ggpubr"))
#devtools::install_github("RobertAdamek/desla")
library(ggplot2) #3.4.2
library(ggpubr) #0.6.0
library(desla) #0.3.0

#use this command set the directory in which the plot will be saved
#setwd("your/path/here")

#if you want to use the processed FREDMD data from a local folder, load_sim_from_local_folder should be set to TRUE, and the path set above MUST be the folder where the data is stored
#if load_sim_from_local_folder is FALSE, it will load the processed FREDMD data saved in the package  
load_sim_from_local_folder <- TRUE

if(load_sim_from_local_folder){
  local_path <- getwd()
  CDbmedium <- readRDS(paste0(local_path,"/CDbmedium.RData"))
}else{
  internal_path<-system.file("extdata", package="HDLPrepro", mustWork = TRUE)
  CDbmedium <- readRDS(paste0(internal_path,"/CDbmedium.RData"))
}
#to see how the dataset "CDbmedium" was made, see the script "processing_FREDMD.R"

################################ settings ######################################
hmax<-49 # maximum horizon - the x axis of the plot will be 0:hmax
lags<-13 # number of lags included in the local projection equations
PI_constant<-0.4 # this is the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008
threads<-parallel::detectCores()-2 # the number of cores used in parallel computation 
set.seed(1) # seed which controls the random number generation for reproducibility. We use set.seed(1) in our application
################################################################################

# estimation --------------------------------------------------------------
# estimating and bootstrapping the FAVAR can take up to a minute
FAVAR_irfs<-make_irf_factors_VAR(CDbmedium, n_lags=lags, n_ahead=hmax, B=499)

# estimating the HDLPs can take SEVERAL DAYS
other_slow<-as.matrix(CDbmedium$slow_data[,-c(which(colnames(CDbmedium$slow_data)=="INDPRO"),
                                              which(colnames(CDbmedium$slow_data)=="CPIAUCSL"))])
fast<-as.matrix(CDbmedium$fast_data)
IP<-as.matrix(CDbmedium$data_all[,"INDPRO"])
CPI<-as.matrix(CDbmedium$data_all[,"CPIAUCSL"])
FFR<-as.matrix(CDbmedium$FFR)


HDLP_FFR_irf<-HDLP(r=cbind(other_slow,IP,CPI), x=FFR, y=FFR, q=fast,
                y_predetermined = F, cumulate_y = F, hmax=hmax, lags=lags, threads = threads, PI_constant = PI_constant)


HDLP_IP_irf<-HDLP(r=cbind(other_slow,CPI), x=FFR, y=IP, q=fast,
               y_predetermined = T, cumulate_y = T, hmax=hmax, lags=lags, threads=threads, PI_constant = PI_constant)


HDLP_CPI_irf<-HDLP(r=cbind(other_slow,IP), x=FFR, y=CPI, q=fast,
                y_predetermined = T, cumulate_y = T, hmax=hmax, lags=lags, threads=threads, PI_constant = PI_constant)

saveRDS(FAVAR_irfs, "FAVAR_irfs.RData")
saveRDS(HDLP_FFR_irf, "HDLP_FFR_irf.RData")
saveRDS(HDLP_IP_irf, "HDLP_IP_irf.RData")
saveRDS(HDLP_CPI_irf, "HDLP_CPI_irf.RData")

# to avoid having to run the above, we include the results as data in the package - read them in by uncommenting the below
#internal_path<-system.file("extdata", package="HDLPrepro", mustWork = TRUE)
#FAVAR_irfs<-readRDS(paste0(local_path,"/FAVAR_irfs.RData"))
#HDLP_FFR_irf<-readRDS(paste0(local_path,"/HDLP_FFR_irf.RData"))
#HDLP_IP_irf<-readRDS(paste0(local_path,"/HDLP_IP_irf.RData"))
#HDLP_CPI_irf<-readRDS(paste0(local_path,"/HDLP_CPI_irf.RData"))

# plotting ----------------------------------------------------------------
# code for Figure 3
p1<-ggplot(data=FAVAR_irfs$plot_data_FFR, mapping=aes(x=horizon, y=FFR))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_ribbon(mapping=aes(ymin=lower, ymax=upper), alpha=0.5)+
  xlab("Horizon")

p2<-ggplot(data=FAVAR_irfs$plot_data_cumulative_IP, mapping=aes(x=horizon, y=cumIP))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_ribbon(mapping=aes(ymin=lower, ymax=upper), alpha=0.5)+
  xlab("Horizon")+ylab("IP")

p3<-ggplot(data=FAVAR_irfs$plot_data_cumulative_CPI, mapping=aes(x=horizon, y=cumCPI))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line()+
  geom_ribbon(mapping=aes(ymin=lower, ymax=upper), alpha=0.5)+
  xlab("Horizon")+ylab("CPI")

second_row<-ggarrange(p1,p2,p3,nrow=1)
second_row<-annotate_figure(second_row,top = text_grob("FAVAR", color = "black", face = "bold", size = 14))

o1<-HDLP_FFR_irf
p4<-ggplot(mapping=aes(x=0:hmax))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line(mapping=aes(y=o1$intervals[,2,]), color="blue")+
  geom_ribbon(mapping=aes(ymin=o1$intervals[,1,], ymax=o1$intervals[,3,]), alpha=0.5, fill="blue")+
  xlab("Horizon")+ylab("FFR")

o2<-HDLP_IP_irf
p5<-ggplot(mapping=aes(x=0:hmax))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line(mapping=aes(y=o2$intervals[,2,]), color="blue")+
  geom_ribbon(mapping=aes(ymin=o2$intervals[,1,], ymax=o2$intervals[,3,]), alpha=0.5, fill="blue")+
  xlab("Horizon")+ylab("IP")


o3<-HDLP_CPI_irf
p6<-ggplot(mapping=aes(x=0:hmax))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  geom_line(mapping=aes(y=o3$intervals[,2,]), color="blue")+
  geom_ribbon(mapping=aes(ymin=o3$intervals[,1,], ymax=o3$intervals[,3,]), alpha=0.5, fill="blue")+
  xlab("Horizon")+ylab("CPI")

first_row<-ggarrange(p4,p5,p6,nrow=1)
first_row<-annotate_figure(first_row,top = text_grob("HDLP", color = "black", face = "bold", size = 14))

ggarrange(first_row,second_row, ncol=1)
ggsave(filename="fig3.pdf",device="pdf",width=18, height = 12, units="cm",dpi=1000)

# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_application4_1.txt")