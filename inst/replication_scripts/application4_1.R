# This script estimates the models and plots the results of our application in Section 4.1, using the same FRED-MD processed data as that used in Section 3.2
rm(list=ls())
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("ggplot2", "ggpubr"))
#devtools::install_github("RobertAdamek/desla")
library(ggplot2) #3.4.2
library(ggpubr) #0.6.0
library(desla) #0.2.0

data("CDbmedium") # to see how this dataset was made, see the script "processing_FREDMD.R"

################################ settings ######################################
hmax<-49 # maximum horizon - the x axis of the plot will be 0:hmax
lags<-13 # number of lags included in the local projection equations
PI_constant<-0.4 # this is the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008
threads<-parallel::detectCores() # the number of cores use in parallel computation 
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

# to avoid having to run the above, we include the results as data in the package
#data("FAVAR_irfs")
#data("HDLP_FFR_irf")
#data("HDLP_IP_irf")
#data("HDLP_CPI_irf")

# plotting ----------------------------------------------------------------
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
#ggsave(filename="application4_1.pdf",device="pdf",width=18, height = 12, units="cm",dpi=1000)

