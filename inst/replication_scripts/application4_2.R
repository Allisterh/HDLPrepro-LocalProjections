# This script estimates the models and plots the results of Section 4.2
rm(list=ls())
library(HDLPrepro) #1.0.0

# in case the following packages are not installed, run:
#install.packages(c("ggplot2", "ggpubr", "ggpattern", "reshape2"))
#devtools::install_github("RobertAdamek/desla")
library(desla) #0.3.0
library(ggplot2) #3.4.2
library(ggpubr) #0.6.0
library(ggpattern) #1.0.1
library(reshape2) #1.4.4


data("dc")# to see how this dataset was made, see the script "processing_R&Z_data.R"

# 40 lags -----------------------------------------------------------------
################################ settings ######################################
hmax=20 # maximum horizon - the x axis of the plot will be 0:hmax
lags=40 # number of lags included in the local projection equations
PIconstant=0.4 # this is the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008
threads<-parallel::detectCores() # the number of cores used in parallel computation 
set.seed(1) # seed which controls the random number generation for reproducibility. We use set.seed(1) in our application
################################################################################
# estimating these HDLPs can take a few minutes

linear_g<-HDLP(r=NULL, x=dc$newsy, y=dc$g, q=cbind(dc$y,dc$taxy), lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)
linear_y<-HDLP(r=NULL, x=dc$newsy, y=dc$y, q=cbind(dc$g,dc$taxy), lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)
nl_g<-HDLP(r=NULL, x=dc$newsy, y=dc$g, q=cbind(dc$y,dc$taxy), state_variables=dc$lag_slack, lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)
nl_y<-HDLP(r=NULL, x=dc$newsy, y=dc$y, q=cbind(dc$g,dc$taxy), state_variables=dc$lag_slack, lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)


# to avoid having to run the above, we include the results as data in the package
#data("linear_g")
#data("linear_y")
#data("nl_g")
#data("nl_y")

# code for Figure 4
g_df<-data.frame(mean_all=linear_g$intervals[,2,1], lb_all=linear_g$intervals[,1,1], ub_all=linear_g$intervals[,3,1],
                 mean_hu=nl_g$intervals[,2,1], lb_hu=nl_g$intervals[,1,1], ub_hu=nl_g$intervals[,3,1],
                 mean_lu=nl_g$intervals[,2,2], lb_lu=nl_g$intervals[,1,2], ub_lu=nl_g$intervals[,3,2],
                 Horizon=0:hmax)
y_df<-data.frame(mean_all=linear_y$intervals[,2,1], lb_all=linear_y$intervals[,1,1], ub_all=linear_y$intervals[,3,1],
                 mean_hu=nl_y$intervals[,2,1], lb_hu=nl_y$intervals[,1,1], ub_hu=nl_y$intervals[,3,1],
                 mean_lu=nl_y$intervals[,2,2], lb_lu=nl_y$intervals[,1,2], ub_lu=nl_y$intervals[,3,2],
                 Horizon=0:hmax)

col1="blue"
col2="red"

p2<-ggplot(data=g_df, aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(aes(y=mean_all), color = "black", linewidth = 1)+
  scale_colour_manual(values = c("black",col1, col2), labels=c("Linear model","High unemployment", "Low unemployment"), breaks = c("linear","high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon(aes(ymin = lb_all, ymax = ub_all), fill = "black", alpha=0.25)+
  ylim(c(min(c(g_df$lb_all,g_df$lb_hu,g_df$lb_lu)),max(c(g_df$ub_all,g_df$ub_hu,g_df$ub_lu))))+
  xlab("Horizon") +
  ylab("Government Spending")+theme_bw()+
  labs(title="Linear model")+
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))

df<-data.frame(Horizon=0:hmax, high=g_df$mean_hu, low=g_df$mean_lu)
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"
p3<-ggplot(data=g_df, aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 0.75)+
  scale_colour_manual(values = c("black",col1, col2), labels=c("Linear model","High unemployment", "Low unemployment"), breaks = c("linear","high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = lb_hu, ymax = ub_hu), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +
  geom_ribbon_pattern(aes(ymin = lb_lu, ymax = ub_lu), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(g_df$lb_all,g_df$lb_hu,g_df$lb_lu)),max(c(g_df$ub_all,g_df$ub_hu,g_df$ub_lu))))+
  xlab("Horizon") +
  ylab("Government Spending")+theme_bw()+
  labs(title="State-dependent model")+
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))

p5<-ggplot(data=y_df, aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(aes(y=mean_all), color = "black", linewidth = 1)+
  scale_colour_manual(values = c("black",col1, col2), labels=c("Linear model","High unemployment", "Low unemployment"), breaks = c("linear","high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon(aes(ymin = lb_all, ymax = ub_all), fill = "black", alpha=0.25)+
  ylim(c(min(c(y_df$lb_all,y_df$lb_hu,y_df$lb_lu)),max(c(y_df$ub_all,y_df$ub_hu,y_df$ub_lu))))+
  xlab("Horizon") +
  ylab("GDP")+theme_bw()

df<-data.frame(Horizon=0:hmax, high=y_df$mean_hu, low=y_df$mean_lu)
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"
p6<-ggplot(data=y_df, aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 1)+
  scale_colour_manual(values = c(col1, col2), labels=c("High unemployment", "Low unemployment"), breaks = c("high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = lb_hu, ymax = ub_hu), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +
  geom_ribbon_pattern(aes(ymin = lb_lu, ymax = ub_lu), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(y_df$lb_all,y_df$lb_hu,y_df$lb_lu)),max(c(y_df$ub_all,y_df$ub_hu,y_df$ub_lu))))+
  xlab("Horizon") +
  ylab("GDP")+theme_bw()

ggarrange(p2,p3,p5,p6, common.legend = TRUE, legend="right")
#ggsave(filename="40_lags_R&Z.pdf",device="pdf",width=18, height = 12, units="cm",dpi=1000)


# two combined states -----------------------------------------------------
dummies<-matrix(0,nrow(dc),ncol=4)
dummies[,1]<-dc$lag_slack*dc$lag_recession
dummies[,2]<-dc$lag_slack*(1-dc$lag_recession)
dummies[,3]<-(1-dc$lag_slack)*dc$lag_recession
dummies[,4]<-(1-dc$lag_slack)*(1-dc$lag_recession)

################################ settings ######################################
hmax=20 # maximum horizon - the x axis of the plot will be 0:hmax
lags=4 # number of lags included in the local projection equations
PIconstant=0.4 # this is the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008
threads=parallel::detectCores() # the number of cores use in parallel computation 
set.seed(1) # seed which controls the random number generation for reproducibility. We use set.seed(1) in our application
################################################################################

# estimating these HDLPs can take up to a minute
{
  nl_g_4states<-HDLP(r=NULL, x=dc$newsy, y=dc$g,
                     q=cbind(dc$y,dc$taxy),
                     state_variables=dummies, lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)
  nl_y_4states<-HDLP(r=NULL, x=dc$newsy, y=dc$y,
                     q=cbind(dc$g,dc$taxy),
                     state_variables=dummies, lags=lags, hmax=hmax, PI_constant=PIconstant, threads=threads)
}

# to avoid having to run the above, we include the results as data in the package
#data("nl_g_4states")
#data("nl_y_4states")

# code for Figure 5
theme_update(plot.title = element_text(hjust = 0.5))
col1<-"blue"
col2<-"red"

g11<-nl_g_4states$intervals[,,1]
g10<-nl_g_4states$intervals[,,2]
g01<-nl_g_4states$intervals[,,3]
g00<-nl_g_4states$intervals[,,4]

df<-data.frame(Horizon=0:hmax, high=g11[,2], low=g01[,2])
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"

Horizon=0:hmax
p1<-ggplot(mapping=aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 0.75)+
  scale_colour_manual(values = c(col1, col2), labels=c("High unemployment", "Low unemployment"), breaks = c("high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = g11[,1], ymax = g11[,3]), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +
  geom_ribbon_pattern(aes(ymin = g01[,1], ymax = g01[,3]), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(g11[,1],g10[,1],g01[,1],g00[,1])),max(c(g11[,3],g10[,3],g01[,3],g00[,3]))))+
  xlab("Horizon") +
  ylab("Government Spending")+theme_bw()+
  labs(title="Recession")+
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))


df<-data.frame(Horizon=0:hmax, high=g10[,2], low=g00[,2])
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"
p2<-ggplot(mapping=aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 0.75)+
  scale_colour_manual(values = c(col1, col2), labels=c("High unemployment", "Low unemployment"), breaks = c("high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = g10[,1], ymax = g10[,3]), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +
  geom_ribbon_pattern(aes(ymin = g00[,1], ymax = g00[,3]), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(g11[,1],g10[,1],g01[,1],g00[,1])),max(c(g11[,3],g10[,3],g01[,3],g00[,3]))))+
  xlab("Horizon") +
  ylab("Government Spending")+theme_bw()+
  labs(title="No Recession")+
  theme(plot.title = element_text(face="bold", size=14, hjust=0.5))



y11<-nl_y_4states$intervals[,,1]
y10<-nl_y_4states$intervals[,,2]
y01<-nl_y_4states$intervals[,,3]
y00<-nl_y_4states$intervals[,,4]

df<-data.frame(Horizon=0:hmax, high=y11[,2], low=y01[,2])
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"
Horizon=0:hmax
p5<-ggplot(mapping=aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 0.75)+
  scale_colour_manual(values = c(col1, col2), labels=c("High unemployment", "Low unemployment"), breaks = c("high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = y11[,1], ymax = y11[,3]), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +
  geom_ribbon_pattern(aes(ymin = y01[,1], ymax = y01[,3]), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(y11[,1],y10[,1],y01[,1],y00[,1])),max(c(y11[,3],y10[,3],y01[,3],y00[,3]))))+
  xlab("Horizon") +
  ylab("GDP")+theme_bw()

df<-data.frame(Horizon=0:hmax, high=y10[,2], low=y00[,2])
df_melt<-melt(df, id="Horizon"); names(df_melt)[2]<-"State"
p6<-ggplot(mapping=aes(x=Horizon)) +
  geom_hline(yintercept = 0, color="black", linewidth=0.5)+
  geom_line(data=df_melt, mapping=aes(y=value, color=State), linewidth = 0.75)+
  scale_colour_manual(values = c(col1, col2), labels=c("High unemployment", "Low unemployment"), breaks = c("high", "low"),
                      guide = guide_legend(direction = "horizontal", title.position = "top",
                                           label.position="bottom", label.hjust = 0.5, label.vjust = 0.5,
                                           label.theme = element_text(angle = 90)))+
  geom_ribbon_pattern(aes(ymin = y10[,1], ymax = y10[,3]), fill = col1, alpha=0.1,
                      color = col1,
                      pattern = "circle",
                      pattern_color=col1,
                      pattern_fill = col1,
                      pattern_angle = 90,
                      pattern_density = 0.2,
                      pattern_spacing = 0.05) +

  geom_line(aes(y=y00[,2]), color = col2, linewidth = 1)+
  geom_ribbon_pattern(aes(ymin = y00[,1], ymax = y00[,3]), fill = col2, alpha=0.1,
                      color = col2,
                      pattern = "none",
                      pattern_color=col2,
                      pattern_fill = col2,
                      pattern_angle = 90,
                      pattern_density = 0.001,
                      pattern_spacing = 0.05) +
  ylim(c(min(c(y11[,1],y10[,1],y01[,1],y00[,1])),max(c(y11[,3],y10[,3],y01[,3],y00[,3]))))+
  xlab("Horizon") +
  ylab("GDP")+theme_bw()



first_col<-ggarrange(p1,p5, nrow=2)
first_col_a<-annotate_figure(first_col,top = text_grob("Recession", color = "black", face = "bold", size = 14))

second_col<-ggarrange(p2,p6, nrow=2)
second_col_a<-annotate_figure(second_col,top = text_grob("No Recession", color = "black", face = "bold", size = 14))

ggarrange(plotlist=list(p1,p2,p5,p6), legend="right", common.legend = TRUE)
#ggsave(filename="4_states_R&Z.pdf",device="pdf",width=18, height = 12, units="cm",dpi=1000)
