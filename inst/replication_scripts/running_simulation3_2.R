# This script uses the estimated DFMs to simulate data and estimate the impulse responses for the simulations in Section 3.2. Saves intermediate files in a user-chosen folder
rm(list=ls())
library(HDLPrepro) #1.0.0

# in case the following package is not installed, run:
#devtools::install_github("RobertAdamek/desla")
library(desla) #0.2.0



setwd("your/path/here")
################################ settings ######################################
M=1000 # number of simulation replications
# Ts=c(200, 400, 600) # These are the default sample sizes considered in the simulation. To change them, pass a different vector to the parApply function on line 30 below
################################################################################
# These simulations could take up to a few days to run

cl <-  parallel::makeCluster( parallel::detectCores()-2)
parallel::clusterEvalQ(cl, library(HDLPrepro))
parallel::clusterEvalQ(cl, library(desla))
#clusterExport(cl, envir=environment())
for(setup in c("pc_OLS", "sWF_lasso")){
  # these commands read in the relevant DFM parameters and IRFs from the package, which is provided to each worker via  parallel::clusterApply(cl, library(HDLPrepro))
  # If you want to use different parameters from your environment, you can pass them to the workers via  parallel::clusterExport(cl, envir=environment()) commented out above, and use these commented commands:
  #dfm<-get(paste0(setup,"_DFM"))
  #irf<-get(paste0(setup,"_IRF"))
  dfm<-getExportedValue("HDLPrepro", paste0(setup,"_DFM"))
  irf<-getExportedValue("HDLPrepro", paste0(setup,"_IRF"))
  
  simulation<- parallel::parLapply(cl = cl, X=vector("list",M), fun=HDLPrepro::one_replication_lean, DFM=dfm, IRF=irf) # for different sample sizes, include them as a vector in an optional argument Ts=...
  saveRDS(simulation, paste0(setup,"_sim.RData"))
}
parallel::stopCluster(cl)
