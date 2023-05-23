library(parallel)
library(HDLPrepro)
library(desla)

setwd("your/path/here")
# These simulations could take up to a few days to run
M=1000
cl <- makeCluster(detectCores()-2)
clusterEvalQ(cl, library(HDLPrepro))
clusterEvalQ(cl, library(desla))
#clusterExport(cl, envir=environment())
for(setup in c("pc_OLS", "sWF_lasso")){
  # these commands read in the relevant DFM parameters and IRFs from the package, which is provided to each worker via clusterApply(cl, library(HDLPrepro))
  # If you want to use different parameters from your environment, you can pass them to the workers via clusterExport(cl, envir=environment()) commented out above, and use these commented commands:
  #dfm<-get(paste0(setup,"_DFM"))
  #irf<-get(paste0(setup,"_IRF"))
  dfm<-getExportedValue("HDLPrepro", paste0(setup,"_DFM"))
  irf<-getExportedValue("HDLPrepro", paste0(setup,"_IRF"))
  
  simulation<-parLapply(cl = cl, X=vector("list",M), fun=HDLPrepro::one_replication_lean, DFM=dfm, IRF=irf)
  saveRDS(simulation, paste0(setup,"_sim.RData"))
}
stopCluster(cl)
