# This script uses the estimated DFMs to simulate data and estimate the impulse responses for the simulations in Section 3.2. Saves intermediate files in a user-chosen folder
rm(list=ls())
start_time<-Sys.time()

library(HDLPrepro) #1.0.0
# in case the following package is not installed, run:
#devtools::install_github("RobertAdamek/desla")
library(desla) #0.3.0

#use this command set the directory in which the simulation outputs will be saved
#setwd("your/path/here")
#if you want to plot simulations saved in a local folder, load_sim_from_local_folder should be set to TRUE, and the path set above MUST be the folder where the simulation outputs were stored
#if load_sim_from_local_folder is FALSE, it will load our simulation results and calibration parameters saved in the package 
load_sim_from_local_folder <- TRUE

if(load_sim_from_local_folder){
  local_path <- getwd()
  CDbmedium <- readRDS(paste0(local_path,"/CDbmedium.RData"))
}else{
  internal_path<-system.file("extdata", package="HDLPrepro", mustWork = TRUE)
  CDbmedium <- readRDS(paste0(internal_path,"/CDbmedium.RData"))
}

########################### calibration settings ###############################
n_f <- 6 # number of factors
p_f <- 1 # vector autoregressive order of the factors
q_v <- 2 # autoregressive order of the idiosyncratic errors
S<-1:122 # indices of the variables included in estimating the factors. 1:122 is all variables in the dataset
h_max=20 #  maximum horizon - the impulse response function is evaluated from horizon 0 to h_max
################################################################################

########################### simulation settings ################################
M=1000 # number of simulation replications
# Ts=c(200, 400, 600) # These are the default sample sizes considered in the simulation. To change them, pass a different vector to the parApply function on line 55 below
threads = parallel::detectCores()-2 # the number of cores used in parallel computation 
set.seed(1) # seed which controls the random number generation for reproducibility. We use set.seed(1) in this simulation
################################################################################


# calibration -------------------------------------------------------------

dat <- CDbmedium$data_sans_dates
FFR_ind <- which(names(dat) == "FEDFUNDS")
IP_ind <- which(names(dat) == "INDPRO")
X <-cbind(dat[, IP_ind, drop = FALSE], dat[, -c(IP_ind, FFR_ind)], dat[, FFR_ind, drop = FALSE]) #ordering IP first and FFR last
N <- ncol(X)

#factors by pc, VAR by OLS, can take a few minutes to run
pc_OLS_DFM<-Estimate_DFM(X, n_f = n_f, lags_f = p_f, lags_v = q_v, max_EV = 0.98, undo_scale=TRUE, factor_method = "pc", VAR_method="OLS")
pc_OLS_IRF<-impulse_response_ABCD(pc_OLS_DFM$factors, pc_OLS_DFM$idio, S, h_max, policy_var = length(S), outcome_var = 1)

#factors by sWF, VAR by lasso, can take a few minutes to run
sWF_lasso_DFM<-Estimate_DFM(X, n_f = n_f, lags_f = p_f, lags_v = q_v, max_EV = 0.98, undo_scale=TRUE, factor_method = "sWF", VAR_method="lasso")
sWF_lasso_IRF<-impulse_response_ABCD(sWF_lasso_DFM$factors, sWF_lasso_DFM$idio, S, h_max, policy_var = length(S), outcome_var = 1)

#saving the calibration parameters
saveRDS(pc_OLS_DFM, file="pc_OLS_DFM.RData")
saveRDS(pc_OLS_IRF, file="pc_OLS_IRF.RData")
saveRDS(sWF_lasso_DFM, file="sWF_lasso_DFM.RData")
saveRDS(sWF_lasso_IRF, file="sWF_lasso_IRF.RData")

# simulation --------------------------------------------------------------

# These simulations could take SEVERAL DAYS to run
cl <-  parallel::makeCluster(threads)
parallel::clusterEvalQ(cl, library(HDLPrepro))
parallel::clusterEvalQ(cl, library(desla))
clusterExport(cl, envir=environment())
for(setup in c("pc_OLS", "sWF_lasso")){
  dfm<-get(paste0(setup,"_DFM"))
  irf<-get(paste0(setup,"_IRF"))
  simulation<- parallel::parLapply(cl = cl, X=vector("list",M), fun=HDLPrepro::one_replication_lean, DFM=dfm, IRF=irf) # for different sample sizes, include them as a vector in an optional argument Ts=...
  saveRDS(simulation, paste0(setup,"_sim.RData"))
}
parallel::stopCluster(cl)

# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_simulation3_2.txt")