# This script runs the simulation of Section 3.1 and saves intermediate files in a user-chosen folder
rm(list=ls())
start_time<-Sys.time()
library(HDLPrepro) #1.0.0

#use this command set the directory in which the simulation outputs will be saved
#setwd("your/path/here")

################################ settings ######################################
Ns<-c(20,40,100) # numbers of variables 
Ts<-c(100,200,500) # sample sizes
PIs<-c(0.4,0.5,0.6,0.7,0.8) # values of the plug-in constant used for the data-dependent selection of the lasso penalization. Generally, higher value gives stronger penalization. For details, see Algorithm 1 in the supplementary appendix C.5 of https://doi.org/10.1016/j.jeconom.2022.08.008. 
run_only_PI0.8=TRUE #  When true, the code only runs and saves the simulation for the plug-in value of 0.8, which is what we present in the paper. To run the simulation for all plug-in values in the PIs vector, set this to true. Note that to actually plot the results for other plug-in values, changes need to be made to the "plotting_simulation3_1.R" script. Running the full simulation may cause errors on non-Windows operating systems. 
M=1000 # number of replications in the simulation
hmax=10 #  maximum horizon - the impulse response function is evaluated from horizon 0 to hmax
VAR_lags=4 # number of VAR lags in the DGP
LP_lags=4 # number of lags included in the local projection estimation equations
selection=4 # type of selection of the penalization parameter. 4 denotes the data-dependent selection which uses the plug-in constant above. 1-3 denote AIC, BIC and EBIC respectively
progress_bar=FALSE # setting this to TRUE will show a progress bar each time simulate_LP is ran, so showing progress over simulation replications
OLS=FALSE # setting whether OLS should be used in estimating the local projections
threads=parallel::detectCores()-2 # the number of cores used in parallel computation 
alphas=0.05 # desired level of the test; equivalently, (1-alpha)% confidence intervals 
z_quantiles=qnorm(alphas/2.0,0.0,1.0,FALSE,FALSE); # quantiles of the Normal distribution associated with alpha
chi2_quantiles=qchisq(alphas,1,FALSE,FALSE); # quantiles of the Chi-squared distribution associated with alpha
################################################################################

# First set: partial DL ---------------------------------------------------
################################ settings ######################################
init_partial=TRUE # should the parameters of interest remain unpenalized in the first step?
################################################################################
set.seed(1) 
################################################################################
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs(N)
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      seeds_gen<-sample(1e8, M)
      seeds_DL<-array(data=sample(1e8, 2*(hmax+1)*M), dim=c(2, hmax+1, M))
      if(PIconstant==0.8 || run_only_PI0.8==FALSE){ 
        sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                         init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                         progress_bar, OLS, threads, seeds_gen, seeds_DL)
        saveRDS(sim, file=paste0("partial_N",N,"_T",T_,"_PI",PIconstant,".RDS"))
      }
    }
  }
}


# Second set: regular DL --------------------------------------------------
################################ settings ######################################
init_partial=FALSE # should the parameters of interest remain unpenalized in the first step?
################################################################################
set.seed(2) 
################################################################################
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs(N)
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      seeds_gen<-sample(1e8, M)
      seeds_DL<-array(data=sample(1e8, 2*(hmax+1)*M), dim=c(2, hmax+1, M))
      if(PIconstant==0.8 || run_only_PI0.8==FALSE){  
        sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                         init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                         progress_bar, OLS, threads, seeds_gen, seeds_DL)
        saveRDS(sim, file=paste0("regular_N",N,"_T",T_,"_PI",PIconstant,".RDS"))
      }
    }
  }
}


# Third set: partial DL switching ----------------------------------------
################################ settings ######################################
init_partial=TRUE # should the parameters of interest remain unpenalized in the first step?
################################################################################
set.seed(3) 
################################################################################
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs2(N) # uses VAR coefficients with switched signs
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      seeds_gen<-sample(1e8, M)
      seeds_DL<-array(data=sample(1e8, 2*(hmax+1)*M), dim=c(2, hmax+1, M))
      if(PIconstant==0.8 || run_only_PI0.8==FALSE){  
        sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                         init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                         progress_bar, OLS, threads, seeds_gen, seeds_DL)
        saveRDS(sim, file=paste0("partial_N",N,"_T",T_,"_PI",PIconstant,"_switching_signs.RDS"))
      }
    }
  }
}


# Fourth set: regular DL switching ----------------------------------------
################################ settings ######################################
init_partial=FALSE # should the parameters of interest remain unpenalized in the first step?
################################################################################
set.seed(4) 
################################################################################
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs2(N) # uses VAR coefficients with switched signs
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      seeds_gen<-sample(1e8, M)
      seeds_DL<-array(data=sample(1e8, 2*(hmax+1)*M), dim=c(2, hmax+1, M))
      if(PIconstant==0.8 || run_only_PI0.8==FALSE){ 
        sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                         init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                         progress_bar, OLS, threads, seeds_gen, seeds_DL)
        saveRDS(sim, file=paste0("regular_N",N,"_T",T_,"_PI",PIconstant,"_switching_signs.RDS"))
      }
    }
  }
}

# noting the time ---------------------------------------------------------
end_time <- Sys.time()
write(paste0("start: ",start_time,", end: ", end_time,", difference: ", end_time-start_time), file="runtime_simulation3_1.txt")