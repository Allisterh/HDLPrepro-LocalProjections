library(parallel)
library(HDLPrepro)

setwd("your/path/here")

Ns<-c(20,40,100)
Ts<-c(100,200,500)
PIs<-c(0.4,0.5,0.6,0.7,0.8)
#PIs<-0.8 # to speed up the simulation significantly, one could run them only for the plug-in constant 0.8 which we present in the paper

M=1000
hmax=10
VAR_lags=4
LP_lags=4


selection=4

progress_bar=FALSE # setting this to TRUE will show a progress bar each time simulate_LP is ran, so showing progress over simulation replications
OLS=FALSE
threads=parallel::detectCores()-2
alphas=0.05
z_quantiles=qnorm(alphas/2.0,0.0,1.0,FALSE,FALSE);
chi2_quantiles=qchisq(alphas,1,FALSE,FALSE);

# First set: partial DL ---------------------------------------------------
init_partial=TRUE
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs(N)
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                       init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                       progress_bar, OLS, threads)
      saveRDS(sim, file=paste0("partial_N",N,"_T",T_,"_PI",PIconstant,".RDS"))
    }
  }
}


# Second set: regular DL --------------------------------------------------
init_partial=FALSE
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs(N)
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                       init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                       progress_bar, OLS, threads)
      saveRDS(sim, file=paste0("regular_N",N,"_T",T_,"_PI",PIconstant,".RDS"))
    }
  }
}


# FThird set: partial DL switching ----------------------------------------
init_partial=TRUE
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs2(N) # uses VAR coefficients with switched signs
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                       init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                       progress_bar, OLS, threads)
      saveRDS(sim, file=paste0("partial_N",N,"_T",T_,"_PI",PIconstant,"_switching_signs.RDS"))
    }
  }
}


# Fourth set: regular DL switching ----------------------------------------
init_partial=FALSE
for(n in 1:length(Ns)){
  for(t in 1:length(Ts)){
    for(pi in 1:length(PIs)){
      N=Ns[n]
      T_=Ts[t]
      PIconstant=PIs[pi]
      VAR_coefficients<-make_VAR_coefs2(N) # uses VAR coefficients with switched signs
      irf_1to1<-irf_from_VAR(VAR_coefficients)
      Sigma_epsilon<-diag(N)
      sim<-simulate_LP(M, T_, LP_lags, hmax, VAR_coefficients, Sigma_epsilon, irf_1to1,
                       init_partial, z_quantiles, chi2_quantiles, selection, PIconstant,
                       progress_bar, OLS, threads)
      saveRDS(sim, file=paste0("regular_N",N,"_T",T_,"_PI",PIconstant,"_switching_signs.RDS"))
    }
  }
}
