# This script uses the processed FRED-MD data to estimate dynamic factor models and compute their implied true impulse response functions
rm(list=ls())
library(HDLPrepro) #1.0.0
data("CDbmedium")
dat <- CDbmedium$data_sans_dates
FFR_ind <- which(names(dat) == "FEDFUNDS")
IP_ind <- which(names(dat) == "INDPRO")
X <-cbind(dat[, IP_ind, drop = FALSE], dat[, -c(IP_ind, FFR_ind)], dat[, FFR_ind, drop = FALSE]) #ordering IP first and FFR last
N <- ncol(X)

################################ settings ######################################
n_f <- 6 # number of factors
p_f <- 1 # vector autoregressive order of the factors
q_v <- 2 # autoregressive order of the idiosyncratic errors
S<-1:122 # indices of the variables included in estimating the factors. 1:122 is all variables in the dataset
h_max=20 #  maximum horizon - the impulse response function is evaluated from horizon 0 to h_max
################################################################################

#factors by pc, VAR by OLS, can take a few minutes to run
pc_OLS_DFM<-Estimate_DFM(X, n_f = n_f, lags_f = p_f, lags_v = q_v, max_EV = 0.98, undo_scale=TRUE, factor_method = "pc", VAR_method="OLS")
pc_OLS_IRF<-impulse_response_ABCD(pc_OLS_DFM$factors, pc_OLS_DFM$idio, S, h_max, policy_var = length(S), outcome_var = 1)

#factors by sWF, VAR by lasso, can take a few minutes to run
sWF_lasso_DFM<-Estimate_DFM(X, n_f = n_f, lags_f = p_f, lags_v = q_v, max_EV = 0.98, undo_scale=TRUE, factor_method = "sWF", VAR_method="lasso")
sWF_lasso_IRF<-impulse_response_ABCD(sWF_lasso_DFM$factors, sWF_lasso_DFM$idio, S, h_max, policy_var = length(S), outcome_var = 1)


