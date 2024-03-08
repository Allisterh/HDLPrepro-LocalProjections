# functions related to the application in Section 4.1

make_factors<-function(x, n_factors=4){
  PCA<-stats::prcomp(x=x, scale=T)
  all_factors<-PCA$x
  factors<-PCA$x[,1:n_factors]
  if(sum(rownames(PCA$rotation)=="INDPRO")>0){
    IP_loadings<-PCA$rotation["INDPRO",1:n_factors]
  }else{
    IP_loadings<-NULL
  }
  if(sum(rownames(PCA$rotation)=="CPIAUCSL")>0){
    CPI_loadings<-PCA$rotation["CPIAUCSL",1:n_factors]
  }else{
    CPI_loadings<-NULL
  }
  return(list(factors=factors, all_factors=all_factors, IP_loadings=IP_loadings,
              CPI_loadings=CPI_loadings))
}


#' @export
make_irf_factors_VAR<-function(CD, n_factors=3, n_lags=13, n_ahead=49, B=499, alpha=0.05){
  PCA_all<-make_factors(CD$data_sans_dates, n_factors=n_factors)
  PCA_slow<-make_factors(CD$slow_data, n_factors=n_factors)
  data_scaled<-scale(CD$data_sans_dates)
  FFR_scaled<-scale(CD$FFR)
  C_hat<-PCA_all$factors
  C_hat_star<-PCA_slow$factors

  reg<-stats::lm(C_hat~C_hat_star+FFR_scaled)
  b_R<-reg$coefficients[ncol(C_hat_star)+2,]
  F_hat<-C_hat-FFR_scaled%*%t(b_R)

  data_var<-data.frame(F_hat, FFR=CD$FFR)
  var<-vars::VAR(y=data_var, p=n_lags, type="const")
  irf1<-vars::irf(x=var, n.ahead=n_ahead, impulse = "FFR", response = colnames(data_var), boot=F)
  var_for_boot<-tsDyn::lineVar(data_var, lag=n_lags, include="const")

  factor_irfs<-irf1$irf$FFR
  #scaling the factors so they are on the same scale as those from local projections
  scale=factor_irfs[1,"FFR"]
  factor_irfs<-apply(X=factor_irfs, MARGIN=2, FUN=function(x){x/scale})

  reg_loading<-stats::lm(as.matrix(CD$data_sans_dates)~0+F_hat)
  loadings<-rbind(reg_loading$coefficients,0)
  rownames(loadings)[n_factors+1]="FFR"

  all_irfs<-data.frame(factor_irfs%*%loadings)

  boot_irfs<-array(c(0,0,0), dim=c(n_ahead+1, ncol(CD$data_sans_dates), B),
                   dimnames=list(horizon=0:n_ahead, variable=colnames(loadings), b=1:B))
  boot_cumulative_IP=array(c(0,0,0), dim=c(n_ahead+1, 1, B),
                           dimnames=list(horizon=0:n_ahead, variable="INDPRO", b=1:B))
  boot_cumulative_CPI=array(c(0,0,0), dim=c(n_ahead+1, 1, B),
                            dimnames=list(horizon=0:n_ahead, variable="CPIAUCSL", b=1:B))
  boot_factor_irfs=array(c(0,0,0), dim=c(n_ahead+1, nrow(loadings), B),
                         dimnames=list(horizon=0:n_ahead, variable=rownames(loadings), b=1:B))
  pb <- utils::txtProgressBar(min = 0, max = B, style = 3)
  for(i in 1:B){
    boot_data<-tsDyn::VAR.boot(VARobject = var_for_boot, boot.scheme="resample")
    boot_var<-vars::VAR(y=boot_data, p=n_lags, type="const")
    boot_irf<-vars::irf(x=boot_var, n.ahead=n_ahead, impulse= "FFR", boot=F)
    boot_factor_irfs[,,i]<-boot_irf$irf$FFR

    scale=boot_factor_irfs[1,"FFR",i]
    boot_factor_irfs[,,i]<-apply(X=boot_factor_irfs[,,i], MARGIN=2, FUN=function(x){x/scale})

    boot_irfs[,,i]<-boot_factor_irfs[,,i]%*%loadings
    boot_cumulative_IP[,,i]<-cumsum(boot_irfs[,"INDPRO",i])
    boot_cumulative_CPI[,,i]<-cumsum(boot_irfs[,"CPIAUCSL",i])
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  plot_data_FFR<-data.frame(FFR=factor_irfs[,"FFR"], lower=rep(0, n_ahead+1), upper=rep(0, n_ahead+1), horizon=0:n_ahead)
  plot_data_cumulative_IP<-data.frame(cumIP=cumsum(all_irfs[,"INDPRO"]), lower=rep(0, n_ahead+1), upper=rep(0, n_ahead+1), horizon=0:n_ahead)
  plot_data_cumulative_CPI<-data.frame(cumCPI=cumsum(all_irfs[,"CPIAUCSL"]), lower=rep(0, n_ahead+1), upper=rep(0, n_ahead+1), horizon=0:n_ahead)
  for(j in 1:(n_ahead+1)){
    plot_data_FFR$lower[j]<-stats::quantile(boot_factor_irfs[j,"FFR",],alpha/2, type=4)
    plot_data_cumulative_IP$lower[j]<-stats::quantile(boot_cumulative_IP[j,"INDPRO",],alpha/2, type=4)
    plot_data_cumulative_CPI$lower[j]<-stats::quantile(boot_cumulative_CPI[j,"CPIAUCSL",],alpha/2, type=4)

    plot_data_FFR$upper[j]<-stats::quantile(boot_factor_irfs[j,"FFR",],1-alpha/2, type=4)
    plot_data_cumulative_IP$upper[j]<-stats::quantile(boot_cumulative_IP[j,"INDPRO",],1-alpha/2, type=4)
    plot_data_cumulative_CPI$upper[j]<-stats::quantile(boot_cumulative_CPI[j,"CPIAUCSL",],1-alpha/2, type=4)
  }
  return(list(plot_data_FFR=plot_data_FFR,
              plot_data_cumulative_CPI=plot_data_cumulative_CPI,
              plot_data_cumulative_IP=plot_data_cumulative_IP))
}

# functions related to processing the FREDMD data

individual_transform<-function(column, code){
  Tn<-length(column)
  if(code==1){
    ret<-column
  }else if(code==2){
    ret<-as.numeric(c(NA, base::diff(column, na.pad=T, differences=1)))
  }else if(code==3){
    ret<-as.numeric(c(NA,NA, base::diff(column, na.pad=T, differences=2)))
  }else if(code==4){
    ret<-as.numeric(log(column))
  }else if(code==5){
    ret<-as.numeric(c(NA, base::diff(log(column), na.pad=T, differences=1)))
  }else if(code==6){
    ret<-as.numeric(c(NA,NA, base::diff(log(column), na.pad=T, differences=2)))
  }else if(code==7){
    ret<-rep(NA,Tn)
    for(i in 2:Tn){
      ret[i]<-column[i]/column[i-1]-1
    }
    ret<-as.numeric(ret)
  }else{warning("not a valid code")}
  return(ret)
}

fred_transform<-function(mat, transform_codes=NULL){
  if(is.null(transform_codes)){
    transform_codes<-mat[1,]
    transform_codes[1]<-1
    transform_codes<-as.numeric(transform_codes)
  }
  transform_codes[1]<-1
  ret<-mat[-c(1),]
  for(i in 1:ncol(mat)){
    ret[,i]<-individual_transform(column=mat[-c(1),i], code=transform_codes[i])
  }
  return(ret)
}

#' @export
group_which<-function(full_set, subset){
  ind<-1:length(subset)
  for(i in 1:length(subset)){
    if(length(which(full_set==subset[i]))>0){
      ind[i]<-which(full_set==subset[i])
    }else{
      ind[i]<-NA
    }

  }
  return(ind)
}

#' @export
clean_data<-function(raw_data, slow_names, FFR_name, fast_names, start_date, end_date, transform_codes=NULL){
  manually_transformed<-fred_transform(mat = raw_data, transform_codes = transform_codes)
  var_names<-colnames(manually_transformed)
  dates<-raw_data[-1,1]
  variables_we_use<-c(slow_names, FFR_name, fast_names)
  indexes<-group_which(var_names, variables_we_use)
  FFR=manually_transformed[,FFR_name]
  data_all_dates<-data.frame(cbind(dates,manually_transformed[,indexes]),lag_FFR=dplyr::lag(FFR))
  start<-which(data_all_dates$dates==start_date)
  end<-which(data_all_dates$dates==end_date)
  data_in_window<-data_all_dates[start:end,]

  delete<-NULL
  for(i in 1:nrow(data_in_window)){
    if(sum(is.na(data_in_window[i,]))>0){
      delete<-c(delete, i)
    }else{
      break
    }
  }
  for(i in nrow(data_in_window):1){
    if(sum(is.na(data_in_window[i,]))>0){
      delete<-c(delete, i)
    }else{
      break
    }
  }
  if(length(delete)>0){
    data_all<-data_in_window[-delete,]
    warning(paste0("Deleted ", length(delete), " rows with missing data"))
  }else{
    data_all<-data_in_window
  }
  lag_FFR<-data_all$lag_FFR
  data_sans_dates<-data_all[,-c(1, which(colnames(data_all)=="lag_FFR"))]
  missing_plot<-NULL
  missing_summary<-NULL
  if(sum(is.na(data_all))>0){
    warning("Internal missing values!")
    missing_values<-is.na(data_all)
    missing_plot<-graphics::image(missing_values, col=c("white", "red"))
    missing_summary<-apply(data_all[-delete,],FUN=function(x){sum(is.na(x))},MARGIN=2)
  }
  return(list(data_all=data_all,
              data_sans_dates=data_sans_dates,
              var_names=var_names,
              slow_data=data_sans_dates[,group_which(variables_we_use, slow_names)],
              fast_data=data_sans_dates[,group_which(variables_we_use, fast_names)],
              FFR=data_sans_dates[,group_which(variables_we_use, FFR_name)],
              missing_plot=missing_plot,
              missing_summary=missing_summary,
              start_date=data_all$dates[1],
              end_date=data_all$dates[nrow(data_all)],
              dates=data_all$dates,
              lag_FFR=lag_FFR
  ))
}

# functions related to running and processing the simulations in Section 3.1

#' @export
sims_to_coverage<-function(path, M, Ns, Ts, PIs, hmax, partial, switching){
  coverage<-array(0,dim = c(length(Ns), length(Ts), length(PIs), hmax+1), dimnames = list(N=Ns, T=Ts, PI=PIs, horizon=0:hmax))
  if(partial==TRUE){
    prefix<-"partial"
  }else{
    prefix<-"regular"
  }

  if(switching==TRUE){
    suffix<-"_switching_signs"
  }else{
    suffix<-NULL
  }

  for(n in 1:length(Ns)){
    for(t in 1:length(Ts)){
      for(pi in 1:length(PIs)){
        N=Ns[n]
        T_=Ts[t]
        PIconstant=PIs[pi]
        if(switching==TRUE){
          VAR_coef<-make_VAR_coefs2(N)
        }else{
          VAR_coef<-make_VAR_coefs(N)
        }
        true_irf<-irf_from_VAR(VAR_coef)
        sim<-readRDS(file=paste0(path,"/", prefix,"_N",N,"_T",T_,"_PI",PIconstant,suffix,".RDS"))
        for(h in 1:(hmax+1)){
          for(i in 1:M){
            if(sim$intervals[h,1,i]<=true_irf[h] && true_irf[h]<=sim$intervals[h,3,i]){
              coverage[n,t,pi,h]<- coverage[n,t,pi,h]+1/M
            }
          }
        }
      }
    }
  }
  return(coverage)
}

#' @export
sims_to_width<-function(path, M, Ns, Ts, PIs, hmax, partial, switching){
  width<-array(0,dim = c(length(Ns), length(Ts), length(PIs), hmax+1), dimnames = list(N=Ns, T=Ts, PI=PIs, horizon=0:hmax))
  if(partial==TRUE){
    prefix<-"partial"
  }else{
    prefix<-"regular"
  }

  if(switching==TRUE){
    suffix<-"_switching_signs"
  }else{
    suffix<-NULL
  }

  for(n in 1:length(Ns)){
    for(t in 1:length(Ts)){
      for(pi in 1:length(PIs)){
        N=Ns[n]
        T_=Ts[t]
        PIconstant=PIs[pi]
        sim<-readRDS(file=paste0(path,"/", prefix,"_N",N,"_T",T_,"_PI",PIconstant,suffix,".RDS"))
        for(h in 1:(hmax+1)){
          for(i in 1:M){
            width[n,t,pi,h]<- width[n,t,pi,h]+(sim$intervals[h,3,i]-sim$intervals[h,1,i])/M
          }
        }
      }
    }
  }
  return(width)
}

#' @export
irf_from_VAR<-function(VAR_coefficients, hmax=10){
  # Inverting the VAR polynomial to the VMA ---------------------------------
  #I'm going off equation (2.6) of https://kevinkotze.github.io/ts-7-var/
  #B will be the VMA coefficients, A will be the VAR coefficients, with 0
  #matrices for lags past 4
  #the indexing of B will start at 0
  N<-dim(VAR_coefficients)[1]
  VAR_lags<-dim(VAR_coefficients)[3]
  B<-array(0,dim=c(N,N,hmax+1))
  B[,,1]<-diag(N)

  A<-array(0,dim=c(N,N,hmax))
  A[,,1:VAR_lags]<-VAR_coefficients

  for(i in 2:(hmax+1)){
    sum=matrix(0,N,N)
    for(j in 1:(i-1)){
      sum<-sum+B[,,i-j]%*%A[,,j]
    }
    B[,,i]<-sum
  }
  irf_1to1<-B[1,1,]
  return(irf_1to1)
}

#' @export
make_VAR_coefs<-function(N,rhos=c(0.2,0.15,0.1,0.05)){
  VAR_lags=length(rhos)
  VAR_coefficients<-array(dim=c(N,N,VAR_lags))
  for(l in 1:VAR_lags){
    for(i in 1:N){
      for(j in 1:N){
        if(abs(i-j)>=N/2){
          VAR_coefficients[i,j,l]=0
        }else{
          VAR_coefficients[i,j,l]=rhos[l]^(abs(i-j)+1)
        }
      }
    }
  }
  return(VAR_coefficients)
}

#' @export
make_VAR_coefs2<-function(N,rhos=c(0.2,0.15,0.1,0.05)){
  VAR_lags=length(rhos)
  VAR_coefficients<-array(dim=c(N,N,VAR_lags))
  for(l in 1:VAR_lags){
    for(i in 1:N){
      for(j in 1:N){
        if(abs(i-j)>=N/2){
          VAR_coefficients[i,j,l]=0
        }else{
          VAR_coefficients[i,j,l]=rhos[l]^(abs(i-j)+1)
        }
      }
    }
  }
  VAR_coefficients[,,2]= -VAR_coefficients[,,2]
  VAR_coefficients[,,4]= -VAR_coefficients[,,4]
  return(VAR_coefficients)
}

# functions related to running and processing the simulations in Section 3.2

#' @export
Estimate_DFM <- function(X, n_f = NULL, lags_f = NULL, lags_v = NULL, max_EV = NULL, undo_scale=TRUE, factor_method="pc", VAR_method="OLS") {
  if (is.null(n_f)) {
    n_f <- IC_BaiNg(X)
  }
  if(factor_method=="pc"){
    SFM <- pc(X, undo_scale=undo_scale)
  }else if(factor_method=="sWF"){
    SFM <- sWF(X, n_f=n_f, undo_scale=undo_scale) 
  }else{
    stop("invalid factor_method")
  }
  if(undo_scale){
    v <- scale(X, center=TRUE, scale=FALSE) - SFM$F_hat[, 1:n_f] %*% t(SFM$Lambda_hat[, 1:n_f])
  }else{
    v <- scale(X, center=TRUE, scale=TRUE) - SFM$F_hat[, 1:n_f] %*% t(SFM$Lambda_hat[, 1:n_f])
  }
  if(VAR_method=="OLS"){
    VAR_factors <- VAR_est(SFM$F_hat[, 1:n_f], p = lags_f)
  }else if(VAR_method=="lasso"){
    VAR_factors <- lasso_VAR_est(SFM$F_hat[, 1:n_f], p = lags_f)
  }else if(VAR_method=="sparse_lasso"){
    VAR_factors <- sparse_lasso_VAR_est(SFM$F_hat[, 1:n_f], p = lags_f)
  }else{ 
    stop("invalid VAR_method")
  }
  
  VAR_idio <- diagVAR_est(v, lags_v)
  if (!is.null(max_EV)) {
    roots_phi <- check_roots(VAR_factors$coef)
    if (max(abs(roots_phi)) > max_EV) {
      Phi <- max_EV * VAR_factors$coef / max(abs(roots_phi))
    } else {
      Phi <- VAR_factors$coef
    }
    roots_delta <- check_roots(VAR_idio$coef)
    if (max(abs(roots_delta)) > max_EV) {
      Delta <- max_EV * VAR_idio$coef / max(abs(roots_delta))
    } else {
      Delta <- VAR_idio$coef
    }
  }
  est_factors <- list(Phi = Phi, Lambda = SFM$Lambda_hat[, 1:n_f],
                      H = focused_H(stats::cov(VAR_factors$resid), SFM$Lambda_hat[, 1:n_f],
                                    policy_var = ncol(X)), F_hat=SFM$F_hat[, 1:n_f])
  est_idio <- list(Delta = Delta, Xi = t(chol(VAR_idio$var)))
  return(list(factors = est_factors, idio = est_idio))
}

ols <- function(x, y) {
  xx_inv <- chol2inv(chol(crossprod(x)))
  b <- xx_inv %*% crossprod(x, y)
  e <- y - x %*% b
  return(list(coef = b, resid = e))
}

pc <- function(z, undo_scale = TRUE) {
  z <- as.matrix(z)
  x <- scale(z)
  if (undo_scale) {
    xs <- z
  } else {
    xs <- x
  }
  n <- nrow(x)
  p <- ncol(x)
  evv <- eigen(x %*% t(x), symmetric = TRUE)
  F.hat <- sqrt(n) * evv$vectors
  l.hat <- crossprod(xs, F.hat) / n
  scree <- evv$values
  V <- rep(NA,n)
  for (r in 1:p) {
    V[r] <- sum(diag(crossprod(xs - F.hat[, 1:r, drop = FALSE] %*% t(l.hat[, 1:r, drop = FALSE])))) / (n * p);
  }
  return(list(F_hat = F.hat, Lambda_hat = l.hat, eigenv = scree, RSS = V))
}

pc_lean <- function(z, undo_scale = TRUE) {
  z <- as.matrix(z)
  x <- scale(z)
  if (undo_scale) {
    xs <- z
  } else {
    xs <- x
  }
  n <- nrow(x)
  p <- ncol(x)
  evv <- eigen(x %*% t(x), symmetric = TRUE)
  F.hat <- sqrt(n) * evv$vectors
  l.hat <- crossprod(xs, F.hat) / n
  return(list(F_hat = F.hat, Lambda_hat = l.hat))
}

sWF <- function(z, n_f=6, undo_scale=TRUE) {
  var_names<-colnames(z)
  z <- as.matrix(z)
  x <- scale(z)
  n <- nrow(x)
  p <- ncol(x)
  F.hat<-matrix(0,n,n)
  l.hat<-matrix(0,p,p);
  control_<-list(penA=FALSE, penD=FALSE, lam.min.factor=10e-6)
  s<-rrpack::sofar(Y=x, nrank=n_f, X=diag(n), control=control_, ic.type="BIC")
  if(length(s$D)>1){
    Lambda<-s$V%*%diag(s$D)
  }else{
    Lambda<-s$V*s$D
  }
  l.hat[,1:ncol(Lambda)]<-Lambda
  F.hat[,1:ncol(s$U)]<-s$U
  if(undo_scale){
    l.hat<-diag(attributes(x)$'scaled:scale')%*%l.hat
  }
  rownames(l.hat)<-var_names
  return(list(F_hat = F.hat, Lambda_hat = l.hat))
}

g.BN <- function(N, T) {
  C.NT.1 <- (N * T)/(N + T)
  C.NT.2 <- min(N, T)
  g <- c(log(C.NT.1) / C.NT.1, log(C.NT.2) / C.NT.1, log(C.NT.2) / C.NT.2)
}

IC_BaiNg <- function(X, k_max = 8 * floor((min(dim(X)) / 100)^(1/4))) {
  PC.X <- pc(X)
  N <- ncol(X)
  T <- nrow(X)
  V.0 <- sum(diag(crossprod(X))) / (T * N)
  IC.m <- matrix(c(V.0, PC.X[[4]][1:k_max]), nrow = k_max + 1, ncol = 3) + (0:k_max) %o% g.BN(N, T)
  r.hat <- apply(IC.m, 2, which.min) - 1
  return(r.hat)
}

VAR_est <- function(X, p = 1) {
  X_lags <- create_lags(X, p, include.original = FALSE)
  return(ols(X_lags, X[-(1:p), ]))
}

lasso_VAR_est <- function(X, p=1){
  # Assuming your data matrix is in the object Y, please use this code then to estimate the VAR sparsely with p=1 lag:
  fit = bigtime::sparseVAR(Y = X, p = p, VARpen = 'L1', selection = 'bic', check_std = F)
  N<-ncol(X)
  Phi<-matrix(0, N*p, N)
  for(i in 1:p){
    Phi[((i-1)*N+1):(i*N),1:N]<-fit$Phihat[1:N,((i-1)*N+1):(i*N)]#Stephan stacks matrices on top of each other, Ines next to each other
  }
  X_lags <- create_lags(X, p, include.original = FALSE)
  e <- X[-(1:p), ] - X_lags %*% Phi
  return(list(coef=Phi, resid=e))
}

sparse_lasso_VAR_est <- function(X, p=1){
  # Assuming your data matrix is in the object Y, please use this code then to estimate the VAR sparsely with p=1 lag:
  fit = bigtime::sparseVAR(Y = X, p = p, VARpen = 'L1', selection = 'bic', verbose = F, check_std = F, VARgran=c(10,10))
  N<-ncol(X)
  Phi<-matrix(0, N*p, N)
  for(i in 1:p){
    Phi[((i-1)*N+1):(i*N),1:N]<-fit$Phihat[1:N,((i-1)*N+1):(i*N)]#Stephan stacks matrices on top of each other, Ines next to each other
  }
  X_lags <- create_lags(X, p, include.original = FALSE)
  e <- X[-(1:p), ] - X_lags %*% Phi
  return(list(coef=Phi, resid=e))
}

diagVAR_est <- function(X, p = 1) {
  N <- NCOL(X)
  coefs <- matrix(0, nrow = N * p, ncol = N)
  res_var <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    X_lags <- create_lags(X[, i], p, include.original = FALSE)
    AR <- ols(X_lags, X[-(1:p), i])
    coefs[1:p * N + i - N, i] <- AR$coef
    res_var[i, i] <- stats::var(AR$resid)
  }
  return(list(coef = coefs, var = res_var))
}

create_lags <- function(y, p = 1, include.original = TRUE, trim = TRUE) {
  x <- as.matrix(y)
  n <- nrow(x)
  k <- ncol(x)
  lx <- matrix(0, nrow = n, ncol = (p + include.original) * k)
  if (is.null(colnames(x))) {
    c.names <- rep("", k)
  } else {
    c.names <- colnames(x)
  }
  colnames(lx) <- rep(c.names, p + include.original)
  for (i in (1 - include.original):p) {
    cols <- k * (i - 1 + include.original) + 1:k
    lx[(1+i):n, cols] <- x[1:(n-i), ]
    colnames(lx)[cols] <- paste(c.names, " l", i, sep = "")
  }
  return(lx[(1 + trim * p):n, , drop = FALSE])
}

#' @export
generate_DFM <- function(n, factors, idio, init = 50, max_EV = 0.95) {
  n_f <- NCOL(factors$Lambda)
  p_f <- NROW(factors$Phi) / n_f
  N <- NCOL(idio$Delta)
  q_v <- NROW(idio$Delta) / N
  n1 <- n + init
  
  roots_phi <- check_roots(factors$Phi)
  if (max(abs(roots_phi)) > max_EV) {
    Phi <- max_EV * factors$Phi / max(abs(roots_phi))
  } else {
    Phi <- factors$Phi
  }
  roots_delta <- check_roots(idio$Delta)
  if (max(abs(roots_delta)) > max_EV) {
    Delta <- max_EV * idio$Delta / max(abs(roots_delta))
  } else {
    Delta <- idio$Delta
  }
  
  eps <- matrix(stats::rnorm(n1 * n_f), nrow = n1, ncol = n_f)
  eta <- eps %*% t(factors$H)
  f <- matrix(0, nrow = n1, ncol = n_f)
  for (t in (p_f + 1):n1) {
    f[t, ] <- c(t(f[t-1:p_f, ])) %*% Phi + eta[t, ]
  }
  
  e <- matrix(stats::rnorm(n1 * N), nrow = n1, ncol = N)
  u <- e %*% t(idio$Xi)
  v <- matrix(0, nrow = n1, ncol = N)
  for (t in (q_v + 1):n1) { 
    v[t, ] <- c(t(v[t-1:q_v, ])) %*% Delta + u[t, ]
  }
  X <- f %*% t(factors$Lambda) + v
  return(list(X = X[init + 1:n, ], obs_shock = eps[init + 1:n, ]))
}

#' @export
impulse_response_ABCD <- function(factors, idio, S, h_max = 15, policy_var = length(S), outcome_var = 1) {
  N <- NROW(factors$Lambda)
  n_w <- length(S)
  n_f <- NCOL(factors$Lambda)
  p_f <- NROW(factors$Phi) / n_f
  
  # Reduce to selection
  q_v <- NROW(idio$Delta) / N
  Delta_w <- idio$Delta[c(outer(S, (1:q_v - 1) * N, "+")), S]
  Lambda_w <- factors$Lambda[S, ]
  Xi_w <- idio$Xi[S, ]
  
  # Find A, B, C, D
  if (p_f <= q_v + 1) {
    n_s <- (1 + q_v) * n_f
    A <- companion(factors$Phi, q = q_v + 1)
    C <- cbind(diag(n_w), -t(Delta_w)) %*% (diag(1 + q_v) %x% Lambda_w)
  } else {
    n_s <- p_f * n_f
    A <- companion(factors$Phi, q = p_f)
    Lambda_blocks <- matrix(0, nrow = (1 + q_v) * n_w, ncol = n_s)
    Lambda_blocks[1:((1 + q_v) * n_w), 1:((1 + q_v) * n_f)] <- diag(1 + q_v) %x% Lambda_w
    C <- cbind(diag(n_w), -t(Delta_w)) %*% Lambda_blocks
  }
  B <- rbind(cbind(factors$H, matrix(0, nrow = n_f, ncol = N)), matrix(0, nrow = n_s - n_f, ncol = N + n_f))
  D <- cbind(matrix(0, nrow = n_w, ncol = n_f), Xi_w)
  
  out <- kalman_recursions(A, B, C, D)
  Sigma_uw <- out$Delta
  
  # g(L) = (I - (A - K C) L)^-1
  # A(L) = (I - C g(L) K L) (I - Delta_w(L)L) = A_w1(L) A_w2(L))]L
  
  g_L <- invert_VAR(t(A - out$K %*% C), q_max = h_max - 1)
  A_w1L <- sapply(1:h_max, function(j){C %*% g_L[, , j] %*% out$K}, simplify = "array")
  A_w1 <- array(c(diag(n_w), - A_w1L), dim = c(n_w, n_w, h_max + 1))
  A_w2 <- array(0, dim = c(n_w, n_w, h_max + 1))
  A_w2[, , 1] <- diag(n_w)
  A_w2[, , 1 + 1:q_v] <- array(-t(Delta_w), dim = c(n_w, n_w, q_v)) 
  A_w <- multiply_polynomials(A_w1, A_w2)[, , 1 + 0:h_max]
  
  C_w <- invert_VAR(A_w, q_max = h_max)
  B_w <- t(chol(Sigma_uw))
  
  CB_w <- sapply(0:h_max + 1, function(h){C_w[, , h] %*% B_w}, simplify = "array")
  
  IRF <- CB_w[outcome_var, policy_var, ] / CB_w[policy_var, policy_var, 1]
  return(IRF)
}

multiply_polynomials <- function(A, B) {
  p <- dim(A)[3]
  k <- dim(A)[1]
  C <- array(0, dim = c(k, k, 2*p - 1))
  for (i in 1:p) {
    for (j in 1:p) {
      C[, , i + j - 1] <- C[, , i + j - 1] + A[, , i] %*% B[, , j]
    }
  }
  return(C)
}

invert_VAR <- function(A, q_max = 20) {
  d <- length(dim(A))
  if (d == 2) {
    B <- invert_VAR_comp(A, q_max = q_max)
  } else if (d == 3) {
    A_mat <- matrix(A[, , -1], nrow = dim(A)[1], ncol = dim(A)[2] * (dim(A)[3] - 1))
    A_mat_comp <- -t(solve(A[, , 1]) %*% A_mat)
    B0 <- invert_VAR_comp(A_mat_comp, q_max = q_max)
    B <- sapply(0:q_max + 1, function(j){A[, , 1] %*% B0[, , j]}, simplify = "array")
  }
  return(B)
}

invert_VAR_comp <- function(A, q_max = 20) {
  k <- ncol(A)
  p <- nrow(A) / k
  A_comp <- companion(A)
  B <- array(dim = c(k, k, 1 + q_max))
  B[, , 1] <- diag(k)
  A_powj <- diag(p * k)
  for (j in 1:q_max) {
    A_powj <- A_powj %*% A_comp
    B[, , 1 + j] <- A_powj[1:k, 1:k]
  }
  return(B)
}

companion <- function(A, q = nrow(A) / ncol(A)) {
  k <- ncol(A)
  p <- nrow(A) / k
  if (q > p) {
    A_comp <- rbind(cbind(t(A), matrix(0, nrow = k , ncol = k * (q - p))), ##the matrix of zeroes needs to have k rows, not k*(q-p)
                    diag(q * k)[1:((q - 1) * k), ])
  } else {
    if (p > 1) {
      A_comp <- rbind(t(A), diag(p * k)[1:((p - 1) * k), ])
    } else {
      A_comp <- t(A)
    }
  }
  return(A_comp)
}

check_roots <- function(A) {
  d <- length(dim(A))
  if (d == 2) {
    A_mat_comp <- A
  } else if (d == 3) {
    A_mat <- matrix(A[, , -1], nrow = dim(A)[1], ncol = dim(A)[2] * (dim(A)[3] - 1))
    A_mat_comp <- -t(solve(A[, , 1]) %*% A_mat)
  }
  A_comp <- companion(A_mat_comp)
  roots <- abs(eigen(A_comp)$values)
  return(roots)
}

kalman_recursions <- function(A, B, C, D, Omega0 = diag(nrow(A)), relax = 0, 
                              tol = 1e-8, max_iter = 1000) {
  Q <- B %*% t(B)
  R <- D %*% t(D)
  S <- B %*% t(D)
  
  for (i in 1:max_iter) {
    Delta <- C %*% Omega0 %*% t(C) + R
    Theta <- A %*% Omega0 %*% t(C) + S
    Delta_inv <- chol2inv(chol(Delta))
    K <- Theta %*% Delta_inv
    Omega <- A %*% Omega0 %*% t(A) + Q - K %*% t(Theta)
    if (max(abs(Omega - Omega0)) < tol) {
      break
    } else {
      Omega0 <- relax * Omega0 + (1 - relax) * Omega
    }
  }
  return(list(Omega = Omega, K = K, Delta = Delta))
}

focused_H <- function(S, Lambda, policy_var, shock_var = 1) {
  h1 <- S %*% Lambda[policy_var, ] / c(sqrt(t(Lambda[policy_var, ]) %*% S %*% Lambda[policy_var, ]))
  G <- t(chol(S))
  q1 <- solve(G, h1)
  x <- qr(q1)
  Q <- qr.Q(x, complete = TRUE)
  Q <- Q * sign(Q[1,1]) * sign(q1[1]) # fix sign
  H <- G %*% Q
  if (shock_var > 1) {
    if (shock_var < NCOL(S)) {
      H <- cbind(H[, 2:shock_var], H[, 1], H[, -(1:shock_var)])
    } else if (shock_var == NCOL(S)) {
      H <- cbind(H[, 2:shock_var], H[, 1])
    }
  }
  return(H)
}

generate_rnorm_toeplitz_factors <- function(N, n_f, p_f, phi_f, rho_f, threshold=0, diagonal=FALSE) {
  Lambda <- matrix(stats::rnorm(N * n_f), nrow = N)
  if(threshold!=0){
    Lambda<-hard_threshold(Lambda, threshold)
  }
  Phi <- matrix(0, nrow = p_f * n_f, ncol = n_f)
  if(!diagonal){
    for (j in 1:p_f) {
      Phi[n_f * (j - 1) + 1:n_f, 1:n_f] <- stats::toeplitz(phi_f[j]^(1:n_f))
    }
  }else{
    for (j in 1:p_f) {
      Phi[n_f * (j - 1) + 1:n_f, 1:n_f] <- phi_f[j]*diag(n_f) 
    }
  }
  
  ################# I took these out of the loop above
  if (check_roots(Phi)[1] > 0.999) {
    stop("Non-invertible lag polynomial")
  }
  Sigma_f <- matrix(rho_f, nrow = n_f, ncol = n_f) + diag(x = 1 - rho_f, nrow = n_f)
  H <- focused_H(Sigma_f, Lambda, N)
  #################
  return(list(Lambda = Lambda, H = H, Phi = Phi))
}

generate_multiAR <- function(N, q_v, delta_v, rho_v) { #####changed p_v to q_v just to avoid confusion with the notation
  Delta <- matrix(0, nrow = q_v * N, ncol = N)
  for (j in 1:q_v) {
    Delta[(j - 1) * N + 1:N, 1:N] <- diag(stats::runif(N, min = delta_v[1, j], max = delta_v[2, j]))
  }
  if (check_roots(Delta)[1] > 0.999) {
    stop("Non-invertible lag polynomial")
  }
  Sigma_v <- matrix(rho_v, nrow = N, ncol = N) + diag(1 - rho_v, nrow = N)
  Xi <- t(chol(Sigma_v))
  return(list(Xi = Xi, Delta = Delta))
}

hard_threshold<-function(mat, threshold){
  apply(X=mat, MARGIN=c(1,2), FUN=function(x){if(abs(x)<=threshold){0}else{x}})
}

#' @export
one_replication_lean<-function(dummy_list=list(),Ts=c(200, 400, 600), DFM, IRF){
  h_max<-length(IRF)-1
  intervals<-array(0,dim=c(h_max+1,3,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    information=c("lower","bhat","upper"), 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  covered<-array(FALSE,dim=c(h_max+1,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  width<-array(0,dim=c(h_max+1,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  for(ts in 1:length(Ts)){
    T_<-Ts[ts]
    S<-1:122
    n_f=6
    data <- generate_DFM(n=T_, DFM$factors, DFM$idio, init = 50, max_EV = 0.98)
    Z <- data$X[, S]
    x <- Z[, ncol(Z)]
    y <- Z[, 1]
    slow <- Z[, 3:ncol(Z) - 1]
    f<-pc(z=slow)
    slow_factors<-f$F_hat[,1:n_f]
    cpi<-Z[,"CPIAUCSL"]
    e <- data$obs_shock[, 1]
    
    HDLP_04 <- desla::HDLP(x = x, y = y, r = slow, PI_constant=0.4, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    #HDLP_08 <- desla::HDLP(x = x, y = y, r = slow, PI_constant=0.8, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    LP <- desla::HDLP(x = x, y = y, r = cpi, OLS=TRUE, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    FALP <- desla::HDLP(x = x, y = y, r = slow_factors, OLS=TRUE, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    #observed <- desla::HDLP(x = e, y = y, r = slow, q = x, PI_constant=0.4, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    for(dgp_name in c("HDLP_04","LP","FALP")){
      for(lrv_name in c("intervals", "intervals_EWC","intervals_NWfb")){
        intervals[,,ts,dgp_name,lrv_name]<-get(lrv_name, get(dgp_name))
        for(h in 1:(h_max+1)){
          lower<-intervals[h,"lower",ts,dgp_name,lrv_name]
          upper<-intervals[h,"upper",ts,dgp_name,lrv_name]
          if(lower<= IRF[h] && IRF[h]<=upper){
            covered[h,ts,dgp_name,lrv_name]<-TRUE
          }else{
            covered[h,ts,dgp_name,lrv_name]<-FALSE
          }
          w<-upper-lower
          width[h,ts,dgp_name,lrv_name]<-w
        }
        
      }
    }
  }
  return(list(covered=covered, width=width))
}

#' @export
one_replication_lean_manual_seed<-function(seed, Ts=c(200, 400, 600), DFM, IRF){
  set.seed(seed)
  h_max<-length(IRF)-1
  intervals<-array(0,dim=c(h_max+1,3,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    information=c("lower","bhat","upper"), 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  covered<-array(FALSE,dim=c(h_max+1,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  width<-array(0,dim=c(h_max+1,length(Ts), 3, 3), dimnames=list(
    horizon=0:h_max, 
    T_=c(paste0("T_",Ts)), 
    DGP=c("HDLP_04", "LP", "FALP"), 
    LRV=c("intervals","intervals_EWC","intervals_NWfb")))
  
  for(ts in 1:length(Ts)){
    T_<-Ts[ts]
    S<-1:122
    n_f=6
    data <- generate_DFM(n=T_, DFM$factors, DFM$idio, init = 50, max_EV = 0.98)
    Z <- data$X[, S]
    x <- Z[, ncol(Z)]
    y <- Z[, 1]
    slow <- Z[, 3:ncol(Z) - 1]
    f<-pc_lean(z=slow)
    slow_factors<-f$F_hat[,1:n_f]
    cpi<-Z[,"CPIAUCSL"]
    e <- data$obs_shock[, 1]
    
    HDLP_04 <- desla::HDLP(x = x, y = y, r = slow, PI_constant=0.4, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    #HDLP_08 <- desla::HDLP(x = x, y = y, r = slow, PI_constant=0.8, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    LP <- desla::HDLP(x = x, y = y, r = cpi, OLS=TRUE, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    FALP <- desla::HDLP(x = x, y = y, r = slow_factors, OLS=TRUE, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    #observed <- desla::HDLP(x = e, y = y, r = slow, q = x, PI_constant=0.4, y_predetermined = TRUE, hmax = h_max, lags = 3, progress_bar = FALSE, parallel=FALSE)
    
    for(dgp_name in c("HDLP_04","LP","FALP")){
      for(lrv_name in c("intervals", "intervals_EWC","intervals_NWfb")){
        intervals[,,ts,dgp_name,lrv_name]<-get(lrv_name, get(dgp_name))
        for(h in 1:(h_max+1)){
          lower<-intervals[h,"lower",ts,dgp_name,lrv_name]
          upper<-intervals[h,"upper",ts,dgp_name,lrv_name]
          if(lower<= IRF[h] && IRF[h]<=upper){
            covered[h,ts,dgp_name,lrv_name]<-TRUE
          }else{
            covered[h,ts,dgp_name,lrv_name]<-FALSE
          }
          w<-upper-lower
          width[h,ts,dgp_name,lrv_name]<-w
        }
        
      }
    }
  }
  return(list(covered=covered, width=width))
}

#' @export
process_sim<-function(sim){
  
  DGPs<-dimnames(sim[[1]]$covered)$DGP
  LRVs<-dimnames(sim[[1]]$covered)$LRV
  M<-length(sim)
  T_strings<-dimnames(sim[[1]]$covered)$T_
  h_max<-dim(sim[[1]]$covered)[1]-1
  
  coverages<-array(0,dim=c(h_max+1,length(T_strings), length(DGPs), length(LRVs)), dimnames=list(
    horizon=0:h_max, 
    T_=T_strings, 
    DGP=DGPs, 
    LRV=LRVs))
  
  median_widths<-array(0,dim=c(h_max+1,length(T_strings), length(DGPs), length(LRVs)), dimnames=list(
    horizon=0:h_max, 
    T_=T_strings, 
    DGP=DGPs, 
    LRV=LRVs))
  
  for(dgp_name in DGPs){
    for(lrv_name in LRVs){
      for(h in 1:(h_max+1)){
        for(ts in T_strings){
          temp_widths<-rep(0,M)
          for(m in 1:M){
            if(sim[[m]]$covered[h,ts,dgp_name,lrv_name]){
              coverages[h,ts,dgp_name,lrv_name]<-coverages[h,ts,dgp_name,lrv_name]+1/M
            }
            temp_widths[m]<-sim[[m]]$width[h,ts,dgp_name,lrv_name]
          }
          median_widths[h,ts,dgp_name,lrv_name]<-stats::median(temp_widths)
        }
      }
    }
  }
  return(list(coverages=coverages, median_widths=median_widths, M=M, h_max=h_max, Ts=as.numeric(gsub("T_", "", T_strings)), DGPs=DGPs, LRVs=LRVs))
}

#' @export
plot_processed_sim<-function(processed_sim, models=NULL, min=0){
  horizon=model=lrv=value=NULL
  DGPs=processed_sim$DGPs 
  LRVs=processed_sim$LRVs
  Ts<-processed_sim$Ts
  T_strings<-paste0("T_", Ts)
  h_max<-processed_sim$h_max
  if(is.null(models)){
    models=DGPs
  }
  for(ts in T_strings){
    temp_cov<-data.frame("value"=NULL, "horizon"=NULL, "model"=NULL, "lrv"=NULL)
    temp_mw<-data.frame("value"=NULL, "horizon"=NULL, "model"=NULL, "lrv"=NULL)
    for(dgp_name in models){
      for(lrv_name in LRVs){
        d1<-data.frame("value"=processed_sim$coverages[,ts,dgp_name,lrv_name], "horizon"=0:h_max, "model"=dgp_name, "lrv"=lrv_name)
        temp_cov<-rbind(temp_cov,d1)
        d2<-data.frame("value"=processed_sim$median_widths[,ts,dgp_name,lrv_name], "horizon"=0:h_max, "model"=dgp_name, "lrv"=lrv_name)
        temp_mw<-rbind(temp_mw,d2)
      }
    }
    #temp_cov<-cbind(temp_cov,data.frame(id=1:nrow(temp_cov)))
    #temp_mw<-cbind(temp_mw,data.frame(id=1:nrow(temp_mw)))
    assign(paste0(ts,"_cov_df"), temp_cov)
    assign(paste0(ts,"_mw_df"), temp_mw)
  }
  plot_list<-vector("list", 2*length(Ts))
  for(dgp_name in models){
    for(lrv_name in c("intervals", "intervals_EWC","intervals_NWfb")){
      for(ts in T_strings){
        p_cov<-ggplot2::ggplot(data=get(paste0(ts,"_cov_df")), mapping=ggplot2::aes(x=horizon, y=value, colour=model, linetype=lrv))+
          ggplot2::theme_bw()+
          ggplot2::geom_hline(yintercept=0.95, color="black", linetype=1, linewidth=0.3)+
          ggplot2::geom_line(linewidth=0.3)+
          ggplot2::scale_linetype_manual(values=c(1,5,3),
                                name="LRV", 
                                breaks=c("intervals", "intervals_EWC","intervals_NWfb"), 
                                labels = c("NW", "EWC", "NWfb"))+
          ggplot2::labs(title=ts)+
          ggplot2::ylab("Coverage")+
          ggplot2::xlab("Horizon")
        assign(paste0("p_cov_",ts),p_cov)
        plot_list[[which(T_strings==ts)]]<-get(paste0("p_cov_",ts))
        
        p_mw<-ggplot2::ggplot(data=get(paste0(ts,"_mw_df")), mapping=ggplot2::aes(x=horizon, y=value, colour=model, linetype=lrv))+
          ggplot2::theme_bw()+
          ggplot2::geom_line(linewidth=0.3)+
          ggplot2::scale_linetype_manual(values=c(1,5,3),
                                name="LRV", 
                                breaks=c("intervals", "intervals_EWC","intervals_NWfb"), 
                                labels = c("NW", "EWC", "NWfb"))+
          ggplot2::labs(title=ts)+
          ggplot2::xlab("Horizon")+
          ggplot2::ylab("Median width")
        assign(paste0("p_mw_",ts),p_mw)
        plot_list[[length(Ts)+which(T_strings==ts)]]<-get(paste0("p_mw_",ts))
      }
    }
  }
  combined<-ggpubr::ggarrange(plotlist=plot_list, common.legend=TRUE, legend="right")
  
  if(setequal(models,c("HDLP_04", "FALP", "LP"))){
    #separate only models
    for(ts in T_strings){
      temp_cov<-data.frame(value=NULL, horizon=NULL, model=NULL,lrv=NULL)
      temp_mw<-data.frame(value=NULL, horizon=NULL, model=NULL,lrv=NULL)
      for(dgp_name in models){
        d1<-data.frame(value=processed_sim$coverages[,ts,dgp_name,"intervals"], horizon=0:h_max, model=dgp_name)
        temp_cov<-rbind(temp_cov,d1)
        d2<-data.frame(value=processed_sim$median_widths[,ts,dgp_name,"intervals"], horizon=0:h_max, model=dgp_name)
        temp_mw<-rbind(temp_mw,d2)
      }
      #temp_cov<-cbind(temp_cov,data.frame(id=1:nrow(temp_cov)))
      #temp_mw<-cbind(temp_mw,data.frame(id=1:nrow(temp_mw)))
      assign(paste0(ts,"_only_models_cov_df"), temp_cov)
      assign(paste0(ts,"_only_models_mw_df"), temp_mw)
    }
    only_models<-vector("list", length(Ts))
    for(ts in T_strings){
      p_cov<-ggplot2::ggplot(data=get(paste0(ts,"_only_models_cov_df")), mapping=ggplot2::aes(x=horizon, y=value, color=model))+
        ggplot2::theme_bw()+
        ggplot2::geom_hline(yintercept=0.95, color="black", linetype=1, linewidth=0.3)+
        ggplot2::geom_line()+
        #ggplot2::labs(title=ts)+
        ggplot2::labs(title=paste0("T=", Ts[which(T_strings==ts)]))+
        ggplot2::scale_color_manual(values=c("blue","darkorange","darkgreen"),
                           name="Model", 
                           breaks=c("HDLP_04", "FALP", "LP"), 
                           labels = c("HDLP", "FA-LP", "LP"))+
        #ggplot2::ylab("Coverage")+
        ggplot2::xlab("Horizon")
      assign(paste0("p_cov_only_models_",ts),p_cov)
      only_models[[which(T_strings==ts)]]<-get(paste0("p_cov_only_models_",ts))
      
      p_mw<-ggplot2::ggplot(data=get(paste0(ts,"_only_models_mw_df")), mapping=ggplot2::aes(x=horizon, y=value, color=model))+
        ggplot2::theme_bw()+
        ggplot2::geom_line()+
        #ggplot2::labs(title=ts)+
        ggplot2::labs(title=paste0("T=", Ts[which(T_strings==ts)]))+
        ggplot2::scale_color_manual(values=c("blue","darkorange","darkgreen"),
                           name="Model", 
                           breaks=c("HDLP_04", "FALP", "LP"), 
                           labels = c("HDLP", "FA-LP", "LP"))+
        #ggplot2::ylab("Median width")+
        ggplot2::xlab("Horizon")
      assign(paste0("p_mw_only_models_",ts),p_mw)
      only_models[[length(Ts)+which(T_strings==ts)]]<-get(paste0("p_mw_only_models_",ts))
    }
    only_models[[1]]<-only_models[[1]]+ggplot2::ylab("Coverage")
    only_models[[length(Ts)+1]]<-only_models[[length(Ts)+1]]+ggplot2::ylab("Median width")
    combined_only_models<-ggpubr::ggarrange(plotlist=only_models, common.legend=TRUE, legend="right")
    
    #only LRVs
    for(ts in T_strings){
      temp_cov<-data.frame(value=NULL, horizon=NULL, model=NULL,lrv=NULL)
      temp_mw<-data.frame(value=NULL, horizon=NULL, model=NULL,lrv=NULL)
      for(lrv_name in c("intervals", "intervals_EWC","intervals_NWfb")){
        d1<-data.frame(value=processed_sim$coverages[,ts,"HDLP_04",lrv_name], horizon=0:h_max, lrv=lrv_name)
        temp_cov<-rbind(temp_cov,d1)
        d2<-data.frame(value=processed_sim$median_widths[,ts,"HDLP_04",lrv_name], horizon=0:h_max, lrv=lrv_name)
        temp_mw<-rbind(temp_mw,d2)
      }
      #temp_cov<-cbind(temp_cov,data.frame(id=1:nrow(temp_cov)))
      #temp_mw<-cbind(temp_mw,data.frame(id=1:nrow(temp_mw)))
      assign(paste0(ts,"_only_lrvs_cov_df"), temp_cov)
      assign(paste0(ts,"_only_lrvs_mw_df"), temp_mw)
    }
    only_lrvs<-vector("list", length(Ts))
    for(ts in T_strings){
      p_cov<-ggplot2::ggplot(data=get(paste0(ts,"_only_lrvs_cov_df")), mapping=ggplot2::aes(x=horizon, y=value, linetype=lrv))+
        ggplot2::theme_bw()+
        ggplot2::geom_hline(yintercept=0.95, color="black", linetype=1, linewidth=0.3)+
        ggplot2::geom_line()+
        #ggplot2::labs(title=ts)+
        ggplot2::labs(title=paste0("T=", Ts[which(T_strings==ts)]))+
        ggplot2::scale_linetype_manual(values=c(1,5,3),
                              name="LRV", 
                              breaks=c("intervals", "intervals_EWC","intervals_NWfb"), 
                              labels = c("NW", "EWC", "NW-fb"))+
        #ggplot2::ylab("Coverage")+
        ggplot2::xlab("Horizon")
      assign(paste0("p_cov_only_lrvs_",ts),p_cov)
      only_lrvs[[which(T_strings==ts)]]<-get(paste0("p_cov_only_lrvs_",ts))
      
      p_mw<-ggplot2::ggplot(data=get(paste0(ts,"_only_lrvs_mw_df")), mapping=ggplot2::aes(x=horizon, y=value, linetype=lrv))+
        ggplot2::theme_bw()+
        ggplot2::geom_line()+
        #ggplot2::labs(title=ts)+
        ggplot2::labs(title=paste0("T=", Ts[which(T_strings==ts)]))+
        ggplot2::scale_linetype_manual(values=c(1,5,3),
                              name="LRV", 
                              breaks=c("intervals", "intervals_EWC","intervals_NWfb"), 
                              labels = c("NW", "EWC", "NW-fb"))+
        #ggplot2::ylab("Median width")+
        ggplot2::xlab("Horizon")
      assign(paste0("p_mw_only_lrvs_",ts),p_mw)
      only_lrvs[[length(Ts)+which(T_strings==ts)]]<-get(paste0("p_mw_only_lrvs_",ts))
    }
    only_lrvs[[1]]<-only_lrvs[[1]]+ggplot2::ylab("Coverage")
    only_lrvs[[length(Ts)+1]]<-only_lrvs[[length(Ts)+1]]+ggplot2::ylab("Median width")
    combined_only_lrvs<-ggpubr::ggarrange(plotlist=only_lrvs, common.legend=TRUE, legend="right")
  }else{
    only_models<-NULL
    combined_only_models<-NULL
    combined_only_lrvs<-NULL
  }
  
  return(list(plot_list=plot_list, combined=combined, only_models=only_models, combined_only_models=combined_only_models, combined_only_lrvs=combined_only_lrvs))
}