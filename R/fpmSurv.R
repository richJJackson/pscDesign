#' Generate a survival function from a flexible parametric model
#'
#' A function to estimate the survival function based on parameter estimates.
#'
#' @param CFM a Counter-Factual model
#' @param beta parameter with which to adjust the baseline function (defaults to
#' beta=0)
#' @param lp a linear predictor which if supplies will be used to adjust the
#' parameters of the survival function
#' @param maxTime maximum time used in the estimation
#'
#' @details
#' This functions extracts the baseline (cumulative) hazard parameters from a
#' counter factual model and uses these to construct survival estimates.  This
#' is used in the simulation of datasets.  If a (log) hazard ratio, beta, or a
#' linear predictor, lp, are supplied - these will be used to adjust the
#' baseline estimates.
#'
#' @return A survival function
#'
### Likelihood Function
fpmSurv <- function(CFM,beta=NULL,lp=NULL,maxTime=24){

  time <- seq(1e-16,maxTime,len=1000)
  logt <- log(time)

  lam <- CFM$lam
  kn <- CFM$kn
  cov_co <- CFM$cov.co
  haz_co <- CFM$haz_co
  k <- CFM$k

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])

  linp <- rep(0,length(H0))

  if(!is.null(lp)) linp <- linp + lp
  if(!is.null(beta)) linp <- linp + beta

  H<- H0*exp(linp)
  S <- exp(-H)

  data.frame("time"=time,"S"=S)

}
