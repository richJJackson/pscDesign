#' Generate a cumulative hazard function from a flexible parametric model
#'
#' A function to estimate the survival function based on parameter estimates.
#'
#' @param CFM a Counter-Factual model

#' @param maxTime maximum time used in the estimation
#'
#' @details
#' This functions extracts the baseline (cumulative) hazard parameters from a
#' counter factual model.  This is used in the simulation of datasets.
#'
#' @return A cumulative Hazard function
#'
### Likelihood Function
fpmH <- function(CFM,maxTime=24){

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

  H <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  data.frame("time"=time,"H"=H)

}
