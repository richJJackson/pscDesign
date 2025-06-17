## Data Sim - Function for simulating data from cfm object


### Example
install.packages("psc")
library(psc)
smod <- psc::gemCFM
covSim(smod)
###

covSim <- function(CFM,n=100){

  dv <- CFM$datavis

  cov.nm <- names(dv)
  attributes(dv)
  ncov <- length(dv)
  xdat <- NULL

  for(i in 1:ncov){

    cov.dat <- dv[[i]]$data
    if(ncol(cov.dat)==1) x.new <- sample(cov.dat[,1],n,replace=T)
    if(ncol(cov.dat)==2) {
      ddat <- rep(cov.dat[,1],cov.dat[,2])
      x.new <- sample(ddat,n,replace=T)
    }
    xdat <- cbind(xdat,x.new)

  }

  xdat <- data.frame(xdat)
  names(xdat) <- cov.nm
  xdat

}



