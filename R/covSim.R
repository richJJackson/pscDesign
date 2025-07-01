## Data Sim - Function for simulating data from cfm object


### Example
#install.packages("psc")
#library(psc)
#smod <- psc::gemCFM
#covSim(smod)[1:3,]

covSim <- function(CFM,n=100){

  dv <- CFM$datavis

  cov.nm <- names(dv)
  attributes(dv)
  ncov <- length(dv)
  xdat <- data.frame("ID"=1:n)

  for(i in 1:ncov){

    cov.dat <- dv[[i]]$data

    if(ncol(cov.dat)==1) {
      x.new <- sample(cov.dat[,1],n,replace=T)
      class(x.new) <- "numeric"
    }

    if(ncol(cov.dat)==2) {
      ddat <- rep(cov.dat[,1],cov.dat[,2])
      x.new <- sample(ddat,n,replace=T)
      class(x.new) <- "factor"
    }
    xdat <- cbind(xdat,x.new)
  }

  xdat <- xdat[,-1]
  names(xdat) <- cov.nm
  xdat

}



