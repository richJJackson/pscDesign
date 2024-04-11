
### Likelihood Function
surv.fpm <- function(beta=0,model.extract,time){

  lam <- model.extract$lam
  kn <- model.extract$kn
  cov_co <- model.extract$cov.co
  haz_co <- model.extract$haz_co
  logt <- log(time)
  k <- model.extract$k

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }

  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  H<- H0*exp(beta)
  S <- exp(-H)

  data.frame("time"=time,"S"=S)

}
