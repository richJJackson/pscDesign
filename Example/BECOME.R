### Sample size for Tave Vs Tace plus Bev


library(clinfun)
library(survival)
#devtools::install_github("RichJJackson/psc")
library(psc)



gsdesign.survival(c(1),haz.ratio=0.65)


home <- "/Volumes/richj23/Fellowship/Collaborations/HCC/Intermediate HCC"

### Getting model
setwd(home)
setwd("Model")
load("tace_fpm.R")


fpm.mod

fpm.extract <- model.extract.fpm(fpm.mod)


### Tace survival function
t <- c(0:60)+1e-6
s_t <- surv.fpm(beta=0,fpm.extract,t)
plot(t,s_t$S,typ="l")




### simulating data from model


beta <- 0
time<-seq(0,60,length=1000)+1e-6

### Simulating Data
N<- 100
simdat <- fpm.sim(N,beta=0,fpm.extract,time)
s.ob <- Surv(simdat$time,simdat$cen)
plot(survfit(s.ob~1))
lines(t,s_t$S,typ="l")



### Creating dataset for PSC (this should get added into future sim functions)























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


