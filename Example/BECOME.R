### Sample size for Tave Vs Tace plus Bev


library(clinfun)
library(survival)
#devtools::install_github("RichJJackson/psc")
library(psc)
library(mvtnorm)


## initial (ish) design
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
data.mn <- fpm.mod$datameans
mn.nm <- names(data.mn)
sim.mn <- t(matrix(rep(data.mn,N),ncol=N))
sim.data <- data.frame(sim.mn,simdat)
names(sim.data)<- c(mn.nm,"time","cen")


res <- psc(fpm.mod,sim.data)

res[,(ncol(res)-1)]



























