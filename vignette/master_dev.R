
### pscSim.flexsurvreg
devtools::load_all()



### simulate and fit psc model


load("~/Documents/GitHub/pscRepository/Models/PDAC/Gem_model/CFM.Rds")
library(psc)
library(survival)



?psc

nsim <- 35
n0 <- 0
n1 <- 40
beta <- log(0.7);beta
maxTime <- 48
nsim.psc <- 2000



psc_err1 <- pscErr.flexsurvreg(CFM,0,100,log(0.8),48,nsim=250)
psc_err2 <- pscErr.flexsurvreg(CFM,50,100,log(0.8),48,nsim=250)
psc_err3 <- pscErr.flexsurvreg(CFM,0,150,log(0.8),48,nsim=250)

psc_err3

plot(psc_err1$alpha_eval,psc_err1$pwrEst,typ="l",ylim=c(0.3,1))
lines(psc_err2$alpha_eval,psc_err2$pwrEst,typ="l",ylim=c(0.5,1),col=2)
lines(psc_err3$alpha_eval,psc_err3$pwrEst,typ="l",ylim=c(0.5,1),col=3)
abline(h=c(0.8,0.9),lty=2,col=6)

## Combine information from synthetic and direct estimation
pscErr.flexsurvreg <- function(CFM,n0,n1,beta,maxTime,nsim=50,nsim.psc=2000,bound=0,direction="greater",alpha_eval=c(0.01,0.025,0.05,0.1,0.15,0.2)){


  ### Simulate Trials
  trialSim <- lapply(c(1:nsim),function(x){
    trialSamp.flexsurvreg(CFM=CFM,n0=n0,n1=n1,
                          beta=beta,maxTime=maxTime,nsim.psc=nsim.psc)})

  ## Esimate proportion of posterior distribution > (<) bound
  trialEval <- lapply(trialSim,function(x){
    mn <- x$post_mn
    sd <- x$post_sd
    postEval(mn,sd,bound=bound,direction=direction)
  }
  )

  ## Evaluate bound agains alpha level
  trialAlp <- lapply(trialEval,function(x) as.numeric(x<alpha_eval))
  pwrEst <- colSums(Reduce(rbind,trialAlp))/nsim

  ###
  estimate <- data.frame(alpha_eval,pwrEst)
  estimate

  }















#######################




