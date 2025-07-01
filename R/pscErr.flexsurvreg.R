
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
