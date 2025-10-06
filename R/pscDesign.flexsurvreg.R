
## Combine information from synthetic and direct estimation
pscDesign.flexsurvreg <- function(CFM,n0,n1,recTime,fuTime,beta,rec=NULL,nsim=50,nsim.psc=2000,bound=0,direction="greater",alpha_eval=c(0.01,0.025,0.05,0.1,0.15,0.2)){

  #### Sorting out recruitment estimate
  if(is.null(rec)){
    n <- n0 + n1
    cumRec <- round(seq(0,n,length=recTime))
    mnthRec <- c(0,diff(cumRec))
    rec <- data.frame("SitesOpen"=1,"Monthly.Rec"=mnthRec,"Cumualtive.Rec."=cumRec)
  }

  if(is.null(rec)){
    n <- max(rec$Cumualtive.Rec.)
  }

  ### Simulate Trials
  trialSim <- lapply(c(1:nsim),function(x){
    trialSamp.flexsurvreg(CFM=CFM,n0=n0,n1=n1,rec=rec,beta=beta,recTime=recTime,
                          fuTime=fuTime,nsim.psc=nsim.psc)})


  ts <- Reduce("rbind",trialSim)

  ## Esimate proportion of posterior distribution > (<) bound
  trialEval <- lapply(trialSim,function(x){
    mn <- x$post_mn
    sd <- x$post_sd
    postEval(mn,sd,bound=bound,direction=direction)
  }
  )

  ## Evaluate bound against alpha level
  trialAlp <- lapply(trialEval,function(x) as.numeric(x<alpha_eval))
  pwrEst <- colSums(Reduce(rbind,trialAlp))/nsim

  ###
  estimate <- data.frame(alpha_eval,pwrEst)
  ret <- list("Rec"=rec,"avEvent"=mean(ts$ne),"avBeta"=mean(ts$post_mn),
              "avSd"=mean(ts$post_sd),"Power_est"=estimate,ts=ts)

}
