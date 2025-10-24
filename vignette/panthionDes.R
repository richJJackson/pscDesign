### Panthion Trial Design


### Loading functions
library(psc)
setwd("~/Documents/GitHub/pscDesign")
devtools::load_all()

### loading a model
load("Data/AB_pfs_prot.Rds")



### recruitment forcast
N.site <- 4
rpm <- 0.87
open.rate <- 1
recTime <- 24
recF <- rec.forcast(N.site,rpm,open.rate,Max.Time=recTime)  #70 patients over 24 months

recF

### Power Estimate
fuTime <- 12
beta <- log(0.7);beta
pscDes <- pscDesign.flexsurvreg(CFM=ab_prg,n0=0,n1=70,rec=recF,recTime=recTime,beta=beta,
                      fuTime=fuTime,nsim=1000,nsim.psc=750,bound=0,
                      direction="greater",alpha_eval=c(0.05,0.1,0.15,0.2))


pscDes$avEvent
pscDes$avBeta
pscDes$avSd
pscDes$Power_est
pscDes$Rec


###
CFM=ab_prg
n0=0
n1=70
recTime=recTime
beta=beta
fuTime=fuTime
nsim=50
nsim.psc=1000
bound=0
direction="greater"
alpha_eval=c(0.05,0.1)



rec <- recF

pscDesign.flexsurvreg <- function(CFM,n0,n1,recTime,fuTime,beta,rec=NULL,nsim=50,nsim.psc=2000,bound=0,direction="greater",alpha_eval=c(0.01,0.025,0.05,0.1,0.15,0.2)){

  #### Sorting out recruitment estimate
  if(is.null(rec)){
    n <- n0 + n1
    cumRec <- round(seq(0,n,length=recTime))
    mnthRec <- c(0,diff(cumRec))
    rec <- data.frame("SitesOpen"=1,"Monthly.Rec"=mnthRec,"Cumualtive.Rec."=cumRec)
  }

  if(!is.null(rec)){
    n <- max(rec$Cumualtive.Rec.)
  }

  ### Simulate Trials
  trialSim <- lapply(c(1:nsim),function(x){
    trialSamp.flexsurvreg(CFM=CFM,n0=n0,n1=n1,rec=rec,beta=beta,recTime=recTime,
                          fuTime=fuTime,nsim.psc=nsim.psc)})



  ## Estimate proportion of posterior distribution > (<) bound
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
  estimate

}

