
### Simulate outcome data from cfm

# CFM:  Counter Factual Model object
# n0: Number of patients on control (default: n0 = 0, i.e a single arm study)
# n1:  Number of patients on intervention
# maxTime: Follow-up time
# beta = log hazard ratio
#pscSim.flexsurvreg(CFM,n0=0,n1,fu)


dataSim.flexsurvreg <- function(CFM,n0=0,n1=100,beta=0,recTime,fuTime,rec=NULL){

  # simualte covariate data
  maxTime <- recTime+fuTime

  ### Getting total number of recruitment
  ### if no rec forecast this is the sum of n0 and n1
  ### if recruitment forecast is set - this is the recruitment max
  if(is.null(rec)){
    n <- n0 + n1
    cumRec <- round(seq(0,n,length=recTime))
    mnthRec <- c(0,diff(cumRec))
    rec <- data.frame("SitesOpen"=1,"Monthly.Rec"=mnthRec,"Cumualtive.Rec."=cumRec)
  }

  if(!is.null(rec)){
    n <- max(rec$Cumualtive.Rec.)
  }

  ### Simulate covariates from the CFM
  sim_cov <- covSim(CFM,n=n)

  ### linear predictor
  co <- CFM$cov_co
  mm <- model.matrix(~.,data=sim_cov)[,-1]
  lp <- t(co%*%t(mm))

  ### adding in the allocation
  alloc.prop <- n1/(n1+n0)
  alloc <- rbinom(n,alloc.prop,1)

  ## adjusted linear predictor
  lp <- lp + alloc*beta

  ### Getting cumulative hazard
  Hest <- fpmH(CFM,maxTime=maxTime)

  ### Personalised survival estimates
  perS <- exp(-Hest$H%*%t(exp(lp)))
  r_s <- runif(n)

  #### Getting survival times
  cond <- data.frame(t(t(perS)-r_s))
  lap <- lapply(cond,FUN=function(x) max(which(x>0)))
  r_t <- Hest$time[unlist(lap)]

  ### detailing censoring (administrative censoring on time=maxTime)
  r_c <- rep(1,n)
  fut_rec <- fuTime+nrow(rec):1
  fut_rec

  cen_tm <- rep(fut_rec,rec$Monthly.Rec)
  cen.id <- which(r_t>=cen_tm);cen.id

  r_t[cen.id] <- cen_tm[cen.id]
  r_c[cen.id] <- 0

  ### returning results
  sim_cov$lp <- lp
  sim_cov$arm <- alloc
  sim_cov$time <- r_t
  sim_cov$cen <- r_c

  sim_cov

}









### est linear predictor


