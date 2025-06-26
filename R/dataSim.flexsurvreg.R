
### Simulate outcome data from cfm

# CFM:  Counter Factual Model object
# n0: Number of patients on control (default: n0 = 0, i.e a single arm study)
# n1:  Number of patients on intervention
# maxTime: Follow-up time
# beta = log hazard ratio
#pscSim.flexsurvreg(CFM,n0=0,n1,fu)


CFM <- smod
n0 <- 25
n1 <- 100
maxTime <- 48
beta <- -0.5

dataSim.flexsurvreg <- function(CFM,n0=0,n1=100,beta=0,maxTime){

  # simualte covariate data
  n <- n0 + n1
  sim_cov <- covSim(CFM,n=n)

  ### linear predictor
  co <- CFM$cov_co
  mm <- model.matrix(~.,data=sim_cov)[,-1]
  lp <- t(co%*%t(mm))

  ### adding in the allocation
  alloc <- rep(1,n);alloc
  if(n0>0){
    alloc <- c(rep(0,n0),rep(1,n1))
  }

  lp <- lp + alloc*beta

  ### Getting cumulative hazard
  Hest <- fpmH(CFM,maxTime=maxTime)

  ### Personalised survival estimates
  perS <- exp(-Hest$H%*%t(exp(lp)))

  #### Getting urvival times
  cond <- data.frame(t(t(perS)-r_s))
  lap <- lapply(cond,FUN=function(x) max(which(x>0)))
  r_t <- Hest$time[unlist(lap)]

  ### detailing censoring (administrative censoring on time=maxTime)
  r_c <- rep(1,n)
  r_c[which(r_t==maxTime)] <- 0

  ### returning results
  sim_cov$lp <- lp
  sim_cov$arm <- alloc
  sim_cov$time <- r_t
  sim_cov$cen <- r_c
  sim_cov

}









### est linear predictor


