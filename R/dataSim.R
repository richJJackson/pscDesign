
dataSim <- function(CFM,n0=0,n1=100,beta=0,fuTime,recTime,rec=NULL,
                     cen.prop=0.05){

  ### Getting total number of recruitment
  ### if no rec forecast this is the sum of n0 and n1
  ### if recruitment forecast is set - this is the recruitment max
  if(is.null(rec)){
    maxTime <- recTime+fuTime
    n <- n0 + n1
    cumRec <- round(seq(0,n,length=recTime))
    mnthRec <- c(0,diff(cumRec))
    rec <- data.frame("SitesOpen"=1,"Monthly.Rec"=mnthRec,"Cumualtive.Rec."=cumRec)
  }


  if(!is.null(rec)){
    n <- max(rec$Cumualtive.Rec.)
    maxTime <- nrow(rec)+fuTime
  }

  ### Simulate covariates from the CFM
  sim_cov <- covSim(CFM,n=n)


  ### linear predictor
  co <- CFM$cov_co
  mm <- model.matrix(~.,data=sim_cov)[,-1]
  lp <- t(co%*%t(mm))

  ### adding in the allocations
  alloc.prop <- n1/(n1+n0);alloc.prop
  alloc <- rbinom(n,1,alloc.prop)

  ## adjusted linear predictor
  lp <- lp + alloc*beta

  ## Getting survival Times
  simtm <- lapply(lp,function(x){
    u <- runif(1);u
    target <- -log(u);target
    optimize(rootfun,interval=c(1e-5,1e6),lp=x,target=target,CFM=CFM)$minimum

  })
  r_t <- unlist(simtm)

  ### detailing censoring (administrative censoring on time=maxTime)
  cen <- rep(1,n)
  if(cen.prop>0){

    cenFun <- function(mt,cp,b=log(5)){
      a <- -log(cp)/mt^exp(b);a
    }

    b <- log(3)
    a <- cenFun(maxTime,cen.prop,b=b)
    ## Adjust by lp
    a <- a*exp(lp)

    r_c <- runif(n)

    t_c <- exp(log(-log(r_c)/a)/exp(b));t_c
    cen <- as.numeric(t_c>r_t);cen
    r_t <- pmin(r_t,t_c)

  }

  ### returning results
  sim_cov$lp <- lp
  sim_cov$arm <- alloc
  sim_cov$time <- r_t
  sim_cov$cen <- cen

  sim_cov

}


rootfun <- function(t,lp,target,CFM){
  (fpmH(CFM,t,lp=lp)$Had - target)^2
}
