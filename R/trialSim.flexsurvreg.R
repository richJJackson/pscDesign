### trialSim.flexsurvreg



#####pscSim
trialSamp.flexsurvreg <- function(CFM,n0,n1,beta,maxTime,nsim.psc=2000){

  ##### Single arm estimation
  if(n0==0){
    ## Simualte data
    ds <- dataSim.flexsurvreg(CFM=CFM,n0=n0,n1=n1,beta=beta,maxTime=maxTime)

    ## fitpsc
    simfit <- pscfit(CFM,ds,nsim=nsim.psc)
    post <- simfit$posterior$beta
    mn <- mean(post)
    sd <- sd(post)
    ret <- data.frame(mn,sd)
    names(ret) <- c("post_mn","post_sd")
  }

  #### Randomisation Estimation
  if(n0>0){

    ## Simualte data
    ds <- dataSim.flexsurvreg(CFM=CFM,n0=n0,n1=n1,beta=beta,maxTime=maxTime)

    ### seperating dataset into arm 0 and arm 1
    ds0 <- ds[ds$arm==0,]
    ds1 <- ds[ds$arm==1,]

    ## fitpsc
    simfit <- pscfit(CFM,ds1,nsim=nsim.psc)
    post <- simfit$posterior$beta
    mn_psc <- mean(post);mn
    sd_psc <- sd(post);sd

    ## direct estimation
    cm <- coxph(Surv(ds$time,ds$cen)~ds$arm);cm
    mn_d <- summary(cm)$coefficients[1]
    sd_d <- summary(cm)$coefficients[3]

    ### if n0 is small cox model may not fit - and a warning is returned

    ### Bayesian Posterior Estimate
    post_var <-  (1/sd_d^2+1/sd_psc^2)^(-1)
    post_sd <- sqrt(post_var)
    num <- ((mn_d/(sd_d^2))+(mn_psc/sd_psc^2))
    post_mn <- num*post_var

    ret <- data.frame(mn_psc,sd_psc,mn_d,sd_d,post_mn,post_sd)
    names(ret) <- c("mn_psc","sd_psc","mn_direct","sd_direct","post_mn","post_sd")

    }

  ret
}
