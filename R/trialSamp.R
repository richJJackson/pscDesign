#' Evaluation of a sampled dataset within pscDesign
#'
#' trialSamp evaluates a sampled dataset and returns the reuslts
#'
#' @param CFM a Counter-Factual model
#' @param n0 number of patients allocated to the control arm (defaults to 0 -
#' i.e. single arm study).
#' @param n1 number of patients allocated to the experimental arm
#' @param beta the (log) HR used in the study design
#' @param fuTime Follow up time for the study design
#' @param recTime Recruitment time for the study design
#' @param rec a study recruitment estimate obtained using the recForcast()
#' function. If specified, recTime will be ignored.
#' @param nsim.psc number of simulations to use in psc estimation
#' @param burn.psc burn in to use in psc estimation
#' @import stats
#' @importFrom psc pscfit
#' @importFrom survival coxph Surv
#' @returns The number of events as well as estimates of the posterior mean and
#' standard deviation
#####pscSim
trialSamp <- function(CFM,n0,n1,beta,fuTime,recTime,rec,
                                  nsim.psc=750,burn.psc=250){

  ##### Single arm estimation
  if(n0==0){
    ## Simualte data
    ds <- dataSim(CFM=CFM,n0=n0,n1=n1,beta=beta,recTime=recTime,
                              fuTime=fuTime,rec=rec)


    ## fitpsc
    simfit <- psc::pscfit(CFM,ds,nsim=nsim.psc,burn=burn.psc)

    co <- data.frame(coef(simfit))
    mn <- as.numeric(co$mean)
    sd <- as.numeric(co$sd)
    ret <- data.frame(sum(ds$cen),mn,sd)
    names(ret) <- c("ne","post_mn","post_sd")
  }



  #### Randomisation Estimation
  if(n0>0){

    ## Simualte data
    ds <- dataSim(CFM=CFM,n0=n0,n1=n1,beta=beta,recTime=recTime,
                               fuTime=fuTime,rec=rec)


    ### seperating dataset into arm 0 and arm 1
    ds0 <- ds[ds$arm==0,]
    ds1 <- ds[ds$arm==1,]

    ## fitpsc
    simfit <- psc::pscfit(CFM,ds1,nsim=nsim.psc)
    co <- data.frame(coef(simfit))
    mn_psc <- as.numeric(co$mean)
    sd_psc <- as.numeric(co$sd);sd_psc

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

    ret <- data.frame(sum(ds$cen),mn_psc,sd_psc,mn_d,sd_d,post_mn,post_sd)
    names(ret) <- c("ne","mn_psc","sd_psc","mn_direct","sd_direct","post_mn","post_sd")

    }

  ret
}
