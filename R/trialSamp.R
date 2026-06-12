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
                      nsim.psc=750,burn.psc=250,cen.prop){

  ##### Single arm estimation
  if(n0==0){
    ## Simualte data
    ds <- dataSim(CFM=CFM,n0=n0,n1=n1,beta=beta,recTime=recTime,
                  fuTime=fuTime,rec=rec,cen.prop=cen.prop)

    ## fitpsc
    simfit <- psc::pscfit(CFM, ds, nsim = nsim.psc, burn = burn.psc)
    ret <- pscBetaDraws(simfit)
  }




  #### Randomised Estimation
  if(n0>0){
    n0 <-20
    ## Simualte data
    ds <- dataSim(CFM=CFM,n0=n0,n1=n1,beta=beta,recTime=recTime,
                  fuTime=fuTime,rec=rec,cen.prop=cen.prop)

    simfit <- psc::pscfit(CFM,ds,nsim=nsim.psc,burn=burn.psc,trt=ds$arm,nchain=1)
    ret <- pscComb(simfit)

  }

  ret

}

pscBetaDraws <- function(simfit) {
  draws <- simfit$draws
  if (is.null(draws)) {
    stop("No posterior draws found in pscfit output.", call. = FALSE)
  }
  if (is.array(draws) && length(dim(draws)) == 3L) {
    return(as.vector(draws[, , "beta_1", drop = TRUE]))
  }
  if (is.data.frame(draws) && "beta_1" %in% names(draws)) {
    return(draws$beta_1)
  }
  if (is.list(draws) && !is.null(draws$beta_1)) {
    return(draws$beta_1)
  }
  stop("Unable to extract beta_1 draws from pscfit output.", call. = FALSE)
}
