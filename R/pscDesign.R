#' Study design using Personalised Synthetic Controls
#'
#' pscDesign performs the simulations required to estimate the Power of a study
#' designed using personalised synthetic controls.
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
#' @param nsim number of simulations
#' @param nsim.psc number of simulations to use in psc estimation
#' @param burn.psc burn in to use in psc estimation
#' @param bound The bound to use in the posterior evaluation (defaults to 0)
#' @param direction The direction which specifies superiority of the efficacy
#' parameter
#' @param alpha_eval Alpha values at which to evaluate the study design.
#'
#' @details
#' The pscDesign function performs a simulation study to estimate the design
#' parameters for a study using personalised synthetic controls.
#'
#' Time parameters (fuTime, recTime) should be expressed on the same scale on
#' which they are specified in the Counter Factual Model.  One of recTime or rec
#' must be specified. If rec is specified, recTime will be ignored.
#'
#' @returns A list containing a summary of the simulated datasets and estimated
#' type-II error rates (Power) against leveld of significance (alpha-levels)
#' @examples
#' gemCFM <- pscDesign::gemCFM
#' pscDesign(gemCFM,n0=0,n1=100,beta=log(0.7),fuTime=12,recTime=12,nsim=2,
#'     nsim.psc=300,burn.psc=100)
#' @export
pscDesign <- function(CFM,n0=0,n1,beta,fuTime,recTime,rec=NULL,
                                  nsim=4,nsim.psc=500,burn.psc=200,bound=0,
                                  direction="greater",
                                  alpha_eval=c(0.01,0.025,0.05,0.1,0.15,0.2)){

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
    trialSamp(CFM=CFM,n0=n0,n1=n1,beta=beta,fuTime=fuTime,
                          recTime=recTime,rec=rec,nsim.psc=nsim.psc,
                          burn.psc=burn.psc)})



  ## Estimate proportion of posterior distribution > (<) bound
  trialEval <- lapply(trialSim,function(x){
    mn <- as.numeric(as.character(x$post_mn))
    sd <- as.numeric(as.character(x$post_sd))
    postEval(mn,sd,bound=bound,direction=direction)
  }
  )

  ## Evaluate bound against alpha level
  trialAlp <- lapply(trialEval,function(x) as.numeric(x<alpha_eval))
  pwrEst <- colSums(Reduce(rbind,trialAlp))/nsim

  ###
  estimate <- data.frame(alpha_eval,pwrEst)

  ### Sumamry of trial parameters
  trialSumm <- colMeans(Reduce(rbind,trialSim))

  ### Returning object
  ret <- list(trialSumm,estimate)
  return(ret)

  }
