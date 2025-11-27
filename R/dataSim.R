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
#' @details
#' The datSim function simulates a dataset including sampled covariatees and
#' outcome data from a counterfactual model
#'
#' Time parameters (fuTime, recTime) should be expressed on the same scale on
#' which they are specified in the Counter Factual Model.  One of recTime or rec
#' must be specified. If rec is specified, recTime will be ignored.
#'
#' @returns A data frame intended for use with the CFM including covariate and
#' outcome data
#' @export
#' @import stats
#' @examples
#' gemCFM <- pscDesign::gemCFM
#' dataSim(gemCFM,n0=10,n1=20,beta=log(0.7),fuTime=12,recTime=12)
dataSim <- function(CFM,n0=0,n1=100,beta=0,fuTime,recTime,rec=NULL){


  # simualte covariate data


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

  ### adding in the allocation
  alloc.prop <- n1/(n1+n0);alloc.prop
  alloc <- rbinom(n,1,alloc.prop)

  rbinom(10,1,0.7)

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
  fut_rec <- fuTime+nrow(rec):1;fut_rec

  cen_tm <- rep(fut_rec,rec$Monthly.Rec);cen_tm
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







