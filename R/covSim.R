#' Simulate covariates from a Counter factual model
#'
#' A function to simulate covariate values from a Counter Factual Model
#'
#' @param CFM a Counter-Factual model
#' @param n number of observation
#'
#' @details
#' This functions extracts the covariates form a counter factual model and
#' samples from them in order to create a simulated dataset.
#'
#' @return A cumulative Hazard function
#'
covSim <- function(CFM,n=100){

  dv <- CFM$datavis

  cov.nm <- names(dv)
  ncov <- length(dv)
  xdat <- data.frame("ID"=1:n)

  # ggplot objects from ggplot2 >= 3.5.0 inherit from S7_object and
  # cannot be accessed with '$', so use '@' when needed.
  get_plot_data <- function(plot_obj) {
    if (inherits(plot_obj, "S7_object")) {
      plot_obj@data
    } else {
      plot_obj$data
    }
  }

  for(i in 1:ncov){

    cov.dat <- get_plot_data(dv[[i]])

    if(ncol(cov.dat)==1) {
      x.new <- sample(cov.dat[,1],n,replace=T)
      class(x.new) <- "numeric"
    }

    if(ncol(cov.dat)==2) {
      ddat <- rep(cov.dat[,1],cov.dat[,2])
      x.new <- sample(ddat,n,replace=T)
      class(x.new) <- "factor"
    }
    xdat <- cbind(xdat,x.new)
  }

  xdat <- xdat[,-1]
  names(xdat) <- cov.nm
  xdat

}


