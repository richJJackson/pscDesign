#' A function to evaluate posterior distribution
#'
#' postEval evaluated a distribition based on given mean and standard deviation
#'
#' @param drs posterior draws for the efficacy parameter
#' @param bound The bound to use in the posterior evaluation (defaults to 0)
#' @param mn distribution mean
#' @param sd distribution sd
#' @param direction The direction which specifies superiority of the efficacy
#' parameter
#' @import stats
#' @export
postEval <- function(drs, bound = 0) {
  mean(drs > bound)
}

#' Assess whether a simulated trial is positive
#'
#' A trial is positive when at least \eqn{1 - \alpha} of the posterior draws
#' lie on the superiority side of \code{bound}.
#'
#' @param drs posterior draws for the efficacy parameter
#' @param bound bound on the log scale (default 0, i.e. HR = 1)
#' @param alpha one-sided significance level
#' @param direction \code{"less"} or \code{"greater"}
#' @return logical scalar
#' @export
trialPositive <- function(drs, bound = 0, alpha = 0.05,
                          direction = c("less", "greater")) {
  trialPower(drs, bound = bound, direction = direction) >= (1 - alpha)
}

#' Posterior probability of superiority
#'
#' @param drs posterior draws for the efficacy parameter
#' @param bound bound on the log scale (default 0, i.e. HR = 1)
#' @param direction \code{"less"} or \code{"greater"}
#' @return posterior probability on the superiority side of \code{bound}
#' @export
trialPower <- function(drs, bound = 0, direction = c("less", "greater")) {
  direction <- match.arg(direction)
  if (direction == "less") {
    mean(drs < bound)
  } else {
    mean(drs > bound)
  }
}
