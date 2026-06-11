#' A function to evaluate posterior distribution
#'
#' postEval evaluated a distribition based on given mean and standard deviation
#'
#' @param bound The bound to use in the posterior evaluation (defaults to 0)
#' @param mn distribution mean
#' @param sd distribution sd
#' @param direction The direction which specifies superiority of the efficacy
#' parameter
#' @import stats
### Evaluate
postEval <- function(drs, bound=0){
  length(which(drs>bound))/length(drs)}
