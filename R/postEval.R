#' A function to evaluate posterior distribution
#'
#' postEval evaluated a distribition based on given mean and standard deviation
#'
#' @param bound The bound to use in the posterior evaluation (defaults to 0)
#' @param mn distribution mean
#' @param sd distribution sd
postEval <- function(mn,sd,bound=0,direction="greater"){
  pn <- pnorm(bound,mn,sd)
  ret <- pn
  if(direction=="greater") ret <- 1-pn
  ret
}
