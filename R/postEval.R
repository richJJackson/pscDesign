postEval <- function(mn,sd,bound=0,direction="greater"){
  pn <- pnorm(bound,mn,sd)
  ret <- pn
  if(direction=="greater") ret <- 1-pn
  ret
}
