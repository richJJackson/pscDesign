### Data simulation tools

fpm.sim <- function(N,beta=0,fpm.extract,time){

  s_est <- surv.fpm(beta=beta,fpm.extract,time)

  md <- lm(time ~ I(S)+I(S^2)+I(S^3)+
             I(S^4)+I(S^5)+I(S^6)+
             I(S^7)+I(S^8)+I(S^9)+
             I(S^10)+I(S^11)+I(S^12),data=s_est)

  ru <- runif(N)
  ruc <- runif(N,0,0.1)

  tm_rand <- predict(md,newdata=data.frame(S=ru));tm_rand <- pmax(tm_rand,1e-06)
  tm_rand_cen <- predict(md,newdata=data.frame(S=ruc));tm_rand_cen


  ## Censoring
  cen.id <- as.numeric(tm_rand_cen>tm_rand)
  tm_rand[cen.id==0] <- tm_rand_cen[cen.id==0]

  ret <- data.frame(time=tm_rand,cen=cen.id)
  ret

}

