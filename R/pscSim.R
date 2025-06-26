
### pscSim.flexsurvreg

### simulate and fit psc model


CFM <- CFM
nsim <- 15
n0 <- 0
n1 <- 50
beta <- log(0.9);beta
maxTime <- 48
nsim.psc <- 2000

#####pscSim


post.est <- NULL
for(i in 1:nsim){
  ## Simualte data
  ds <- dataSim.flexsurvreg(CFM=CFM,n0=n0,n1=n1,beta=beta,maxTime=maxTime)

  ## fitpsc
  simfit <- pscfit(CFM,ds,nsim=nsim.psc)
  plot(simfit)
  post <- simfit$posterior$beta
  post.est <- cbind(post.est,post)

  if(i ==1) plot(density(post.est[,i]))
  if(i > 1) lines(density(post.est[,i]))
  }

plot(simfit)

CFM

exp(mean(colMeans(post.est)))

### issue with small sample sizes!!!

post.est <- data.frame(post.est)

mn <- lapply(post.est,median)
sd <- lapply(post.est,sd)
pr<- lapply(post.est,function(x) length(which(x>0)))


post.est[1:10,]

unlist(pr)


median(unlist(mn))



mean(post)
sd(post)


summary(simfit)





