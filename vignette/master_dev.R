
### pscSim.flexsurvreg
#devtools::load_all()


### Loading packages
library(survival)


### loading a model
load("Data/AB_pfs_prot.Rds")


### Look at package structure
#library(pkgnet)
#CreatePackageReport("pscDesign")



### Setting Design Parameters
nsim <- 50


CFM <- ab_prg ### counter factual model for the control
n0 <- 0 ## control patients
n1 <- 80 ## experimental patients

beta <- log(0.7);beta ## log hazard ratio
maxTime <- 36 ## time of follow-up and recruitment
nsim.psc <- 2000 ## how many simulations for the psc model

bound <- 0 ## boundary for efficacy
direction <- "greater" ## direction to demonstrate efficacy
alpha_eval <- c(0.01,0.025,0.05,0.1,0.15,0.2) ## Alpha levels to evaluate



psc_err1 <- pscErr.flexsurvreg(CFM,n0=0,n1=40,beta=beta,maxTime=maxTime,
                      nsim=250,nsim.psc=100,bound=0,direction="greater",
                      alpha_eval=c(0.01,0.025,0.05,0.1,0.15,0.2))

warnings()

psc_err1





mn <- 1
sd <- 1
bound <- 0
direction="greater"









#######################




