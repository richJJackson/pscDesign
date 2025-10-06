### Panthion Trial Design


### Loading functions
library(psc)
setwd("~/Documents/GitHub/pscDesign")
devtools::load_all()

### loading a model
load("Data/AB_pfs_prot.Rds")



### recruitment forcast
N.site <- 4
rpm <- 0.87
open.rate <- 1
recTime <- 24
recF <- rec.forcast(N.site,rpm,open.rate,Max.Time=recTime)  #70 patients over 24 months

recF

### Power Estimate
fuTime <- 12
beta <- log(0.7);beta
pscDes <- pscDesign.flexsurvreg(CFM=ab_prg,n0=0,n1=70,rec=recF,recTime=recTime,beta=beta,
                      fuTime=fuTime,nsim=1000,nsim.psc=750,bound=0,
                      direction="greater",alpha_eval=c(0.05,0.1,0.15,0.2))


pscDes$avEvent
pscDes$avBeta
pscDes$avSd
pscDes$Power_est
pscDes$Rec


###
CFM=ab_prg
n0=0
n1=70
recTime=recTime
beta=beta
fuTime=fuTime
nsim=50
nsim.psc=1000
bound=0
direction="greater"
alpha_eval=c(0.05,0.1)





