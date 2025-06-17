### Function to calculate accumulating events based on recruitment and survival estimates


accEvent <- function(Mrec,surv,endMonth,drop=0.05){
	
	if(length(surv)<endMonth){
		stop("survival function smaller than trial length")
	}
	
	if(length(Mrec)>length(surv)){
		stop("recruitment longer than survival function")
	}

	## Making sure all structures are the right length
	surv <- surv[1:endMonth]
	PrDead <- 1-surv
	
	## Extending Monthly recruitment
	Mrec <- c(Mrec,rep(0,length(surv)-length(Mrec)));Mrec
	accEvent <- rep(NA,endMonth)
	for(i in 1:endMonth){
		accEvent[i] <- sum(rev(PrDead[1:i])*Mrec[1:i]*(1-drop))
	}

	## Results data frame
	res <- data.frame("Month"=1:endMonth,"MonthlyRec"=Mrec,"CumulativeRec"=cumsum(Mrec),"SurvivalFn"=surv,"accEvent"=accEvent)
	
	## returning results
	res
	}

