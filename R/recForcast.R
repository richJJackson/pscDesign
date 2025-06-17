### Function for estimating the recruitment rate of a future trial


rec.forcast <- function(N.site,rpm,open.rate,Max.Time,penal=0.5,plot=TRUE,...){


## Getting the number of open sites per month
open.site<-seq(1,N.site,by=open.rate)
if(max(open.site)!=N.site) open.site <- c(open.site,N.site)


if(length(open.site)<Max.Time){
open.site<-c(open.site,rep(N.site,Max.Time-length(open.site)))

} else {
open.site <- open.site[1:Max.Time]
warning("Not enough time to open all sites!")
}

### Basic average rate per site approach
month.rate<-open.site*rpm

## penalisng monthly recruitment (recruits 1/2 as much in first month)
penalty <- diff(c(0,month.rate))*penal
month.rate <- month.rate-penalty

cum.rec<-round(cumsum(month.rate))
month.rate <- diff(c(0,cum.rec))

rec<-data.frame("SitesOpen"=round(open.site),"Monthly Rec"=month.rate,"Cumualtive Rec."=cum.rec)

if(plot) plot(cum.rec,typ="l",xlab="Time (Months)",ylab="Cumulative Recruitment",font.lab=3,...)

return(rec)

}

