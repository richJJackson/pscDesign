### A function for estimating the recruitment rates
#'
#' Estimating recruitment rates based on the number of sites and average site
#' recruitment rates.
#'
#' @param N.site The number of recruiting sites
#' @param rpm The average recruitment per site per month
#' @param openRate the rate at which sites are expected to open to recruitment
#' @param Max.Time maximum time used in the estimation
#' @param penal A penalising factor for the recruitment in the intial month for
#' each site (defaults to penal=0.5)
#' @param plot shoud results be plotted?
#'
#' @details
#' This functions estimates monthly recruitment rates based on the number of
#' sites and the average monthly recruitment rate.  The resulting dataset can be
#' passed to pscDesign to improve estimation of design parameters.
#'
#' @return A dataset giving the monthly recruitment rate
#' @export
#'
recForcast <- function(N.site,rpm,open.rate,Max.Time,penal=0.5,plot=TRUE,...){


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

