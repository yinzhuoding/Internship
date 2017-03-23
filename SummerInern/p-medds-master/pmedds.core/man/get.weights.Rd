\name{get.weights}
\alias{get.weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get weekly weights for PMEDDS objective function
}
\description{
 Sub-function of get.data() that sets/retrieves weighting values for each week of epi data.
}
\usage{wweightOUT = get.weights(wflag=0, nweeks=52,dataType="MPZ", wweightIN=NULL,  patients=NULL)
}

\arguments{
	\item{wflag }{Designates method used to set weights: 0 - all weeks equal (default), 1 - User-input, 2 - Retrieve from database, 3 - Use CDC Total Patients.}
	\item{mydata }{Data-structure containing PMEDDS input data. This function requires the fields mydata$nweeks and (for wflag=3) mydata$patients.}
	\item{dataType }{Data type. Expected "MPZ", "CDC", "GFT", or "GFTPlus". Used to verify that wflag=3 is appropriate.}
	\item{wweightIN }{User-input weights. Used in the case of wflag=1. Requirements: length(wweightIN)==mydata$nweeks, sum(wweight)>0}
}

\value{
	\item{wweightOUT }{Weekly weighting normalized such that sum(wweightOUT)=length(wweightOUT)}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- The default behavior ----
require(pmedds.core)
mydata = list()
mydata$nweeks = 10
test = get.weights(mydata=mydata)

##---- Use CDC Patients as weights (wflag=3) ----
mydata$patients = runif(mydata$nweeks,0,1)
test2 = get.weights(wflag=3,mydata=mydata,dataType="CDC")

##---- Use User-Input as weights (wflag=1) ----
UserInput = runif(mydata$nweeks,0,1)
test3 = get.weights(wflag=1,mydata=mydata,wweightIN=UserInput)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
