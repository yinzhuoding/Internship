\name{wallinga1_import}
\alias{wallinga1_import}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calculates Wallinga's estimate for the daily reproduction number
	}
\description{
The function wallinga1_import calculates the relative likelihood that case k has been infected by case l and then uses this with the onset days and importation days to calculate the estimate for the daily reproduction number. The function calls the weibull Fortran code. 
}
\usage{
	Xneg <-wallinga1_import(times = times,import = import,weibull.par = weibull.par)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
  	An array with onset or report day for each SARS case
  	}
  \item{import}{
  	An array of zero's of length total number of SARS probable cases
  }
  \item{weibull.par}{
  An array with the scale and shape parameters for a Weibull distribution, the wallinga1_import will modify this array
  }
}
\details{
 See the README file and example.wt.R script in the "examples" directory for more details on all these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 Returns an array, Xneg, with the daily estimate for the reproduction number

%% ...
}
\references{
Wallinga J, Teunis P (2004) Different Epidemic Curves for Severe Acute Respiratory Syndrome Reveal Similar Impacts of Control Measures. Am J Epidemiol 160: 509–516.
}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	Xneg <-wallinga1_import(times = times,import = import,weibull.par = weibull.par)

## The function is currently defined as
wallinga1_import <- function(times, import, weibull.par){

	#function to calculate the relative likelihoods that case k
	#has been infected by case l (output is a k x l matrix)
	#Wallinga 2004 AJE
	unique.t<- unique(times)
	n <- length(unique.t)
	import.n <- onset.n <- c(NA)
	Xneg <- c(NA)

	for (i in 1:length(unique.t)){
	onset.n[i]<-length(times[which(times==unique.t[i])])
	}

	for (i in 1:length(unique.t)){
	import.n[i]<-length(times[which(times==unique.t[i] & import==1)])
	}

	output.mat<- matrix(0.0, ncol=n, nrow=n)
	# the keyword 'norm' tells the weibull routine if it should normalize the matrix (norm=0) or not (norm!=0)

	out <- .Fortran("weibull",n=as.integer(n),output.mat=as.double(output.mat),times=as.double(unique.t),shape=as.double(weibull.par[2]),scale=as.double(weibull.par[1]),norm=as.integer(1))

	output <- matrix(out$output.mat,nc=n)


	for (i in 1:length(unique.t)){
	Xneg[i]<-0
	for (j in 1:length(unique.t)){
	temp <- as.numeric(((onset.n[j]-import.n[j])*output.mat[j,i]))/as.numeric((onset.n%*%(output.mat[j,])))
	Xneg[i] <- sum(Xneg[i], temp, na.rm=T)

	}
	Xneg[i]<- onset.n[i]* Xneg[i]

	}

	Xneg
} 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
