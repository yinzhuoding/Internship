\name{wt.calc.stats}
\alias{wt.calc.stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Calculates the statistics for the daily R(t) in the W-T model
	}
\description{
Calculates the unique onset/report days and the number of cases per such day along with the mean, median and quantile for the W-T estimate for the daily value of the force of infection.  Works with the 2003 SARS data.
}
\usage{
	stats <- wt.calc.stats(Rl=Rl,times=times,probs=probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rl}{
  	a large matrix which holds all our daily R(t) realizations
  	}
  \item{times}{
  	A long array of length ncases with the onset or report day for each case
  }
  \item{probs}{
  	An array or number with desired quantiles
  }
}
\details{
 See the README file adn example.wt.R script in the 'examples' directory for more details on all these parameters as well as the output of runPMEDDS.wt
}
\value{
	A list with the following items:
   \item{unique.t }{
   	An array with unique onset or report days
   }
   \item{onset.n }{
   	An array with the number of cases on the unique onset/report days 
   }
   \item{Rl.qntl }{
   	An array with the 95\% CI for the daily value of R(t)
   }
   \item{Rl.mean }{
   	An array with our daily mean estimate for R(t)
   	}
   	\item{Rl.median }{
   	An array with our daily median estimate for R(t)
   	}

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
##--	stats <- wt.calc.stats(Rl=Rl,times=times,probs=probs)

## The function is currently defined as
function(Rl=NULL,times=NULL,probs=0.95) {
#unique onset days and times

	unique.t<- unique(times)
	onset.n <- c(NA)



	for (i in 1:length(unique.t)){
		onset.n[i]<-length(times[which(times==unique.t[i])])
	}

	Rl.qntl <-rep(0.0,length(unique.t))
	Rl.mean <-rep(0.0,length(unique.t))
	Rl.median <-rep(0.0,length(unique.t))
	probs = 0.95

# calculate the 95  quantile and mean for each case Rl
	for (it in 1:length(unique.t)) {
	temp <- which(times == unique.t[it])
	Rl.temp <-rbind(Rl[temp,])
	Rl.mean[it] = mean(Rl.temp)
	Rl.median[it] = median(Rl.temp)
	Rl.qntl[it] = quantile(Rl.temp,probs=probs) 

	}	
	
	list(unique.t=unique.t,onset.n=onset.n,Rl.mean=Rl.mean,Rl.median=Rl.median,Rl.qntl=Rl.qntl)	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
