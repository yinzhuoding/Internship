\name{wt.write.results}
\alias{wt.write.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Writes a compact csv file with the statistics for the daily estimate for R(t) using the W-T procedure
	}
\description{
Writes a csv file with the date (onset or report), day number, number of cases, mean daily value of R(t), median daily value of R(t) and the 95\% daily value of R(t)
}
\usage{
	success <- wt.write.results(mycountry=mycountry,cases.order=cases.order,Rl=Rl,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,log=logfile,subDir=subDir)
}
\arguments{
  \item{mycountry}{
  	The country name
  	}
  \item{cases.order}{
  	An array with the cases ordered by unique onset or report day
  }
  \item{Rl}{
  	A matrix with our typically 10,00 realizations for the daily value of R(t)
  }
  \item{Rlq.order}{
  An array with the 95\% CI for the daily value of R(t)	
  }
  \item{Rlm.order}{
  	An array with our daily mean estimate for R(t)
  }
  \item{Rlmd.order}{
  	An array with our daily median estimate for R(t)
  }
  \item{dates}{
  	Array with unique onset or report dates
  }
  \item{log}{
  	Name of log file for run
  }
  \item{subDir}{
  	Name of directory where output and plots are saved
  }
  \item{probs}{
  	An array or number with desired quantiles
  }
}
\details{
 See the README file and example.wt.R in the 'examples' directory for more details on all these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 success - set to TRUE if csv file was written

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
##--	success <- wt.write.results(mycountry=mycountry,cases.order=cases.order,Rl=Rl,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,log=logfile,subDir=subDir)

## The function is currently defined as
function(mycountry=NULL,cases.order=NULL,Rl=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,log=NULL,subDir=NULL) {
	
	  # check to see if "data" sub-directory exists, if not create it
	  subDir=getwd()
	  subDir = paste(subDir,"/output",sep="")

      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }
      
      
	# write the sampled results so we can re-use it to calculate all sorts of statistics
	
	fileout = paste(subDir,"/Rl.",mycountry,".csv",sep="")
	write.csv(Rl,file=fileout)
	cat(" Writing the sampled results to file: ",fileout,"\n")
	text <- paste(" Writing the sampled results to file: ",fileout,"\n",sep="")
	writeLines(text=text,con=log)
	
#write all the ordered information to a file
	mytable = list()
	mytable$date    = dates[cases.order[1,]]
	mytable$day     = cases.order[1,]
	mytable$cases   = cases.order[2,]
	mytable$Rmean   = Rlm.order[2,]
	mytable$Rmedian = Rlmd.order[2,]
	mytable$R95     = Rlq.order[2,]
	fileout = paste(subDir,"/Rstats-",mycountry,".csv",sep="")
	write.csv(mytable,file=fileout)
	cat(" Writing ordered results to file: ",fileout,"\n")
	text <- paste(" Writing ordered results to file: ",fileout,"\n",sep="")
	writeLines(text=text,con=log)
	success = TRUE
	success
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
