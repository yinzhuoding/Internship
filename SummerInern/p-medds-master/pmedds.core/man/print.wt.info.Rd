\name{print.wt.info}
\alias{print.wt.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Prints information about a W-T modeling of the 2003 SARS data
	}
\description{
Prints to screen and to a file information about a W-T run for a given country using the 2003 SARS data.  Information includes user name, date and time of run, information on the machine, direcotry where job is running and directory where all the data and plots will be saved. 
}
\usage{
	success <- print.wt.info(mydata=mydata,shape=shape,scale=scale,ireal=ireal,subDir=subDir,log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
  	A list with an array of case numbers, an array of onset or report day for these cases, total number of cases and country name.
  	}
  \item{shape}{
  	The shape parameter for a Weibull distribution, defualt is: 3.8
  }
  \item{scale}{
  	The scale parameter for a Weibull distribution, defualt is: 8.4
  }
  \item{ireal}{
  How many realizations of R(t) to calculate, default is 10,000
  }
  \item{subDir}{
  	Name of directory where output and plots are saved
  }
  \item{log}{
  	Name of log file for this run
  }
}
\details{
 See the README file in the 'examples' directory for more details on all these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 success - set to TRUE if function was properly executed

%% ...
}
\references{
Wallinga J, Teunis P (2004) Different Epidemic Curves for Severe Acute Respiratory Syndrome Reveal Similar Impacts of Control Measures. Am J Epidemiol 160: 509–516.

http://en.wikipedia.org/wiki/Weibull_distribution
}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	success <- print.wt.info(mydata=mydata,shape=shape,scale=scale,ireal=ireal,subDir=subDir,log=logfile)

## The function is currently defined as
function(mydata=NULL,scale=NULL,shape=NULL,ireal=NULL,job.name=" ",subDir="output",log=NULL) {
      
	  mycountry = mydata$mycountry
	  ncases = mydata$ncases
	  subDir=paste(getwd(),"/",subDir,sep="")
      
      s <- Sys.getenv() # This will give a long list of environment variables. To see the naems use names(s)
      m <- Sys.info()   # This will give a lot of information on her hardware and OS
      cat("\n\n P-MEDDS Package Version 001 \n\n")
      cat(" Job Name: ",job.name)
      cat(" Job Started on: ",format(Sys.time(),"\%a \%b \%d \%X \%Y"),"\n")
      cat(" Job Running on: ",m[["nodename"]],"\n")
      cat(" OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n")
      cat(" Job Running by User: ",s["USER"],"\n")
      cat(" Job Running in Directory: ",getwd(),"\n")
      cat(" All Data and Plots Saved in Sub-Directory: ",subDir,"\n\n")
      cat(" Modeling ", mycountry,"2003 SARS Data","\n")
	  cat(" Running ",ireal, ' Realizations for a Total of ',ncases," cases","\n")
      
      text <- paste("\n\n P-MEDDS Package Version 001\n",
                    " Job Name: ",job.name,"\n",
                    " Job Started on: ",format(Sys.time(),"\%a \%b \%d \%X \%Y"),"\n",
                    " Job Running on: ",m[["nodename"]],"\n",
                    " OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n",
                    " Job Running by User: ",s["USER"],"\n",
                    " Job Running in Directory: ",getwd(),"\n",
                    " All Data and Plots Saved in Sub-Directory: ",subDir,"\n\n",
                    " Modeling ", mycountry,"2003 SARS Data","\n",
                    " Running ",ireal, ' Realizations for a Total of ',ncases," cases","\n")


      writeLines(text=text,con=log)
      return(TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
