\name{wt.write.data}
\alias{wt.write.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Writes a binary output file for a W-T run of SARS data
	}
\description{
Writes all the information (input and output) for a W-T run on the SARS 2003 data.  The information includes the input which is the data we are modeling, our (typically) 10,000 estimates for the daily value of R(t), the SARS cases ordered by onset or report day, and our estimate of the mean, median and 95\%CI of the daily R(t).  If this file is loaded this information is sufficient for reproducing all the plots and re-calculating all the statistics. 
}
\usage{
	success <- wt.write.data(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,ireal=ireal,log=logfile,subDir=subDir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata }{
   A list that holds all the data for the run
  }
  \item{Rl }{a large matrix which holds all our daily R(t) realizations}
  \item{cases.order }{
  	The SARS cases ordered by onset or report day
  }
  \item{Rlq.order }{
  	Our daily ordered 95\% estimate for R(t)
  }
  \item{Rlm.order}{
  	Our daily ordered mean estimate for R(t)
  }
  \item{Rlmd.order}{
  	Our daily ordered median estimate for R(t)
  }
  \item{ireal}{
   The number of realizations for the daily value of R(t)
  }
  \item{log}{
   name of output log file for the run
}
  \item{plot}{
  	sub-directory where all output and plots are saved, set to 'output'
  }
}
\details{
 See the README file adn example.wt.R script in the 'examples' directory for more details on all of these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
   Writes a binary data file but does not return any information
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
##--	success <- wt.write.data(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,ireal=ireal,log=logfile,subDir=subDir)

## The function is currently defined as
function(mydata=NULL,Rl=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,ireal=NULL,log=NULL,subDir=NULL) {
	 # check to see if "data" sub-directory exists, if not create it
	subDir=getwd()
    subDir <- paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }
      
      filename <- paste(subDir,"/",mydata$mycountry,".RData",sep="")
      
      cat("\n Writing R object Data: ",filename,"\n")   
      text <- paste(" Writing R object Data file: ",filename,"\n",sep=" ") 
      writeLines(text=text,con=log)
      # save the complete chain here
      save(mydata,Rl,cases.order,Rlq.order,Rlm.order,Rlmd.order,ireal,file=filename)      
      save.image()      

}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
