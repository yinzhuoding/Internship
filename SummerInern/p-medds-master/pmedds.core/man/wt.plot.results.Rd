\name{wt.plot.results}
\alias{wt.plot.results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	A two-panel plot of the results for the 2003 SARS W-T procedure
	}
\description{
	Plots the daily probable number of cases for a given country for the 2003 SARS epidemic (either by onset or report day) and the estimated daily reproduction number, R(t): Red is mean value, green median value and blue the 95\% CI.  The horizontal grey line marks the critical value of one. 
}
\usage{
	success <- wt.plot.results(mycountry=mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir)
}
\arguments{
  \item{mycountry}{
  	The country name
  	}
  \item{cases.order}{
  	An array with the cases ordered by unique onset or report day
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
  \item{device}{
  	device name for plotting
  }
  \item{log}{
  	Name of log file for run
  }
  \item{subDir}{
  	Name of directory where output and plots are saved
  }
}
\details{
 See the README file and example.wt.R script in the 'examples' directory for more details on all of these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 Returns a single parameter - success - set to TRUE if csv file was written

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
##--	success <- wt.plot.results(mycountry=mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir)

## The function is currently defined as
function(mycountry=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,device="X11",log=NULL,subDir=NULL) {
	
	  # check to see if "data" sub-directory exists, if not create it
	  subDir=getwd()
      subDir <- paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }	

	
	if (device == "PDF" | device == "pdf") {
		pdfname = paste(subDir,"/W_T_",mycountry,".pdf",sep="")
		pdf(file=pdfname,onefile=TRUE,paper="a4r",width=11.5,height=8)
		cat(" Plotting Results to: ",pdfname,"\n")
		text <- paste(" Plotting Results to: ",pdfname,"\n",sep="")
		writeLines(text=text,con=log)
	}


	par(mfcol=c(2,1),mar=c(5,4,3,4))
	if (mycountry == "China") {
		title=paste("Probable cases of SARS by date of report - ",mycountry,sep="")
	} else {
		title=paste("Probable cases of SARS by date of onset - ",mycountry,sep="")
	}
	
	plot(cases.order[1,],cases.order[2,],type="h",col="red",xlim=range(cases.order[1,]),xlab='',ylab = "Cases",main=title,xaxt="n")
	axis(1,at=cases.order[1,],label=dates[cases.order[1,]],las=2,cex.axis=0.7)
	title="Estimated daily reproduction number (95\% CI, mean-red and median-green)"
	plot(Rlq.order[1,],Rlq.order[2,],type="h",col="blue",xlim=range(cases.order[1,]),ylim=c(0,14),xlab = '',ylab = "Reproduction Number",,main=title,xaxt="n")
	lines(Rlm.order[1,],Rlm.order[2,],type="p",col="red",xlab = '',ylab = "")
	lines(Rlmd.order[1,],Rlmd.order[2,],type="p",col="green",xlab = '',ylab = "")
	axis(1,at=cases.order[1,],label=dates[cases.order[1,]],las=2,cex.axis=0.7)
	abline(h=1.0,col="grey")

#
	if (device == "PDF" | device == "pdf") dev.off()
	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
