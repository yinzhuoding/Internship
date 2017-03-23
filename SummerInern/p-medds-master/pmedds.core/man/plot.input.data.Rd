\name{plot.input.data}
\alias{plot.input.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plots the inputs for an MCMC session.  
}
\description{
Plots ILI incidence, specific humidity, and school schedule.  If 'nweeksFit' is less than 'nweeksData', the plot will also indicate shade un-fitted data. 
School breaks are indicated by a cyan marker.  The presence of a marker indicates that schools are on break that week.   
}
\usage{
plot.input.data(myName=NULL,FY=NULL,nweeksFit=NULL,nweeksData=NULL,week=NULL,epi=NULL,sh=NULL,school=NULL,device.name="X11",log=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myName}{
  A descriptive name for the data including the data type, year, and zip code or region number
}
  \item{FY}{
  A character string describing the years included in the flu season.  Ex. "2014-2015"
}
  \item{nweeksFit}{
  Number of weeks that are to be fitted by the MCMC procedure
}
  \item{nweeksData}{
  Number of weeks in the data
}
  \item{week}{
  	The week labels for the data.  Used to label the horizontal axis.
  }
  \item{epi}{
  The epidemic profile (for PMEDDs this is generally influenza-like-illness cases)
}
  \item{sh}{
  The specific humidity data profile
}
  \item{school}{
  The school schedule data 
}
  \item{device.name}{
   device name for plotting: "pdf", "png", or "X11"
}
 \item{log}{
 	Name of log file for the run
 }
}

\value{
%%  ~Describe the value returned
    No return value
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
library(pmedds.core)
mydata = get.data()
logfile <- file(description="log.txt",open="wt")
plot.input.data(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeks,week=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,log=logfile)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
