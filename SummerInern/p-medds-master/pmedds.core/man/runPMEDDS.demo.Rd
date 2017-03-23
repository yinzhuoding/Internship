\name{runPMEDDS.demo}
\alias{runPMEDDS.demo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	P-MEDDS MCMC demonstration using included ILI data.  
}
\description{
This function is intended to demonstrate the output of a PMEDDS MCMC optimization with a minimal amount of user input.  Using the user-selected data set, runPMEDDS.demo automatically sets many model and optimization parameters. This should result in a reasonable model-fit and short runtime.  A robust optimization requires many realizations, random initial conditions, and longer MCMC chains.   
}
\usage{
runPMEDDS.demo(dataType="MPZ",job.year=2009,region=1,national=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataType}{
    The data type: MPZ (Military), CDC, GFT, or GFTPlus
  }
  \item{job.year}{
    Automatically, pulls data for one flu season. `job.year' specifies the calendar year at the begining of the flu season.  
  }
  \item{region}{
    Region of the United States. Regions are divided per CDC specification.
  }
  \item{national}{
    Boolean flag. TRUE: use national data and value of `region' is ignored. FALSE: us data from specified region.
  }
}
\details{
  Generally designed to be called from 'demo.MCMC.R'.  Output images and data sets are written directly to files. File names reflect the dataType, job year, and region of data used.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
No returned value
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}

\examples{
 ##---- Should be DIRECTLY executable !! ----
 ##--   
 ##--	runPMEDDS.demo(dataType="CDC",job.year=2007,region=1,national=TRUE)
 
 This call will produce a number of image and data files in the folder ./output. %plot-CDC_2007_national.pdf, chain-CDC_2007_national-1.pdf, car-CDC_2007_national.pdf, CDC_2007_national-epi-sh-school.pdf, mcmc-CDC_2007_national-1.RData, param-quantiles-CDC_2007_national-1.csv, param-stats-CDC_2007_national-1.csv, param-table-CDC_2007_national-1.csv
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
