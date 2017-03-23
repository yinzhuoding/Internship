\name{readSH}
\alias{readSH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads the Specific Humidity data from package p-medds
}
\description{
Reads the weely specific humidity file ILI.small.specific.humidity.by.zip5.20090320.20100628. Extracts the week numbers, week dates, SH for each base, number of weeks, number of bases and their zip names.
}
\usage{
readSH(epi_data$nweeks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nweeks_epi}{
  	The number of weeks in the epi profile
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
	\item{curves }{The SH curves for all bases in file}
	\item{date }{The week date}
	\item{week }{The week number (using CDC definition)}
	\item{nweeks }{Number of weeks in data file}
	\item{nbase }{Number of bases in data file}
	\item{zipname }{zip codes of the bases}
%% ...
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.

}
\author{
Predictive Science Inc. 
}
\note{
The code does NOT assume that order of the bases in the SH and EPI files is the same. The code uses the 5-digit zip code to identify the installation.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- sh_data = readSH(epi_data$nweeks)


## The function is currently defined as
function (nweeks_epi=66) 
{
    data(ILI.small.specific.humidity.by.zip5.20090320.20100628, 
        package = "pmedds.core")
    raw_sh <- ILI.small.specific.humidity.by.zip5.20090320.20100628
    sh_week <- raw_sh$week
    sh_date <- as.Date(raw_sh$date, format = "\%m/\%d/\%y")
    sh <- sapply(raw_sh[, 4:dim(raw_sh)[2]], as.ts)
    nweeks_sh <- length(sh_week)
    zipname <- colnames(sh)
    zipname <- substr(zipname, start = 2, stop = 10)
    nbase <- length(zipname)
    nweeks_sh <- length(sh_week)
    list(curves = sh, date = sh_date, week = sh_week, nweeks = nweeks_sh, 
        nbase = nbase, zipname = zipname)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
