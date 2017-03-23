\name{readEpi}
\alias{readEpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads an EPI file from package P-MEDDS
}
\description{
	Reads the epi data from ILI.small.pandemic.curves.by.zip5.20090320.20100628 and extracts from it the week number, week date and number of new cases per week for all bases along with their zip codes. 

}
\usage{
readEpi()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
  \item{curves }{The weekly number of ILI cases for all bases}
  \item{date }{The week date}
  \item{week }{The week number (using CDC definition of week numbers)}
  \item{nweeks }{number of weeks in FY. It is 66 for the 2009-2010 pandemic year and 52 or 53 for non-pandemic years}
  \item{nbase }{total number of military bases in data file}
  \item{zipname }{zip codes of all the military bases in the file}
%% ...
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
\note{
%%  ~~further notes~~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- epi_data = readEpi

## The function is currently defined as
function () 
{
    data(ILI.small.pandemic.curves.by.zip5.20090320.20100628, 
        package = "pmedds.core")
    raw_ili <- ILI.small.pandemic.curves.by.zip5.20090320.20100628
    epi_week <- raw_ili$week
    epi_date <- as.Date(raw_ili$date, format = "\%m/\%d/\%y")
    epi <- sapply(raw_ili[, 3:dim(raw_ili)[2]], as.ts)
    zipname <- colnames(epi)
    zipname <- substr(zipname, start = 2, stop = 10)
    nbase <- length(zipname)
    nweeks_epi <- length(epi_week)
    list(curves = epi, date = epi_date, week = epi_week, nweeks = nweeks_epi, 
        nbase = nbase, zipname = zipname)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
