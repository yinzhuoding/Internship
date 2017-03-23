\name{CDCweek2date}
\alias{CDCweek2date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Given a CDC week/year, return the date of the first day (Sunday) of that week
}
\description{
 CDC weeks start on Sunday and end on Saturday.  
 Week numbering: the CDC considers week 1 to be the first week of four or more days (equivalently: the week containing the first Wednesday of the year).  By this standard some years will have 53 weeks.
}
\usage{
date = CDCweek2date(CDCweek,year)
}

\arguments{
	\item{CDCweek }{CDC calendar week to be converted (integer)}
	\item{year }{calendar year (integer)}
}

\value{
	\item{date }{A Date object with value corresponding to Sunday of the input week}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
 http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?&Dataset=NCEP/DOE+AMIP-II+Reanalysis+(Reanalysis-2)+Daily+Averages&Variable=Specific+humidity
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----

## The function is currently defined as
CDCweek2date <- function(CDCweek,year) {
	# Convert CDC-week to a date (Sunday)
	# CDC week numbering: first Wednesday of the year is in week 1
	# R week numbering: first Sunday of the year is in week 1
	# R & CDC weeks: a week starts Sunday and ends Saturday
	
	# Determine if CDCweeks and Rweeks are the same using 1-Jan-year
	Jan1 = weekdays(as.Date(paste0(year,'-01-01')))
	if (any(Jan1==c('Monday','Tuesday','Wednesday'))) {
		# They are not, shift by 1
		Rweek = CDCweek-1
		if (Rweek==0) {
			date = as.Date(paste0('0-',1,'-',year),format='%w-%U-%Y')-7
		} else {
			date = as.Date(paste0('0-',Rweek,'-',year),format='%w-%U-%Y')
		}
	} else { # this year Rweeks and CDCweeks are the same
		Rweek = CDCweek
		if (Rweek==53) {
			date = as.Date(paste0('0-',52,'-',year),format='%w-%U-%Y')+7
		} else {
			date = as.Date(paste0('0-',Rweek,'-',year),format='%w-%U-%Y')
		}
	}
	return(date)	
}

Example usage:
> CDCweek2date(CDCweek=1,year=2008)
[1] "2007-12-30"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
