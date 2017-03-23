\name{get.School.data}
\alias{get.School.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get weekly School Closure Data for one of the ten CDC Regions, a state, or the entire US
}
\description{
 Get weekly School Closure Data for a given epidemic/pandemic season for one of the ten CDC Regions, a state, or the entire US.
 The School data was compiled from school years 2014-2015 and 2015-2016.  School district calendars were inspected from the three cities with largest population in each state.
}
\usage{
mydata = get.School.data(state=NULL, reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009, year.end = 2010)
}

\arguments{
	\item{state }{State name as a character string. Spaces " " in the string should be replaced with a period "."} 
	\item{reg }{Region number, for modeling (between 1-10).  Ignored if national = TRUE or !is.null(state)}
	\item{national	}{TRUE or FALSE.  Set to TRUE to model the entire US EPI profile. Ignored if !is.null(state)}
	\item{week.start }{Starting week for epidemic/pandemic year.}
	\item{week.end }{Ending week for epidemic/pandemic year.}
	\item{year.start }{Starting year for requested data.}
	\item{year.end }{Ending year for requested data.} 
}

\value{
	\item{school }{weekly approximation of school schedule. 0=School in, 1=no school.}
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
##-- mydata = get.School.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009, year.end = 2010)
## The function is currently defined as
get.School.data <- function(state=NULL, reg=1, national=FALSE, week.start=27, week.end=26, year.start=2009, year.end=2010) {
	
	# Determine row name
	if(!is.null(state)) {
		SchoolRowName = state
	} else if(national) {
		SchoolRowName = "National"
	} else {
		SchoolRowName = paste0("Region",reg)
	}
	
	# Ensure data is loaded
	data(school_region_nation,package="pmedds.core")
	
	# Determine col indices
	start.ind = which(school_region_nation$week==week.start & school_region_nation$year==year.start)
	end.ind = which(school_region_nation$week==week.end & school_region_nation$year==year.end)
	
	# pull data
	school = school_region_nation[start.ind:end.ind,SchoolRowName]
	return(school)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
