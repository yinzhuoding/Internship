\name{get.Temp.data}
\alias{get.Temp.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get weekly averaged daily air temperature data for one of the ten CDC Regions or the entire US
}
\description{
 Get weekly air temperature data for a given epidemic/pandemic season for one of the ten CDC Regions or the entire US. From 2010 to 2014. 
 The temperature data was compiled from NOAAs NCEP/DOE AMIP-II Reanalysis (Reanalysis-2) Daily averages which are yearly NCDF files.
 http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBListFiles.pl?did=34&tid=45605&vid=1227
}
\usage{
mydata = get.Temp.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009)
}

\arguments{
	\item{reg }{Region number, for modeling (between 1-10).  Ignored if national = TRUE}
	\item{national	}{TRUE or FALSE.  Set to TRUE to model the entire US EPI profile.}
	\item{week.start }{Starting weeek for epidemic/pandemic year. If NULL set to 27 unless this is the pandemic year in which case it is set to 13.}
	\item{week.end }{Ending week for epidemic/pandemic year.  If NULL set to 26.}
	\item{year.start }{Starting year for epidemic (or pandemic) season.}
}

\value{
	A list with
	\item{temp }{weekly temperature in Celsius}
	\item{weeks }{week number using CDC definition}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
 http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBListFiles.pl?did=34&tid=45605&vid=1227
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = get.Temp.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009)
## The function is currently defined as
function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009) {
      # reg        = index of particular USA region of interest (1-10)     
       
      #FY year.start - year.end
      if (!is.null(year.start)){
            year.end = year.start + 1
      } 
   	  if (!is.null(week.start) && !is.null(week.end)) {
            date.start = as.Date(paste('0-',week.start,'-',year.start,sep=''),format='%w-%U-%Y')
            date.end   = as.Date(paste('0-',week.end  ,'-',year.end  ,sep=''),format='%w-%U-%Y')   	  	
   	  } else {
            date.start = as.Date(paste('0-',27,'-',year.start,sep=''),format='%w-%U-%Y')
            date.end   = as.Date(paste('0-',26,'-',year.end  ,sep=''),format='%w-%U-%Y')   	  	   	  	
   	  } 
   	  # this loads a data frame named sh_region_state
	  data(temp_region_state,package="pmedds.core") 
	  
	  # first subset the rows that we need and then get the columns
	  
	  subs = subset(temp_region_state, date >= date.start & date <= date.end)
	  weeks = subs[,'week']
	  if (national == TRUE) {
	  	#Need to avearge over all ten regions
	  	temp = subs[,"national"]
	  } else {
	  	temp = subs[,paste('Region',reg,sep="")]

	  }
	  
	  list(temp=temp,weeks=weeks)
	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
