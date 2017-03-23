\name{sh_region_state}
\alias{sh_region_state}
\docType{data}
\title{
  Weekly Averaged Specific Humidity for 46 states, ten CDC defined regions and the entire US 
}
\description{
 The weekly avereaged specific humidity for 46 states, ten CDC defined regions and the entire US.
 Time range is September 28, 2003 to October 19, 2014.  
 The data was complied from NOAA NCEP/DOE AMIP-II Reanalysis (Reanalysis-2) Daily averages:
 http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?&Dataset=NCEP/DOE+AMIP-II+Reanalysis+(Reanalysis-2)+Daily+Averages&Variable=Specific+humidity
 This data set covers the entire world and here we use it for the US.

}
\usage{data(sh_region_state)}
\format{
  A data frame with 578 observations on the following variables.
  \describe{
    \item{\code{X}}{Index vector from 2-627 can be ignored}

    \item{\code{date}}{Week Date} 
    \item{\code{Year}}{4-digit Year}        
    \item{\code{week}}{Week Number for each FY}
    \item{\code{Hawaii}}{Specific Humidity for Hawaii}
    \item{\code{Florida}}{Specific Humidity for Florida etc. (total of 46 states)}    
    \item{\code{Region1}}{Specific Humidity for CDC Region 1}
    \item{\code{Region2}}{Specific Humidity for CDC Region 2}
    \item{\code{Region3}}{Specific Humidity for CDC Region 3}
    \item{\code{Region4}}{Specific Humidity for CDC Region 4}
    \item{\code{Region5}}{Specific Humidity for CDC Region 5}
    \item{\code{Region6}}{Specific Humidity for CDC Region 6}
    \item{\code{Region7}}{Specific Humidity for CDC Region 7}
    \item{\code{Region8}}{Specific Humidity for CDC Region 8}            
    \item{\code{Region9}}{Specific Humidity for CDC Region 9}            
    \item{\code{Region10}}{Specific Humidity for CDC Region 10}   
    \item{\code{national}}{Specific Humidity for the entire US}                                 
  }
}
\details{
  We used the NOAA data to create weekly averaged SH for each state and then properly averged to get the values for each region and the entire US.
}
\source{
  The raw SH values came from NOAAs data base, see:
  http://www.esrl.noaa.gov/psd/cgi-bin/db_search/DBSearch.pl?&Dataset=NCEP/DOE+AMIP-II+Reanalysis+(Reanalysis-2)+Daily+Averages&Variable=Specific+humidity
}
\references{
  Read the description in the URL above
}
\examples{
data(sh_region_state)
## maybe names(sh_region_state) ; head(sh_region_state) ...
}
\keyword{datasets}

