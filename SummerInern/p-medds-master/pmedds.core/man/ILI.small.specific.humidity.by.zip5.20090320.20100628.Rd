\name{ILI.small.specific.humidity.by.zip5.20090320.20100628}
\alias{ILI.small.specific.humidity.by.zip5.20090320.20100628}
\docType{data}
\title{
  Weekly Averaged Specific Humidity for top 50 bases for the 2009-2010
}
\description{
 The weekly avereaged specific humidity for the top 50 bases from March 20, 2009
 to June 28, 2010.  For each base we used the lon/lat coordinates along with specific
 humidity data from NASA.  Bases in CONUS come from one source and non-conus from another.
}
\usage{data(ILI.small.specific.humidity.by.zip5.20090320.20100628)}
\format{
  A data frame with 92 observations on the following 53 variables.
  \describe{
    \item{\code{X}}{Index vector from 2-627 can be ignored}
    \item{\code{week}}{Week Number for each FY}
    \item{\code{date}}{Week Ending Date format} 
    \item{\code{X23708}}{Specific Humidity for individual base by zip 5}
    \item{\code{X80913}}{Specific Humidity for individual base by zip 5}
    \item{\code{X78236}}{Specific Humidity for individual base by zip 5}
    \item{\code{X92134}}{Specific Humidity for individual base by zip 5}
    \item{\code{X23665}}{Specific Humidity for individual base by zip 5}
    \item{\code{X92055}}{Specific Humidity for individual base by zip 5}
    \item{\code{X99506}}{Specific Humidity for individual base by zip 5}
    \item{\code{X29207}}{Specific Humidity for individual base by zip 5}
    \item{\code{X78234}}{Specific Humidity for individual base by zip 5}
    \item{\code{X32542}}{Specific Humidity for individual base by zip 5}
    \item{\code{X42223}}{Specific Humidity for individual base by zip 5}
    \item{\code{X23511}}{Specific Humidity for individual base by zip 5}
    \item{\code{X76544}}{Specific Humidity for individual base by zip 5}
    \item{\code{X79920}}{Specific Humidity for individual base by zip 5}
    \item{\code{X22134}}{Specific Humidity for individual base by zip 5}
    \item{\code{X89191}}{Specific Humidity for individual base by zip 5}
    \item{\code{X28547}}{Specific Humidity for individual base by zip 5}
    \item{\code{X94535}}{Specific Humidity for individual base by zip 5}
    \item{\code{X98431}}{Specific Humidity for individual base by zip 5}
    \item{\code{X28310}}{Specific Humidity for individual base by zip 5}
    \item{\code{X32214}}{Specific Humidity for individual base by zip 5}
    \item{\code{X85309}}{Specific Humidity for individual base by zip 5}
    \item{\code{X30905}}{Specific Humidity for individual base by zip 5}
    \item{\code{X20762}}{Specific Humidity for individual base by zip 5}
    \item{\code{X22060}}{Specific Humidity for individual base by zip 5}
    \item{\code{X40121}}{Specific Humidity for individual base by zip 5}
    \item{\code{X39534}}{Specific Humidity for individual base by zip 5}
    \item{\code{X23801}}{Specific Humidity for individual base by zip 5}
    \item{\code{X29905}}{Specific Humidity for individual base by zip 5}
    \item{\code{X23604}}{Specific Humidity for individual base by zip 5}
    \item{\code{X32228}}{Specific Humidity for individual base by zip 5}
    \item{\code{X20889}}{Specific Humidity for individual base by zip 5}
    \item{\code{X92278}}{Specific Humidity for individual base by zip 5}
    \item{\code{X60088}}{Specific Humidity for individual base by zip 5}
    \item{\code{X9180}}{Specific Humidity for individual base by zip 5}
    \item{\code{X83648}}{Specific Humidity for individual base by zip 5}
    \item{\code{X66442}}{Specific Humidity for individual base by zip 5}
    \item{\code{X36112}}{Specific Humidity for individual base by zip 5}
    \item{\code{X87117}}{Specific Humidity for individual base by zip 5}
    \item{\code{X96859}}{a logical vector}
    \item{\code{X65473}}{Specific Humidity for individual base by zip 5}
    \item{\code{X29152}}{Specific Humidity for individual base by zip 5}
    \item{\code{X31314}}{Specific Humidity for individual base by zip 5}
    \item{\code{X96319}}{a logical vector}
    \item{\code{X96350}}{a logical vector}
    \item{\code{X78843}}{Specific Humidity for individual base by zip 5}
    \item{\code{X84056}}{Specific Humidity for individual base by zip 5}
    \item{\code{X31905}}{Specific Humidity for individual base by zip 5}
    \item{\code{X57706}}{Specific Humidity for individual base by zip 5}
    \item{\code{X71459}}{Specific Humidity for individual base by zip 5}
  }
}
\details{
  We interpolate the specific humidity weekly averaged to the lon/lat coordinates of each base.
}
\source{
 The Specific Humidity Data for CONUS comes from NASA NLDAS-2 project.
 See http://ldas.gsfc.nasa.gov/nldas/NLDASnews.php for a description and
 http://disc.sci.gsfc.nasa.gov/hydrology/data-holdings for down load.
 For bases outside CONUS we used: http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.gaussian.html 
}
\references{
  Read the description in the URL above
}
\examples{
data(ILI.small.specific.humidity.by.zip5.20090320.20100628)
## maybe str(ILI.small.specific.humidity.by.zip5.20090320.20100628) ; plot(ILI.small.specific.humidity.by.zip5.20090320.20100628) ...
}
\keyword{datasets}

