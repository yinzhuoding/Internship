\name{ILI.small.pandemic.curves.by.zip5.20090320.20100628}
\alias{ILI.small.pandemic.curves.by.zip5.20090320.20100628}
\docType{data}
\title{
  Weekly ILI EPI data for Top 50 bases for the pandemic year 2009-2010
}
\description{
  Weekly incidence for Top 50 bases ordered by the cumulative nubmer of cases per base.
  The time span is March 20, 2009 to June, 28 2010 - the pandemic period. 
  Bases are identified by their 5-digit zip-code, preceded by the lettre 'X' which the code removes.
}
\usage{data(ILI.small.pandemic.curves.by.zip5.20090320.20100628)}
\format{
  A data frame with 66 observations on the following 52 variables.
  \describe{
    \item{\code{week}}{Week number in FY 2009-2010 using CDC week convention}
    \item{\code{date}}{Week Ending Date} 
    \item{\code{X23708}}{Incidence for individual base by zip 5}
    \item{\code{X80913}}{Incidence for individual base by zip 5}
    \item{\code{X78236}}{Incidence for individual base by zip 5}
    \item{\code{X92134}}{Incidence for individual base by zip 5}
    \item{\code{X23665}}{Incidence for individual base by zip 5}
    \item{\code{X92055}}{Incidence for individual base by zip 5}
    \item{\code{X99506}}{Incidence for individual base by zip 5}
    \item{\code{X29207}}{Incidence for individual base by zip 5}
    \item{\code{X78234}}{Incidence for individual base by zip 5}
    \item{\code{X32542}}{Incidence for individual base by zip 5}
    \item{\code{X42223}}{Incidence for individual base by zip 5}
    \item{\code{X23511}}{Incidence for individual base by zip 5}
    \item{\code{X76544}}{Incidence for individual base by zip 5}
    \item{\code{X79920}}{Incidence for individual base by zip 5}
    \item{\code{X22134}}{Incidence for individual base by zip 5}
    \item{\code{X89191}}{Incidence for individual base by zip 5}
    \item{\code{X28547}}{Incidence for individual base by zip 5}
    \item{\code{X94535}}{Incidence for individual base by zip 5}
    \item{\code{X98431}}{Incidence for individual base by zip 5}
    \item{\code{X28310}}{Incidence for individual base by zip 5}
    \item{\code{X32214}}{Incidence for individual base by zip 5}
    \item{\code{X85309}}{Incidence for individual base by zip 5}
    \item{\code{X30905}}{Incidence for individual base by zip 5}
    \item{\code{X20762}}{Incidence for individual base by zip 5}
    \item{\code{X22060}}{Incidence for individual base by zip 5}
    \item{\code{X40121}}{Incidence for individual base by zip 5}
    \item{\code{X39534}}{Incidence for individual base by zip 5}
    \item{\code{X23801}}{Incidence for individual base by zip 5}
    \item{\code{X29905}}{Incidence for individual base by zip 5}
    \item{\code{X23604}}{Incidence for individual base by zip 5}
    \item{\code{X32228}}{Incidence for individual base by zip 5}
    \item{\code{X20889}}{Incidence for individual base by zip 5}
    \item{\code{X92278}}{Incidence for individual base by zip 5}
    \item{\code{X60088}}{Incidence for individual base by zip 5}
    \item{\code{X9180}}{Incidence for individual base by zip 5}
    \item{\code{X83648}}{Incidence for individual base by zip 5}
    \item{\code{X66442}}{Incidence for individual base by zip 5}
    \item{\code{X36112}}{Incidence for individual base by zip 5}
    \item{\code{X87117}}{Incidence for individual base by zip 5}
    \item{\code{X96859}}{Incidence for individual base by zip 5}
    \item{\code{X65473}}{Incidence for individual base by zip 5}
    \item{\code{X29152}}{Incidence for individual base by zip 5}
    \item{\code{X31314}}{Incidence for individual base by zip 5}
    \item{\code{X96319}}{Incidence for individual base by zip 5}
    \item{\code{X96350}}{Incidence for individual base by zip 5}
    \item{\code{X78843}}{Incidence for individual base by zip 5}
    \item{\code{X84056}}{Incidence for individual base by zip 5}
    \item{\code{X31905}}{Incidence for individual base by zip 5}
    \item{\code{X57706}}{Incidence for individual base by zip 5}
    \item{\code{X71459}}{Incidence for individual base by zip 5}
  }
}
\details{
  The 50 bases are ranked according to the cumulative number of cases during this pandemic time period.
}
\source{
Original data is at:
  ~/Dropbox/LEPR01 (1)/data/AFHSC-ILI-2000-2011.csv together with ~/Dropbox/LEPR01 (1)/data/dmisid.2010.csv
  and the code for extracting the weekly ILI cases is at: ~/Dropbox/LEPR01 (1)/code/genNewILISmall.original.R
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ILI.small.pandemic.curves.by.zip5.20090320.20100628)
}
\keyword{datasets}
