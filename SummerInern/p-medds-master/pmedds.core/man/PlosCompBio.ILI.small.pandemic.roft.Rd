\name{PlosCompBio.ILI.small.pandemic.roft}
\alias{PlosCompBio.ILI.small.pandemic.roft}
\docType{data}
\title{
  Predicted force of infection as published in Plos Comp Biology: 
  http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004392
}
\description{
  Weekly prediction for R(t) using a two value model for the top 50 bases.
  bases are ordered by the cumulative nubmer of cases per base.
  The time span is March 20, 2009 to June, 28 2010 - the pandemic period. 
  Bases are identified by their 5-digit zip-code, preceded by the lettre 'X' which the code removes.
}
\usage{data(PlosCompBio.ILI.small.pandemic.profiles)}
\format{
  A data frame with 66 observations on the following 52 variables.
  \describe{
    \item{\code{week}}{Week number in FY 2009-2010 using CDC week convention}
    \item{\code{date}}{Week Ending Date} 
    \item{\code{X23708}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X80913}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X78236}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X92134}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X23665}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X92055}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X99506}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X29207}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X78234}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X32542}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X42223}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X23511}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X76544}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X79920}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X22134}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X89191}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X28547}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X94535}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X98431}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X28310}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X32214}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X85309}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X30905}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X20762}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X22060}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X40121}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X39534}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X23801}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X29905}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X23604}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X32228}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X20889}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X92278}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X60088}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X9180}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X83648}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X66442}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X36112}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X87117}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X96859}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X65473}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X29152}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X31314}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X96319}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X96350}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X78843}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X84056}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X31905}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X57706}}{R(t) using a two-value model for individual base by zip 5}
    \item{\code{X71459}}{R(t) using a two-value model for individual base by zip 5}
  }
}
\details{
  The 50 bases are ranked according to the cumulative number of cases during this pandemic time period.
}
\source{
	For the full article see:
	http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1004392

}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(PlosCompBio.ILI.small.pandemic.roft)
}
\keyword{datasets}
