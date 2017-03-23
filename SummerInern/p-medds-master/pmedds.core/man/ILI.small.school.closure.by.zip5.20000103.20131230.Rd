\name{ILI.small.school.closure.by.zip5.20000103.20131230}
\alias{ILI.small.school.closure.by.zip5.20000103.20131230}
\docType{data}
\title{
  School schedule for the Top 10 bases from 2000-2013
}
\description{
  When school is closed the whole week, the number is 1. 
  When it is closed for about half the week the number is 2 and this is currently 
  replaced by 1 in the code itself.
  When school is open we have NA and after loading the data the code changes this to zero.
  When data is missing we have NA and after loading the data the code changes this to zero
}
\usage{data(ILI.small.school.closure.by.zip5.20000103.20131230)}
\format{
  A data frame with 731 observations on the following 12 variables.
  \describe{
    \item{\code{week}}{Week Number, an index from 1 to 731}
    \item{\code{date}}{Ending Date for Week format m/d/y}
    \item{\code{X23708}}{Open (NA) or closed (1 or 2)}
    \item{\code{X80913}}{Open (NA) or closed (1 or 2)}
    \item{\code{X78236}}{Open (NA) or closed (1 or 2)}
    \item{\code{X92134}}{Open (NA) or closed (1 or 2)}
    \item{\code{X92055}}{Open (NA) or closed (1 or 2)}
    \item{\code{X99506}}{Open (NA) or closed (1 or 2)}
    \item{\code{X29207}}{Open (NA) or closed (1 or 2)}
    \item{\code{X78234}}{Open (NA) or closed (1 or 2)}
    \item{\code{X32542}}{Open (NA) or closed (1 or 2)}
    \item{\code{X76544}}{Open (NA) or closed (1 or 2)}
  }
}
\details{
	Data was collected from the web and inferred in many cases based on the information for the current and previous couple of years.
	For each MPZ we found the relevant school district(s) and used their web site to record or infer the dates
}
\source{
  For each MPZ the relevant school district(s) web site
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ILI.small.school.closure.by.zip5.20000103.20131230)
}
\keyword{datasets}
