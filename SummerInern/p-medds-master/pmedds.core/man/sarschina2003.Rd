\name{sarschina2003}
\alias{sarschina2003}
\docType{data}
\title{
   SARS Probable Cases, CHINA 2003
}
\description{
  A list of SARS probable cases in CHINA by report day from  February 10, 2003 to June 25, 2003
  Total number of cases is 5,568
}
\usage{data(sarschina2003)}
\format{
  A text file with two columns: report day and number of cases. 
  Data has been digitized from the WHO web site
  \describe{
    \item{\code{day}}{Report day, day one is January 1st, 2003}
    \item{\code{cases}}{number of cases}
  }
}
\details{
  Used for the W-T SARS procedure.  The code we use follows the W-T 2004 paper.
  Data has some inaccuracies in it, total number of cases (5,568) is greater than what it should be (5,550)
}
\source{
  http://www.who.int/csr/sars/epicurve/epiindex/en/index3.html
  Data was digitized and converted to integers when needed. 
}
\references{
 Jacco Wallinga and Peter Teunis, American Journal of Epidemiology, vol. 160, Number 6, page 509 (2004)
 
 http://www.who.int/csr/sars/epicurve/epiindex/en/index3.html
}
\examples{
data(sarschina2003)
}
\keyword{datasets}
