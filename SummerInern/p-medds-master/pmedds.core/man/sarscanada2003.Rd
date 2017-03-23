\name{sarscanada2003}
\alias{sarscanada2003}
\docType{data}
\title{
   SARS Probable Cases by day of onset, CANADA 2003
}
\description{
  A list of SARS probable cases in CANADA by onset day From February 1, 2003 to July 4, 2003
  Total number of cases is 250
}
\usage{data(sarscanada2003)}
\format{
  A text file with two columns: onset day and number of cases. 
  Need to substract 37622 from day number, this is a legacy
  item has to do with the way MS saves dates - January 1st, 2003.
  \describe{
    \item{\code{day}}{Onset day for symptoms day1=37622, January 1st 2003}
    \item{\code{cases}}{number of cases}
  }
}
\details{
  Used for the W-T SARS procedure.  The code we use follows the W-T 2004 paper.
}
\source{
  http://www.who.int/csr/sars/epicurve/epiindex/en/index6.html
  The Text file was given to us by the Wallinga-Teunis Group
}
\references{
 Jacco Wallinga and Peter Teunis, American Journal of Epidemiology, vol. 160, Number 6, page 509 (2004)
 
 http://www.who.int/csr/sars/epicurve/epiindex/en/index6.html
}
\examples{
data(sarscanada2003) 
}
\keyword{datasets}
