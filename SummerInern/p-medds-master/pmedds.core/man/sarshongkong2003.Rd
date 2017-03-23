\name{sarshongkong2003}
\alias{sarshongkong2003}
\docType{data}
\title{
   SARS Probable Cases, HONG-KONG 2003
}
\description{
  A list of SARS probable cases in HONG-KONG by day of onset from February 1, 2003 to July 10, 2003.
  Total number of cases is 1,734.
}
\usage{data(sarshongkong2003)}
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
  http://www.who.int/csr/sars/epicurve/epiindex/en/index4.html
  The Text file was given to us by the Wallinga-Teunis Group
}
\references{
 Jacco Wallinga and Peter Teunis, American Journal of Epidemiology, vol. 160, Number 6, page 509 (2004)
 
 http://www.who.int/csr/sars/epicurve/epiindex/en/index4.html
}
\examples{
data(sarshongkong2003)
}
\keyword{datasets}
