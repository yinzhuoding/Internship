\name{mers2014}
\alias{mers2014}
\docType{data}
\title{
   Middle East Respiratory Syndrome Corona-Virus (MERS-coV) 
   }
\description{
   MERS data is from January 1, 2014 to June 9, 2014.  
}
\usage{data(mers2014)}
\format{
	A text file with four columns: date, week number, number of cases and number of deaths
  \describe{
  	\item{\code{date}}{date of week}
    \item{\code{week}}{week of symptom onset}
    \item{\code{cases}}{number of cases for that week}
    \item{\code{death}}{number of death for that week}
  }
}
\details{
  Eventually we will Run a simple S-I-R model on the data
}
\source{
 http://www.who.int/csr/disease/coronavirus_infections/MERS-CoV_summary_update_20140611.pdf?ua=1
  The data comes from digitizing Figure 1 in this report
}
\references{
 http://www.who.int/csr/disease/coronavirus_infections/en/
}
\examples{
data(mers2014)
## plot(population$npop,type="b")
## maybe str(population) ; plot(population) ...
}
\keyword{datasets}
