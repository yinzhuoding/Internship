\name{population}
\alias{population}
\docType{data}
\title{
   Effective populations for Top ~100 Military Bases
}
\description{
  These are the base populations calculated using the number of weekly visits to clinics at each military zip code. These number are multiplied by omega=104.3 to obtain the effective base population.
}
\usage{data(population)}
\format{
  A data frame, named population, with ~100 observations on the following 3 variables.
  \describe{
    \item{\code{X}}{Index number of base, a numeric vector}
    \item{\code{zip50}}{The 5-digit zip code of the base, a numeric vector}
    \item{\code{npop}}{Population of base, a numeric vector}    
  }
}
\details{
  We do not assume anything about the order of the bases. We use the 5-digit zip-code to identify the base.
}
\source{
  The numbers were derived from the AFHSC data base
}
\references{

}
\examples{
data(population)
plot(population$npop,type="b")
}
\keyword{datasets}
