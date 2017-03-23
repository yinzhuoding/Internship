\name{readPopulation}
\alias{readPopulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads the population file from package pmedds.core
}
\description{
Reads the effective population of the top ~100 military bases.  This is the denominator data.  The population is scaled by 'omega' which is an empirical factor that scales the effective population to a true estimate for the military base population. 
}
\usage{
readPopulation(omega = 104.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{omega}{
Omega scales the effective population to our estimate of the actual population of the military bases.  The same scaling factor is used for all the bases
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
  \item{pop }{An array with the denominator information (population) of each base}
  \item{zipname }{The zip code of each of the bases}
  \item{nbase }{The number of bases in the file}
%% ...
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	pop_data = readPopulation(omega=104.3)

## The function is currently defined as
readPopulation <- function(omega=104.3) {
      data(population, package="pmedds.core")
      pop <- population$npop * omega
      zipname <- population$zip
      nbase <- length(zipname)
      list(pop=pop,zipname=zipname,nbase=nbase)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
