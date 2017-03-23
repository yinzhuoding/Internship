\name{get.mpz.names}
\alias{get.mpz.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the list of Military Zip codes for Top-50 bases
}
\description{
Uses the column names in the ILI.small.pandemic.curves.by.zip5.20090320.20100628 data file to retrieve the zip codes of the top 50 bases during the 2009-2010 pandemic year.
}
\usage{
get.mpz.names()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  No need for any arguments}
\value{
An array, zipnames, with the zip codes
%% ...
}
\references{
Data provided by AHFSC
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- zipnames = get.mpz.names()

## The function is currently defined as
get.mpz.names <- function() {
      
      data(ILI.small.pandemic.curves.by.zip5.20090320.20100628,package="pmedds.core")
      raw_ili <- ILI.small.pandemic.curves.by.zip5.20090320.20100628
      
      epi      <- sapply(raw_ili[,3:dim(raw_ili)[2]], as.ts) 
      
      # get the 5-digit MPZ and remove the "X" from it
      zipnames <- colnames(epi)
      zipnames <- substr(zipnames,start=2,stop=10)
	  zipnames
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
