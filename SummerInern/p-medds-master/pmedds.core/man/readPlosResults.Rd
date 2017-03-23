\name{readPlosResults}
\alias{readPlosResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads the Plos Comput Biol results for the ILI profiles and the two-valued R(t) for the 2009-2010 Military data set
}
\description{
	 Reads the Plos Comput Biol results for the ILI profiles and the two-valued R(t) for the 2009-2010 Military data set

}
\usage{
readPlosResults()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
  \item{plos.ili }{The Plos Comput Biol ILI results}
  \item{plos.roft}{ The Plos Comput Biol results for the two-valued R(t) model}
  \item{zipname }{zip codes of all the military bases in the file}
%% ...
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
\note{
%%  ~~further notes~~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- epi_data = readEpi

## The function is currently defined as
function() {
      

      data(PlosCompBio.ILI.small.pandemic.profiles,package="pmedds.core")
      raw_prfl <- PlosCompBio.ILI.small.pandemic.profiles
      
      #we now extract Plos profiles for the 50 bases 
      #removing the columns of idex, week number and date - these are the same
      #as the ILI profiles      
      plos.ili  <- sapply(raw_prfl[,4:dim(raw_prfl)[2]], as.ts) 

      # get the 5-digit MPZ and remove the "X" from it
      zipname <- colnames(plos.ili)
      zipname <- substr(zipname,start=2,stop=10)


      # repeat for R(t)
      data(PlosCompBio.ILI.small.pandemic.roft,package="pmedds.core")
      raw_roft <- PlosCompBio.ILI.small.pandemic.roft   
      plos.roft  <- sapply(raw_roft[,4:dim(raw_roft)[2]], as.ts)

      colnames(plos.ili ) <- zipname
      colnames(plos.roft) <- zipname
            
      list(plos.ili=plos.ili, plos.roft=plos.roft, plos.zipname = zipname)      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
