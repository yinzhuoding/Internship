\name{mcmc.boxplot}
\alias{mcmc.boxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Post-processing of MCMC data.  
}
\description{
Post MCMC optimization, consolidates 'R0' and 'pC' values to the form utilized for box-plots in plot.results.mcmc().   
}
\usage{
mcmc.boxplot(tab=NULL,model=NULL,mydata=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
  A vector that contains the value of all parameters for each saved step from the MCMC chain.
  }
  \item{model}{
  A list/data structure containing all MCMC and model parameters. see setup().
  }
  \item{mydata}{
  A list/data structure containing all input data for the flu season. see get.data()
  }
  
}

\value{
%%  ~Describe the value returned
    No return value
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.}
\author{
Predictive Science Inc.
}

\examples{
## This function is specific to results returned during the execution of runPMEDDS() and is not intended for general use

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
