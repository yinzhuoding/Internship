\name{setup.ini.rand}
\alias{setup.ini.rand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up initial conditions for model parameters
}
\description{
	Set up the initial guess for all the parameters that are being optimized and default values for the ones that are fixed.  Certain default values depend on the model the user chooses.
	}
\usage{
setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min}{
  Minimum values for parameters
}
  \item{max}{
  Maximum values for parameters
}
  \item{default}{
  Default values for parameters
}
  \item{vecopt}{
  Names of parameters that will be optimized
}
  \item{vecnames}{
  Names of all parameters that the package knows
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{base_ps }{Initial values for all the parameters}
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	model$base_ps <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps

## The function is currently defined as
function(min=NULL,max=NULL,default=NULL,vecopt=NULL,vecnames=NULL) {
      nparam <- length(vecnames)
      base_ps <- default
      for (i in 1:nparam) {	      
            if (vecnames[i] %in% vecopt) base_ps[i] <- runif(1,min=min[i],max=max[i])
            if (vecnames[i] %in% vecopt[1]) base_ps[i] <- runif(1,min=1.2,max=2)
            if (vecnames[i] == 'pC') base_ps[i] <- runif(1,min=0.001,max=0.1)
      }
      
      
      list(base_ps=base_ps)
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
