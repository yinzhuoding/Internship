\name{setup.ini.halfwidth}
\alias{setup.ini.halfwidth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up initial conditions for model parameters
}
\description{
	Use an R0-fitting procedure to approximate initial values for R0, pC, t0, and Baseline. 
}
\usage{
out = setup.ini.halfwidth(mydata=NULL,model=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
    Data structure returned by get.data() containing flu season details.
  }
  \item{model}{
    Data structure returned by setup() containing model and optimization parameters
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{out}{'model'-data structure with initial parameter values updated to the appropriate approximations.}
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
 ##-- library(pmedds.core)
 ##-- mydata = get.data(dataType="CDC",national=TRUE,job.year=2007)
 ##-- ptab = list(model=4, Tg=2.6, nMCMC=1E4, nlines=1E2, reals=1, seed=NULL, debug=FALSE, verbose=TRUE, plot=TRUE, device="pdf", optTg=FALSE)
 ##-- model = setup(ptab=ptab,epi=mydata$cases,dataType="CDC",npop=mydata$npop,national=mydata$national, dr=NULL)
 ##-- model2 <- setup.ini(mydata=mydata,model=model)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
