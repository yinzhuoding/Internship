\name{setup}
\alias{setup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up the calculation
}
\description{
Setup calls the individual set-up routines for the model, the SIR PDEs, the MCMC fitting procedure and the initial guess for the parameters
}
\usage{
setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=mydata$national)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ptab}{
  A list that holds the model number, value for Tg, number of MCMC steps (nMCMC), number of lines in MCMC history file (nlines), number of MCMC chains (reals), seed for RNG (seed), debug, verbose, plot and device name
}
  \item{epi}{
  Weekly EPI data for modeling
}
  \item{dataType}{
  The data type: MPZ/CDC/GFT/GFTPlus
}
  \item{npop}{
  The effective population of military installation or civilian region (can be national)
}
  \item{national}{
  	For national modeling the step-size in the MCMC procedure may need to be smaller
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{setup }{A list of lists holdeing the model list, sim list, mcmc list and ini list}
}
\references{
Riley P, Ben­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­like­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	model <- setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=mydata$national)

## The function is currently defined as

function(ptab=NULL,epi=NULL,dataType="MPZ",npop=1e5,national=FALSE) {
      model <- setup.model(imodel=ptab$model,dataType=dataType,epi=epi,npop=npop,Tg=ptab$Tg,national=national)
      sim   <- setup.sim(nweeks=length(epi))
      mcmc  <- setup.mcmc(nMCMC=ptab$nMCMC,nlines=ptab$nlines,reals=ptab$reals,vecnames=model$vecnames,vecopt=model$vecopt,vecmin=model$par.min,seed=ptab$seed)
      ini   <- setup.ini(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)
      setup <- c(model,sim,mcmc,ini)
      
      return(setup)
}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
