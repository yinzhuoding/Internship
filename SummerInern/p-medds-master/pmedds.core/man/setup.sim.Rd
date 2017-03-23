\name{setup.sim}
\alias{setup.sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up all required paramaters for the integration of the PDEs for a compartmental SIR model. 
}
\description{
Set parameters for the integration of the SIR equations: the number of steps per week, the time step, arrays for the incidence profile and for the R(t) profile
}
\usage{
sim   <- setup.sim(nweeks=length(epi))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nweeks}{
   The number of EPI weeks.  66 for the pandemic year and 52 (or 53) for all other years
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
A list with the following items:
    \item{nstep }{Number of time steps per week}
    \item{dt }{Time step (weeks^-1), set to 1/nstep }
    \item{dsdt }{array of length nweeks, will hold an incidence profile}
    \item{rvec }{array of length nweeks, will hole the profile of R(t)}
    \item{pois }{place holder for the maximum likelihood value}
%% ...
}
\references{
	Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science, Inc.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	sim   <- setup.sim(nweeks=length(epi))

## The function is currently defined as
setup.sim <- function(nweeks=66) {

      # number of steps per week and time step
      nstep <-  50
      dt  <- 1./nstep
      dsdt <- rep(0.0,nweeks) 
      rvec <- rep(0.0,nweeks) 
      pois <- 0.0
      list(nstep=nstep,dt=dt,dsdt=dsdt,rvec=rvec,pois=pois)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
