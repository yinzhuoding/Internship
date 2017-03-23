\name{setup.mcmc}
\alias{setup.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up all the parametrs for an MCMC chain
}
\description{
Sets up all the parametrs needed for an MCMC fitting procedure.  These include: the number of MCMC steps (nMCMC), the number of times the history of the chain will be saved (nlines <= nMCMC), how many chains to run (reals), the names of all the parameters P-MEDDS supports (vecnames), the names of the paremters that will be optimized (vecopt), the minimum values for all the parameters (vecmin), and the seed for the random number generator (seed).
}
\usage{
setup.mcmc(nMCMC=ptab$nMCMC,nlines=ptab$nlines,reals=ptab$reals,vecnames=model$vecnames,vecopt=model$vecopt,vecmin=model$par.min,seed=ptab$seed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nMCMC}{
  Number of MCMC steps in chain
}
  \item{nlines}{
  Number of lines to save to MCMC history file.
}
  \item{reals}{
  Number of MCMC chains to run 
}
  \item{vecnames}{
  Names of all parameters that P-MEDDS supports
}
  \item{vecopt}{
  Names of parameters that will be optimized
}
  \item{vecmin}{
  Minimum values for parameters
}
  \item{seed}{
  Seed for Random Number Generator
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{ithin }{How often to save the MCMC inforamtion}
    \item{iseed }{Seed for random number generator}
    \item{logbase }{Base for uniform-log sampling in MCMC procedure, set to 10}
    \item{logvec }{array of length nparam with 0 - use uniform sample or 1 - use uniform-log sampling}
    \item{iupdate }{0 or 1, for MCMC updates - one at a time or all at once }
    \item{imask }{an array with sub-list of the parameters that will be optimized}
    \item{nMCMC }{Number of MCMC steps in each chain}
    \item{reals }{Number of MCMC chains}
    \item{tab }{A large matrix that will hold the history of the MCMC chain}
    \item{accpet }{A place holder for the acceptance rate of the chain}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	mcmc  <- setup.mcmc(nMCMC=ptab$nMCMC,nlines=ptab$nlines,reals=ptab$reals,vecnames=model$vecnames,vecopt=model$vecopt,vecmin=model$par.min,seed=ptab$seed)

## The function is currently defined as
function(nMCMC=1e5,nlines = 1e4,reals=1,vecnames=NULL,vecopt=NULL,vecmin=NULL,seed=NULL) {
      
      ithin <- nMCMC / nlines
      #set seed for random number generator - if NULL we seed it here and will get a different sequence each time 
      
      set.seed(seed=seed)
      iseed <-runif(1) * 1e6
      iseed <- as.integer(iseed)
      
      nparam <- length(vecnames)
      nparam.opt <- length(vecopt)
      
      logbase = 10  #use log base 10 when needed 
      logvec <- rep(1,nparam) #1 use log uniform sampling , 0 use uniform sampling - must use this for any parameter that can be negative
      logvec[which(vecmin < 0)] <- 0
      
      iupdate <- 1  #in MCMC update all parameters at once ipudate > 0 or one at a time iupdate < 0
      imask  <- rep(-1,nparam)
      names(imask) <- vecnames
      imask[vecopt] <- 1        #vecopt holds the sub-list of what we want to optimize
      
      accept <- 0.0 # accept rate for MCMC procedure
      
      tab <- tab <- matrix(data=0.0,nr=nlines,nc=(nparam+1))
      
      list(ithin=ithin,iseed=iseed,logbase=logbase,logvec=logvec,iupdate=iupdate,imask=imask,nMCMC=nMCMC,reals=reals,tab=tab,accept=0.0)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
