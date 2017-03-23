\name{setup.model}
\alias{setup.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set up all the parameters that define the model
}
\description{
	Define and set the parameters of an SIR compartmental model.  The function sets all the parameters and then selects the ones that will be optimized based on the User's selection of model number. Default values are set along with min/max values for all the parameters.  
}
\usage{
setup.model(imodel=ptab$model,dataType=dataType,epi=epi,npop=npop,Tg=ptab$Tg,national=national)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataType}{The type of data we are modeling: MPZ, CDC or GFT}
  \item{imodel}{
	Model code - options are 1 to 5 for MPZ data type and 4 or 5 for CDC/GFT/GFTPlus data type.
	imodel=1 R(t) = R0 x a specific-humidity term x a school schedule term
	imodel=2 R(t) = R0 x a specific-humidity term
	imodel=3 R(t) = R0 x a school schedule term
	imodel=4 R(t) = R0 a constant
	imodel=5 R(t) = R0 x [1+delta(ts,ts+dur)] R(t) changes at time ts (by R0 * delta )and then back to original value at ts+dur
}
  \item{epi}{
	Epi - the epi profile 
}
  \item{Tg}{
	The infectious period in days
}
  \item{npop}{
  	The effective population 
  }
  \item{national}{
  	For civilian modeling this can be TRUE or FALSE and is required for the step-size in the MCMC procedure
  }
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
  \item{imodel }{Model number}
  \item{Tg }{The infectious period in weeks}
  \item{vecnames }{Names of all the parameters that P-MEDDS supports}
  \item{vecopt }{Names of the parameters that will be optimized}
  \item{vec.zero }{parameters that are NOT optimized}
  \item{param.default }{Default values for the parameters}
  \item{par.min }{Minimum values of the parameters}
  \item{par.max }{Maximum values of the parameters}
  \item{dx }{step size for MCMC procedure}
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­like­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	model <- setup.model(imodel=ptab$model,dataType=dataType,epi=epi,npop=npop,Tg=ptab$Tg,national=national)

## The function is currently defined as
function (dataType="MPZ",imodel=5,epi=NULL,Tg=2.6,npop=1e5,national=FALSE) {
      
	
      vecnames <- c("R0min","deltaR","aparam","pC","Baseline","Tg","N","t0","alpha","delta","ts","dur")
      
      
      if (imodel == 1) {
            vecopt   <- c("R0min","deltaR","aparam","Baseline","pC","t0","alpha")
            veczero <- c("delta","ts","dur")
      } 
      if (imodel == 2) {
            vecopt   <- c("R0min","deltaR","aparam","Baseline","pC","t0")
            veczero <- c("alpha","delta","ts","dur")
      }
      if (imodel == 3) {
            vecopt   <- c("R0min","Baseline","pC","t0","alpha")
            veczero <- c("deltaR","aparam","delta","ts","dur")
      }
      if (imodel == 4) {
            vecopt <- c("R0min","Baseline","pC","t0")
            veczero <- c("deltaR","aparam","alpha","delta","ts","dur")
      }
      if (imodel == 5) {
            vecopt <- c("R0min","Baseline","pC","t0","delta","ts","dur")
            veczero <- c("deltaR","aparam","alpha")
      }
      
      
      #number of weeks
      nweeks = length(epi)
      # total number of parameters 
      nparam <- length(vecnames)	
      
      # Number of parameters that will be optimized
      nparam.opt <- length(vecopt)
      #days per week
      days.per.week <- 7
      Tg = Tg / days.per.week
      Baseline = mean(c(head(epi,5),tail(epi,5)))
            
      # default values for parameters they will be overwritten later and zeroed if using vec.zero above
      
      param.default <- c(R0min=1.4, deltaR=0.2,aparam=180.,pC=0.01, Baseline=Baseline,Tg=Tg,N=npop,t0=2,alpha=0.1,delta=-0.2,ts=12,dur=2)
      param.default[veczero] <- 0
      
      params_min <- c(R0min=0.5,deltaR=1e-6,aparam=1  ,pC=1e-6,Baseline=Baseline*1e-2  ,Tg=Tg,N=npop,t0=1.0        ,alpha=1e-6,delta=-1.0,ts=1.0,dur=1.)
      params_max <- c(R0min=4.0,deltaR=2.0 ,aparam=400,pC=1.0 ,Baseline=Baseline*10.0 , Tg=Tg,N=npop,t0=nweeks*0.7 ,alpha=1.0 ,delta=1.0 ,ts=40.0,dur=40.)
      
      xmin <- params_min[vecnames]
      xmax <- params_max[vecnames]
      # default step size
      dr=0.02
      if (dataType == "MPZ") dr=0.02
      if (dataType == "GFT" ) {
      	dr = 0.005
      	if (national == TRUE) dr=0.001
      }
            
      if (dataType == "CDC" | dataType == "GFTPlus") {
      	dr=0.005
      	if (national == TRUE) dr=0.001
      }

      dx <- rep(dr,length(vecnames)) #Since we move to the same scale for all parameters we use the same step size, but this size does depend on the data type
      
      list(imodel=imodel,Tg=Tg,vecnames=vecnames,vecopt=vecopt,vec.zero=veczero,param.default=param.default,par.min=params_min,par.max=params_max,par.dx=dx)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
