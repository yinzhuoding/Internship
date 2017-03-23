\name{runPMEDDS}
\alias{runPMEDDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	The driver for running P-MEDDS on ILI data.  
}
\description{
runPMEDDS is the driver function for running pmedds.core on ILI data.  It is the function the user calls after loading the ILI (and SH/school) data and after setting all the parameters for the model and for the MCMC chains.  For specific examples see the README file and R scripts in the examples directory. 
}
\usage{
runPMEDDS(dataType="MPZ",mydata=mydata,imodel=5,Tg=2.6,nMCMC=1e6,nlines=1e4,reals=1,iseed=NULL,debug=FALSE,verbose=FALSE,plot=TRUE,device='pdf',job.name='test.pmedds',job.year=2009)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataType}{
  The data type: MPZ (Military), CDC, GFT, or GFTPlus
}
  \item{mydata}{
   A list that holds all the data (ILI, SH, School, population, year, zip code or region number)
}
  \item{imodel}{
   The specific model for R(t) chosen by the user 
}
  \item{Tg}{
  The infectious period, in days, set by the user
}
  \item{nMCMC}{
  The number of MCMC steps in each chain
}
  \item{nlines}{
  	The number of times information is saved for the MCMC history (for each chain nlines <= nMCMC)
  }
  \item{reals}{
  	The number of MCMC chains in this run
  }
  \item{iseed}{
  	The seed for the random number generator
  }
  \item{debug}{
  	set to TRUE or FALSE, controls the amount of information printed and plotted
  }
  \item{verbose}{
   set to TRUE or FALSE, controls the amount of information printed
}
  \item{plot}{
  	set to TRUE or FALSE, controls if plots are made.  Should always be set to true
  }
  \item{device.name}{
    device anme for plotting: X11, pdf or png
}
}
\details{
 See the README file in the 'examples ' directory for more details on all these parameters as well as the output of runPMEDDS
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
    \item{mcmclist }{A list of MCMC objects, the number of objects in the list is the number of MCMC chains}
    \item{model }{All the model parameters}
    \item{chain.param }{100 randomly selected sets of parameters from the best MCMC chain}
    \item{llk }{mean value of the LLK for each chain}

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
##--	out <- runPMEDDS(dataType="MPZ",mydata=mydata,imodel=5,Tg=2.6,nMCMC=1e6,nlines=1e4,reals=1,iseed=NULL,debug=FALSE,verbose=FALSE,plot=TRUE,device='pdf',job.name='test.pmedds',job.year=2009)

## The function is currently defined as
function(dataType="MPZ",mydata=mydata,imodel=5,Tg=2.6,nMCMC=1e6,nlines=1e4,reals=1,iseed=NULL,debug=FALSE,verbose=FALSE,plot=TRUE,device="X11",job.name="",job.year=2009) {
      
      # start the clock
      start.time <- proc.time()
      # open a log file
      logfile <- file(description="log.txt",open="wt")
      
      # pack all the parameters to a list
      ptab <- list(model=imodel,Tg=Tg,nMCMC=nMCMC,nlines=nlines,reals=reals,seed=iseed,debug=debug,verbose=verbose,plot=plot,device=device)

      
      require("coda")

      # first step is to read the data in
      	  
      iverbose <- 0
      if (ptab$verbose == TRUE) iverbose=1
      
            
      # set up all the parameters for the model and chains using the information given by the user
      model <- setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=mydata$national)

      success <- print.info(dataType=dataType,ptab=ptab,model=model,mydata=mydata,job.name=job.name,job.year=job.year,subDir="output",log=logfile)

      # In case of MPZ data plot the base EPI profile along with SH and school data 

       if (dataType == "MPZ") plot.base(myName=mydata$dataName,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile)
 
      # set up a matrix for time series of the results

      dsdt <- matrix(0.0,nr=mydata$nweeks,nc=model$reals)	
      rmat <- matrix(0.0,nr=mydata$nweeks,nc=model$reals)	
      
      # prepare a list where the MCMC objects will reside
      mcmclist=list()
      mean.llk=rep(0,model$reals)
      mcmc.param=list()

      for (ix in 1: model$reals) {
            
            cat("\n Running MCMC Chain Number ",ix,"\n\n")
            text <- paste(" Running MCMC Chain Number ",ix,"\n",sep="")
            writeLines(text=text,con=logfile)
            
            model$epi = mydata$cases
            model$base_ps <- setup.ini(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps

            # calculate initial guess there is no real need for this 
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),param=as.double(model$base_ps),nparam = as.integer(length(model$base_ps)),pois=as.double(model$pois),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec))

            # # Run the chain with Fortran code

            sol <-.Fortran("mcmc",data=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),nweeks=as.integer(mydata$nweeks),nparam=as.integer(length(model$base_ps)),dt=as.double(model$dt), nstep=as.integer(model$nstep),nsamps=as.integer(model$nMCMC),logbase=as.integer(model$logbase),pmax=as.double(model$par.max),pmin=as.double(model$par.min),ilog=as.integer(model$logvec),step=as.double(model$par.dx),imask=as.integer(model$imask),pval=as.double(model$base_ps),iupdate=as.integer(model$iupdate),ithin=as.integer(model$ithin),iseed=as.integer(model$iseed * ix),solbest=as.double(model$dsdt),rvec=as.double(model$rvec),tab=as.double(model$tab),iverbose=as.integer(iverbose),accept=model$accept)

            ## keep track of the best profile for this chain
            model$dsdt = dsdt[,ix] <- sol$solbest
            model$rvec = rmat[,ix] <- sol$rvec	
            ## call the routine that will write an R object data file for this chain and report statistics on chain
            ## the statistics is currently just printed to the screen

            mcmc.write.out <- mcmc.write(tab=sol$tab,model=model,ireal=ix,nweeks=mydata$nweeks,myName=mydata$dataName,accept=sol$accept,log=logfile)
            mcmc.out       <- mcmc.write.out$mcmc

            ## write mcmc also randomly selects 100 values from each chain - this is needed for plotting at the end	
            
            mcmc.param[[ix]] <- mcmc.write.out$chain.param
            
            ## put the mcmc chain in the proper place in the MCMC list
            mcmclist[[ix]] <- mcmc.out
            
            ## record the mean value of -LLK of this chain
            mean.llk[ix] <- mcmc.write.out$mean.llk

            # plot the chain parameter history to a file 

            chain.plot <- plot.mcmc.chain(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile)

      }

      # select the best chain
      
      iBest <- which.min(mean.llk)
      myparam <- mcmc.param[[iBest]]
      nRnd <- dim(myparam)[1]
      nparam <- dim(myparam)[2]
      rnd.dsdt <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      rnd.rvec <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      # send the randomly selected set of parameters from teh best chain to a routine that will calculate the profiles
      
      for (i in 1:dim(myparam)[1]) {
            
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),param=as.double(myparam[i,]),nparam = as.integer(length(model$base_ps)),pois=as.double(model$pois),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec))
            
            rnd.dsdt[,i] <- out$dsdt
            rnd.rvec[,i] <- out$rvec

      }

      
      # all the results are plotted together
      # we may want to add here plotting of say 100 profiles using random sets of values from the chains
      if (ptab$plot == TRUE) {
            plot.results.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,device.name=ptab$device,log=logfile)
            plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt,device.name=ptab$device,log=logfile)
      }

      # stop the clock	
      cat("\n\nElapsed Time: \n\n")
      text <- paste(" Elapsed Time: ",sep="\n")
      writeLines(text=text,con=logfile)
      end.time = proc.time()
      run.time = end.time-start.time
      text.label = "user    system   elapsed"
      text.time  = paste(run.time[[1]],run.time[[2]],run.time[[3]],sep="   ")
      print(end.time - start.time)
      
      writeLines(text=c(text.label,text.time),con=logfile,sep="\n")
      close(logfile)
      
      # Need to think if there is anything else we want to return
      list(mcmc=mcmclist,model=model,chain.param=chain.param,llk=mean.llk)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
