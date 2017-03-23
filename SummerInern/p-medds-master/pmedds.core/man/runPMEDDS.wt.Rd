\name{runPMEDDS.wt}
\alias{runPMEDDS.wt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	The driver for running P-MEDDS on the SARS 2003 data using the Wallinga-Teunis procedure
	}
\description{
runPMEDDS.wt is the driver function for running pmedds.core on the 2003 SARS data.  It is the function the user calls after loading the SARS data and after setting all the parameters for the W-T procedure.  For a specific example see the R script example.wt.R in the examples directory. 
}
\usage{
runPMEDDS.WT(mydata=mydata,ireal=1e4,shape=3.8,scale=8.4,iseed=12398,debug=FALSE,verbose=FALSE,device="pdf",job.name='wt.pmedds')}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
   A list that holds all the data for the run
  }
  \item{ireal}{
  	The number of realizations for R(t). 10,000 will suffice for converged results
  }
  \item{shape}{
  	The shape parameter for a Weibull distribution, default is 3.8 days
  }
  \item{scale}{
  	The scale parameter for a Weibull distribution, default is 8.4
  }
  \item{iseed}{
  	seed for Random Number Generator
  }
  \item{debug}{
  	TRUE/FALSE controls the amount of printing
  }
  \item{verbose}{
   set to TRUE or FALSE, controls the amount of information printed when debug is set to TRUE
}
  \item{plot}{
  	set to TRUE or FALSE, controls if plots are made.  Should always be set to true
  }
  \item{device.name}{
    device name for plotting: X11, pdf or png
}
  \item{job.name}{
  	Name for job
  }
}
\details{
 See the README file and example.wt.R script in the 'examples' directory for more details on all these parameters as well as the output of runPMEDDS.wt
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{mydata }{The data/inforamtion we are modeling}
    \item{Rl }{A large matrix with the 10,000 realizations of R(t)}
    \item{cases.order }{ordered list with the onset/report day and the number of cases on that day}
    \item{Rlq.order }{daily 95precentile CI value of R(t)}
    \item{Rlm.order }{daily mean value of R(t)}

%% ...
}
\references{
Wallinga J, Teunis P (2004) Different Epidemic Curves for Severe Acute Respiratory Syndrome Reveal Similar Impacts of Control Measures. Am J Epidemiol 160: 509–516.

http://en.wikipedia.org/wiki/Weibull_distribution
}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##-- out <-runPMEDDS.WT(mydata=mydata,ireal=1e4,shape=3.8,scale=8.4,iseed=12398,debug=FALSE,verbose=FALSE,device="pdf",job.name='wt.pmedds')

## The function is currently defined as
function(mydata=mydata,shape=3.8,scale=8.4,ireal=1e4,iseed=NULL,debug=FALSE,verbose=FALSE,device="pdf",job.name="") {

      # start the clock
      start.time <- proc.time()
      # open a log file
      logfile <- file(description="log-wt.txt",open="wt")
      
      
	  #set seed for random number generator - if NULL we will get a different sequence each time 
      
      set.seed(seed=iseed)
      iseed <-runif(1) * 1e6
      iseed <- as.integer(iseed)

	  ncases = mydata$ncases
	  times  = mydata$times
	  case_number = mydata$case_number
	  
	  subDir="output"
	
      # Print some information to the screen and to a text file 
      success <- print.wt.info(mydata=mydata,shape=shape,scale=scale,ireal=ireal,subDir=subDir,log=logfile)

	  # calculate the ncases x ncases matrix of relative likelihhod that case k has
	  # been infected by case l
	  pkl.mat = matrix(0.0,nrow=mydata$ncases,ncol=mydata$ncases)

	      
# the keyword 'norm' tells the weibull routine if it should normalize the matrix (norm=0) or not (norm!=0)
     out <- .Fortran("weibull",ncases=as.integer(ncases),pkl.mat=as.double(pkl.mat),times=as.double(times),shape=as.double(shape),scale=as.double(scale),norm=as.integer(0))
     
      pkl.mat = matrix(out$pkl.mat,nc=ncases)


	  Rl.expect <- rep(0.0,ncases)
	  for (l in 1:ncases){
		for (k in 1:ncases){
			Rl.expect[l] = Rl.expect[l]+pkl.mat[k,l]	
		}

	  }
	  
	  
	## This is what takes most of the time.  Need to see if there is a way to improve it. 
	# sample from a Bernoulli distribution with 
	nwrtf = 1000   # How often to print progress to the screen
	
	Rl <- matrix(0.0,nrow=ncases,ncol=ireal) #create a matrix to store the statistics

	# Here call the Fortran subroutine
	iverbose = 0
	if (verbose == TRUE) iverbose=1
	out <- .Fortran("binom",ireal=as.integer(ireal),ncases=as.integer(ncases),nwrtf=as.integer(nwrtf),pkl.mat=as.double(pkl.mat),Rl=as.numeric(Rl),iseed=as.integer(iseed),iverbose=as.integer(iverbose))
	
	# This holds all the realizations 
	
	Rl <- matrix(out$Rl,nc=ireal)

	# calculate some statistics on Rl
	
	probs=0.95
	stats <- wt.calc.stats(Rl=Rl,times=times,probs=probs)
	
	unique.t = stats$unique.t
	onset.n  = stats$onset.n
	Rl.qntl  = stats$Rl.qntl
	Rl.mean  = stats$Rl.mean
	Rl.median = stats$Rl.median
	
	nrow = length(case_number)
	import <- rep.int(0,nrow)
	weibull.par <- c(scale,shape)
	
	#Point estimate of Rt
	
	Xneg <-wallinga1_import(times = times,import = import,weibull.par = weibull.par)

	# R(t) - is the point estimate for Rt
	Rt <- Xneg / onset.n

	# Order the onset day and R(t) before plotting it

	o <-order(unique.t)
	Rt.order   <- rbind(unique.t[o],Rt[o])
	Rlq.order  <-rbind(unique.t[o],Rl.qntl[o])
	Rlm.order  <-rbind(unique.t[o],Rl.mean[o])
	Rlmd.order <-rbind(unique.t[o],Rl.median[o])
	cases.order<- rbind(unique.t[o],onset.n[o])

	#create a unique day vector for 2003
	dates.vec <- seq(as.Date("2003-01-01"),by="days",length=365)
	
	#Write an R object Data file with everything in it
	
	success <- wt.write.data(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,ireal=ireal,log=logfile,subDir=subDir)
	
	
	#write out the Results and realizations to csv files
	success <- wt.write.results(mycountry=mycountry,cases.order=cases.order,Rl=Rl,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,log=logfile,subDir=subDir)
	
	#plot the results
	success <- wt.plot.results(mycountry=mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir)
	
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
      
      list(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order)
	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
