rm(list=ls())
#detach("package:pmedds.core",unload=TRUE)
require(pmedds.core)


PrintError = function(dataType=NULL,job.year=NULL,wflag=NULL,wweight=NULL, myMPZ=NULL, iregion=NULL, national=NULL, job.name=NULL, imodel=NULL, Tg=NULL, optTg=NULL, RandInit=NULL, nMCMC=NULL, nlines=NULL, dr=NULL, reals=NULL, walkers=NULL, iseed=NULL, debug=NULL, verbose=NULL, plot=NULL, device=NULL, MCMC=NULL) {
	
	
}


testGetData = function(dataType="MPZ",myMPZ="23708",iregion=1,national=FALSE, job.year=2009, wweight=NULL, wflag=0, week.start=NULL, week.end=NULL, year.end=NULL) {
	stdout = vector('character')
	output = textConnection('stdout','wr',local=TRUE)
	sink(output)
	
	mydata = try(get.data(dataType=dataType,myMPZ=myMPZ,iregion=iregion, national=national, job.year=job.year, wflag=wflag, wweight=wweight))
	
	sink()
	close(output)
	
	return(list(stdout=stdout,mydata=mydata))
}

testMCMC = function(dataType="MPZ",mydata=mydata,imodel=5, Tg=2.6, optTg=NULL, nMCMC=1e6, nlines=1e4, reals=1, iseed=NULL, debug=FALSE, verbose=FALSE, plot=TRUE, device="pdf", job.name="test.pmedds", job.year=2009, RandInit=TRUE, dr=NULL) {
	
	stdout = vector('character')
	output = textConnection('stdout','wr',local=TRUE)
	sink(output)

	test = try(runPMEDDS(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr))

	sink()
	close(output)
	
	return(list(stdout=stdout,test=test))	
}

testEMCEE = function(dataType="MPZ",mydata=mydata,imodel=5, Tg=2.6, optTg=NULL, nMCMC=1e6, nlines=1e4, walkers=5, iseed=NULL, debug=FALSE, verbose=FALSE, plot=TRUE, device="pdf", job.name="test.pmedds", job.year=2009) {
	
	stdout = vector('character')
	output = textConnection('stdout','wr',local=TRUE)
	sink(output)

	test = try(runPMEDDS.EMCEE(dataType=dataType,mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year))

	sink()
	close(output)
	
	return(list(stdout=stdout,test=test))	
}

TestFunction = function() {
	# Set default values				# No. of options
	wflag    = 0						# 3
	wweight  = NULL 				
	dataType = "CDC"					# 4
	myMPZ    = "23708"					# 50
	region   = 4						# 10
	national = FALSE 					# 2
	job.year = 2012 					# depends on dataType
	
	job.name = "test.changes"
	imodel   = 4 						# 5
	Tg       = 2.6
	optTg    = FALSE 					# 2
	RandInit = TRUE #TRUE				# 2
	nMCMC    = 5
	nlines   = 2
	dr       = NULL 					# 2
	reals    = 1						# 2
	walkers  = 4
	iseed    = 123456 #NULL
	
	debug   = FALSE
	verbose = FALSE
	plot    = FALSE
	device  = "pdf" # "pdf" "png" "X11"
	MCMC    = TRUE 						# 2

	zipnames = get.mpz.names()
	# loop over data types
	for (dataType in c('MPZ','CDC','GFT','GFTPlus')) {
		if (dataType=='MPZ') {
			years   = 2009
			regions = 1:length(zipnames)
			cat("\nTesting all MPZ data/model combinations. This will take a minute....... ")
		} else if(dataType=='CDC') {
			years   = 2000:2013
			regions = 1:11
			cat("Testing all CDC data/model combinations.  This takes approximately 5 minutes....... ")
		} else {
			years   = 2004:2013
			regions = 1:11
			cat("Testing all",dataType,"data/model combinations. This takes approximately 5 minutes....... ")
		}
		
		# loop over data years
		for (job.year in years) {
			# loop over region/national
			for (region in regions) {
				if (dataType == 'MPZ') {
					myMPZ = zipnames[region]
				} else {
					if (region<11) {
						national = FALSE
					} else national = TRUE
				}
				
				# Test get.data() function
				test = testGetData(dataType=dataType,myMPZ=myMPZ,iregion=region, national=national, job.year=job.year, wflag=wflag, wweight=wweight)
			
				if(is.character(test$mydata)) {
					# print error msg
					errmsg = test$mydata
					cat("An error occured in execution of get.data() for the following parameters: \n")
					dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
					numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=MCMC)
					ParList = list(dataPar=dataPar,MCMCpar=numericPar)
					str(ParList)
					# stop loop execution
					return(list(stdout=test$stdout,errmsg=test$mydata, ParList=ParList))
				} else {
					mydata = test$mydata
				}
				
				# Loop over models
				for (imodel in 1:5) {
					# Test runPMEDDS() function
					test1 = testMCMC(dataType=dataType,mydata=mydata,imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr)
				
					if (is.character(test1$test)) {
						cat("Error occurred in execution of runPMEDDS() for the following parameters: \n")
						dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
						numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=TRUE)
						ParList = list(dataPar=dataPar,MCMCpar=numericPar)
						str(ParList)
						return(list(stdout=test1$stdout,errmsg=test1$test, ParList=ParList))		
					}
					
					# Test runPMEDDS.EMCEE() function
					test2 = testEMCEE(dataType=dataType,mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year)
				
					if (is.character(test2$test)) {
						cat("Error occurred in execution of runPMEDDS.EMCEE() for the following parameters: \n")
						dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
						numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=FALSE)
						ParList = list(dataPar=dataPar,MCMCpar=numericPar)
						str(ParList)
						return(list(stdout=test2$stdout,errmsg=test2$test, ParList=ParList))
					}
				}
			}
		}
		cat("no errors found \n")
		system("rm -rf ./output/*")
	}
	
	# do quick test of other MCMC options
	models   = 1:5
	datTypes = c('MPZ','CDC','MPZ','GFT','GFT')
	years    = c(2009,2009,2009,2008,2012)
	natVec   = c(TRUE,TRUE,TRUE,FALSE,FALSE)
	MPZvec   = c(23665,23665,22134,22134,22134)
	regions  = c(1,1,1,7,3)
	
	NumOpts = 11
	wflags  = rep(0,NumOpts); wflags[1]=1; wflags[2]=3
	wweight = runif(66)
	optTgs  = rep(FALSE,NumOpts); optTgs[3]=TRUE
	RandInits = rep(TRUE,NumOpts); RandInits[4]=FALSE
	drs		= rep(NA,NumOpts); drs[5] = 0.05
	realvec = rep(1,NumOpts); realvec[6] = 4
	seeds   = rep(123456,NumOpts); seeds[7] = NA
	debugs  = rep(FALSE,NumOpts); debugs[8] = TRUE
	verbosi = rep(FALSE,NumOpts); verbosi[9] = TRUE
	plots	= rep(FALSE,NumOpts); plots[10] = TRUE; plots[11] = TRUE
	devici  = rep('pdf',NumOpts); devici[11] = 'png'
	
	cat("Testing other auxiliary PMEDDS functions....... ")
	for (imodel in models) {
		dataType = datTypes[imodel]
		job.year = years[imodel]
		national = natVec[imodel]
		myMPZ    = MPZvec[imodel]
		region   = regions[imodel]
		
		for (ii in 1:NumOpts) {
			wflag 	 = wflags[ii]
			optTg 	 = optTgs[ii]
			RandInit = RandInits[ii]
			dr 		   = drs[ii];	if (is.na(dr)) dr=NULL
			reals 	 = realvec[ii]
			iseed 	 = seeds[ii]; if (is.na(iseed)) iseed=NULL			
			debug    = debugs[ii]
			verbose  = verbosi[ii]
			plot 	   = plots[ii]
			device	 = devici[ii]			
			
			# Test get.data() function
			test = testGetData(dataType=dataType,myMPZ=myMPZ,iregion=region, national=national, job.year=job.year, wflag=wflag, wweight=wweight)
		
			if(is.character(test$mydata)) {
				# print error msg
				errmsg = test$mydata
				cat("An error occured in execution of get.data() for the following parameters: \n")
				dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
				numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=MCMC)
				ParList = list(dataPar=dataPar,MCMCpar=numericPar)
				str(ParList)
				# stop loop execution
				return(list(stdout=test$stdout,errmsg=test$mydata, ParList=ParList))
			} else {
				mydata = test$mydata
			}
			
			# Test runPMEDDS() function
			test1 = testMCMC(dataType=dataType,mydata=mydata,imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr)
		
			if (is.character(test1$test)) {
				cat("Error occurred in execution of runPMEDDS() for the following parameters: \n")
				dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
				numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=TRUE)
				ParList = list(dataPar=dataPar,MCMCpar=numericPar)
				str(ParList)
				return(list(stdout=test1$stdout,errmsg=test1$test, ParList=ParList))		
			}
			
			# Test runPMEDDS.EMCEE() function
			test2 = testEMCEE(dataType=dataType,mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year)
		
			if (is.character(test2$test)) {
				cat("Error occurred in execution of runPMEDDS.EMCEE() for the following parameters: \n")
				dataPar = list(job.name=job.name, dataType=dataType, job.year=job.year, myMPZ=myMPZ, national=national, iregion=region, wflag=wflag, wweight=wweight)
				numericPar = list(imodel=imodel, Tg=Tg, optTg=optTg, RandInit=RandInit, nMCMC=nMCMC, nlines=nlines, dr=dr, reals=reals, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, MCMC=FALSE)
				ParList = list(dataPar=dataPar,MCMCpar=numericPar)
				str(ParList)
				return(list(stdout=test2$stdout,errmsg=test2$test, ParList=ParList))
			}
					
		}
	}
	cat("no errors found \n\n")
	cat("Test complete. \n\n")
	system("rm -rf ./output/*")
}





#----- Main Script -------------------------

start.time <- proc.time()

output = TestFunction()

# stop the clock	
      cat("\n\nElapsed Time: \n\n")
      end.time = proc.time()
      run.time = end.time-start.time
      text.label = "user    system   elapsed"
      text.time  = paste(run.time[[1]],run.time[[2]],run.time[[3]],sep="   ")
      print(end.time - start.time)

#print("Erasing all test files:")
#system("find ./output/* -maxdepth 1 -delete")

#print("post-test operation")
