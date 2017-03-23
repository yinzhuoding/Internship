rm(list=ls())

DiffFiles = function(refFile,outFile) {
	
	command = paste("diff",refFile,outFile,sep=" ")

	SaveWarn = getOption("warn")
	options(warn=-1)
	test = system(command=command,intern=TRUE)
	#test = system(command=command,intern=FALSE)
	options(warn=SaveWarn)

	same = FALSE
	if(length(test)==0) same=TRUE
	#if(test==0) same=TRUE
	
	return(same)
}


test = TRUE
# test=TRUE: test output vs reference files
# test=FALSE: generate (and overwrite) reference files            

# check that PMEDDS can be loaded
flag = require(pmedds.core)
if (flag) {
	cat("Successfully loaded pmedds.core \n")
} else {
	stop("Cannot load PMEDDS. Possible causes: bad installation, installation location/R library path")
}

# check that we are in the \test directory
dir = getwd()
if (basename(dir)!="test") {
	stop("This script must be executed from the \test subdirectory of p-medds")
}

# check to see if "data" sub-directory exists, if not create it
subDir <- getwd()
subDir = paste(subDir,"/output",sep="")
      
if (!file.exists(subDir)) {
	dir.create(file.path(subDir))
} else {
	# clean \output directory
	system("rm -rf output/*")
}


# load sample dataset and check
wflag = 0
wweight = NULL 	#ex. rep(1,52) or runif(52,min=0,max=1)
dataType = "CDC"
myMPZ    = "23708"
region   = 4
national = FALSE
job.year = 2012
# load data
mydata = get.data(dataType=dataType, myMPZ=myMPZ, iregion=region, national=national, job.year=job.year, wweight=wweight, wflag=wflag)
filename = "Data_CDC_2012_reg4.RData"
if(!test) {
	save(mydata,file=paste0("reference/",filename))
	cat(paste0("Reference file reference/",filename," generated and saved.\n"))
} else {
	cat("Testing data loading: get.data().....  ")
	# compare binaries
	save(mydata,file=paste0("output/",filename))
	flag = DiffFiles(paste0("reference/",filename),paste0("output/",filename))
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		mydata2 = mydata
		load(paste0("reference/",filename))
		test1 = all(abs(mydata$cases-mydata2$cases) <= abs((mydata$cases+mydata2$cases)/2*1e-12))
		test2 = all(abs(mydata$gamaepi-mydata2$gamaepi) <= abs((mydata$gamaepi+mydata2$gamaepi)/2*1e-12))
		
		if (test1 & test2) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of get.data(CDC,2012,reg4) does not match reference file Data_CDC_2012_reg4.RData\n")
		}
	}
}


job.name = "test.install"
imodel <- 4
Tg <- 2.6
optTg = FALSE
RandInit = TRUE #TRUE
nMCMC <- 100
nlines <- 10
dr <- NULL 


reals <- 1
walkers <- 10
debug   = FALSE
verbose = FALSE
plot    = FALSE
device = "pdf" # "pdf" "png" "X11"
iseed = -123456 #NULL

# run MCMC optimization for CDC, 2012, reg4, model4
cat("Testing MCMC on sample CDC 2012 dataset.....  ")
ireal=1
fileout = filename <- paste0("output/mcmc-", mydata$dataName, "-", ireal, ".RData")
fileref = "reference/Sim_CDC_2012_reg4_mod4.RData"
temp = capture.output(out <- runPMEDDS(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr))
# out <- runPMEDDS(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr)
if(!test) {
	file.rename(from=fileout,to=fileref)
	#command = paste("mv",fileout,fileref,sep=" ")
	#system(command)
	cat(paste0("Reference file ",fileref," generated and saved.\n"))
} else {
	# compare binaries
	flag = DiffFiles(fileout,fileref)
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		load(fileref)
		output2 = output
		load(fileout)
		test1 = all(abs(output$mcmc-output2$mcmc) <= abs((output$mcmc+output2$mcmc)/2*1e-12))
				
		if (test1) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of get.data(CDC,2012,reg4) does not match reference file Data_CDC_2012_reg4.RData\n")
		}
	}
}


# run MCMC optimization for MPZ, 2009, 23708, model5
cat("Testing MCMC on sample MPZ 2009 dataset.....  ")
ireal = 1
dataType = "MPZ"
job.year = 2009
imodel = 5
mydata = get.data(dataType=dataType,job.year=job.year,myMPZ=myMPZ)
fileout = filename <- paste0("output/mcmc-", mydata$dataName, "-", ireal, ".RData")
fileref = "reference/Sim_MPZ_2009_23708_mod4.RData"
temp = capture.output(out <- runPMEDDS(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr))

if(!test) {
	file.rename(from=fileout,to=fileref)
	cat(paste0("Reference file ",fileref," generated and saved.\n"))
} else {
	# compare binaries
	flag = DiffFiles(fileout,fileref)
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		load(fileref)
		output2 = output
		load(fileout)
		test1 = all(abs(output$mcmc-output2$mcmc) <= abs((output$mcmc+output2$mcmc)/2*1e-12))
				
		if (test1) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of runPMEDDS(MPZ,2009,23708,model5) does not match reference file Sim_MPZ_2009_23708_mod4.RData\n")
		}
	}
}


# run MCMC optimization for GFT, 2005, national, model2
cat("Testing MCMC on sample GFT 2005 dataset.....  ")
ireal = 1
dataType = "GFT"
job.year = 2005
imodel = 2
national = TRUE
mydata = get.data(dataType=dataType,job.year=job.year,national=national, myMPZ=myMPZ)
fileout = filename <- paste0("output/mcmc-", mydata$dataName, "-", ireal, ".RData")
fileref = "reference/Sim_GFT_2005_nat_mod2.RData"
temp = capture.output(out <- runPMEDDS(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr))

if(!test) {
	file.rename(from=fileout,to=fileref)
	cat(paste0("Reference file ",fileref," generated and saved.\n"))
} else {
	# compare binaries
	flag = DiffFiles(fileout,fileref)
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		load(fileref)
		output2 = output
		load(fileout)
		test1 = all(abs(output$mcmc-output2$mcmc) <= abs((output$mcmc+output2$mcmc)/2*1e-12))
				
		if (test1) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of runPMEDDS(GFT,2005,national,model2) does not match reference file Sim_GFT_2005_nat_mod2.RData\n")
		}
	}
}


# run MCMC optimization for GFTPlus, 2008, region10, model1
cat("Testing MCMC on sample GFTPlus 2008 dataset.....  ")
ireal = 1
dataType = "GFTPlus"
job.year = 2008
imodel   = 1
national = FALSE
iregion  = 10
mydata = get.data(dataType=dataType,job.year=job.year,national=national, myMPZ=myMPZ, iregion=iregion)
fileout = filename <- paste0("output/mcmc-", mydata$dataName, "-", ireal, ".RData")
fileref = "reference/Sim_GFTPlus_2008_reg10_mod1.RData"
temp = capture.output(out <- runPMEDDS(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, reals=reals, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year, RandInit=RandInit, dr=dr))

if(!test) {
	file.rename(from=fileout,to=fileref)
	cat(paste0("Reference file ",fileref," generated and saved.\n"))
} else {
	# compare binaries
	flag = DiffFiles(fileout,fileref)
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		load(fileref)
		output2 = output
		load(fileout)
		test1 = all(abs(output$mcmc-output2$mcmc) <= abs((output$mcmc+output2$mcmc)/2*1e-12))
				
		if (test1) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of runPMEDDS(GFTPlus,2008,reg10,model1) does not match reference file Sim_GFTPlus_2008_reg10_mod1.RData\n")
		}
	}
}
	

# run MCMC optimization for CDC, 2012, region4, model4
cat("Testing EMCEE on sample CDC 2012 dataset.....  ")
ireal = 1
dataType = "CDC"
job.year = 2012
imodel   = 4
national = FALSE
iregion  = 4
mydata = get.data(dataType=dataType,job.year=job.year,national=national, myMPZ=myMPZ, iregion=iregion)
fileout = filename <- paste0("output/mcmc-", mydata$dataName, "-", ireal, ".RData")
fileref = "reference/EMCEE_CDC_2012_reg4_mod4.RData"
temp = capture.output(out <- runPMEDDS.EMCEE(dataType=dataType, mydata=mydata, imodel=imodel, Tg=Tg, optTg=optTg, nMCMC=nMCMC, nlines=nlines, walkers=walkers, iseed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, job.name=job.name, job.year=job.year))

if(!test) {
	file.rename(from=fileout,to=fileref)
	cat(paste0("Reference file ",fileref," generated and saved.\n"))
} else {
	# compare binaries
	flag = DiffFiles(fileout,fileref)
	if (flag) {
		cat("passed. Bitwise identical to reference file.\n")
	} else {
		# compare floating-point to 12 digits
		load(fileref)
		output2 = output
		load(fileout)
		test1 = all(abs(output$mcmc-output2$mcmc) <= abs((output$mcmc+output2$mcmc)/2*1e-12))
				
		if (test1) {
			cat("passed. 12-digit floating point identical to reference file.\n")
		} else {
			cat("failed. Result of runPMEDDS(GFTPlus,2008,reg10,model1) does not match reference file Sim_GFTPlus_2008_reg10_mod1.RData\n")
		}
	}
}	
	

# clean \output directory
system("rm -rf output/*")
