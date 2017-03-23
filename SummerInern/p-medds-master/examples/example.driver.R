##
## SAMPLE EXAMPLE DRIVER FOR P-MEDDS PACKAGE - USING A COMPARTMENTAL SIR MODEL AND ILI DATA OF EITHER MILITARY (MPZ), CDC OR GFT DATA
##

##
#clean all previous data and objects
rm(list=ls())

##
## turn all previous graphics windows off
graphics.off()

##
## Load the package
require(pmedds.core)

##
## If you would like to see what data comes with the pmedds.core package uncomment the following line
## data(package="pmedds.core")

##
## Give the job  a name

job.name="example.pmedds"

##
## Choose what type of data to model, options are MPZ (for Military) CDC, GFT, GFTPlus. Default is MPZ

dataType = "CDC"  # "MPZ" "CDC" "GFT" "GFTPlus"

##
## Year for modeling- start of season
## for Military modeling we can only model 2009 
## for CDC we have 2000- to date
## and GFT/GFTPlus 2004-2014 

job.year=2015

##
## Following parameter is specific for MPZ modeling.  It is ignored (but needs to be set to some value) in case of civilian data
 
## To see the list of all 50 Military zip codes uncomment the following two lines
zipnames = get.mpz.names()
#print(zipnames)

##
## Default is to use the first MPZ
myMPZ = zipnames[1] #"23708"

##
## Following two parameters are relevant only for civilian modeling (CDC or GFT/GFTPlus) 
## choose the region number (between 1 to 1) or set national=TRUE to model the entire US
## If national = TRUE region number is ignored 

iregion=1 #options are 1 to 10, ignored in case of national modeling
national = FALSE  #set to TRUE or FALSE

## 
## Choose how many weeks of data to fit.  Default is to fit all the available data for the entire season
## The code will do this if nweeksFit = 0 
## The minimum number of weeks to fit is ten and the maximum is the entire season 
## (which is 52 or 53 for all years except for the pandemic year where it is 66)
## If the user sets it outside this range it will be reset by the code either to the minimum (if too low)
## or the maximum (if too high)
## if it is set to zero, it will be reset to the entire season
##

nweeksFit = 0

##
## choose the model - default is paper 2 which is model 5.  For civilian modeling only models  2, 4 and 5 work due to lack of School data
##
# Model options:
# 1 - R(t) is R0 times a school and specific humidity terms
# 2 - R(t) is R0 times a specific humidity term
# 3 - R(t) is R0 times a school schedule term
# 4 - R(t)==R0 fixed in time
# 5 - Paper 2 model
	                     
imodel <- 5

##
## set the infectious period, Tg in days. Code will convert to weeks. Default is 2.6 days

Tg <- 2.6

##
## If you would like to Optimize Tg set optTg = TRUE. Default is False.
## Note that it is numerically unstable to optimize both Tg and R0
##

optTg = FALSE

##
## Set how initial parameter values are generated: TRUE - random, FALSE - automatically uses mydata$cases to approximate R0,pC,t0,Baseline. Approximated initial parameter values cause the MCMC chain to converge to a reasonable solution quickly. It is more robust to use random initial values, long chains, and many realizations. When a short/demo run is desired, set to RandInit=FALSE, set nMCMC=1e4, and set reals=1
RandInit = TRUE

##
## Set the number of MCMC steps in each chain. Default is 1e6 which will give quick reasonable results but these are likely not going to be converged MCMC chains
## For a converged calculation use 1e7 step and keep nlines at 1e4

nMCMC <- 1e6

##
##Set the number of times to save the MCMC chain parameters, must be equal or smaller than nMCMC. Default is 1e4,but for shorter chains it is OK to set it lower
##
nlines <- 1e3

##
## Set MCMC step-size. Step-size should be adjusted such that the acceptance rate falls in the range 20%-60%. 
## Decrease/Increase as needed A value of 'NULL' results in a default set by dataType. Ex: (CDC|GFT|GFTPlus)&national==TRUE->.001, MPZ->.02
##
dr <- NULL #.05

##
## number of MCMC chains to run, default is 1, for publication level calculations use 3-5
##

reals <- 1

##
## number of walkers for EMCEE procedure. Suggested range: 100 to 1000. For a quick run, set to 10 and nMCMC=1e5
##

walkers <- 100

##
## debug - For debug mode TRUE or FALSE, default is FALSE.
## verbose - Controls the amount of printing TRUE or FALSE, default is TRUE.
## plot - TRUE/FALSE/EXTRENAL  Default is TRUE. Should really always be set to TRUE or "EXTERNAL"
#

debug   = FALSE
verbose = TRUE 
plot    = TRUE

##
## device for plotting (string) currrently supporting X11 and pdf/png. Default is pdf. 

device = "pdf" # "pdf" "png" "X11"

##
## iseed integer - seed for random number generator.  If set to NULL the R code will seed it.  To reproduce your results results set iseed here 

iseed = NULL

##
## weekly weighting - apply a weight to each week in the objective function
## wflag - choose how the weighting vector wweight is created:
# 0 - All weeks equal (default)
# 1 - User input (set below)
# 2 - From database (not operational yet)
# 3 - Use CDC Total Patients
wflag = 0

# if wflag=1 above, then set wweight here.  The 'wweight' vector must have the same length as the epi data and sum(wweight) must be positive.  Generally, 'wweight' is expected be a vector of non-negative numbers.
wweight = NULL 	#ex. rep(1,52) or runif(52,min=0,max=1)

##
## Load the EPI data, population data and possibly other data (specific humidity, school schedule)

##
## if dataType=MPZ - the loaded Military data will include our results as published in Plos Computational Biology - for the ILI profile and the two-value model of R(t)
## To access it - for plotting purposes for example use: 
##  data(PlosCompBio.ILI.small.pandemic.profiles) to load the ILI profiles
##  data(PlosCompBio.ILI.small.pandemic.roft)     to load the weekly value of R(t) - for the two-value model of the Plos manuscript
##  or after calling get.data you can just use mydata$plos$plos.ili and mydata$plos$plos.roft - for both colnames is the zip code
##

mydata = get.data(dataType=dataType,myMPZ=myMPZ,iregion=iregion,national=national,job.year=job.year,wweight=wweight,wflag=wflag,nweeksFit=nweeksFit,imodel=imodel)


##
##  runPMEDDS is the interface to the driver for the code which runs the MCMC chains 
##  Below we list all the parameters, any parameter that is ommited will be set in PMEDDS to its default value


##
## All plots, tables and binary R data are written in the 'output' sub-directory
## to check what the 'out' object contains use names(out)
##


MCMC = TRUE #If FALSE we will run the EMCEE code

if (MCMC) {
	
out <- runPMEDDS(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg,optTg=optTg,nMCMC=nMCMC,nlines=nlines,reals=reals,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name,job.year=job.year,RandInit=RandInit,dr=dr,outputDir = 'test')


# Open all the relevant pdf or png files from this run if device was not set to X11
# When truly running in a batch mode this is not something that you want to do 
if (device != "X11" & device != "x11") {
	if (device == "pdf" | device == "PDF") command_to_open = paste("open ","output/*",mydata$dataName,"*pdf",sep="")
	if (device == "png" | device == "PNG") command_to_open = paste("open ","output/*",mydata$dataName,"*png",sep="")
	system(command_to_open)
}


## To RUN THE EMCEE procedure use this call (by setting MCMC FALSE above ) and set walkers>=50 (or better even 100), nMcMC=1e4, nlines = 1e2 
} else {

out <- runPMEDDS.EMCEE(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg,optTg=optTg,nMCMC=nMCMC,nlines=nlines,walkers=walkers,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name,job.year=job.year,outputDir = 'test')	

#ONLY open the plots of the data/best walker and its CAR - otherwise nwalker plots will open and this can be in the hundreds..

if (device != "X11" & device != "x11" ) {
	if (device == "pdf" | device == "PDF") {
		command_to_open = paste("open ","output/",mydata$dataName,"-epi-sh-school.pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/plot-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/car-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
	}
	if (device == "png" | device == "PNG") {
		command_to_open = paste("open ","output/",mydata$dataName,"-epi-sh-school.pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/plot-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/car-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
	}
	
}


}







