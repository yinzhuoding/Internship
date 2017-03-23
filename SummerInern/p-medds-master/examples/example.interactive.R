##
## SAMPLE EXAMPLE DRIVER FOR P-MEDDS PACKAGE - USING A COMPARTMENTAL AN SIR MODEL INTERACTIVE VERSION 
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
## call the function that will help the user to set all the parameters for an ILI run 
setup.run <- set.interactive()	
 
##
## retrieve all needed parameters from setup.run object  
job.name =setup.run$job.name
dataType= setup.run$dataType
job.year= setup.run$job.year
myMPZ   = setup.run$myMPZ
iregion = setup.run$iregion
national= setup.run$national
imodel  = setup.run$imodel
Tg      = setup.run$Tg
nMCMC   = setup.run$nMCMC
nlines  = setup.run$nlines
reals   = setup.run$reals
debug   = setup.run$debug
verbose = setup.run$verbose
plot    = setup.run$plot
device  = setup.run$device
iseed   = setup.run$iseed
wflag   = setup.run$wflag
wweight = setup.run$wweight
dr      = setup.run$dr
optTg   = setup.run$optTg
RandInit = setup.run$RandInit

## 
## Using the choices the user has set get the data 

mydata = get.data(dataType=dataType,myMPZ=myMPZ,iregion=iregion,national=national,job.year=job.year,wweight=wweight,wflag=wflag)

##  runPMEDDS is the interface to the driver for the code which runs the MCMC chains 
# Below we list all the parameters, any parameter that is ommited will be set in PMEDDS to its default value
out <- runPMEDDS(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg,optTg=optTg,nMCMC=nMCMC,nlines=nlines,reals=reals,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name,job.year=job.year,RandInit=RandInit,dr=dr)

## Open all the relevant pdf or png files from this run 
if (device != "X11" && device != "x11") {
	if (device == "pdf" | device == "PDF") command_to_open = paste("open ","output/*",mydata$dataName,"*pdf",sep="")
	if (device == "png" | device == "PNG") command_to_open = paste("open ","output/*",mydata$dataName,"*png",sep="")
	system(command_to_open)
}


##
## All plots, tables and binary R data are written in the 'output' sub-directory
## to check what the 'out' object contains use names(out)
##
