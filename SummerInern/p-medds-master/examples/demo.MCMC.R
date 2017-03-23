##
## SAMPLE MCMC EXAMPLE DRIVER FOR P-MEDDS PACKAGE - USING A COMPARTMENTAL SIR MODEL AND ILI DATA OF EITHER MILITARY (MPZ), CDC OR GFT DATA
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
## Choose what type of data to model, options are MPZ (for Military) CDC, GFT, GFTPlus. Default is MPZ

dataType = "MPZ"  # "MPZ" "CDC" "GFT" "GFTPlus"

##
## Year for modeling- start of season
## for Military modeling we can only model 2009 for CDC we have 1997-2014 and GFT/GFTPlus 2004-2014

job.year=2009

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

iregion=7 #options are 1 to 10, ignored in case of national modeling
national = FALSE  #set to TRUE or FALSE

## Call the demo-function from 'pmedds.core' package.
runPMEDDS.demo(dataType=dataType,job.year=job.year,region=iregion,national=national)

