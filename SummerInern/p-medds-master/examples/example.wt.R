##
## SAMPLE EXAMPLE DRIVER FOR P-MEDDS PACKAGE USING THE W-T PROCEDURE AND THE SARS DATA
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

job.name="test.pmedds-wt"

##
## debug - For debug mode.  Default is FALSE.
## verbose - controls the amount of printing and plotting.  Default is FALSE.
## plot - TRUE/FALSE/EXTRENAL  Default is TRUE. Should really always be set to TRUE or EXTERNAL

debug   = FALSE
verbose = FALSE
plot    = TRUE


##
## device for plotting (character) currrently supporting X11 and pdf. Default is X11. 
##

device = "pdf"

## 
## seed for random number generator
##

iseed = 123456

## 
## Parameters for the Weibull distribution, these are the values used by Wallinga and Teunis, units are days
## shape (beta) and scale (alpha)
##

alpha = 8.4 
beta  = 3.8


##
## Select number of realizations 10,000 is reasonable
## The code executation time scales with ncases x ncases x number_of_realizations
## See below for the number of cases for each country

ireal = 1e4

## Select country for 2003 SARS modeling options are: Singapore (206), Hong-Kong (1734), Canada (250), Vietnam (62) and China (5568)
## The numberin brackets is the cumulative number of cases for ach one

## To see bar plots of the number of cases as a function of onset date: http://www.who.int/csr/sars/epicurve/epiindex/en/

## 
## select a single country for modeling, get the data for the country and run - or use the loop below to run all the countries one after the other
## Estimated run times: Singapore - 9 sec., HK - 7 minutes, Canada -  20 sec. Vietnam - 2 sec., and China - 100 minutes
##

#mycountry="Singapore" # "Hong-Kong","Canada","Vietnam","China"
#mydata = get.data.wt(mycountry=mycountry) 
#out <- runPMEDDS.WT(mydata=mydata,ireal=ireal,shape=beta,scale=alpha,iseed=iseed,debug=debug,verbose=verbose,device=device,job.name=job.name)
##To immediately open and view the PDF file from within script if device was set to pdf
##	if (device == "pdf" | device=="PDF") {
##		mypdf = paste("output/W_T_",mycountry,".pdf",sep="")
##		command_to_open = paste("open ",mypdf,sep="")
##		system(command_to_open)		
##	}

## For running all the countries one after the other 
##

country.vec = c("Singapore","Hong-Kong","Canada","Vietnam","China")
ncountry = length(country.vec)

for (i in 1:ncountry) {

	#set the country
	
	mycountry=country.vec[i]
	
	##Get the data for desired country

	mydata = get.data.wt(mycountry=mycountry)

	#mydata has case_number, case day (times), total number of cases (ncases) and country name (mycountry)
	# Run the W-T procedure 
	
	
	out <- runPMEDDS.WT(mydata=mydata,ireal=ireal,shape=beta,scale=alpha,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name)
	
	#To immediately open and view the PDF file from within the script
	if (device == "pdf" | device=="PDF") {
		mypdf = paste("output/W_T_",mycountry,".pdf",sep="")
		command_to_open = paste("open ",mypdf,sep="")
		system(command_to_open)		
	}

	
}


##
## For a log file of your run see 'log-wt.txt' in your working directory
## 
## For plots, tables and an RData file see the 'output' sub-directory in your working directory
##
## Plot file: W_T_mycountry.pdf - tope is number of cases by onset or report day. Bottom is daily estimated value of R(t): mean, median and 95% CI
##
## Rstats-mycountry.csv: date,day number, number of cases, mean daily value of R(t), median daily value of R(t) and the 95% CI of daily R(t)
##
## Rl.mycountry.csv: a large file with ncases rows and ireals columns where ireals is the number of realizations set above, and ncases is the total number of cases.  
## This is the output of the W-T procedure and can be reused to calculate all the statistcs 
##
## mycountry.RData - a binary R data file with the all the input and output of the run. Can be loaded and re-used for statistics and plotting. 
## To load the file use: load("output/mycountry.RData") 
## to see what type of data is in the object just print: results
## Then you can access the data using: results$mydata has case number, onset (or report) day, total number of cases and country name
## The daily ordered statistics on R(t) using: 
## results$Rlm.order - mean daily value of R(t), results$Rlmd.order - median daily value and results$Rlq.order - 95%CI of daily R(t)
## results$ireal - number of realizations that were run
## results$Rl    - the output of each of the realizations 
##
## For running with your own data - comment the call to get.data.wt and provide the code with your own version of 'mydata' that has the following components: 
## ncases -   which is the total cumulative number of cases
## mycountry - a string definning the area/region we are modeling
## case_number: just a sequence of integers from 1 to ncases (which is the total cumulative number of cases)
## times -  a sequence of length ncases which gives the onset (or report) day for each case
## set the rest of the parameters (use this script or your own version) and call runPMEDDS.WT
## 




