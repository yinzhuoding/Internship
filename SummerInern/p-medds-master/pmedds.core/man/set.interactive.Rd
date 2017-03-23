\name{set.interactive}
\alias{set.interactive}
\title{
Interactive setting of all the parameters for an ILI run with a compartmental SIR model.
}
\description{
Interactive setting of all the parameters for an ILI run with a compartmental SIR model and a robust MCMC fitting procedure.
Data can be set to MPZ, CDC, GFT or GFT+.  
The time-dependent force of infection can be chosen from five models for MPZ and three for CDC/GFT/GFT+.
The function will guide the user through all the parameters.  
See the README file and example.interactive.R script in the 'examples' directory.
}
\usage{
set.interactive()
}
\details{
CDC region definition:
Region 1: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and Vermont. 

Region 2: New Jersey, New York, Puerto Rico, and the U.S. Virgin Islands. 

Region 3: Delaware, the District of Columbia, Maryland, Pennsylvania, Virginia, and West Virginia. 

Region 4: Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina, South Carolina, and Tennessee. 

Region 5: Illinois, Indiana, Michigan, Minnesota, Ohio, and Wisconsin. 

Region 6: Arkansas, Louisiana, New Mexico, Oklahoma, and Texas. 

Region 7: Iowa, Kansas, Missouri, and Nebraska. 

Region 8: Colorado, Montana, North Dakota, South Dakota, Utah, and Wyoming. 

Region 9: Arizona, California, Hawaii, Nevada, American Samoa, Commonwealth of the Northern Mariana Islands, Federated States of Micronesia, Guam, Marshall Islands, and Republic of Palau. 

Region 10: Alaska, Idaho, Oregon, and Washington.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
Returns a list with the following items:
   \item{jobname }{User given job name, can be descriptive}
   \item{dataType }{Data we are modeling: MPZ, CDC, GFT or GFT+}
   \item{job.year }{The starting year for the FY being modeled, e.g. for 2009-2010 use 2009}
   \item{myMPZ }{Relevant only for MPZ modeling. The 5-digit (and sometimes 4-digit) zip code of the base}
   \item{ibase }{Relevant only for MPZ modeling.  The base number based on the order in the 2009-2010 ILI file}
   \item{iregion }{Relevant only for CDC/GFT/GFT+ modeling.  The 1-10 regions are defined by the CDC. Ignored in the case of national modeling. See below.}
   \item{national}{Relevant only for CDC/GFT/GFT+ modeling.  Can be set to TRUE (model the entire US epi profile) or FALSE (model a specific iregion, set between 1-10,see above).  }
   \item{imodel }{Model number for the force of infection: (1) R0 times a school and times specific humidity term, (2) R0 times a specific humisity term, (3) R0 times a school term, (4) R(t)=R0 fixed in time, (5) R(t) starts at RA changes to RB and later changes back to RA}
   \item{Tg }{The generation time in days (2.6 is recommended for ILI)}
   \item{nMCMC }{Number of steps in each MCMC chain.  1e5 is for quick runs and 1e7 or more for fully converged}
   \item{nlines }{Number of steps saved for each MCMC chain.  1e4 is recommended even for long chains.}
   \item{reals }{Number of MCMC chains. 1 for a quick test 3-5 for converged results}
   \item{debug }{TRUE or FALSE controls the amount of printing by the package. Default FALSE}
   \item{verbose}{TRUE or FALSE, another control for the amount of printing. Default FALSE}
   \item{plot}{TRUE or FALSE. Should always be set to TRUE}
   \item{device}{character "X11" "pdf" "png".  "pdf" or "png" are recomended}
   \item{iseed}{integer - seed for RNG.  Using zero lets the code seed the RNG.}
%% ...
}
\references{
	Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
	
	For CDC regions definition see:
}
\author{
 Predictive Science Inc.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	mydata = set.interactive()
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
## The function is currently defined as
function() {
	
	cat("\n\n Welcome to the P-MEDDS Interactive Example Driver Script","\n\n")
	cat("Please Enter a Job Name (e.g. test.pmedds) \n")
	fil <- readLines(con="stdin", 1)
	job.name=as.character(fil)
	#here split between W-T procedure for SARS or compramental model for ILI
	cat("Please choose type of data to model: CDC, GFT, GFTPlus, MPZ ","\n")
	fil <- readLines(con="stdin", 1)
	dataType=fil
	if (dataType == "CDC" | dataType == "GFT" | dataType == "GFTPlus") {
		cat("Please enter starting Year for Modeling, range is 1997-2013 for CDC and GFT/GFTPlus 2003-2013 \n")
		fil <- readLines(con="stdin", 1)
		job.year=as.numeric(fil)
		if (dataType == "CDC") {
			if (job.year < 1997 | job.year > 2013) {
				cat("\n Starting FY not in correct range, defaulting to 2009-2010 pandemic year \n")
				job.year = 2009
			}
		}  else {
			if (job.year < 2003 | job.year > 2013) {
				cat("\n Starting FY not in correct range, defaulting to 2009-2010 pandemic year \n")
				job.year = 2009
			}			
		}
		
		cat("Please choose National or Regional Modeling (N or R) \n")
		fil <- readLines(con="stdin", 1)
		if (fil == "R") {
	     cat("Please choose Region number for modeling, options are 1-10 ","\n")
	     cat("Region 1: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and Vermont \n")
	     cat("Region 2: New Jersey, New York, Puerto Rico, and the U.S. Virgin Islands \n")
	     cat("Region 3: Delaware, District of Columbia, Maryland, Pennsylvania, Virginia, and West Virginia \n")
	     cat("Region 4: Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina, South Carolina, and Tennessee \n")
	     cat("Region 5: Illinois, Indiana, Michigan, Minnesota, Ohio, and Wisconsin \n")
	     cat("Region 6: Arkansas, Louisiana, New Mexico, Oklahoma, and Texas\n")
	     cat("Region 7: Iowa, Kansas, Missouri, and Nebraska \n")
	     cat("Region 8: Colorado, Montana, North Dakota, South Dakota, Utah, and Wyoming \n")
	     cat("Region 9: Arizona, California, Hawaii, and Nevada \n")
	     cat("Region 10: Alaska, Idaho, Oregon, and Washington \n")
	     fil <- readLines(con="stdin", 1) 
	     iregion = as.numeric(fil)
	     if (iregion < 1 | iregion > 10) {
	     	cat("\n\n Region Entered not in Correct 1-10 Range, Code will Default to Region 1 \n\n")
	     	iregion = 1
	     }
	     national = FALSE
		} else {
		 national = TRUE
		 iregion = 1 #will be ignored but must set it
		}
		#Not used in the case of CDC/GFT/GFTPlus but must set to some value
    	myMPZ=23708 
    	ibase = 1


	} else {
		cat("Please Enter A Military Zip Code For Modeling from this List: ",'\n')
		zipnames = get.mpz.names()
		print(zipnames)
		fil <- readLines(con="stdin", 1)
		ibase = which(zipnames == fil)
		if(length(ibase) == 0) {
			ibase = 1
			cat("Wrong Militray Zip Code, Reverting to First MPZ: ",zipnames[ibase],"\n")
		}
		
		#get base MPZ 
		myMPZ = zipnames[ibase]
        job.year = 2009 #Currently modeling only the pandemic year
        # Ignored in the case of MPZ modeling but we have to set them to some value
        iregion=1
        national=T
	}
	
	cat("Please Enter Model Number For the Basic Reproduction Number, R(t): \n")
	cat("1: R(t) R0 times a school and specific humidity terms \n")
	cat("2: R(t) is R0 times a specific humidity term \n")	
	cat("3: R(t) is R0 times a school schedule term \n")
	cat("4: R(t) is fixed in time, R(t) == R0\n")
	cat("5: R(t) changes from RA to RB and then back to RA\n")
	cat("For Civilian Data (CDC and GFT/GFTPlus) only options 2, 4 & 5 are possible\n")
	fil <- readLines(con="stdin", 1)
	imodel = as.integer(fil)
	if (imodel == 1 | imodel == 3 | imodel > 5 & dataType != "MPZ") {
		cat("\n\n For Civilian Data Please Choose between models 2, 4 or 5 \n\n")
		fil <- readLines(con="stdin", 1)
		imodel = as.integer(fil)		
	}
	if (imodel > 5 | imodel < 1) {
		cat("\n\n\ Error in Model Number, defaulting to Model Number 5 \n\n")
		imodel = 5
	}
	
	cat("Please Enter the Infectious Period in Days (2.6 days is the accepted value for ILI) \n")
	fil <- readLines(con="stdin", 1)
	Tg = as.numeric(fil)
	if (length(Tg) == 0) {
		cat("\n\n Error in value for Tg, defaulting to 2.6 days \n\n")
		Tg = 2.6 
	}
	cat("Please Enter the Number of MCMC Chains \n")
	cat("For a quick run use 1 for a converged run use 3-5 \n")
	fil <- readLines(con="stdin", 1)
	reals = as.integer(fil)
	if (length (reals) == 0 | reals == 0) {
		cat("\n\n Error in number of MCMC chains, deafulting to 1 chain \n\n")
		reals = 1
	}
	cat("Please Enter the number of MCMC steps in each chain\n")
	cat("Example values are: 1e5 for a quick run, 1e7 for a converged run \n")
	fil <- readLines(con="stdin", 1)
	nMCMC = as.integer(fil)	
	if(length(nMCMC) == 0) {
		cat("\n Error in number of MCMC step, defaulting to 1e5 \n")
		nMCMC=1e5
	}
	#Number of lines in MCMC output files
	nlines = min(1e4,nMCMC) 
	if (length(nlines) == 0) nlines = 1e4

	cat("Please Enter the Seed for the Random Number Generator \n")
	cat("Using this Same Seed in future runs will reproduce the results of this run \n")
	cat("Using 0 will trigger seeding by the code \n")
	fil <- readLines(con="stdin", 1)
	iseed = as.integer(fil)
	if(iseed == 0) iseed = NULL
    #Set the debug, verbose and plot to TRUE or FALSE. (plot should really always be true)
	debug = FALSE
	verbose = FALSE
	plot = TRUE    
	# For interactive version makes sense only to set to pdf or png
	device = "pdf" #"pdf" "png"
#	cat("Set device name for plotting X11/pdf/png default is pdf/png\n")
#	fil <- readLines(con="stdin", 1)
#	device = fil
	
	list(job.name=job.name,dataType=dataType,job.year=job.year,myMPZ=myMPZ,ibase=ibase,iregion=iregion,national=national,imodel=imodel,Tg=Tg,nMCMC=nMCMC,nlines=nlines,reals=reals,debug=debug,verbose=verbose,plot=plot,device=device,iseed=iseed)
	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
