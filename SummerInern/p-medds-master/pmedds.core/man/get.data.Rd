\name{get.data}
\alias{get.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get ILI, specific humidity and school schedule data for a military installation or a CDC/GFT/GFT+ region (or the entire US)
}
\description{
Use this function to load what you would like to model with any of our compartmental SIR models.  Depending on the data type (MPZ/CDC/GFT/GFT+) the function will call the routine that will load and extract the required data set (MPZ/CDC/GFT/GFT+).  For all data sets we also load the specific humidity data. For the military data set we also load the school schedule data.  For the civilian data sets we currently set this to zero. If data type is set to "MPZ" the iregion and national are ignored.  If dataType is set to "CDC" or "GFT"/"GFT+" then the myMPZ is ignored. And in the case of civilian modeling if national is set to TRUE then the iregion is ignored.
}
\usage{
mydata = get.data.(dataType="MPZ",myMPZ="23708",iregion=1,national=FALSE,job.year=2009) 
}

\arguments{
	\item{dataType	}{The type of data we want to model: MPZ, CDC, GFT }
	\item{myMPZ }{5-digit, and in some cases 4-digit, zip code of the military installation. Ignored in the case of civilain modeling}
	\item{iregion }{Region number for modeling, between 1-10.  Ignored in the case of military modeling}
	\item{national }{Relevant only for civilian modeling.  Set to TRUE if modeing the EPI data for the entire US.  The iregion parameter is ignored in this case}
	\item{job.year }{Starting FY for modeling.  For military modeling we currently support only the 2009-2010 pandemic year, job.year=2009}
}

\value{
	A list called mydata with the following items
	\item{cases }{An array with the weekly number of ILI cases}
	\item{sh }{An array with the weekly averaged specific humidity}
	\item{school }{An array with the weekly school schedule (open=0 closed=1). If no information set to zero for all weeks}
	\item{gamaepi }{An array with the the natural logarithm of the weekly number of (ILI cases + 1)}
	\item{nweeks }{Total number of weeks we are modeling}
	\item{weeks }{An array with the week number using CDC definition}
	\item{dates }{An array with week dates}
	\item{zipname }{The 5- or 4-digit zip code.  Ignored in the case of civilian modeling}
	\item{ibase }{The militray base index number.  Ignored in the case of civilian modeling}
	\item{iregion }{Region number relevant only for civilian modeling and when national is set to FALSE}
	\item{national }{TRUE or FALSE relevant only for civilian modeling}
	\item{npop }{The effective population of the military installation, civilian region, or entire US}
%% ...
}
\references{
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = get.data.wt(mycountry="Singapore")
## The function is currently defined as
function(dataType="MPZ",myMPZ="23708",iregion=1,national=FALSE,job.year=2009)  {
	
	if (dataType=="MPZ") {
	  #retrieve epi/sh/school data for all bases 
	  #we get a list called mydata with elements epi, sh, school and pop			
	  # each one of these is a list by itself
      mydata <- get.MPZ.data()
      epi_data <- mydata$epi #cases for all bases
      zipnames <- mydata$epi$zipname #MPZ for all bases
      ibase <- which(zipnames == myMPZ) #find the base number using the zip code
      if (length(ibase) == 0) { #if not found set to base number one
      	ibase = 1
      	myMPZ = zipnames[1]
      	
      	cat("User provided MPZ does not exists in data base \n")
      	cat("Reverting to Default - First MPZ ",myMPZ,"\n")
      	txt= "User provided MPZ does not exists in data base"

      }
      #check to see that the user has set the job.year properly
      
      if (job.year != 2009) {
      	cat("\n\n For MPZ modeling we currently only model the pandemic year 2009-2010 \n")
      	cat(" Will reset to FY 2009-2010 \n\n")
      	job.year = 2009
      }
      
      nbase <- mydata$epi$nbase
      school_data  <- mydata$school
      sh_data      <- mydata$sh
      pop_data     <- mydata$pop
	  #Now select the base we are modeling 
	  epi=epi_data$curves[,ibase]
	  nweeks <- mydata$epi$nweeks
      weeks   <- mydata$epi$week
      dates   <- mydata$epi$date
      
      zipname <- zipnames[ibase]
      sh <- sh_data$curves[,sh_data$zipname==zipname]
      school <- school_data$curves[,school_data$zipname==zipname]
      school[is.na(school)] <- 0 #replace NAs with zero
      
      npop <- pop_data$pop[pop_data$zipname ==zipname]
      
      copyepi = epi
      gamaepi <- epi
	  # to save computational time we also calculate the gamma(epi+1)
      for (i in 1:nweeks) {
      	gamaepi[i] <- lgamma((copyepi[i]+1.))	
      }
      

	  mydata=list()
	  mydata$cases=epi
	  mydata$sh=sh
	  mydata$school=school
	  mydata$gamaepi=gamaepi
	  
	  mydata$nweeks=nweeks
	  mydata$weeks=weeks
	  mydata$dates=dates
	  mydata$zipname=zipname
	  mydata$npop=npop
	  mydata$base=ibase
	  mydata$dataName = paste(dataType,'_',job.year,'_',zipname,sep='')
	  	  
	} else if (dataType=="CDC" | dataType=="GFT" | dataType=="GFTPlus") {
	  #Some set up work - need to change so this works with CDC EPI week numbers and not dates
      year = job.year
      reg  = iregion
      
      
      # for the 2009-2010 pandemic use this 3-30-2009 to 6-28-2010
      
      if (year != 2009) {
      #This is the CDC definition of start/end of EPI year
      	week.start = 27
      	week.end   = 26            	
      } else {
      	week.start = 13
      	week.end   = 26
      }
    
      
      #Include (at most) one of the following:            
      
      #The features below are not ready yet
      state  = NULL      # Full state name or abbrievation (ie, 'CA' or 'California')
      county = NULL      # Full county name ('Orange County')
      city   = NULL      # Full city name
                  
      #Retrieve epi data
      if(dataType=="GFT") {
      	mydata = get.GFT.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year)
      } else if (dataType == "GFTPlus"){
      	mydata = get.GFTPlus.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year)
      } else { #default is CDC data 
      	mydata = get.CDC.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year) 
      }
      # get the SH data 
      mydata$sh = get.SH.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year)$sh

      epi = mydata$cases
      copyepi = epi
      gamaepi = epi
	  nweeks=length(epi)
	  
      for (i in 1:nweeks) {
      	gamaepi[i] <- lgamma((copyepi[i]+1.))	
      } 
      
      mydata$nweeks = nweeks
      mydata$gamaepi = gamaepi
      
      if (national){
            mydata$dataName = paste(dataType,'_',year,'_national',sep='')
      } else if(!is.null(reg)){      
            mydata$dataName = paste(dataType,'_',year,'_reg',reg,sep='')
      }
      
      #this should give us CDC like week numbers
      weeks = mydata$weeks
      
      #Retrieve population data
      npop = get.census.data(national = national, year=year, reg = reg)
	  mydata$npop=as.double(npop)
      #For now we  do not have School data so we put zero's but we should included it (definately the former)
#      mydata$sh = rep(0,nweeks)
      mydata$school = rep(0,nweeks)
      mydata$national=national
	  mydata$region = iregion
	  
	} else {
		cat("Error: dataType can only be set to MPZ/CDC/GFT/GFTPlus \n")
		cat("Currently dataType set to: ",dataType,"\n")
		cat("Code will exit \n")
		quit(save="no")
	}
	
	
	mydata
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
