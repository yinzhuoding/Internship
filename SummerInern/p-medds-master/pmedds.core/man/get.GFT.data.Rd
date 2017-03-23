\name{get.GFT.data}
\alias{get.GFT.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get GFT weekly ILI data
}
\description{
 Gets GFT weekly ILI data for a given epidemic/pandemic year and a given GFT region (or the entire US).
 The 10 GFT regions are defined using the CDC region definition.
}
\usage{
mydata = get.GFT.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009)
}

\arguments{
	\item{reg }{Region number for modeling, between 1-10.  Ignored if national = TRUE}
	\item{national	}{TRUE or FALSE.  Set to TRUE to model the entire US EPI profile }
	\item{week.start }{Starting weeek for epidemic year. If NULL set to 27 unless this is the pandemic year in which case it is set to 13}
	\item{week.end }{Ending week for epidemic year.  If NULL set to 26}
	\item{year.start }{Starting year for epidemic (or pandemic) season}
}

\value{
	Returns a list with teh following items:
	\item{cases }{weekly number of ILI cases}
	\item{weeks }{week number using CDC definition}
	\item{old }{raw weekly number of cases - does not try to properly scale using the CDC numbers}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = et.CDC.data(reg = 1, national = FALSE, week.start=27, week.end=26, year.start = 2009)
## The function is currently defined as
function(reg=1,national = FALSE, week.start=13,week.end=26, year.start=2009){       
      # reg        = index of particular USA region of interest (1-10)     

      #FY year.start - year.end
      if (!is.null(year.start)){
            year.end = year.start + 1
      } 
      
   	  if (!is.null(week.start) && !is.null(week.end)) {
            date.start = as.Date(paste('0-',week.start,'-',year.start,sep=''),format='\%w-\%U-\%Y')
            date.end   = as.Date(paste('0-',week.end  ,'-',year.end  ,sep=''),format='\%w-\%U-\%Y')   	  	
   	  } else {
            date.start = as.Date(paste('0-',27,'-',year.start,sep=''),format='\%w-\%U-\%Y')
            date.end   = as.Date(paste('0-',26,'-',year.end  ,sep=''),format='\%w-\%U-\%Y')   	  	   	  	
   	  }       
      
      data(gft_region_nation,package="pmedds.core")
      
      dat = gft_region_nation
      dat$Date = as.Date(dat$Date,'\%m/\%d/\%y')
      dat = subset(dat,dat$Date >= date.start & dat$Date <= date.end) 
      weeks = as.numeric(format(as.Date(dat[,'Date'],format='\%Y-\%M-\%D'),'\%U')) #Extract dates for cases and convert them to Week #
      
      data(cdc_region_nation,package="pmedds.core") #Import CDC data to get patient data
      
      if (national){ 
            
            cases = dat[,'United.States']      
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == 'National')            
            
      } else if (!is.null(reg)){
            
            region.ind = grep(paste('Region.',reg, '.',sep=''),names(dat),fixed=TRUE) #find column index in GFT data corresponding to reg
            cases = dat[,region.ind]         
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == paste('Region',reg))            
      }
      
      #Get patient data
      subs$DATE = mapply(function(U,Y) as.Date(paste('0-',U,'-',Y,sep=''),format='\%w-\%U-\%Y'),subs$WEEK,subs$YEAR) #Convert year/week to a date from 1-1-1970
      subs = subset(subs,(DATE >= date.start & DATE <= date.end))
      patients = subs[,'TOTAL.PATIENTS']
      
      old = cases
      cases = round(cases * (patients/100000.0))
      
      list(cases=cases,weeks=weeks,old=old)           
      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
