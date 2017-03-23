\name{get.CDC.data}
\alias{get.CDC.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get CDC weekly ILI data
}
\description{
 Gets CDC weekly ILI data for a given epidemic/pandemic year and a given CDC region (or the entire US)
}
\usage{
mydata = get.CDC.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009)
}

\arguments{
	\item{reg }{Region number, for modeling (between 1-10).  Ignored if national = TRUE}
	\item{national	}{TRUE or FALSE.  Set to TRUE to model the entire US EPI profile.}
	\item{week.start }{Starting weeek for epidemic/pandemic year. If NULL set to 27 unless this is the pandemic year in which case it is set to 13.}
	\item{week.end }{Ending week for epidemic/pandemic year.  If NULL set to 26.}
	\item{year.start }{Starting year for epidemic (or pandemic) season.}
}

\value{
	A list with
	\item{cases }{weekly number of ILI cases}
	\item{weeks }{week number using CDC definition}
	\item{weighted }{weighted percentage of ILI cases out of total number of outpatient visits}
	\item{patients }{weekly number of out patient visits}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = get.CDC.data(reg = 1, national = FALSE, week.start=13, week.end=26, year.start = 2009)
## The function is currently defined as
function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009){       
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
      
      data(cdc_region_nation,package="pmedds.core")
      
      if (national == TRUE){
            
            #Extract the data for the entire US. Output the data into a simple matrix (cases) containing week dates and incidence numbers     
            dat = subset(cdc_region_nation,REGION == 'National')            
            
      } else if (!is.null(reg)){
            
            #Extract the data for each region/year. Output the data into a simple matrix (cases) containing week dates and incidence numbers     
            dat = subset(cdc_region_nation,REGION == paste('Region',reg))   
            
      }

      dat$DATE = mapply(function(U,Y) as.Date(paste('0-',U,'-',Y,sep=''),format='\%w-\%U-\%Y'),dat$WEEK,dat$YEAR) #Convert year/week to a date from 1-1-1970
      subs = subset(dat,(DATE >= date.start & DATE <= date.end))      
      
      weeks = subs[,'WEEK']
      weighted = subs[,'X..WEIGHTED.ILI']                                     
      patients = subs[,'TOTAL.PATIENTS']
      cases = round((weighted/100)*patients)
       
      list(cases=cases,weeks=weeks,weighted=weighted,patients=patients)  
      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
