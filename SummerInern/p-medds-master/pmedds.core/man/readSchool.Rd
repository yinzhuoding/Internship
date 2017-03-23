\name{readSchool}
\alias{readSchool}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads school schedule data from package p-medds
}
\description{
Reads the school schedule data for the military bases from the file ILI.small.school.closure.by.zip5.20000103.20131230 and extracts from it the schedule for the 2009-2010 FY. Currently we have data only for the top 11 bases.  The start and end date for extracting data is determined by the start/end of the EPI data loaded before. 
 
}
\usage{
readSchool(start, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start }{
Starting date for extracting data}
  \item{end }{
Ending date for extracting data}
}
\details{
Currently the data set is incomplete - we have numbers only for the top 11 bases.  The number 1 indicates that the school was closed the whol week and 'NA' means open or no information.  The number 2 indicates that the school was closed for only 1/2 of the week. The code will replace all the 'NA' with 0 and the 2 with 1.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{curves }{Weekly school schedule. 1 closed NA open. Data only for top 11 bases}
  \item{date }{Week date}
  \item{week }{Week number (using CDC definition)}
  \item{nweeks }{The number of weeks. 66 for the 2009-2010 pandemic year}
  \item{nbase }{Number of bases in data file}
  \item{zipname }{Zip codes of the bases in the data file}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.	
}
\author{
Predictive Science Inc.
}
\note{
The code does NOT assume that the bases are in the same order as in the EPI file
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	school_data=readSchool(start,end)

## The function is currently defined as
function(start,end) {
      
      data(ILI.small.school.closure.by.zip5.20000103.20131230,package="pmedds.core")
      raw_school <- ILI.small.school.closure.by.zip5.20000103.20131230
      #school_date <- as.Date(ILI.small.school.closure.by.zip5.20000103.20131230$date,format="%m/%d/%y")
      school_date <- as.Date(ILI.small.school.closure.by.zip5.20000103.20131230$date)
      school      <- sapply(raw_school[,3:dim(raw_school)[2]], as.ts)
      
      range    <- which(school_date >= start & school_date <= end)
      
      school_date <- school_date[range]
      school      <- school[range,] 
      
      # replace '2' with 1  
      school[school==2] <-1 
      zipname <- colnames(school)
      zipname <- substr(zipname,start=2,stop=10)
      nbase <- length(zipname)
      nweeks_school <- length(school_date)
      school_week <- raw_school$week[range] 
      # we do not have week number for this data base
      list(curves=school, date = school_date, week = school_week, nweeks = nweeks_school, nbase = nbase, zipname = zipname)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
