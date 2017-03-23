\name{get.data.wt}
\alias{get.data.wt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get 2003 SARS data for a W-T model run
}
\description{
Reads the 2003 SARS data file for given country and processes it as required to return a list of case numbers (case_number) with their onset or report day (times) along with the total number of cases (ncases) and the country name (mycountry).
}
\usage{
mydata = get.data.wt(mycountry=mycountry) 
}

\arguments{
	\item{mycountry	}{The country name. Options are: Canada, China, Hong-Kong, Singapore, Vietnam}
}

\value{
	\item{case_number }{An array with the case numbers}
	\item{times }{An array with onset or report day for each of the case numbers}
	\item{ncases }{Total number of cases}
	\item{mycountry}{Country name}
%% ...
}
\references{
Wallinga J, Teunis P (2004) Different Epidemic Curves for Severe Acute Respiratory Syndrome Reveal Similar Impacts of Control Measures. Am J Epidemiol 160: 509–516.

The WHO 2003 SARS Information: http://www.who.int/csr/sars/epicurve/epiindex/en/
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = get.data.wt(mycountry="Singapore")
## The function is currently defined as
function(mycountry="Singapore") {
	day1 = 37622 #This is a legacy item, due to the way MS saves January 1st 2003
	if (mycountry == "Singapore") {
         data(sarssingapore2003,package="pmedds.core")
         mydata <- sarssingapore2003
    } else if (mycountry == "Hong-Kong") {
         data(sarshongkong2003,package="pmedds.core")
         mydata <- sarshongkong2003
    } else if (mycountry == "Canada") {
         data(sarscanada2003,package="pmedds.core")
         mydata <- sarscanada2003
    } else if (mycountry == "Vietnam") {
         data(sarsvietnam2003,package="pmedds.core")
         mydata <- sarsvietnam2003
    } else if (mycountry == "China") {
         data(sarschina2003,package="pmedds.core")
         mydata <- sarschina2003         
    } else {
        cat(" Missing country name \n")
        cat(" Options are: Singapore, Hong-Kong, Canada, Vietnam and China\n")
        cat(" Code will exit \n")
        quit(save="no")         
    }
    
   	day1 = 37622 #This is a legacy item, due to the way MS saves January 1st 2003

	#Singapore it the only country for which the data is already in the format that the code can handle
	# for all other countires the columns are in the opposite order and it is a shorter list with unique days and the number of cases for that day
	# so we need to expand this list to day- case number 
	if (mycountry == "Singapore") {
		case_number <- mydata[,1]
    	times <-mydata[,2] - day1		
    	
	} else if (mycountry == "China") {
        cases = mydata[,2]
        days  = mydata[,1]
        ncaseOld=0
        
        case_number = rep(NA,sum(cases))
        times       = rep(NA,sum(cases))
        
        for (i in 1:length(days)) {
                ncase = cases[i]
                nstart = ncaseOld +1
                nend   = nstart + ncase - 1
                case_number[nstart:nend] = nstart:nend
                times[nstart:nend]       = days[i]
                ncaseOld = nend
        }
		
	} else {

        cases = mydata[,2]
        days  = mydata[,1]-day1
        ncaseOld=0
        
        case_number = rep(NA,sum(cases))
        times       = rep(NA,sum(cases))
        
        for (i in 1:length(days)) {
                ncase = cases[i]
                nstart = ncaseOld +1
                nend   = nstart + ncase - 1
                case_number[nstart:nend] = nstart:nend
                times[nstart:nend]       = days[i]
                ncaseOld = nend
        }
		
	}

    ncases = length(times)

	list(case_number=case_number,times=times,ncases=ncases,mycountry=mycountry)
	
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
