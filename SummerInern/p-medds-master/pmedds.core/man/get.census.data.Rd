\name{get.census.data}
\alias{get.census.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Get population census data for a region or for the entire US. 
	This function can also provide census data for a single state using the 2-letter abbreviation.
}
\description{
 Gets civilian population census data for a given epidemic (or pandemic) season for a region or for the entire US
}
\usage{
npop = get.census.data(national = FALSE, year=2009, reg = 1, ...)
}

\arguments{
	\item{national	}{TRUE or FALSE.  Set to TRUE to model the entire US EPI profile }
	\item{year }{Starting year for epidemic (or pandemic) season}
	\item{reg }{Region number for modeling, between 1-10.  Ignored if national = TRUE}

}

\value{
	Returns the effective population for the requested region or the entire US
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
##-- npop = get.census.data(national = FALSE, year=2009, reg = 1)
## The function is currently defined as
function (year=2009, national = FALSE, reg = 1, state = NULL, county = NULL, city = NULL){
      
      data(region_state_census,package="pmedds.core")
      
      if (year == 2014) #Use 2013 data for now. 2014 population will be available online 12/2014
            year = 2013
      
      #If national is not null and not false
      if (!is.null(national) && national){
            N = census_data['United States',paste('POP',year,sep='')]
            
      } else if (!is.null(reg)){
            N = census_data[paste('Region',reg,sep=''),paste('POP',year,sep='')]    
            
      } else if (!is.null(state)){
            
            if (nchar(state) == 2) {#If an abbreviation is supplied
                  state = toupper(state)
                  state = state.name[which(state.abb==state)] #Use R state dataset to convert abbreviation to full name
            } else{ #Assume full name supplied
                  state = paste0(toupper(substr(state,1,1)), tolower(substr(state,2,nchar(state)))) #Make sure only first letter is capitalized                
            }
            
            if (is.element(state,rownames(census_data))){
                  N = census_data[state,paste('POP',year,sep='')]
            } else{
                  stop('Invalid State Name: Provide either the full name or the 2-letter abbreviation of a state in the USA')
            }
      }
      npop = as.double(N)
      npop
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
