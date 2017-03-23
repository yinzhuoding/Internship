\name{get.MPZ.data}
\alias{get.MPZ.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get EPI, Specific Humidity, School Schedule and Population data for the Military Installations.  Example Model results also loaded.
}
\description{
Calls the functions that will read the EPI, SH, School and Population data for the Military Installations.  Uses the EPI data to determine the start/end of the ILI season
This function also loads results from Plos Computational Biology paper - for the ILI profiles and the two-value model of R(t)
}
\usage{
get.MPZ.data()
}

\arguments{
	No arguments are needed}

\value{
	Returns a list with the following items:
	\item{epi}{A list with weekly epi data for all installations, epi week number, epi_date, number of installations, number of weeks, and the 5-digit zip codes of the bases}
	\item{sh}{A list with weekly specific humidity data for all installations, epi week number, epi_date, number of installations, number of weeks, and the 5-digit zip codes of the bases}
	\item{school}{A list with weekly school data for top 11 installations, epi week number, epi_date, number of installations, number of weeks, and the 5-digit zip codes of the bases}
	\item{pop}{A list with our estimate for the bases population, their 5-digit zip names, and the number of bases}
	\item{plos}{A list with the Plos Comp Biology estimate for the ILI profiles and the two-value model of R(t)}
%% ...
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
Riley P, Ben-Nun M, Linker JA, Cost AA, et al. (2015) Erly characterization of the Severity and Transmissibility of Pandemic Influenza Uisng Clinical Episode Data from Multiple Populations, PloS Comput Biol published 24 Sep 2015 DOI: 10.1371/journal.pcbi.1004392
}
\author{
Predictive Science Inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- mydata = get.MPZ.data()
## The function is currently defined as
et.MPZ.data <- function() {
      
      ## 
      ## read ILI curves and determine the number of weeks
      ## --------------------------------------
      epi_data <- readEpi()
      
      ##
      ## read specific humidity 
      ## --------------------------------------
      sh_data <- readSH(epi_data$nweeks)
      
      ##
      ## read school data for the date-range of epi_date
      ## --------------------------------------
      start <- epi_data$date[1]
      end   <- epi_data$date[epi_data$nweeks]
      school_data <- readSchool(start,end)
      
      ## read the base population and multiply by our omega of 104.3
      pop_data <- readPopulation(omega=104.3)
      
      ##
      ## read the results of the Plos Computational Biology manuscript
      ## This loads the predicted profile and predicted R(t) using a two-value model
      ## --------------------------------------
      
      plos.results <- readPlosResults()
      
      ## define a class for each data
      
      class(epi_data) <-"epi"
      class(sh_data) <- "sh"
      class(school_data) <- "school"
      class(pop_data)    <- "pop"
      
      list(epi=epi_data,sh=sh_data,school=school_data,pop=pop_data,plos=plos.results)
}

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
