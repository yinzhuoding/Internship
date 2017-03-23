\name{pmedds.core-package}
\alias{pmedds.core-package}
\alias{pmedds.core}
\docType{package}
\title{
P-MEDDS: Prototype-Military Epidemic Disease Dynamics System
}
\description{
P-MEDDS: A tool for modeling infectious disease dynamics in military and civilian populations.  The package includes a database of weekly influenza-like-illness (ILI) for top military installations and a civilian database that includes all publically available data from the Centers for Disease Control and Prevention (CDC) and Google Flu Trends (GFT/GFT+).  Data on the 2003 Severe Acute Respiratory Syndrome (SARS) published by the World Health Organization (WHO) are also included.  

P-MEDDS uses a compartmental S-I-R (Susceptible-Infectious-Recovered) model along with a robust Markov-Chain-Monte-Carlo (MCMC) procedure to model the weekly ILI data allowing for various time-dependent models for the force of infection.  The P-MEDDS database includes weekly-averaged specific humidity for: the top military installations, each of the 50 states, and all of the cities in the GFT database. Complete US census data are also provided along with some data about school schedule at certain military installations.  The package supports five different models for the time dependency of the force of infection: (1) Force of infection is a constant times a specific humidity-dependent term and times a school schedule term, (2) Force of infection is a constant times a specific humidity-dependent term only, (3) Force of infection is a constant times a school schedule term only, (4) Force of infection is a constant, (5) Force of infection is allowed to change from one value to another and later back to the initial value. 

For the 2003 SARS data, we provide an efficient version of the Wallinga-Teunis procedure for evaluating the daily force of infection.}
\details{
\tabular{ll}{
Package: \tab pmedds.core\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-11-28\cr
License: \tab Currently Used by PSI and Leidos Only\cr
}

}
\author{
Predictive Science Inc.

Maintainer:  <mbennun@predsci.com>
}
\references{
Riley P, Ben-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-like-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.

Centers for Disease Control and Prevention Influenza Surveillance: http://www.cdc.gov/flu/weekly/fluactivitysurv.htm

Google Flu Trends: http://www.google.org/flutrends/us/

World Health Organization SARS data: http://www.who.int/csr/sars/epicurve/epiindex/en/

Wallinga J, Teunis P (2004) Different epidemic curves for severe acute respiratory syndrome reveal similar impacts of control measures. Am J Epidemiol 160: 509–516.
}

\keyword{ package }
\seealso{
README file in the p-medds main directory.

README file and all scripts in the 'examples' directory.
}
\examples{
The pmedds.core package has an examples directory with scripts that will familiarize you with the package capabilities.  A detailed README file is also provided in this directory.
	
To learn how to access and model the military or civilian ILI data please see the:
	
	example.driver.R script - this script can be run from the command line or within an R session.  Please go over the script, familiarize yourself with the parameters, run it as it is and then start changing the parameters.   
	
	example.interactive.R script - for an interactive version of a P-MEDDS run.  This script will guide you through all the required parameters

To learn how to access and model the SARS 2003 data please see the:
   
   example.wt.R script - this script explains all the parameters and shows you how to run individual countries or all the countries one after the other.
   
The examples directory has a 'tests.output' sub-directory where sample output plots, tables and R binary data files are kept.  
}
