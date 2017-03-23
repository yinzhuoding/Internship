\name{setup.ini.halfwidth.HalfPeakWidth}
\alias{setup.ini.halfwidth.HalfPeakWidth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sub function for setup.ini.halfwidth().
}
\description{
	Given an epidemic profile 'cases' and a baseline value, estimate the profile width at halfpeak height by linear interpolation. 
}
\usage{
out = setup.ini.halfwidth.HalfPeakWidth(cases,Baseline)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cases}{
    Vector of cases/incedence as a time-series.
  }
  \item{Baseline}{
    Constant value to be removed from the profile.
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    \item{out}{A list with two entries: $goodpeak==TRUE indicates a width was found, $goodpeak==FALSE indicates a width was not found, $width is the width of the profile at half-height.}
%% ...
}
\references{
  Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
  Predictive Science Inc.
}
\examples{
 ##---- Should be DIRECTLY executable !! ----
 library(pmedds.core)
 mydata = get.data(dataType="CDC",national=TRUE,job.year=2007)
 Baseline = mean(c(head(mydata$cases),tail(mydata$cases)))
 DatWidth = setup.ini.halfwidth.HalfPeakWidth(cases=mydata$cases,Baseline=Baseline)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
