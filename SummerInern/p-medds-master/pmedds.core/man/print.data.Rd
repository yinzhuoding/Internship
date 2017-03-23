\name{print.data}
\alias{print.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints information about a data object.
}
\description{
	If running with debug = TRUE the call to print.data will trigger printing of an object which can be of class Epi, SH, School or Population.  The amount of printing will depend on the value of verbose (TRUE/FALSE), and the class will depend on what 'x' is.
}
\usage{
print.data(x, verbose = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The object that will be printed of class EPI/SH/School/Pop
}
  \item{verbose}{
Controls the amount of printing TRUE or FALSE
}
}

\value{
	No value is returned.  Information is printed to the screen
%% ...
}
\author{
Predictive Science inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	print.data(x=epi_data,verbose=FALSE)

## The function is currently defined as

print.data <- function(x=NULL, verbose=FALSE) {
      cat("\n\n")
      if (class(x) == "epi")    cat("Information for EPI Data: ",x$nbase," bases weekly Incidence Numbers\n\n")
      if (class(x) == "sh")     cat("Information for Specific Humidity Data: ",x$nbase," bases weekly averaged SH\n\n")
      if (class(x) == "school") cat("Information for School Data: ",x$nbase," bases 0 or 1 (open/closed)\n\n")
      if (class(x) == "pop")    cat("Base Population For ", x$nbase," bases\n\n")
      
      if(class(x) != "pop") {
            cat("Dates: \n\n")
            print(x$date)
            cat("\n\n")
            cat("MPZs: \n\n")
            print(x$zipname)
            if (verbose == TRUE) {
                  if (class(x) == "epi") cat("Incidence: \n\n")
                  if (class(x) == "sh")  cat("Specific Humidity (kg/kg): \n\n")
                  if (class(x) == "school") cat("School: \n")
                  print(x$curves)      	
            }		
      } else { #For the population
            xdata <- data.frame(zipname=x$zipname,pop=x$pop)
            if (verbose == TRUE) {
                  cat("MPZs: \n\n")
                  print(x$zipname)
                  cat("Base population: \n\n")
                  print(x$pop)
            }
      }
      
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
