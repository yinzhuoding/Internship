\name{plot.ini.mcmc}
\alias{plot.ini.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the EPI profile using the initial guess for the MCMC parameters
}
\description{
Plots the EPI base profile along with the initial guess for the fitted profile.  Currently not used.  The initial guess is likely very different from the final result of the robust MCMC fitting procedure.
}
\usage{
plot.ini.mcmc(zip = "23708", weeks=mydata$weeks, epi=mydata$cases, sh=mydata$sh, school=mydata$school, dsdt=NULL, device.name = "X11")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zip}{
The 5-digit zip code of the base
}
  \item{weeks}{
An array with week numbers using the CDC convention
}
  \item{epi}{
The weekly ILI number of cases
}
  \item{sh}{
The weekly averaged specific humidity for the military installation
}
  \item{school}{
The school schedule: 0=open, 1=closed
}
  \item{dsdt}{
Our initial guess for the ILI profile
}
  \item{device.name}{
Device name for the plot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
No return value
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (zip = NULL, weeks=NULL, epi=NULL, sh=NULL, school=NULL, dsdt=NULL, device.name = "pdf") 
{
    nweeks <- length(weeks)
    title = " Initial Guess for Profile"
    name <- paste("plot.ini.mpz.", zip, sep = "")
    if (device.name == "pdf" | device.name == "PDF") {
        fname <- paste(fname, ".pdf", sep = "")
        pdf(file = fname, width = 9, height = 7)
        cat("\n Initial MCMC profile written to: ", fname, "\n")
    }
    else {
        dev.new()
        dev.next(width = 9, height = 6)
    }
    plot(epi, type = "l", col = "blue", lwd = 2, xaxt = "n", 
        xlab = "Week Number FY 2009-2010", ylab = "Incidence", 
        main = title)
    lines(dsdt, type = "l", col = "grey", lwd = 2, xaxt = "n")
    axis(1, at = 1:nweeks, label = weeks)
    legend("topright", legend = paste("MPZ - ", zip, sep = ""), 
        bty = "n", text.col = "blue")
    if (device.name == "pdf" | device.name == "PDF") 
        dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
