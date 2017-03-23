\name{plot.allbases}
\alias{plot.allbases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the EPI, SH and School profiles of the bases
}
\description{
This function is Currently not used.  If callled, it plots the information that we have for all the bases for the 2009-2010 pandemic year.  This included the EPI data, SH and for some bases the school schedule data. 
}
\usage{
plot.allbases(epi_data, sh_data, school_data, device.name = "X11",log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{epi_data}{
	List with EPI data for all bases
}
  \item{sh_data}{
	List with SH data for all bases
}
  \item{school_data}{
	List with School schedule data for top 11 bases.  Open == NA closed = 1
}
  \item{device.name}{
	For plots: X11/pdf/png
}
  \item{log}{
  	Name of log file to write information about the plots
  }
}

\value{
No return value
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	plot.allbases(epi=epi_data,sh=sh_data,school=school_data,device.name=device.name,log=logfile)
## The function is currently defined as
plot.allbases <- function(epi_data,sh_data,school_data,device.name="X11",log=NULL) {
      nbase_epi  <- epi_data$nbase
      zip_name   <- epi_data$zipname
      week_epi   <- epi_data$week
      curve_epi  <- epi_data$curve
      nweeks_epi <- length(week_epi)
      colvec     <- rainbow(nbase_epi)
      subDir = getwd()
      subDir = paste(subDir,"/output",sep="")
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") {
            fname <- paste(subDir,"/Allbases-epi-sh-school.pdf",sep="")
            pdf(file=fname,width=7,height=9)
            cat("Plotting EPI, SH and School Schedule for all bases to:",fname,"\n")
            text = paste("Plotting EPI, SH and School Schedule for all bases to:",fname,sep="")
            writeLines(text=text,con=log)
      } else {
            dev.next()
            dev.new(width=7,height=9)		
      }
      par(mfrow=c(3,1),mar=c(3.85,4,0.5,2))
      plot(1:nweeks_epi,curve_epi[,1],type="l",col=colvec[1],xaxt="n",xlab="",ylab="Incidence")
      for (i in 1:nbase_epi) {
            lines(1:nweeks_epi,curve_epi[,i],type="l",col=colvec[i],xaxt="n",ylab="",xlab="")
      }
      axis(1,at=1:nweeks_epi,lab=week_epi)
      
      nbase_sh  <- sh_data$nbase
      curve_sh  <- sh_data$curves
      week_sh   <- sh_data$week
      nweeks_sh <- length(week_sh)
      
      plot(1:nweeks_sh,curve_sh[,1],type="l",col=colvec[1],xaxt="n",xlab="",ylab="Specific Humidity (kg/kg)",ylim=range(curve_sh,na.rm=TRUE))
      for (i in 1:nbase_sh) {
            lines(1:nweeks_sh,curve_sh[,i],type="l",col=colvec[i],xaxt="n",ylab="",xlab="")
      }
      axis(1,at=1:nweeks_sh,lab=week_sh)
      
      nbase_school  <- school_data$nbase
      curve_school  <- school_data$curves
      week_school   <- school_data$week
      nweeks_school <- length(week_school)
      
      plot(1:nweeks_school,curve_school[,1],type="l",col=colvec[1],xlab="Week FY 2009-2010",xaxt="n",ylab="School Closure",ylim=c(0,2))
      for (i in 1:nbase_school) {
            fac <- (i-1)*0.1+1
            lines(1:nweeks_school,(curve_school[,i]*fac),type="l",col=colvec[i],xaxt="n",ylab="",xlab="",lwd=5)
      }
      
      axis(1,at=1:nweeks_school,lab=week_school)
      if (device.name == "pdf" | device.name == "PDF" | device.name="png" | device.name="PNG") dev.off()
      
      #Repeat with png file 
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") {
            fname <- paste(subDir,"/Allbases-epi-sh-school.png",sep="")
			png(file=fname,width=720,height=480)
            cat("Plotting EPI, SH and School Schedule for all bases to:",fname,"\n")
            text = paste("Plotting EPI, SH and School Schedule for all bases to:",fname,sep="")
            writeLines(text=text,con=log)
      } else {
            dev.next()
            dev.new(width=7,height=9)		
      }
      par(mfrow=c(3,1),mar=c(3.85,4,0.5,2))
      plot(1:nweeks_epi,curve_epi[,1],type="l",col=colvec[1],xaxt="n",xlab="",ylab="Incidence")
      for (i in 1:nbase_epi) {
            lines(1:nweeks_epi,curve_epi[,i],type="l",col=colvec[i],xaxt="n",ylab="",xlab="")
      }
      axis(1,at=1:nweeks_epi,lab=week_epi)
      
      nbase_sh  <- sh_data$nbase
      curve_sh  <- sh_data$curves
      week_sh   <- sh_data$week
      nweeks_sh <- length(week_sh)
      
      plot(1:nweeks_sh,curve_sh[,1],type="l",col=colvec[1],xaxt="n",xlab="",ylab="Specific Humidity (kg/kg)",ylim=range(curve_sh,na.rm=TRUE))
      for (i in 1:nbase_sh) {
            lines(1:nweeks_sh,curve_sh[,i],type="l",col=colvec[i],xaxt="n",ylab="",xlab="")
      }
      axis(1,at=1:nweeks_sh,lab=week_sh)
      
      nbase_school  <- school_data$nbase
      curve_school  <- school_data$curves
      week_school   <- school_data$week
      nweeks_school <- length(week_school)
      
      plot(1:nweeks_school,curve_school[,1],type="l",col=colvec[1],xlab="Week FY 2009-2010",xaxt="n",ylab="School Closure",ylim=c(0,2))
      for (i in 1:nbase_school) {
            fac <- (i-1)*0.1+1
            lines(1:nweeks_school,(curve_school[,i]*fac),type="l",col=colvec[i],xaxt="n",ylab="",xlab="",lwd=5)
      }
      
      axis(1,at=1:nweeks_school,lab=week_school)
      if (device.name == "pdf" | device.name == "PDF" | device.name="png" | device.name="PNG") dev.off()
      
      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
