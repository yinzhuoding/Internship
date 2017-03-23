\name{plot.base}
\alias{plot.base}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
For a single base plots EPI, specific humidity and school schedule
}
\description{
Plots the EPI profile, weekly averaged specific humidity and if available school closure dates for the 2009-2010 FY. 
}
\usage{
plot.base(myName=mydata$dataName,week=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name="X11",log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myName }{
Descriptive name for base includes the zip code and FY
}
  \item{week }{
Array with week numbers using CDC convention
}
  \item{epi }{
Weekly ILI numbers
}
  \item{sh }{
weekly averaged specific humidity
}
  \item{school }{
Weekly school schdeule (closed=1 open =NA)
}
  \item{device.name }{
Device name for plotting, X11 pdf or png}
  \item{log }{
Name of log file for the run - messages are written to log file and to screen
}
}

\value{
	Function does not return any value - only creates a plot
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
## plot.base(myName=mydata$dataName,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile)

## The function is currently defined as
plot.base <- function(myName=NULL,week=NULL,epi=NULL,sh=NULL,school=NULL,device.name="X11",log=NULL) {
      #replace the zero's in school data with NA - better for plotting
      school[school == 0] <- NA
      school[school != NA] <- 1
      nweeks <- length(week)
      
      # check to see if "data" sub-directory exists, if not create it
      subDir = getwd()
      subDir <- paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") {
            fname <- paste(subDir,"/",myName,"-epi-sh-school.pdf",sep="")
            pdf(file=fname,width=9,height=6)
            cat("\n\n For a PDF Plot of Base Profile, Specific Humidity and School Closure See: ",fname,"\n\n")
            text <- paste(" For a PDF Plot of Base Profile, Specific Humidity and School Closure See: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      } else {
            dev.next()
            dev.new(width=9,height=6)	
      }
      
      par(mfcol=c(1,1),mar=c(4,4,2,4))
      
      title <- myName
      plot(epi,type="l",col="red",lwd=2,ylab="",xlab="Week FY 2009-2010",xaxt="n",main=title,col.axis="red")
      legend("topleft",legend=c("ILI","SH","School"),col=c("red","blue","cyan"),bty="n",lwd=2)
      factor <- max(epi)/2
      lines((school*factor),type="l",lwd=10,col="cyan")
      par(new=TRUE)
      plot(sh,type="l",col="blue",lwd=2,ylab="",xlab="",xaxt="n",yaxt="n")
      axis(side=4,col.axis="blue")
      axis(side=1,at=1:nweeks,label=week)
      mtext("Incidence",side=2,line=3,cex=1,col="red")
      mtext("Specific Humidity (kg/kg)",side=4,line=3,cex=1,col="blue")
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") dev.off()
      if (device.name == "X11") return()
      
      if (device.name == "png" | device.name == "PNG" | device.name == "pdf" | device.name == "PDF") {
            fname <- paste(subDir,"/",myName,"-epi-sh-school.png",sep="")
            png(file=fname,width=720,height=480)
            cat("\n\n For a PNG Plot of Base Profile, Specific Humidity and School Closure See: ",fname,"\n\n")
            text <- paste(" For a PNG Plot of Base Profile, Specific Humidity and School Closure See: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      } else {
            dev.next()
            dev.new(width=9,height=6)	
      }
      
      par(mfcol=c(1,1),mar=c(4,4,2,4))
      
      title <- myName
      plot(epi,type="l",col="red",lwd=2,ylab="",xlab="Week FY 2009-2010",xaxt="n",main=title,col.axis="red")
      legend("topleft",legend=c("ILI","SH","School"),col=c("red","blue","cyan"),bty="n",lwd=2)
      factor <- max(epi)/2
      lines((school*factor),type="l",lwd=10,col="cyan")
      par(new=TRUE)
      plot(sh,type="l",col="blue",lwd=2,ylab="",xlab="",xaxt="n",yaxt="n")
      axis(side=4,col.axis="blue")
      axis(side=1,at=1:nweeks,label=week)
      mtext("Incidence",side=2,line=3,cex=1,col="red")
      mtext("Specific Humidity (kg/kg)",side=4,line=3,cex=1,col="blue")
      if (device.name == "png" | device.name == "PNG" | device.name == "pdf" | device.name == "PDF") dev.off()	
      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
