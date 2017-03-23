\name{plot.car.mcmc}
\alias{plot.car.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the ILI cumulative attack rate 
}
\description{
Plots the input weekly ILI cumulative attack rate (red) along with our best estimates (in grey) as a function of FY week
}
\usage{
plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt,device.name=ptab$device,log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myName}{
  A descriptive name for the data includes the data type, year, zip name or region number
}
  \item{job.year}{
  The starting year for FY
}
  \item{imodel}{
  Model number
}
  \item{reals}{
  Number of MCMC chains in this run
}
  \item{weeks}{
  	The EPI weeks using CDC convention
  }
  \item{epi}{
  The EPI data profile
}
  \item{dsdt}{
  The best estimate from the MCMC chain
}
  \item{device.name}{
   device name for plotting
}
 \item{log}{
 	Name of log file for the run
 }
}

\value{
%%  ~Describe the value returned
    No return value
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt,device.name=ptab$device,log=logfile)

## The function is currently defined as
function(myName="",job.year=2009,imodel=5,reals=1,weeks=NULL,epi=NULL,dsdt=NULL,roft=NULL,device.name="X11",log=NULL) {
      nweeks <- length(weeks)
      title =title =paste(" Cumulative Attack Rate - ",myName,sep="")
      
      # check to see if "data" sub-directory exists, if not create it
      subDir = getwd()
      subDir <- paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }

      fbase <- paste(subDir,"/car-",myName,sep="")
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
            fname <- paste(fbase,".pdf",sep="")
            pdf(file=fname,width=9,height=7)
            cat("\n PDF Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n")
            text <- paste(" PDF Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      }  else {
            dev.next()
            dev.new()		
      }
      
      par(mfcol=c(1,1),mar=c(5,5,5,5))
      cumsum.epi <- cumsum(epi)
      cumsum <- matrix(0.0,nr=nweeks,nc=reals)
      for (i in 1:reals) {
            cumsum[,i] <- cumsum(dsdt[,i])
      }
      ylim <- c(0,max(cumsum.epi,cumsum))
      xtitle = paste("Week Number FY ",job.year,"-",job.year+1,sep="")
      plot(cumsum.epi,type="l",col="red",lwd=2,xaxt="n",xlab=xtitle,ylab="C.A.R",main=title,ylim=ylim)
      for (ix in 1:reals) {
            lines(cumsum[,ix],type="l",col="grey",lwd=2,xaxt="n")		
      }
      axis(1,at=1:nweeks,label=weeks)
      legend("topleft",legend=c("data","model"),col=c("red","grey"),bty="n",lwd=2)
      
      
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      if (device.name=="X11") return()
      
      # now make the same plots for a PNG file 
      
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
            fname <- paste(fbase,".png",sep="")
            png(file=fname,width=620,height=420)
            cat("\n PNG Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n")
            text <- paste(" PNG Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
            
      }  else {
            dev.next()
            dev.new()		
      }
      par(mfcol=c(1,1),mar=c(5,5,5,5))
      
      plot(cumsum.epi,type="l",col="red",lwd=2,xaxt="n",xlab=xtitle,ylab="C.A.R",main=title,ylim=ylim)
      for (ix in 1:reals) {
            lines(cumsum[,ix],type="l",col="grey",lwd=2,xaxt="n")		
      }
      axis(1,at=1:nweeks,label=weeks)
      legend("topleft",legend=c("data","model"),col=c("red","grey"),bty="n",lwd=2)
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
