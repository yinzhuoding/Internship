\name{plot.results.mcmc}
\alias{plot.results.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Plots the results of the MCMC fitting procedure
Plot the base EPI profile along with the best MCMC result and 100 randomly selected sets from the MCMC chain.  The plot also shows this information for R(t)
}
\description{
Plots the results of an MCMC fitting for the EPI profile along with the input EPI profile (in red). Our best fit estimate is in blue and 100 randomly selected estimates from the MCMC chain are in grey.  The green lines and right y-axis show our prediction for R(t) with the dark green being our best estimate.  
}
\usage{
plot.results.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,device.name=ptab$device,log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{myName}{
  A descriptive name for the data includes the data type, year, and zip code or region number
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
  \item{sh}{
  The SH data profile
}
  \item{school}{
  The school data profile
}
  \item{dsdt}{
  The best estimate from the MCMC chain
}
  \item{roft}{
  The best estimate for R(t) from the MCMC chain
}
  \item{rnd.dsdt}{
  100 random estimates from the MCMC chain
}
  \item{rnd.rvec}{
  100 random estimates for R(t) from the MCMC chain
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
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like­-Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	plot.results.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,reals=model$reals,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,device.name=ptab$device,log=logfile)

## The function is currently defined as
function(myName="",job.year=2009,imodel=5,weeks=NULL,epi=NULL,sh=NULL,school=NULL,dsdt=NULL,roft=NULL,rnd.dsdt=NULL,rnd.rvec=NULL,device.name="X11",log=log) {
      nweeks <- length(weeks)
      title =" Current Estimate for Profile"
      school[school == 0] <- NA #for plotting we only show when the school is closed
      # check to see if "data" sub-directory exists, if not create it
      subDir = getwd()
      subDir = paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }
      fbase <- paste(subDir,"/plot-",myName,sep="")
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
      	    fname = paste(fbase,".pdf",sep="")
            pdf(file=fname,width=9,height=7)
            cat("\n PDF Plot of MCMC Profiles written to: ",fname,"\n")
            text <- paste("\n PDF Plot of MCMC Profiles written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      }  else {
            dev.next()
            dev.new()		
      }
      par(mfcol=c(1,1),mar=c(5,5,5,5))
      ylim <- ylim <- c(0,max(epi,dsdt,rnd.dsdt))
      xlab.title = paste("Week Number FY ",job.year,"-",(job.year+1),sep="")
      plot(epi,type="l",col="red",lwd=2,xaxt="n",xlab=xlab.title,ylab="Incidence",main=title,ylim=ylim)
      irnd <- dim(rnd.dsdt)[2]
      #In grey we plot 100 random estimates from the best chain
      for (ix in 1:irnd) {
            lines(rnd.dsdt[,ix],type="l",col="grey",lwd=1,xaxt="n")
            
      }	
      
      #In Blue we plot the best estimate from the best chain
      lines(dsdt,type="l",col="blue",lwd=2,xaxt="n")		
	  #Red is the EPI profile we are fitting
      lines(epi,col="red",lwd=2,type="l",xaxt="n")
      # for models that take the school schedule into account we will plot it
      if (imodel == 1 | imodel == 3) {
            factor <- 0.5 * max(epi,dsdt)
            lines((school*factor),type="p",lwd=5,col="cyan")
      }
      axis(1,at=1:nweeks,label=weeks,col.axis="black")
      legend("topleft",legend=c("data","Best MCMC","Random MCMC"),col=c("red","blue","grey"),bty="n",lwd=2)
      
      
      par(new=TRUE)
      R.ylim=c(0,4)
      R.ylim=c(min(roft,rnd.rvec)*0.8,max(roft,rnd.rvec)*1.2)
      
      plot(roft,type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",xlab="",ylab="",ylim=R.ylim)
      
      for (ix in 1:irnd) {
            lines(rnd.rvec[,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n")
      }
      
      lines(roft,type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n")

      axis(4,col.axis="green")
      mtext(text="R(t)",side=4,line=2,las=2,col="green")
      
      legend("topright",legend=c("Best MCMC","Random MCMC"),col=c("darkgreen","green"),bty="n",lwd=2)
      
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      if (device.name == "X11") return()
      
      # now make the same plots for a PNG file 
      
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
            fname <- paste(fbase,".png",sep="")
            png(file=fname,width=620,height=420)
            cat("\n PNG Plot of MCMC Profiles written to: ",fname,"\n")
            text <- paste(" PNG Plot of MCMC Profiles written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      }  else {
            dev.next()
            dev.new()		
      }
      par(mfcol=c(1,1),mar=c(5,5,5,5))
      
      plot(epi,type="l",col="red",lwd=2,xaxt="n",xlab=xlab.title,ylab="Incidence",main=title,ylim=ylim)
      for (ix in 1:irnd) {
            lines(rnd.dsdt[,ix],type="l",col="grey",lwd=1,xaxt="n")
            
      }	
      lines(dsdt,type="l",col="blue",lwd=2,xaxt="n")		
      lines(epi,col="red",lwd=2,type="l",xaxt="n")
      
      # for models that take the school schedule into account we will plot it
      if (imodel == 1 | imodel == 3) {
            factor <- 0.5 * max(epi,dsdt)
            lines((school*factor),type="p",lwd=5,col="cyan")
      }
      axis(1,at=1:nweeks,label=weeks,col.axis="black")
      legend("topleft",legend=c("data","Best MCMC","Random MCMC"),col=c("red","blue","grey"),bty="n",lwd=2)
      
      
      par(new=TRUE)
      plot(roft,type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",xlab="",ylab="",ylim=R.ylim)
      for (ix in 1:irnd) {
            lines(rnd.rvec[,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n")
      }
      lines(roft,type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n")

      axis(4,col.axis="green")
      mtext(text="R(t)",side=4,line=2,las=2,col="green")
      legend("topright",legend=c("Best MCMC","Random MCMC"),col=c("darkgreen","green"),bty="n",lwd=2)
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
