\name{plot.mcmc.chain}
\alias{plot.mcmc.chain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots the history of the MCMC chain parameters
}
\description{
Plots all the parameters of an MCMC chain that are optimized along with the value of the log-likelihood. 
The complete history that was saved is plotted. 
}
\usage{
plot.mcmc.chain(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
  The matrix that holds the history of the MCMC chain
}
  \item{model}{
  A list that holds many parameters for the model
}
  \item{ireal}{
  	The MCMC chain number.  Used to build the file name for the plot
  }
  \item{myName}{
  A descriptive name for the run includes the data type, region number or zip code and the year
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
Riley P, Ben­-Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza­-like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	plot.mcmc.chain(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile)

## The function is currently defined as
function(tab=NULL,model=NULL,ireal=1,myName="",device.name="X11",log=log) {

      nparam  <- length(model$base_ps)
      tab <- matrix(tab,nc=(nparam+1))
      colnames(tab) <- c(model$vecnames,"AICc")
      vecnames <- model$vecnames
      vecopt   <- model$vecopt
      zipname  <- myName
      iburn    <-  1 # dim(tab)[1]/2 plot the complete chain

      tab.plot <- tab[iburn:dim(tab)[1],c(vecopt,"AICc")]
      nopt <- length(vecopt)
      nc = 2
      if (nopt %% 2 == 0) {
            nr = nopt/2
      } else {
            nr = nopt/2 + 1
      }
      title ="MCMC Chains"
      
      # check to see if "data" sub-directory exists, if not create it
      subDir = getwd()
      subDir <- paste(subDir,"/output",sep="")
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }

      fbase <- paste(subDir,"/chain-",myName,"-",ireal,sep="")
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
            fname <- paste(fbase,".pdf",sep="")
            pdf(file=fname,width=9,height=7)
            cat("\n PDF Plot of MCMC Chain written to: ",fname,"\n")
            text <- paste(" PDF Plot of MCMC Chain written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      }  else {
            dev.next()
            dev.new()		
      }
      
      title = paste("MCMC Chain Number: ",ireal,sep="")
      par(mfcol=c(nr,nc)) 
      for (i in 1:nopt) {
            ylab = colnames(tab.plot)[i]
            ymin = model$model$par.min[ylab]
            ymax = model$model$par.max[ylab]
            main = ""
            if (i == 1) main=title
            plot(tab.plot[,i],xlab="",ylab=ylab,type="p",ylim=c(ymin,ymax),main=main)
      }
      ylab = colnames(tab.plot)[(nopt+1)]
      plot(tab.plot[,(nopt+1)],xlab="",ylab=ylab,type="p",main="")
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      if (device.name == "X11") return()
      
      #repeat with png file
      if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
            fname <- paste(fbase,".png",sep="")
            png(file=fname,width=620,height=600)
            cat("\n PNG Plot of MCMC Chain written to: ",fname,"\n")
            text <- paste(" PNG Plot of MCMC Chain written to: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      }  else {
            dev.next()
            dev.new()		
      }
      
      par(mfcol=c(nr,nc)) 
      for (i in 1:nopt) {
            ylab = colnames(tab.plot)[i]
            ymin = model$model$par.min[ylab]
            ymax = model$mode$par.max[ylab]
            main = ""
            if (i == 1) main=title		
            plot(tab.plot[,i],xlab="",ylab=ylab,type="p",ylim=c(ymin,ymax),main=main)
      }
      ylab = colnames(tab.plot)[(nopt+1)]
      plot(tab.plot[,(nopt+1)],xlab="",ylab=ylab,type="p",main="")	
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()	
      
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
