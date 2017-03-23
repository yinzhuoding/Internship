\name{print.info}
\alias{print.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints information about a P-MEDDS ILI run
}
\description{
Prints to screen and to a file information about an ILI run for a given military installation or CDC/GFT/GFT+ region.  Information includes user name, date and time of run, information on the machine, direcotry where job is running and directory where all the data and plots will be saved.
}
\usage{
success <- print.info(dataType=dataType,ptab=ptab,model=model,mydata=mydata,job.name=job.name,job.year=job.year,subDir="output",log=logfile) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataType }{The dataType we are modeling.  MPZ/CDC/GFT/GFTPlus}
  \item{ptab }{A list with parameters set by the user: number of MCMC chains, number of steps in each MCMC chain, model number, debug, verbose and plot (TRUE/FALSE for last three)}
  \item{model }{A list which holds the names of parametrs that will be optimized (as well as other parameters)}
  \item{mydata }{The complete data loaded with the get.data function}
  \item{job.name }{job name set by the user}
  \item{job.year }{starting FY year for modeling}
  \item{subDir }{directory where plots, tables and binary files will be saved}
  \item{log }{name of log file for this run}
}

\value{
 Returns TRUE if function executed correctly
%% ...
}
\author{
Predictive Science inc.}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	success <- print.info(dataType=dataType,ptab=ptab,model=model,mydata=mydata,job.name=job.name,job.year=job.year,subDir="output",log=logfile)

## The function is currently defined as
function(dataType="MPZ",ptab=NULL,model=NULL,mydata=NULL,job.name=" ",job.year=2009,subDir="output",log=NULL) {
      
      reals=ptab$reals
      nMCMC=ptab$nMCMC
      imodel=ptab$model
      ibase=mydata$base
      iregion=mydata$region
      debug=ptab$debug
      verbose=ptab$verbose
      plot=ptab$plot
      device=ptab$device
      Tg = ptab$Tg  #in ptab Tg is in days
      zipname=mydata$zipname
      dataName=mydata$dataName
      N = mydata$npop
	  national = mydata$national
	  
      vecopt=model$vecopt
      vecopt=paste(vecopt,collapse=" ",sep=" ")
      
      subDir=paste(getwd(),"/",subDir,sep="")
      s <- Sys.getenv() # This will give a long list of environment variables. To see the naems use names(s)
      m <- Sys.info()   # This will give a lot of information on her hardware and OS
      cat("\n\n P-MEDDS Package Version 001 \n\n")
      cat(" Job Name: ",job.name)
      
      cat(" Job Started on: ",format(Sys.time(),"\%a \%b \%d \%X \%Y"),"\n")
      cat(" Job Running on: ",m[["nodename"]],"\n")
      cat(" OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n")
      cat(" Job Running by User: ",s["USER"],"\n")
      cat(" Job Running in Directory: ",getwd(),"\n")
      cat(" All Data and Plots Saved in Sub-Directory: ",subDir,"\n")
      cat(" Modeling FY ",job.year,"-",(as.numeric(job.year)+1),"\n")
	  cat(" Modeling ",dataType,"Data ","\n")
      if(dataType == "MPZ") {
        cat(" Modeling Base Number: ",ibase,"\n")      	
      	cat(" Using a Fixed Base Population of: ",as.integer(N),"\n")
      	cat(" Modeling MPZ: ",zipname,"\n")
      } else {
      	if (national == FALSE ) {
      	cat(" Modeling Region Number: ",iregion,"\n")
      	cat(" Using a Fixed Region Population of: ",as.integer(N),"\n")            		
      	} else {
      		cat(" Modeling National US Data \n")
      		cat(" Using a US Population of: ",as.integer(N),"\n") 
      	}
	
      }
      cat(" Using a Fixed Generation Time of: ",(Tg)," days\n")
      cat(" Optimizing the Following Parameters: ",vecopt,"\n")
      cat(" Running ", reals, " MCMC Chains \n")
      cat(" Model Set to Number ",imodel,"\n") #Here more info must be printed
      cat(" Number of MCMC Steps in Each Chain Set to ",nMCMC,"\n")
      cat(" DEBUG set to ",debug,"\n")
      
      text <- paste("\n\n P-MEDDS Package Version 001\n",
                    " Job Name: ",job.name,"\n",
                    " Job Started on: ",format(Sys.time(),"\%a \%b \%d \%X \%Y"),"\n",
                    " Job Running on: ",m[["nodename"]],"\n",
                    " OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n",
                    " Job Running by User: ",s["USER"],"\n",
                    " Job Running in Directory: ",getwd(),"\n",
                    " All Data and Plots Saved in Sub-Directory: ",subDir,"\n",
                    " Modeling FY ",job.year,"-",(as.numeric(job.year)+1),"\n",
                    " Modeling ",dataType,"Data ","\n")
      if (dataType == "MPZ") {
      	text <- paste(text,
      	" Modeling Base Number: ",ibase,"\n",
      	" Using a Fixed Base Population of: ",as.integer(N),"\n",
      	" Modeling MPZ: ",zipname,"\n",sep="")
      } else {
       	text <- paste(text,
      	" Modeling Region Number: ",ibase,"\n",
      	" Using a Fixed Region Population of: ",as.integer(N),"\n",sep="")     	
      }
      
      text <- paste(text,
                    " Using a Fixed Generation Time of: ",(Tg)," days\n",
                    " Optimizing the Following Parameters: ",vecopt,"\n",
                    " Running ", reals, " MCMC Chains","\n",
                    " Model Set to Number ",imodel,"\n",
                    " Number of MCMC Steps in Each Chain Set to ",nMCMC,"\n",
                    " DEBUG set to ",debug,sep="")
      writeLines(text=text,con=log)
      return(TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
