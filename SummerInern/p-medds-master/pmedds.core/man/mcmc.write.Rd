\name{mcmc.write}
\alias{mcmc.write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a binary R-object file with the chain and model information for each MCMC chain
}
\description{
Writes a binary R-data file with the chain and model information for each MCMC chain. 
The RData file contains all the information needed to reconstruct a run: the data, model parameters, run parameters and the complete history of the MCMC chain.   A separate file is written for each chain. The function also calculates the statistics for the chain and writes this into three separate files: param-stats-myName.csv param-table-myName.csv and param-quantiles-myName.csv (where myName is a descriptive name for the ILI data). The R-package "coda" is required for this function.
}
\usage{
mcmc.write(tab=sol$tab,model=model,ireal=ix,nweeks=mydata$nweeks,myName=mydata$dataName,accept=sol$accept,log=logfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
     The matrix with the history of the MCMC chain
}
  \item{model}{
    A list with parameters for the model
}
  \item{ireal}{
   The MCMC chain number
}
  \item{nweeks}{
  	The number of weeks in the ILI profile
  }
  \item{myName}{
  	Descriptive name for the run. Includes data type, region or zip code and year
  }
  \item{accept}{
  	The acceptance rate for the MCMC chain
  }
  \item{log}{
    The name of the log file for the run
}
}

\value{
%%  ~Describe the value returned
    A list with the following items:
    \item{mcmc }{An MCMC object, see the "coda" package documentation for more details}
    \item{mean.llk }{An array with the mean LLK values of all the MCMC chains}
    \item{chain.param }{100 randomly selected sets of paramaters from the MCMC chain}
}
\references{
Riley P, Ben-­Nun M, Armenta R, Linker JA, Eick AA, et al. (2013) Multiple Estimates of Transmissibility for the 2009 Influenza Pandemic Based on Influenza-­like-­Illness Data from Small US Military Populations. PLoS Comput Biol 9: e1003064. doi:10.1371/journal.pcbi.1003064.
}
\author{
Predictive Science Inc.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	mcmc.write(tab=sol$tab,model=model,ireal=ix,nweeks=mydata$nweeks,myName=mydata$dataName,accept=sol$accept,log=logfile)

## The function is currently defined as
function (tab=NULL,model=5,ireal=1,nweeks=66,myName="",accept=NULL,log=log) {
      
      # convert MCMC output back to matrix and to an MCMC object
      nparam <- length(model$base_ps)
      tab <- matrix(tab,nc=(nparam+1))
      colnames(tab) <- c(model$vecnames,"AICc")
      # convert the negLLK to actual AICc score
      

      tab[,"AICc"] <- 2.0 * tab[,"AICc"]+2.0*nparam
      tab[,"AICc"] <- tab[,"AICc"] + (2*nparam*(nparam+1))/(nweeks-nparam-1) 
	  
	  #this overcomes a bug in R - if the parameter was not optimized some of the sttas comes out as NA instead of zero.
	  #this seems to happen only for the population and rally only when it is large (GFT/GFT+/CDC cases)
	  if (!("N" \%in\% model$vecopt)) {
	  	Nsave = tab[,"N"] #save the population
		tab[,"N"]  = tab[,"AICc"] #replace with any column
	  }
	  imax = dim(tab)[1]	
	   
	  #In the case of imodel = 5
	  #Also build the RA and RB vectors so we can report statistics for them, and for R0 (which is the maximum of the two) This is done only in the case of model=5
      #we will add these three variables to 'tab' so the mcmc statistics is immediately reported on them also
      
	  tabR = tab[,c("R0min","delta","N")] #just need 3-columns we over-write the numbers below
	  colnames(tabR) = c("RA","RB","R0")

	  if (model$imodel == 5) {
	  	tabR[,"RA"]=tab[,"R0min"]
	  	tabR[,"RB"]=tab[,"R0min"]*(1.+tab[,"delta"])
	  	for (i in 1:imax) {
	  		tabR[i,"R0"] = max(tabR[i,"RA"],tabR[i,"RB"])
	  	}
		cat(mean(tabR[,"R0"]),"\n")
	  	#attach both tables so we immediately get all the statistics on these new parameters also
	  	tab = cbind(tab,tabR)
		cat(mean(tab[,"R0"]),"\n")
	  }
	 
      # how many steps to burn - here we set it to half which is very rigid
      iburn <- imax/2
	  
	  #This mcmc object is only from 'iburn' and up and used only for the purpose of the statistics
	  mcmc <- mcmc(data=tab[iburn:imax,],start=(model$ithin*iburn),end=model$nMCMC,thin=model$ithin) 	  
	  
      # check to see if "data" sub-directory exists, if not create it
      subDir <- getwd()
      subDir = paste(subDir,"/output",sep="")
      
      if (!file.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, " Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }
      # here write an R data file	
      # print the chains statistics to the screen-only for optimized variables
	  if (model$imodel == 5) {
	  	print(summary(mcmc[,c(model$vecopt,"AICc","RA","RB","R0")],quantiles=c(0.05,0.25,0.5,0.75,0.95),digits=4))
	  } else {
	  	print(summary(mcmc[,c(model$vecopt,"AICc")],quantiles=c(0.05,0.25,0.5,0.75,0.95),digits=4))
	  }
      
      # create csv file with mcmc statistics
      mcmc.summary <- summary(mcmc,quantiles=c(0.05,0.25,0.5,0.75,0.95))
      # this is a bug in R - when a parameter is fixed sometimes NA comes in the Time-series SE
      
      mcmc.quantiles <- mcmc.summary$quantiles
      mcmc.stats     <- mcmc.summary$statistics
      
      #now need to replace back the proper values for N if needed
	  if (!("N" \%in\% model$vecopt)) {
	  	tab[,"N"] = Nsave 
		mcmc.quantiles["N",] = tab[1,"N"]
		mcmc.stats["N",] = 0
		mcmc.stats["N",1]    = tab[1,"N"]
	  }      
      mcmc.stats[is.na(mcmc.stats)] = 0
      # retrive the mean AICc of this chain
      mean.llk <- mcmc.stats["AICc","Mean"]
      
      # randomly select 100 sets of parameters from the chain
      myparam <- matrix(nr=100,nc=length(model$vecnames))
      colnames(myparam) <- model$vecnames
      for (i in 1:100) {
            irnd <- runif(1,min=(dim(tab)[1]/2),max=dim(tab)[1])
            irnd <- floor(irnd)
            myparam[i,] <- tab[irnd,model$vecnames]       
      }
      
     
      # give the parameter names to the rows
      if (model$imodel == 5) {
      	rownames(mcmc.quantiles) <- c(model$vecnames,"AICc","RA","RB","R0")
      	rownames(mcmc.stats)     <- c(model$vecnames,"AICc","RA","RB","R0")
      } else {
    	rownames(mcmc.quantiles) <- c(model$vecnames,"AICc")
      	rownames(mcmc.stats)     <- c(model$vecnames,"AICc")      	
      }

      # create two csv files with the stats and quantiles
      filename <- paste(subDir,"/param-stats-",myName,"-",ireal,".csv",sep="")
      write.csv(mcmc.stats,file=filename)
      cat("\n Writing MCMC Statistics for this Chain to File: ",filename,"\n") 
      text <- paste(" Writing MCMC Statistics for this Chain to File: ",filename,"\n",sep=" ")
      writeLines(text=text,con=log)
      filename <- paste(subDir,"/param-quantiles-",myName,"-",ireal,".csv",sep="")
      write.csv(mcmc.quantiles,file=filename)    
      cat("\n Writing MCMC Quantiles for this Chain to File: ",filename,"\n")
      text <- paste(" Writing MCMC Quantiles for this Chain to File: ",filename,"\n",sep=" ")
      writeLines(text=text,con=log)
      # Now write a more condensed Table with only the optimized parameters and Mean, SD and quantiles
      mcmc.both <- cbind(mcmc.stats,mcmc.quantiles)
      colnames(mcmc.both) <- c(colnames(mcmc.stats),colnames(mcmc.quantiles))
      rownames(mcmc.both) <- rownames(mcmc.stats)
      if (model$imodel == 5) {
      	mcmc.both <- mcmc.both[c(model$vecopt,"AICc","RA","RB","R0"),]
      } else {
      	mcmc.both <- mcmc.both[c(model$vecopt,"AICc"),]
      }

      mcmc.both <- mcmc.both[,c("Mean","SD",colnames(mcmc.quantiles))]
      filename <- paste(subDir,"/param-table-",myName,"-",ireal,".csv",sep="")
      write.csv(mcmc.both,file=filename) 
      cat("\n Writing MCMC Condensed Statistics for this Chain to File: ",filename,"\n")
      text <- paste(" Writing MCMC Condensed Statistics for this Chain to File: ",filename,"\n",sep=" ")
      writeLines(text=text,con=log)    
      
      
      cat("\n  Acceptance rate for Chain: ",accept,"\n")
      text <- paste("Acceptance rate for Chain: ",accept,"\n",sep=" ")
      writeLines(text=text,con=log)
      filename <- paste(subDir,"/mcmc-",myName,"-",ireal,".RData",sep="")
      cat("\n Writing R object Data file for this Chain: ",filename,"\n")   
      text <- paste(" Writing R object Data file for this Chain: ",filename,"\n",sep=" ") 
      writeLines(text=text,con=log)
      
      # save the complete chain here
      mcmc <- mcmc(data=tab,start=model$ithin,end=model$nMCMC,thin=model$ithin)
      save(mcmc,model,file=filename)
      save.image()
          
      list(mcmc=mcmc,mean.llk=mean.llk,chain.param=myparam)
      
	}
}
