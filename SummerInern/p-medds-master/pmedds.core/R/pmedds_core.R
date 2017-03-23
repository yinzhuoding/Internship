##
## If the example.drive.R is invoked with the --interactive flag this function will help the user set all the parameters needed for an ILI run
##

set.interactive <- function() {
	
	cat("\n\n Welcome to the P-MEDDS Interactive Example Driver Script","\n\n")
	cat("Please Enter a Job Name (e.g. test.pmedds) \n")
	fil <- readLines(con="stdin", 1)
	job.name=as.character(fil)
	#here split between W-T procedure for SARS or compramental model for ILI
	cat("Please choose type of data to model: CDC, GFT, GFTPlus, MPZ ","\n")
	fil <- readLines(con="stdin", 1)
	dataType=fil
	if (dataType == "CDC" | dataType == "GFT" | dataType == "GFTPlus") {
		cat("Please enter starting Year for Modeling, range is 1997-2013 for CDC and GFT/GFTPlus 2003-2013 \n")
		fil <- readLines(con="stdin", 1)
		job.year=as.numeric(fil)
		if (dataType == "CDC") {
			if (job.year < 1997 | job.year > 2013) {
				cat("\n Starting FY not in correct range, defaulting to 2009-2010 pandemic year \n")
				job.year = 2009
			}
		}  else {
			if (job.year < 2003 | job.year > 2013) {
				cat("\n Starting FY not in correct range, defaulting to 2009-2010 pandemic year \n")
				job.year = 2009
			}			
		}
		
		cat("Please choose National or Regional Modeling (N or R) \n")
		fil <- readLines(con="stdin", 1)
		if (fil == "R") {
	     cat("Please choose Region number for modeling, options are 1-10 ","\n")
	     cat("Region 1: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and Vermont \n")
	     cat("Region 2: New Jersey, New York, Puerto Rico, and the U.S. Virgin Islands \n")
	     cat("Region 3: Delaware, District of Columbia, Maryland, Pennsylvania, Virginia, and West Virginia \n")
	     cat("Region 4: Alabama, Florida, Georgia, Kentucky, Mississippi, North Carolina, South Carolina, and Tennessee \n")
	     cat("Region 5: Illinois, Indiana, Michigan, Minnesota, Ohio, and Wisconsin \n")
	     cat("Region 6: Arkansas, Louisiana, New Mexico, Oklahoma, and Texas\n")
	     cat("Region 7: Iowa, Kansas, Missouri, and Nebraska \n")
	     cat("Region 8: Colorado, Montana, North Dakota, South Dakota, Utah, and Wyoming \n")
	     cat("Region 9: Arizona, California, Hawaii, and Nevada \n")
	     cat("Region 10: Alaska, Idaho, Oregon, and Washington \n")
	     fil <- readLines(con="stdin", 1) 
	     iregion = as.numeric(fil)
	     if (iregion < 1 | iregion > 10) {
	     	cat("\n\n Region Entered not in Correct 1-10 Range, Code will Default to Region 1 \n\n")
	     	iregion = 1
	     }
	     national = FALSE
		} else {
		 national = TRUE
		 iregion = 1 #will be ignored but must set it
		}
		#Not used in the case of CDC/GFT/GFTPlus but must set to some value
    	myMPZ=23708 
    	ibase = 1


	} else {
		cat("Please Enter A Military Zip Code For Modeling from this List: ",'\n')
		zipnames = get.mpz.names()
		print(zipnames)
		fil <- readLines(con="stdin", 1)
		ibase = which(zipnames == fil)
		if(length(ibase) == 0) {
			ibase = 1
			cat("Wrong Militray Zip Code, Reverting to First MPZ: ",zipnames[ibase],"\n")
		}
		
		#get base MPZ 
		myMPZ = zipnames[ibase]
        job.year = 2009 #Currently modeling only the pandemic year
        # Ignored in the case of MPZ modeling but we have to set them to some value
        iregion=1
        national=T
	}
	
	cat("Please Enter Model Number For the Basic Reproduction Number, R(t): \n")
	cat("1: R(t) R0 times a school and specific humidity terms \n")
	cat("2: R(t) is R0 times a specific humidity term \n")	
	cat("3: R(t) is R0 times a school schedule term \n")
	cat("4: R(t) is fixed in time, R(t) == R0\n")
	cat("5: R(t) changes from RA to RB and then back to RA\n")
	cat("For Civilian Data (CDC and GFT/GFTPlus) only options 2, 4 & 5 are possible\n")
	fil <- readLines(con="stdin", 1)
	imodel = as.integer(fil)
	if (imodel == 1 | imodel == 3 | imodel > 5 & dataType != "MPZ") {
		cat("\n\n For Civilian Data Please Choose between models 2, 4 or 5 \n\n")
		fil <- readLines(con="stdin", 1)
		imodel = as.integer(fil)		
	}
	if (imodel > 5 | imodel < 1) {
		cat("\n\n\ Error in Model Number, defaulting to Model Number 5 \n\n")
		imodel = 5
	}
	
	cat("Please Enter the Infectious Period in Days (2.6 days is the accepted value for ILI) \n")
	fil <- readLines(con="stdin", 1)
	Tg = as.numeric(fil)
	if (length(Tg) == 0) {
		cat("\n\n Error in value for Tg, defaulting to 2.6 days \n\n")
		Tg = 2.6 
	}
	cat("Please Enter the Number of MCMC Chains \n")
	cat("For a quick run use 1 for a converged run use 3-5 \n")
	fil <- readLines(con="stdin", 1)
	reals = as.integer(fil)
	if (length (reals) == 0 | reals == 0) {
		cat("\n\n Error in number of MCMC chains, deafulting to 1 chain \n\n")
		reals = 1
	}
	cat("Please Enter the number of MCMC steps in each chain\n")
	cat("Example values are: 1e5 for a quick run, 1e7 for a converged run \n")
	fil <- readLines(con="stdin", 1)
	nMCMC = as.integer(fil)	
	if(length(nMCMC) == 0) {
		cat("\n Error in number of MCMC step, defaulting to 1e5 \n")
		nMCMC=1e5
	}
	#Number of lines in MCMC output files
	nlines = min(1e4,nMCMC) 
	if (length(nlines) == 0) nlines = 1e4

	cat("Please Enter the Seed for the Random Number Generator \n")
	cat("Using this Same Seed in future runs will reproduce the results of this run \n")
	cat("Using 0 will trigger seeding by the code \n")
	fil <- readLines(con="stdin", 1)
	iseed = as.integer(fil)
	if(iseed == 0) iseed = NULL
    #Set the debug, verbose and plot to TRUE or FALSE. (plot should really always be true)
	debug = FALSE
	verbose = FALSE
	plot = TRUE    
	# For interactive version makes sense only to set to pdf or png
	device = "pdf" #"pdf" "png"
#	cat("Set device name for plotting X11/pdf/png default is pdf/png\n")
#	fil <- readLines(con="stdin", 1)
#	device = fil

	# Request weighting flag 'wflag'
	cat("Please Enter the method for weekly weights: \n")
	cat("0: All weeks equal (default) \n")
	cat("1: Input user vector (not available in interactive) \n")
	cat("2: Retrieve from database (under development) \n")
	cat("3: Use weights proportional to CDC Total Patients (CDC only) \n")
	fil = readLines(con="stdin",1)
	wflag = as.integer(fil)
	if(wflag!=0 & !(wflag==3 & dataType=="CDC")) {
		cat("\n Error in weekly weight method, defaulting to method 0 \n")
		wflag = 0
	}
	wweight= NULL
	dr     = NULL
	optTg  = FALSE
	RandInit = TRUE
	list(job.name=job.name,dataType=dataType,job.year=job.year,myMPZ=myMPZ,ibase=ibase,iregion=iregion,national=national,imodel=imodel,Tg=Tg,nMCMC=nMCMC,nlines=nlines,reals=reals,debug=debug,verbose=verbose,plot=plot,device=device,iseed=iseed,wflag=wflag,wweight=wweight,dr=dr,optTg=optTg,RandInit=RandInit)
	
}

get.mpz.names <- function() {
      
      data(ILI.small.pandemic.curves.by.zip5.20090320.20100628,package="pmedds.core")
      raw_ili <- ILI.small.pandemic.curves.by.zip5.20090320.20100628
      epi      <- sapply(raw_ili[,3:dim(raw_ili)[2]], as.ts) 

      #db=database()
      #raw_ili=dbReadTable(db,"pH1N1_mpz_zip_cases")      
      #epi      <- sapply(raw_ili[,4:dim(raw_ili)[2]], as.ts)
      
      # get the 5-digit MPZ and remove the "X" from it
      zipnames <- colnames(epi)
      zipnames <- substr(zipnames,start=2,stop=10)
	 zipnames
}


# ---------------------------------------------------
# Load the top 50 EPI data for 2009-2010 
# ---------------------------------------------------

readEpi <- function() {
      
      ##db <- database()
      ##raw_ili <- dbReadTable(db,"pH1N1_mpz_zip_cases")
      
      data(ILI.small.pandemic.curves.by.zip5.20090320.20100628,package="pmedds.core")
      raw_ili <- ILI.small.pandemic.curves.by.zip5.20090320.20100628
      
      #we now extract the week number, ending date for each week and the profiles for the 50 bases
      epi_week <- raw_ili$week
      epi_date <- as.Date(raw_ili$date,format="%m/%d/%y")
      #epi_date <- as.Date(raw_ili$date)
      epi      <- sapply(raw_ili[,3:dim(raw_ili)[2]], as.ts) 
      #epi      <- sapply(raw_ili[,4:dim(raw_ili)[2]], as.ts) 
      
      # get the 5-digit MPZ and remove the "X" from it
      zipname <- colnames(epi)
      zipname <- substr(zipname,start=2,stop=10)
      nbase <- length(zipname)
      nweeks_epi <- length(epi_week)
      list(curves=epi, date=epi_date, week = epi_week, nweeks = nweeks_epi, nbase = nbase, zipname = zipname)      
}

# -----------------------------------------------------
# Load the Specific Humidity for top 50 bases 2009-2010
# -----------------------------------------------------

readSH <- function(nweeks_epi=66) {
      
      data(ILI.small.specific.humidity.by.zip5.20090320.20100628,package="pmedds.core")
      raw_sh <- ILI.small.specific.humidity.by.zip5.20090320.20100628
      
      sh_week <- raw_sh$week
      sh_date <- as.Date(raw_sh$date,format="%m/%d/%y")
      sh      <- sapply(raw_sh[,4:dim(raw_sh)[2]], as.ts) 
      
      # now need to trim the SH data which extend over a longer period than the ILI
      
      nweeks_sh  <- length(sh_week)
      zipname <- colnames(sh)
      zipname <- substr(zipname,start=2,stop=10)
      nbase <- length(zipname)
      nweeks_sh <- length(sh_week)
      
      list(curves=sh, date = sh_date, week = sh_week, nweeks = nweeks_sh, nbase = nbase, zipname = zipname)      
}

# ------------------------------------------------------
# Load the School Schedule for top 10 bases 2000-2010
# _____________________________________________________

readSchool <- function(start,end) {
      
      data(ILI.small.school.closure.by.zip5.20000103.20131230,package="pmedds.core")
      raw_school <- ILI.small.school.closure.by.zip5.20000103.20131230
      school_date <- as.Date(ILI.small.school.closure.by.zip5.20000103.20131230$date,format="%m/%d/%y")
      school      <- sapply(raw_school[,3:dim(raw_school)[2]], as.ts)
      
      range    <- which(school_date >= start & school_date <= end)
      
      school_date <- school_date[range]
      school      <- school[range,] 
      
      # replace '2' with 1  and make sure tehre are no NA
      school[school==2] <-1 
      school[is.na(school)]  <- 0.
      school[is.nan(school)] <- 0.
      zipname <- colnames(school)
      zipname <- substr(zipname,start=2,stop=10)
      nbase <- length(zipname)
      nweeks_school <- length(school_date)
      school_week <- raw_school$week[range] 
      
      # we do not have week number for this data base
      list(curves=school, date = school_date, week = school_week, nweeks = nweeks_school, nbase = nbase, zipname = zipname)
}

# ------------------------------------------------------
# load the population file 
# ------------------------------------------------------

readPopulation <- function(omega=104.3) {
      data(population, package="pmedds.core")
      pop <- population$npop * omega
      zipname <- population$zip
      nbase <- length(zipname)
      list(pop=pop,zipname=zipname,nbase=nbase)
}

# ------------------------------------------------------
# load all the data: epi, school and specific-humidity for MPZs
# ------------------------------------------------------
get.MPZ.data <- function() {
      
      ## 
      ## read ILI curves and determine the number of weeks
      ## --------------------------------------
      epi_data <- readEpi()
      
      ##
      ## read specific humidity 
      ## --------------------------------------
      sh_data <- readSH(epi_data$nweeks)
      
      ##
      ## read school data for the date-range of epi_date
      ## --------------------------------------
      start <- epi_data$date[1]
      end   <- epi_data$date[epi_data$nweeks]
      school_data <- readSchool(start,end)
      
      ## read the base population and multiply by our omega of 104.3
      pop_data <- readPopulation(omega=104.3)
      
      ## define a class for each data
      
      class(epi_data) <-"epi"
      class(sh_data) <- "sh"
      class(school_data) <- "school"
      class(pop_data)    <- "pop"
      
      list(epi=epi_data,sh=sh_data,school=school_data,pop=pop_data,nweeksData=length(epi_data))
}


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


plot.input.data <- function(myName=NULL,FY=NULL,nweeksFit=NULL,nweeksData=NULL,week=NULL,epi=NULL,sh=NULL,school=NULL,device.name="X11",log=NULL, subDir = file.path(getwd(), "output")) {
      #replace the zero's in school data with NA - better for plotting
      school[school == 0] <- NA
      school[school != NA] <- 1
      nweeks <- length(week)
      if(is.null(nweeksFit)) nweeksFit = nweeks
      
      # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") {
            fname <- paste(subDir,"/",myName,"-epi-sh-school.pdf",sep="")
            pdf(file=fname,width=9,height=6)
            cat("\n\n For a PDF Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n\n")
            text <- paste(" For a PDF Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n",sep="")
            writeLines(text=text,con=log)
      } else {
            dev.next()
            dev.new(width=9,height=6)	
      }
      if (nweeksData < nweeks) epi[(nweeksData+1):nweeks] = NA
      par(mfcol=c(1,1),mar=c(4,4,2,4))
      
      title <- myName
      xlab = paste("Week FY ",FY,sep="")
      
      plot(1:nweeks,epi[1:nweeks],type="n",col="red",lwd=2,lty=1,ylab="",xlab=xlab,xaxt="n",main=title,col.axis="red")

      if (nweeksFit < nweeks) {
      	mycolor=col2rgb('thistle')
      	mycolor=as.numeric(mycolor)/256
      	rect(xleft=(nweeksFit),ybottom=0,xright=(nweeks+2),ytop=max(epi*1.5,na.rm=TRUE),col=rgb(red=mycolor[1],green=mycolor[2],blue=mycolor[3],alpha=0.3),border=NA)
      }
      lines(1:nweeksFit     ,epi[1:nweeksFit]     ,type='l',col='red',lwd=2,lty=1,xlab='',ylab='',xaxt='n')
      lines((nweeksFit):nweeks,epi[(nweeksFit):nweeks],type='l',col='red',lwd=2,lty=2,xlab='',ylab='',xaxt='n')
      
      if (all(is.na(school))) {
      	legend("topleft",legend=c("ILI","SH"),col=c("red","blue"),bty="n",lwd=2)
      } else {
      	legend("topleft", legend=c("ILI","SH","School"), col=c("red","blue","cyan"), bty="n", lwd=2)	
      }
      
      factor <- max(epi,na.rm=TRUE)/2
      if (all(is.na(school))) { 
      	cat("\n\n\ No school closure information for this Location\n\n")
      	text <-paste("No school closure information for this Location")
      	if(!is.null(log)) {
      	  writeLines(text=text,con=log)
      	}
      	} else {
      	lines((school*factor),type="p",lwd=5,col="cyan")
      }

      par(new=TRUE)
      plot(sh,type="n",col="blue",lwd=2,ylab="",xlab="",xaxt="n",yaxt="n")
      lines(1:nweeksFit     ,sh[1:nweeksFit]         ,type='l',col='blue',lwd=2,lty=1,xlab='',ylab='',xaxt='n')
      lines((nweeksFit):nweeks,sh[(nweeksFit):nweeks],type='l',col='blue',lwd=2,lty=2,xlab='',ylab='',xaxt='n')      
      
      axis(side=4,col.axis="blue")
      axis(side=1,at=1:nweeks,label=week)
      mtext("Incidence",side=2,line=3,cex=1,col="red")
      mtext("Specific Humidity (kg/kg)",side=4,line=3,cex=1,col="blue")
      if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") dev.off()
      if (device.name == "X11") return()
      
      if (device.name == "png" | device.name == "PNG" | device.name == "pdf" | device.name == "PDF") {
            fname <- paste(subDir,"/",myName,"-epi-sh-school.png",sep="")
            png(file=fname,width=720,height=480)
            cat("\n\n For a PNG Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n\n")
            text <- paste(" For a PNG Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n",sep="")
            if(!is.null(log)) {
              writeLines(text=text,con=log)
            }
      } else {
            dev.next()
            dev.new(width=9,height=6)	
      }
      
      par(mfcol=c(1,1),mar=c(4,4,2,4))
      
      title <- myName
      
      plot(1:nweeks,epi[1:nweeks],type="n",col="red",lwd=2,lty=1,ylab="",xlab=xlab,xaxt="n",main=title,col.axis="red")

      
      if (nweeksFit < nweeks) {
      	mycolor=col2rgb('thistle')
      	mycolor=as.numeric(mycolor)/256
      	rect(xleft=(nweeksFit),ybottom=0,xright=(nweeks+2),ytop=max(epi*1.5),col=rgb(red=mycolor[1],green=mycolor[2],blue=mycolor[3],alpha=0.3),border=NA)
      }
      lines(1:nweeksFit     ,epi[1:nweeksFit]     ,type='l',col='red',lwd=2,lty=1,xlab='',ylab='',xaxt='n')
      lines((nweeksFit):nweeks,epi[(nweeksFit):nweeks],type='l',col='red',lwd=2,lty=2,xlab='',ylab='',xaxt='n')
      factor <- max(epi)/2
      if (!all(is.na(school)))lines((school*factor),type="l",lwd=10,col="cyan")
      
      par(new=TRUE)
      plot(sh,type="n",col="blue",lwd=2,ylab="",xlab="",xaxt="n",yaxt="n")
      lines(1:nweeksFit     ,sh[1:nweeksFit]     ,type='l',col='blue',lwd=2,lty=1,xlab='',ylab='',xaxt='n')
      lines((nweeksFit):nweeks,sh[(nweeksFit):nweeks],type='l',col='blue',lwd=2,lty=2,xlab='',ylab='',xaxt='n')    
      axis(side=4,col.axis="blue")
      axis(side=1,at=1:nweeks,label=week)
      mtext("Incidence",side=2,line=3,cex=1,col="red")
      mtext("Specific Humidity (kg/kg)",side=4,line=3,cex=1,col="blue")
      
      if (device.name == "png" | device.name == "PNG" | device.name == "pdf" | device.name == "PDF") dev.off()	
      
}

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
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      
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
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      
      
}


pmedds.core.debug <- function(epi_data=epi_data,sh_data=sh_data,school_data=school_data,pop_data=pop_data,ibase=1,verbose=FALSE,device.name="X11",log=NULL) {
      
      # print some information about the data files we read.  
      # details depend on the class (epi, sh, school, pop) adn verbose (TRUE/FALSE)
      
      print.data(epi_data,verbose)
      
      print.data(sh_data,verbose)
      
      print.data(school_data,verbose)
      
      print.data(pop_data,verbose)
      
      # for single base plotting - use the ibase
      
      # We have school data for only a subset of bases so we will use the average of the bases we have info for the others
      
      if (ibase > school_data$nbase) {
            school <- rep(NA,school_data$nweeks)
      } else {
            school <- school_data$curves[,ibase]
      }
      
      # plot for a single base: epi, sh and school data
      
      plot.base(zipname = epi_data$zipname[ibase],week=epi_data$week, epi=epi_data$curves[,ibase],sh=sh_data$curves[,ibase],school=school,device.name=device.name,log=log)
      
      # for plotting all the bases - we will plot three panels: EPI, SH and School
      plot.allbases(epi=epi_data,sh=sh_data,school=school_data,device.name=device.name,log=log)
      
}

# ------------------------------------------------------------------
# FUNCTIONS SPECIFICALLY FOR MILITARY MODELING
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# function for setting up model related parameters
# ------------------------------------------------------------------

setup.model <- function (dataType="MPZ",imodel=5,epi=NULL,Tg=2.6,optTg=NULL,npop=1e5,national=FALSE, dr=NULL) {
      
	
      vecnames <- c("R0min","deltaR","aparam","pC","Baseline","Tg","N","t0","alpha","delta","ts","dur")
      
      
      if (imodel == 1) {
            vecopt   <- c("R0min","deltaR","aparam","Baseline","pC","t0","alpha")
            veczero <- c("delta","ts","dur")
      } 
      if (imodel == 2) {
            vecopt   <- c("R0min","deltaR","aparam","Baseline","pC","t0")
            veczero <- c("alpha","delta","ts","dur")
      }
      if (imodel == 3) {
            vecopt   <- c("R0min","Baseline","pC","t0","alpha")
            veczero <- c("deltaR","aparam","delta","ts","dur")
      }
      if (imodel == 4) {
            vecopt <- c("R0min","Baseline","pC","t0")
            veczero <- c("deltaR","aparam","alpha","delta","ts","dur")
      }
      if (imodel == 5) {
            vecopt <- c("R0min","Baseline","pC","t0","delta","ts","dur")
            veczero <- c("deltaR","aparam","alpha")
      }
      
      # in case optTg == TRUE include it in the list of parameters that are optimized
      if (optTg) vecopt = c(vecopt,"Tg")
            
      #number of weeks
      nweeks = length(epi)
      # total number of parameters 
      nparam <- length(vecnames)	
      
      # Number of parameters that will be optimized
      nparam.opt <- length(vecopt)
      #days per week
      days.per.week <- 7
      Tg = Tg / days.per.week
      Baseline = mean(c(head(epi,5),tail(epi,5)))
      if (Baseline < 1) Baseline = 1.
      # default values for parameters they will be overwritten later and zeroed if using vec.zero above
      
      param.default <- c(R0min=1.4, deltaR=0.5,aparam=200.,pC=0.01, Baseline=Baseline,Tg=Tg,N=npop,t0=1,alpha=0.1,delta=0.1,ts=15,dur=5)
      param.default[veczero] <- 0
      
      params_min <- c(R0min=0.1,deltaR=1.e-3,aparam=1. ,pC=1e-6,Baseline=Baseline*1e-1  ,Tg=Tg,N=npop , t0=0.001     ,alpha=1e-6,delta=-1.0, ts=1.0, dur=1. )
      params_max <- c(R0min=4.0,deltaR=2.0  ,aparam=500,pC=1.0 ,Baseline=Baseline*10.0  , Tg=Tg,N=npop, t0=nweeks*0.2,alpha=1.0 ,delta=1.0 , ts=40.0,dur=40.)
      
      # In case of model 4 - R0 is a constant and really should not go below 1 if we want the epidemic to 'take off'
      if (imodel == 4) params_min["R0min"] = 1.0
      
      xmin <- params_min[vecnames]
      xmax <- params_max[vecnames]
      
      # If optimizing Tg - set limits to be between 1 and 7 days
      if (optTg) {
      	params_min["Tg"] = 1./7.
      	params_max["Tg"] = 7./7.
      }

      # default step size
      if (is.null(dr)) {
	      dr=0.025
	      if (dataType == "MPZ") dr=0.02
	      if (dataType == "GFT" ) {
	      	dr = 0.004
	      	if (national == TRUE) dr=0.001
	      }
	            
	      if (dataType == "CDC" | dataType == "GFTPlus") {
	      	dr=0.004
	      	if (national == TRUE) dr=0.001
	      }
	  }

      dx <- rep(dr,length(vecnames)) #Since we move to the same scale for all parameters we use the same step size, but this size does depend on the data type
      
      list(imodel=imodel,Tg=Tg,optTg=optTg,vecnames=vecnames,vecopt=vecopt,vec.zero=veczero,param.default=param.default,par.min=params_min,par.max=params_max,par.dx=dx)
}

# ------------------------------------------------------------------
# function for items related to the PDEs we are solving and the data we are modeling
# ------------------------------------------------------------------

setup.sim <- function(nweeks=66) {

      # number of steps per week and time step
      nstep <-  50
      dt  <- 1./nstep
      dsdt <- rep(0.0,nweeks) 
      rvec <- rep(0.0,nweeks) 
      pois <- 0.0
      list(nstep=nstep,dt=dt,dsdt=dsdt,rvec=rvec,pois=pois)
}

# ------------------------------------------------------------------
#
# ------------------------------------------------------------------

setup.mcmc <- function(nMCMC=1e5,nlines = 1e4,reals=1,vecnames=NULL,vecopt=NULL,vecmin=NULL,seed=NULL) {
      
      ithin <- nMCMC / nlines
      #set seed for random number generator - if NULL we seed it here and will get a different sequence each time 
      set.seed(seed=seed) #This needs to be done even if seed is NULL - in this case R will seed the RNG
      if (is.null(seed)) {
      	iseed <- runif(1) * 1e4
      	iseed <- as.integer(iseed)      	
      } else iseed = seed
     
      nparam <- length(vecnames)
      nparam.opt <- length(vecopt)
      
      logbase = 10  #use log base 10 when needed 
      logvec <- rep(1,nparam) #1 use log uniform sampling , 0 use uniform sampling - must use this for any parameter that can be negative
      logvec[which(vecmin < 0)] <- 0
      
      iupdate <- 1  #in MCMC update all parameters at once ipudate > 0 or one at a time iupdate < 0
      imask  <- rep(-1,nparam)
      names(imask) <- vecnames
      imask[vecopt] <- 1        #vecopt holds the sub-list of what we want to optimize
      
      accept <- 0.0 # accept rate for MCMC procedure
      
      tab <- tab <- matrix(data=0.0,nr=nlines,nc=(nparam+1))
      
      list(ithin=ithin,iseed=iseed,logbase=logbase,logvec=logvec,iupdate=iupdate,imask=imask,nMCMC=nMCMC,reals=reals,tab=tab,accept=0.0)
}


# ------------------------------------------------------------------
# sample initial conditions only for parameters that are optimized
# ------------------------------------------------------------------

setup.ini.rand <- function(min=NULL,max=NULL,default=NULL,vecopt=NULL,vecnames=NULL) {

      nparam <- length(vecnames)
      base_ps <- default
      for (i in 1:nparam) {	

            if (vecnames[i] %in% vecopt) {
            	base_ps[i] <- runif(1,min=min[i],max=max[i])
            	if (vecnames[i] == 'R0min') base_ps[i] <- runif(1,min=1.2,max=2.0) # for R0
            	if (vecnames[i] == 'pC'   ) base_ps[i] <- runif(1,min=0.01,max=0.1)
            	if (vecnames[i] == 'delta' ) base_ps[i] = runif(1,-0.2,0.2)
           	    if (vecnames[i] == 't0'    ) base_ps[i] = runif(1,0.01,5.)
            	if (vecnames[i] == 'dur'   ) base_ps[i] = runif(1,1,5)

           }            	

      }
            
      list(base_ps=base_ps)
}

# ------------------------------------------------------------------
# Use width-at-half-peak to approximate R0,pC,t0,Baseline
# ------------------------------------------------------------------

# subfunction for setup.ini.halfwidth: given a profile 'cases', find the width-at-half-peak 'width'. The flag 'goodpeak' indicates if two half-peak points exist.
setup.ini.halfwidth.HalfPeakWidth <- function(cases,Baseline) {
	nweeks   = length(cases)
	SIRdata  = cases - Baseline
	tpeak    = which.max(SIRdata)
	IncPeak  = SIRdata[tpeak]
	HalfPeak = IncPeak/2
	
	goodpeak = FALSE
	tt = tpeak
	while(tt>1) {
		tt = tt - 1
		if (SIRdata[tt]<=HalfPeak) {
			tLow = tt + (HalfPeak-SIRdata[tt])/(SIRdata[tt+1]-SIRdata[tt])
			goodpeak = TRUE
			break
		}
	}
	
	tt = tpeak
	if (goodpeak == TRUE) {
		goodpeak = FALSE
		while(tt<(nweeks-1)) {
			tt = tt + 1
			if (SIRdata[tt]<=HalfPeak) {
				tHigh = tt - (HalfPeak-SIRdata[tt])/(SIRdata[tt-1]-SIRdata[tt])
				goodpeak = TRUE
				break
			}
		}
	}
	if (goodpeak) width = tHigh-tLow
	else width = Inf
	
	return(list(goodpeak=goodpeak,width=width))
}

# Given an epi profile 'mydata$cases', approximate Baseline, R0, pC, t0
setup.ini.halfwidth <- function(mydata=NULL,model=NULL) {
	# ---- bisection parameters ----
	Rhighlim = 3
	Rlowlim  = 1.05
	Bsteps   = 10	
	
	Baseline = mean(c(head(mydata$cases,5),tail(mydata$cases,5)))
	DatWidth = setup.ini.halfwidth.HalfPeakWidth(mydata$cases,Baseline)
	
	temp = model
	temp$base_ps['pC'] = 1
	temp$base_ps['t0'] = 0	
	temp$base_ps['Baseline'] = Baseline
	nparam = length(model$vecnames)

	Alg2 = FALSE
	# Algorithm 1: use R0 to fit the half-peak-width, then use t0 to match data-peak-time and R0-model-peak-time, last use pC to match data-peak-mag and R0-model-peak-mag
	if (DatWidth$goodpeak) {  # if there are half-peak points in the data
		# Solve R0 such that the resulting SIR profile half-peak-width matches the data
		Rhigh = Rhighlim
		Rlow  = Rlowlim
		for (jj in 1:Bsteps) {
			Rhalf = (Rhigh+Rlow)/2
			temp$base_ps['R0min'] = Rhalf
			# use pars to generate an SIR profile
			out <- .Fortran("subprop",y=as.double(mydata$cases), gamay=as.double(mydata$gamaepi), sh=as.double(mydata$sh), school=as.double(mydata$school), dt=as.double(temp$dt), nstep=as.integer(temp$nstep), nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit), param=as.double(temp$base_ps), nparam = as.integer(nparam), pois=as.double(temp$pois), dsdt=as.double(temp$dsdt), rvec=as.double(temp$rvec), wweight=as.double(mydata$wweight))
			# calculate the SIR width-at-half-peak
			ModWidth = setup.ini.halfwidth.HalfPeakWidth(out$dsdt,Baseline)
			
			# refine R0 by bisection
			if (ModWidth$width>DatWidth$width | ModWidth$goodpeak==FALSE) {
				Rlow = Rhalf
			} else {
				Rhigh = Rhalf
			}
		}
		R0 = Rhalf
		
		ModTpeak = which.max(out$dsdt)
		DatTpeak = which.max(mydata$cases)
		t0       = DatTpeak - ModTpeak
		
		ModIpeak = out$dsdt[ModTpeak]
		DatIpeak = mydata$cases[DatTpeak]
		pC 		 = (DatIpeak-Baseline)/(ModIpeak-Baseline)
	} else {  # if one or both half-peak values lie outside the dataset, assume t0=0 for algorithm 2
		#stop("Parameter values cannot be approximated. Please choose time-window such that entire peak is included.")
		Alg2 = TRUE
		t0 = 0
		Rlow = Rlowlim
		Rhigh = Rhighlim
		DatTpeak = which.max(mydata$cases)
	}
	
	# If the estimated t0 falls outside the t0-range, set it to the corresponding range-limit for algorithm 2 
	if (t0 < temp$par.min['t0']) {
		t0 = temp$par.min['t0']
		Rlow = R0
		Rhigh = Rhighlim
		Alg2 = TRUE	
	}
	
	if (t0 > temp$par.max['t0']) {
		t0 = temp$par.max['t0']
		Rlow = Rlowlim
		Rhigh = R0
		Alg2 = TRUE
	}

	# Algorithm 2: if algorithm 1 fails for any reason, give t0 a fixed value and then solve R0 such that data-peak-time matched model-peak-time. then solve pC such that data-peak-mag and R0-model-peak-mag match.
	if (Alg2) {  # 
		temp$base_ps['t0'] = t0
		for (jj in 1:10) {
			Rhalf = (Rhigh+Rlow)/2
			temp$base_ps['R0min'] = Rhalf
			# use pars to generate an SIR profile
			out <- .Fortran("subprop",y=as.double(mydata$cases), gamay=as.double(mydata$gamaepi), sh=as.double(mydata$sh), school=as.double(mydata$school), dt=as.double(temp$dt), nstep=as.integer(temp$nstep), nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit), param=as.double(temp$base_ps), nparam = as.integer(nparam), pois=as.double(temp$pois), dsdt=as.double(temp$dsdt), rvec=as.double(temp$rvec), wweight=as.double(mydata$wweight))
			ModTpeak = which.max(out$dsdt)
			
			if (ModTpeak>DatTpeak) {
				Rlow = Rhalf
			} else if (ModTpeak<DatTpeak) {
				Rhigh = Rhalf
			} else {
				# if((jj%%2)==1) Rlow = Rlow + (Rhigh-Rlow)/4
				# else Rhigh= Rlow + 3*(Rhigh-Rlow)/4
				break
			}
		}
		R0 = Rhalf
		ModIpeak = out$dsdt[ModTpeak]
		DatIpeak = mydata$cases[DatTpeak]
		pC		 = (DatIpeak-Baseline)/(ModIpeak-Baseline)
	}
	
	# base_ps = model$param.default
	model$base_ps['R0min'] 	  = R0
	model$base_ps['pC'] 	  = pC
	model$base_ps['t0']	 	  = t0
	model$base_ps['Baseline'] = Baseline
	
   	return(model)
}


# ------------------------------------------------------------------
#
# ------------------------------------------------------------------

setup <- function(ptab=NULL,epi=NULL,dataType="MPZ",npop=1e5,national=FALSE, dr=
NULL) {
      model <- setup.model(imodel=ptab$model, dataType=dataType, epi=epi, npop=npop,Tg=ptab$Tg, optTg=ptab$optTg, national=national, dr=dr)
      sim   <- setup.sim(nweeks=length(epi))
      mcmc  <- setup.mcmc(nMCMC=ptab$nMCMC,nlines=ptab$nlines,reals=ptab$reals,vecnames=model$vecnames,vecopt=model$vecopt,vecmin=model$par.min,seed=ptab$seed)
      ini   <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)
            
      setup <- c(model,sim,mcmc,ini)
      
      return(setup)
}


# ------------------------------------------------------------------
# PLOTTING FUNCTIONS
# ------------------------------------------------------------------

plot.results.mcmc <- function(myName="",job.year=2009,imodel=5,weeks=NULL,nweeksFit=nweeksFit,nweeksData=52,epi=NULL,sh=NULL,school=NULL,dsdt=NULL,roft=NULL,rnd.dsdt=NULL,rnd.rvec=NULL,boxplot.data=NULL,device.name="X11",log=log, subDir = file.path(getwd(), "output")) {
      nweeks <- length(weeks)
      
      pC.vec=boxplot.data$pC.vec
      Roft.vec=boxplot.data$Roft.vec
      
      title =paste("Current Estimate for Profile: ",myName,sep="")
      school[school == 0] <- NA #for plotting we only show when the school is closed
      # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
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
      
      # In case the data length is shorter than the number of weeks we have - it means that it was padded with zero's and we now want to put back NA instead of these zeros
      if (nweeksData < nweeks) epi[(nweeksData+1):nweeks] = NA
      par(mfcol=c(1,1),mar=c(5,5,5,5))

      ylim <-  range(epi,dsdt,rnd.dsdt,na.rm=TRUE) #c(0,max(epi,dsdt,rnd.dsdt,na.rm=TRUE))
      xlab.title = paste("Week Number FY ",job.year,"-",(job.year+1),sep="")
      plot(1:nweeksFit,epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n",xlab=xlab.title,ylab="Incidence",main=title,ylim=ylim,xlim=c(1,nweeks))
	  lines(nweeksFit:nweeks,epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2,xlim=c(1,nweeks))
	  
      irnd <- dim(rnd.dsdt)[2]
      #In grey we plot 100 random estimates from the best chain
      for (ix in 1:irnd) {
            lines(1:nweeksFit,rnd.dsdt[1:nweeksFit,ix],type="l",col="grey",lwd=1,xaxt="n")
            lines(nweeksFit:nweeks,rnd.dsdt[nweeksFit:nweeks,ix],type="l",col="grey",lwd=1,xaxt="n",lty=2)
            
      }	
           
      #In Blue we plot the best estimate from the best chain
      lines(1:nweeksFit,dsdt[1:nweeksFit],type="l",col="blue",lwd=2,xaxt="n")
      lines(nweeksFit:nweeks,dsdt[nweeksFit:nweeks],type="l",col="blue",lwd=2,xaxt="n",lty=2)		
	  #Red is the EPI profile we are fitting
      lines(1:nweeksFit,epi[1:nweeksFit],col="red",lwd=2,type="l",xaxt="n")
      lines(nweeksFit:nweeks,epi[nweeksFit:nweeks],col="red",lwd=2,type="l",xaxt="n",lty=2)
      # for models that take the school schedule into account we will plot it
      if (imodel == 1 | imodel == 3) {
            factor <- 0.5 * max(epi,dsdt,na.rm=TRUE)
            lines((school*factor),type="p",lwd=5,col="cyan")
      }
      axis(1,at=1:nweeks,label=weeks,col.axis="black")
      legend("topleft",legend=c("data","Best MCMC","Random MCMC"),col=c("red","blue","grey"),bty="n",lwd=2)
      
      
      par(new=TRUE)
      R.ylim=c(0,4)
      R.ylim=c(min(roft,rnd.rvec,na.rm=TRUE)*0.8,max(roft,rnd.rvec,na.rm=TRUE)*1.2)
      
      plot(1:nweeksFit,roft[1:nweeksFit],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",xlab="",ylab="",ylim=R.ylim,xlim=c(1,nweeks))
      
      for (ix in 1:irnd) {
            lines(1:nweeksFit,rnd.rvec[1:nweeksFit,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n")
            lines(nweeksFit:nweeks,rnd.rvec[nweeksFit:nweeks,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n",lty=2)
      }
      
      lines(1:nweeksFit,roft[1:nweeksFit],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n")
      lines(nweeksFit:nweeks,roft[nweeksFit:nweeks],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",lty=2)      

	  if(nweeksFit < nweeks) abline(v=nweeksFit,col='thistle',lwd=5)
	  
      axis(4,col.axis="green")
      mtext(text="R(t)",side=4,line=2,las=2,col="green")
      
      legend("topright",legend=c("Best MCMC","Random MCMC"),col=c("darkgreen","green"),bty="n",lwd=2)
      
      par(new=TRUE)	  
      pos =  c(0.70,0.75,0.40,0.7)
      par(fig=pos,mai=c(0,0,0,0))
	  boxplot(pC.vec,xlab = "",ylim=c(1.e-4,1.),range=0.0,log="y",name="",cex=0.8) 
      mtext("pC",side=3)  

      par(new=TRUE)

      pos =  c(0.80,0.85,0.40,0.7)

      par(fig=pos,mai=c(0,0,0,0))

      
	  boxplot(Roft.vec,xlab = "",ylim=c(0.5,5),range=0.0,name="",cex=0.8)
      mtext("R0",side=3)
      

      
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
      
      plot(1:nweeksFit,epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n",xlab=xlab.title,ylab="Incidence",main=title,ylim=ylim,xlim=c(1,nweeks))
	  lines(nweeksFit:nweeks,epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2,xlim=c(1,nweeks))
	  
      irnd <- dim(rnd.dsdt)[2]
      #In grey we plot 100 random estimates from the best chain
      for (ix in 1:irnd) {
            lines(1:nweeksFit,rnd.dsdt[1:nweeksFit,ix],type="l",col="grey",lwd=1,xaxt="n")
            lines(nweeksFit:nweeks,rnd.dsdt[nweeksFit:nweeks,ix],type="l",col="grey",lwd=1,xaxt="n",lty=2)
            
      }	
           
      #In Blue we plot the best estimate from the best chain
      lines(1:nweeksFit,dsdt[1:nweeksFit],type="l",col="blue",lwd=2,xaxt="n")
      lines(nweeksFit:nweeks,dsdt[nweeksFit:nweeks],type="l",col="blue",lwd=2,xaxt="n",lty=2)		
	  #Red is the EPI profile we are fitting
      lines(1:nweeksFit,epi[1:nweeksFit],col="red",lwd=2,type="l",xaxt="n")
      lines(nweeksFit:nweeks,epi[nweeksFit:nweeks],col="red",lwd=2,type="l",xaxt="n",lty=2)
      # for models that take the school schedule into account we will plot it
      if (imodel == 1 | imodel == 3) {
            factor <- 0.5 * max(epi,dsdt,na.rm=TRUE)
            lines((school*factor),type="p",lwd=5,col="cyan")
      }
      axis(1,at=1:nweeks,label=weeks,col.axis="black")
      legend("topleft",legend=c("data","Best MCMC","Random MCMC"),col=c("red","blue","grey"),bty="n",lwd=2)
      
      
      par(new=TRUE)
      R.ylim=c(0,4)
      R.ylim=c(min(roft,rnd.rvec,na.rm=TRUE)*0.8,max(roft,rnd.rvec,na.rm=TRUE)*1.2)
      
      plot(1:nweeksFit,roft[1:nweeksFit],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",xlab="",ylab="",ylim=R.ylim,xlim=c(1,nweeks))
      
      for (ix in 1:irnd) {
            lines(1:nweeksFit,rnd.rvec[1:nweeksFit,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n")
            lines(nweeksFit:nweeks,rnd.rvec[nweeksFit:nweeks,ix],type="l",col="green",lwd=2,xaxt="n",yaxt="n",lty=2)
      }
      
      lines(1:nweeksFit,roft[1:nweeksFit],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n")
      lines(nweeksFit:nweeks,roft[nweeksFit:nweeks],type="l",col="darkgreen",lwd=2,xaxt="n",yaxt="n",lty=2)      

	  if(nweeksFit < nweeks) abline(v=nweeksFit,col='thistle',lwd=5)
	  
      axis(4,col.axis="green")
      mtext(text="R(t)",side=4,line=2,las=2,col="green")
      legend("topright",legend=c("Best MCMC","Random MCMC"),col=c("darkgreen","green"),bty="n",lwd=2)
      
      par(new=TRUE)	  
      pos =  c(0.70,0.75,0.40,0.7)
      par(fig=pos,mai=c(0,0,0,0))
	  boxplot(pC.vec,xlab = "",ylim=c(1.e-4,1.),range=0.0,log="y",name="",cex=0.8) 
      mtext("pC",side=3)  

      par(new=TRUE)

      pos =  c(0.80,0.85,0.40,0.7)

      par(fig=pos,mai=c(0,0,0,0))
      
	  boxplot(Roft.vec,xlab = "",ylim=c(0.5,5),range=0.0,name="",cex=0.8)
      mtext("R0",side=3)
      
      
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
      
}

plot.car.mcmc <- function(myName="",job.year=2009,imodel=5,nweeksFit=NULL,nweeksData=52,weeks=NULL,epi=NULL,dsdt=NULL,rnd.dsdt=NULL,device.name="pdf",log=NULL, subDir = file(getwd(), "output")) {
      nweeks <- length(weeks)
      if (is.null(nweeksFit)) nweeksFit=nweeks
      
      title =title =paste(" Cumulative Attack Rate - ",myName,sep="")
      
      # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
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
      # For the current season if the number of data points is less than the season duration
      if (nweeksData < nweeks) cumsum.epi[(nweeksData+1):nweeks] = NA
      rnd.reals = dim(rnd.dsdt)[2]
      cumsum <- matrix(0.0,nr=nweeks,nc=rnd.reals)
      for (i in 1:rnd.reals) {
            cumsum[,i] <- cumsum(rnd.dsdt[,i])
      }
      ylim <- c(0,max(cumsum.epi,cumsum,na.rm=TRUE))
      xtitle = paste("Week Number FY ",job.year,"-",job.year+1,sep="")
      plot(1:nweeksFit,cumsum.epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n",xlab=xtitle,ylab="C.A.R.",main=title,ylim=ylim,xlim=c(1,nweeks))
      lines(nweeksFit:nweeks,cumsum.epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2)
      
      #grey are the random selections from this best chain
      for (ix in 1:rnd.reals) {
            lines(1:nweeksFit,cumsum[1:nweeksFit,ix],type="l",col="grey",lwd=1,xaxt="n")
            lines(nweeksFit:nweeks,cumsum[nweeksFit:nweeks,ix],type="l",col="grey",lwd=1,xaxt="n",lty=2)		
      }
      #blue is the best result from the best chain
      lines(1:nweeksFit,cumsum(dsdt[1:nweeksFit]),type="l",col="blue",lwd=2,xaxt="n")
      lines(1:nweeks,cumsum(dsdt[1:nweeks]),type="l",col="blue",lwd=2,lty=2,xaxt="n")
      
      #red is the c.a.r. of the EPI profile we fitted
      lines(1:nweeksFit,cumsum.epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n")
      lines(nweeksFit:nweeks,cumsum.epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2)
      if (nweeksFit < nweeks) abline(v=nweeksFit,col='thistle',lwd=5)
      
      axis(1,at=1:nweeks,label=weeks)
      legend("topleft",legend=c("Data","Best MCMC","Random MCMC"),col=c("red","blue","grey"),bty="n",lwd=2)
      
      
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
      
      plot(1:nweeksFit,cumsum.epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n",xlab=xtitle,ylab="C.A.R.",main=title,ylim=ylim,xlim=c(1,nweeks))
      lines(nweeksFit:nweeks,cumsum.epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2)
      
      #grey are the random selections from this best chain
      for (ix in 1:rnd.reals) {
            lines(1:nweeksFit,cumsum[1:nweeksFit,ix],type="l",col="grey",lwd=1,xaxt="n")
            lines(nweeksFit:nweeks,cumsum[nweeksFit:nweeks,ix],type="l",col="grey",lwd=1,xaxt="n",lty=2)		
      }
      #blue is the best result from the best chain
      lines(1:nweeksFit,cumsum(dsdt[1:nweeksFit]),type="l",col="blue",lwd=2,xaxt="n")
      lines(1:nweeks,cumsum(dsdt[1:nweeks]),type="l",col="blue",lwd=2,lty=2,xaxt="n")
      
      #red is the c.a.r. of the EPI profile we fitted
      lines(1:nweeksFit,cumsum.epi[1:nweeksFit],type="l",col="red",lwd=2,xaxt="n")
      lines(nweeksFit:nweeks,cumsum.epi[nweeksFit:nweeks],type="l",col="red",lwd=2,xaxt="n",lty=2)
      if (nweeksFit < nweeks) abline(v=nweeksFit,col='thistle',lwd=5)
      
  
      axis(1,at=1:nweeks,label=weeks)
      legend("topleft",legend=c("data","model"),col=c("red","grey"),bty="n",lwd=2)
      if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
}

plot.mcmc.chain <- function(tab=NULL,model=NULL,ireal=1,myName="",device.name="X11",log=log, subDir = file.path(getwd(), "output")) {

      nparam  <- length(model$vecnames)
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
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }

      fbase <- file.path(subDir,paste("chain-",myName,"-",ireal,sep=""))
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

## Write each MCMC chain to a file as an R object

mcmc.write <- function (tab=NULL,model=NULL,ireal=1,mydata=NULL,accept=NULL,log=NULL,verbose=TRUE, subDir = file.path(getwd(), "output")) {
      
	  nweeks = mydata$nweeks
	  myName = mydata$dataName

      # convert MCMC output back to matrix and to an MCMC object
      nparam <- length(model$vecnames)
      tab <- matrix(tab,nc=(nparam+1))

      colnames(tab) <- c(model$vecnames,"AICc")
      # convert the negLLK to actual AICc score
      
      nopt = length(model$vecopt)
      
      tab[,"AICc"] <- 2.0 * tab[,"AICc"]+2.0*nopt
      tab[,"AICc"] <- tab[,"AICc"] + (2*nopt*(nopt+1))/(nweeks-nopt-1) 
	  
	  #this overcomes a bug in R - if the parameter was not optimized some of the sttas comes out as NA instead of zero.
	  #this seems to happen only for the population and really only when it is large (GFT/GFTPlus/CDC cases)
	  if (!("N" %in% model$vecopt)) {
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

	  	#attach both tables so we immediately get all the statistics on these new parameters also
	  	tab = cbind(tab,tabR)

	  }
	 
      # how many steps to burn - here we set it to half which is very rigid
      iburn <- imax/2
	  
	  #This mcmc object is only from 'iburn' and up and used only for the purpose of the statistics
	  mcmc <- mcmc(data=tab[iburn:imax,],start=(model$ithin*iburn),end=model$nMCMC,thin=model$ithin) 	  
      
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
            text <- paste(" Created ", subDir, " Directory for all the Data of MCMC chain","\n",sep="")
            writeLines(text=text,con=log)
      }
      # here write an R data file	
      # print the chains statistics to the screen-only for optimized variables and only if verbose is TRUE
      
      if (verbose) {
	  if (model$imodel == 5) {
	  	print(summary(mcmc[,c(model$vecopt,"AICc","RA","RB","R0")],quantiles=c(0.05,0.25,0.5,0.75,0.95,na.rm=TRUE),digits=4))
	  } else {
	  	print(summary(mcmc[,c(model$vecopt,"AICc")],quantiles=c(0.05,0.25,0.5,0.75,0.95,na.rm=TRUE),digits=4))
	  }
      }
      
      # create csv file with mcmc statistics
      mcmc.summary <- summary(mcmc,quantiles=c(0.05,0.25,0.5,0.75,0.95,na.rm=TRUE))
      # this is a bug in R - when a parameter is fixed sometimes NA comes in the Time-series SE
      
      mcmc.quantiles <- mcmc.summary$quantiles
      mcmc.stats     <- mcmc.summary$statistics
      
      #now need to replace back the proper values for N if needed
	  if (!("N" %in% model$vecopt)) {
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
      output = list(mcmc=mcmc,model=model,data=mydata)
      save(output,file=filename)
      save.image()
          
      list(mcmc=mcmc,mean.llk=mean.llk,chain.param=myparam)
      
}

mcmc.boxplot <- function (tab=NULL,model=NULL,mydata=NULL) {
      
    # convert MCMC output back to matrix and to an MCMC object
    nparam <- length(model$vecnames)
    tab <- matrix(tab,nc=(nparam+1))

    colnames(tab) = c(model$vecnames,'negLLK')
	
	nx  <- dim(tab)[1]
	nx2 <- nx/2	
	pC.vec = tab[nx2:nx,"pC"]
	R0.vec   = tab[nx2:nx,'R0min'] 
    Roft.vec = tab[nx2:nx,'R0min'] * (1.0 + tab[nx2:nx,'delta'])

	  
    nkeep = length(Roft.vec)
      
    if (model$imodel <= 3) {
       for (k in nx2:nx) {
      	tmpR = tab[k,'R0min']*((1.+tab[k,'deltaR']*exp(-tab[k,'aparam']*mydata$sh))*(1.-tab[k,'alpha'] *mydata$school))       		
      	Roft.vec[k-nx2+1] = mean(tmpR)

		} 
	 } 
	 else if (model$imodel == 4) {
	  	Roft.vec = R0.vec
	 }
	 else if (model$imodel == 5) {
	  for (k in 1:nkeep) {
	  	Roft.vec[k] = max(Roft.vec[k],R0.vec[k])
	 }
	 }
       else {
      	Roft.vec = R0.vec
     }

	list(Roft.vec=Roft.vec,pC.vec=pC.vec)
		      
    }
      
      
      
# Simplified version of runPMEDDS for demonstration purposes. User selects from included epi-data sets. Then model and optimization parameters are set such that a short MCMC optimization will produce a reasonable model-fit.  
runPMEDDS.demo <- function(dataType="MPZ",job.year=2009,region=1,national=TRUE){
	
	job.name="demo.pmedds"
           
    
	imodel <- 5
	Tg <- 2.6
	optTg = FALSE
	RandInit = TRUE #FALSE
	nMCMC <- 1e5
	nlines <- 1e3
	dr <- NULL 
	if (dataType == 'MPZ') dr = 0.02 

	reals <- 1
	walkers <- 100
	debug   = FALSE
	verbose = TRUE #FALSE
	plot    = TRUE
	device = "pdf" # "pdf" "png" "X11"
	iseed = NULL #123456 #NULL

	wflag = 0
	wweight = NULL 	#ex. rep(1,52) or runif(52,min=0,max=1)
	
	# load data
	mydata = get.data(dataType=dataType,myMPZ=myMPZ,iregion=region,national=national,job.year=job.year,wweight=wweight,wflag=wflag)
	
	# run MCMC optimization
	out <- runPMEDDS(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg,optTg=optTg,nMCMC=nMCMC,nlines=nlines,reals=reals,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name,job.year=job.year,RandInit=RandInit,dr=dr)
	
	# Open all the relevant pdf or png files from this run if device was not set to X11
	# When truly running in a batch mode this is not something that you want to do 
	if (device != "X11" & device != "x11") {
		if (device == "pdf" | device == "PDF") command_to_open = paste("open ","output/*",mydata$dataName,"*pdf",sep="")
		if (device == "png" | device == "PNG") command_to_open = paste("open ","output/*",mydata$dataName,"*png",sep="")
		system(command_to_open)
	}

}

runPMEDDS <- function(dataType="MPZ",mydata=mydata,imodel=5,Tg=2.6,optTg=NULL,nMCMC=1e6,nlines=1e4,reals=1,iseed=NULL,debug=FALSE,verbose=FALSE,plot=TRUE,device="pdf",job.name="test.pmedds",job.year=2009,RandInit=TRUE,dr=NULL, outputDir = file.path(getwd(), "output")) {
      
 	  if (toupper(plot) == "EXTERNAL" || as.character(plot) == "2") {
	    path <- normalizePath(paste0(getwd(), "/externalPlot.R"))
	    if (file.exists(path)) {
	      source(path)
	    } else {
	     stop ("Setting plot to EXTERNAL requires a user defined function externalPlots.R", call. = FALSE)
	    }
	  }
		     
      subDir <- outputDir
      
      # start the clock
      start.time <- proc.time()
      # open a log file
      logfile <- file(description="log.txt",open="wt")
      
     # if optTg is not set - set it to FALSE
      if(is.null(optTg)) optTg = FALSE
            
      # pack all the parameters to a list
      ptab <- list(model=imodel, Tg=Tg, nMCMC=nMCMC, nlines=nlines, reals=reals, seed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, optTg=optTg)

      # first step is to read the data in
      	  
      iverbose <- 0
      if (ptab$verbose == TRUE) iverbose=1
      
            
      # set up all the parameters for the model and chains using the information given by the user
      model <- setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=mydata$national, dr=dr)

      # if (!RandInit) {
      	# model = setup.ini.halfwidth(mydata=mydata,model=model)
      # }
      
      success <- print.info(dataType=dataType,ptab=ptab,model=model,mydata=mydata,job.name=job.name,job.year=job.year,subDir=outputDir,log=logfile)

      # Plot the EPI profile along with SH and School data (the latter may or may-not exist)

      
     switch (as.character(plot), 
	"FALSE" =,
	"0" = {},
	"TRUE" =,
	"1" = plot.input.data(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile, subDir = outputDir),
 	"EXTERNAL" =,
	"external" =,
	"2" = plot.input.data.external(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile, subDir= outputDir)
      )
 
      # set up a matrix for time series of the results

      dsdt <- matrix(0.0,nr=mydata$nweeks,nc=model$reals)	
      rmat <- matrix(0.0,nr=mydata$nweeks,nc=model$reals)	
      
      # prepare a list where the MCMC objects will reside
      mcmclist=list()
      mean.llk=rep(0,model$reals)
      mcmc.param=list()

      for (ix in 1: model$reals) {
            
            cat("\n Running MCMC Chain Number ",ix,"\n\n")
            text <- paste(" Running MCMC Chain Number ",ix,"\n",sep="")
            writeLines(text=text,con=logfile)
            
            model$epi = mydata$cases
            if(RandInit) {
            	model$base_ps <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps
            } else {
            	# model$base_ps <- setup.ini.guess(cases=mydata$cases,npop=mydata$npop,min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps
            	model = setup.ini.halfwidth(mydata=mydata,model=model)
            }

            # calculate initial guess there is no real need for this 
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),param=as.double(model$base_ps),nparam = as.integer(length(model$base_ps)),pois=as.double(model$pois),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec),wweight=as.double(mydata$wweight))

            # # Run the MCMC chain with Fortran code

            sol <-.Fortran("mcmc",data=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),nparam=as.integer(length(model$base_ps)),dt=as.double(model$dt), nstep=as.integer(model$nstep),nsamps=as.integer(model$nMCMC),logbase=as.integer(model$logbase),pmax=as.double(model$par.max),pmin=as.double(model$par.min),ilog=as.integer(model$logvec),step=as.double(model$par.dx),imask=as.integer(model$imask),pval=as.double(model$base_ps),iupdate=as.integer(model$iupdate),ithin=as.integer(model$ithin),iseed=as.integer(model$iseed * ix),solbest=as.double(model$dsdt),rvec=as.double(model$rvec),tab=as.double(model$tab),iverbose=as.integer(iverbose),accept=as.double(model$accept),wweight=as.double(mydata$wweight))

            ## keep track of the best profile for this chain
            model$dsdt = dsdt[,ix] <- sol$solbest
            model$rvec = rmat[,ix] <- sol$rvec	
            ## call the routine that will write an R object data file for this chain and report statistics on chain
            ## the statistics is currently just printed to the screen

            mcmc.write.out <- mcmc.write(tab=sol$tab,model=model,ireal=ix, mydata=mydata,accept=sol$accept,log=logfile,verbose=TRUE, subDir = outputDir)
            
            mcmc.out       <- mcmc.write.out$mcmc

			## For the added whisker plots - analyze R(t) and pC 
			boxplot.data <- mcmc.boxplot(tab=sol$tab,model=model,mydata=mydata)
			
			
            ## write mcmc also randomly selects 100 values from each chain - this is needed for plotting at the end	
            
            mcmc.param[[ix]] <- mcmc.write.out$chain.param
            
            ## put the mcmc chain in the proper place in the MCMC list
            mcmclist[[ix]] <- mcmc.out
            
            ## record the mean value of -LLK of this chain
            mean.llk[ix] <- mcmc.write.out$mean.llk

            # plot the chain parameter history to a file 
            switch (as.character(plot),
        "FALSE" =,
        "0" = {},
        "TRUE" =,
        "1" =
            chain.plot <- plot.mcmc.chain(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile, subDir = outputDir),
        "EXTERNAL" =,
        "external" =,
        "2" = 
            chain.plot <- plot.mcmc.chain.external(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile, subDir = outputDir)
      )
 
      }

      # select the best chain
      
      iBest <- which.min(mean.llk)
      myparam <- mcmc.param[[iBest]]

      nRnd <- dim(myparam)[1]

      nparam <- dim(myparam)[2]
      rnd.dsdt <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      rnd.rvec <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      # send the randomly selected set of parameters from the best chain to a routine that will calculate the profiles

      for (i in 1:nRnd) {
 
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),param=as.double(myparam[i,]),nparam = as.integer(length(model$base_ps)),pois=as.double(0.),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec),wweight=as.double(mydata$wweight))
            
            rnd.dsdt[,i] <- out$dsdt
            rnd.rvec[,i] <- out$rvec

      }

      
       switch (as.character(plot),
        "FALSE" =,
        "0" = {},
        "TRUE" =,
        "1" = {
           plot.results.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt[,iBest],roft=rmat[,iBest],rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,boxplot.data=boxplot.data,device.name=ptab$device,log=logfile, subDir = outputDir)
            plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt[,iBest],rnd.dsdt=rnd.dsdt,device.name=ptab$device,log=logfile, subDir = outputDir) },
        "EXTERNAL" =,
        "external" =,
        "2" = {
           plot.results.mcmc.external(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt[,iBest],roft=rmat[,iBest],rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,boxplot.data=boxplot.data,device.name=ptab$device,log=logfile, subDir = outputDir)
            plot.car.mcmc.external(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt[,iBest],rnd.dsdt=rnd.dsdt,device.name=ptab$device,log=logfile, subDir = outputDir) }
      )


      # stop the clock	
      cat("\n\nElapsed Time: \n\n")
      text <- paste(" Elapsed Time: ",sep="\n")
      writeLines(text=text,con=logfile)
      end.time = proc.time()
      run.time = end.time-start.time
      text.label = "user    system   elapsed"
      text.time  = paste(run.time[[1]],run.time[[2]],run.time[[3]],sep="   ")
      print(end.time - start.time)
      
      writeLines(text=c(text.label,text.time),con=logfile,sep="\n")
      close(logfile)
      
      # Need to think if there is anything else we want to return
      list(mcmc=mcmclist,model=model,chain.param=mcmc.param,llk=mean.llk)
}

      
      
# Simplified version of runPMEDDS.EMCEE for demonstration purposes. User selects from included epi-data sets. Then model and optimization parameters are set such that a short EMCEE optimization will produce a reasonable model-fit.  
runPMEDDS.EMCEE.demo <- function(dataType="MPZ",job.year=2009,region=1,national=TRUE){
	
	job.name="demo.pmedds.emcee"
               
	imodel <- 5
	Tg <- 2.6
	optTg = FALSE
	RandInit = TRUE #FALSE
	nMCMC <- 1e3
	nlines <- 10

	walkers <- 100
	debug   = FALSE
	verbose = TRUE #FALSE
	plot    = TRUE
	device = "pdf" # "pdf" "png" "X11"
	iseed = NULL #123456 #NULL

	wflag = 0
	wweight = NULL 	#ex. rep(1,52) or runif(52,min=0,max=1)
	
	# load data
	mydata = get.data(dataType=dataType,myMPZ=myMPZ,iregion=region,national=national,job.year=job.year,wweight=wweight,wflag=wflag)
	
	# run EMCEE optimization
	
out <- runPMEDDS.EMCEE(dataType=dataType,mydata=mydata,imodel=imodel,Tg=Tg,optTg=optTg,nMCMC=nMCMC,nlines=nlines,walkers=walkers,iseed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,job.name=job.name,job.year=job.year, output = "output")	

#ONLY open the plots of the data/best walker and its CAR - otherwise nwalker plots will open and this can be in the hundreds..

if (device != "X11" & device != "x11" ) {
	if (device == "pdf" | device == "PDF") {
		command_to_open = paste("open ","output/",mydata$dataName,"-epi-sh-school.pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/plot-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/car-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
	}
	if (device == "png" | device == "PNG") {
		command_to_open = paste("open ","output/",mydata$dataName,"-epi-sh-school.pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/plot-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
		command_to_open = paste("open ","output/car-",mydata$dataName,"*pdf",sep="")
		system(command_to_open)
	}
	
}


}



runPMEDDS.EMCEE <- function(dataType="MPZ",mydata=mydata,imodel=5,Tg=2.6,optTg=NULL,nMCMC=1e6,nlines=1e4,walkers=5,iseed=NULL,debug=FALSE,verbose=FALSE,plot=TRUE,device="pdf",job.name="test.pmedds",job.year=2009, outputDir = file.path(getwd(), "output")) {
      
      # check that walkers >= 2
      if (walkers<2) {
        cat("Warning: the number of walkers must be 2 or greater. walkers=",walkers," is being overwritten by walkers=2.")
        walkers = 2	
      }
      # in the remainder of the code, 'walkers' takes the place of 'reals' in all data structures
      reals = walkers
      subDir <- outputDir
      
      # start the clock
      start.time <- proc.time()
      # open a log file
      logfile <- file(description="log.txt",open="wt")
      
     # if optTg is not set - set it to FALSE
      if(is.null(optTg)) optTg = FALSE
            
      # pack all the parameters to a list
      ptab <- list(model=imodel,Tg=Tg,nMCMC=nMCMC,nlines=nlines,reals=reals,seed=iseed,debug=debug,verbose=verbose,plot=plot,device=device,optTg=optTg)

      # first step is to read the data in
      	  
      iverbose <- 0
      if (ptab$verbose == TRUE) iverbose=1
      
            
      # set up all the parameters for the model and chains using the information given by the user
      model <- setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=mydata$national)

      success <- print.info(dataType=dataType,ptab=ptab,model=model,mydata=mydata,job.name=job.name,job.year=job.year,subDir=outputDir,log=logfile)

      # Plot the EPI profile along with SH and School data (the latter may or may-not exist)
   		switch (as.character(plot),
        "FALSE" =,
        "0" = {},
        "TRUE" =,
        "1" =
            plot.input.data(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile, subDir = outputDir),
        "EXTERNAL" =,
        "external" =,
        "2" = 
       	   plot.input.data.external(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData = mydata$nweeksData,week=mydata$weeks, epi=mydata$cases,sh=mydata$sh,school=mydata$school,device.name=ptab$device,log=logfile, subDir = outputDir)
      	)	
      
      # set up a matrix for time series of the results

      dsdt <- rep(0.0,mydata$nweeks)
      rmat <- rep(0.0,mydata$nweeks)
      
      # prepare a list where the MCMC objects will reside
      mcmclist=list()
      mean.llk=rep(0,model$reals)
      mcmc.param=list()
	  boxplot.list = list()

      model$epi = mydata$cases
      
      nparam = length(model$param.default)
      
      # base_ps = rep(0,nparam)
      base_ps = matrix(data=0,nrow=nparam,ncol=1)
      for (i in 1:reals) {
      	if (i == 1) {
      		# base_ps <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps
      		tmp <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps
      		base_ps[,1] = tmp
      		rownames(base_ps) = names(tmp)
      	} else {
      		tmp     <- setup.ini.rand(min=model$par.min,max=model$par.max,default=model$param.default,vecopt=model$vecopt,vecnames=model$vecnames)$base_ps
      		base_ps = cbind(base_ps,tmp)

      	}
      
      }
	  colnames(base_ps) = paste('walker',1:reals,sep="")
	  model$base_ps = base_ps
      for (i in 1:reals) {
      	      # calculate initial guess there is no real need for this 
      	out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),param=as.double(model$base_ps[,i]),nparam = as.integer(length(model$base_ps)),pois=as.double(model$pois),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec),wweight=as.double(mydata$wweight))

      }

# we need to re-create here the'model$tab' which holds the history of the MCMC chain so that it can hold it for all the walkers
	model$tab = array(0.,c(nlines,(nparam+1),reals))
	
	base_ps = as.numeric(base_ps)

            # # Run the MCMC chain with Fortran code
			
            sol <-.Fortran("emcee",data=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),nparam=as.integer(length(model$base_ps[,1])),dt=as.double(model$dt), nstep=as.integer(model$nstep),nsamps=as.integer(model$nMCMC),pmax=as.double(model$par.max),pmin=as.double(model$par.min),pval=as.double(base_ps),imask=as.integer(model$imask),ithin=as.integer(model$ithin),iseed=as.integer(model$iseed),solbest=as.double(model$dsdt),rvec=as.double(model$rvec),tab=as.double(model$tab),iverbose=as.integer(iverbose),accept=as.double(model$accept),wweight=as.double(mydata$wweight),nwalk=as.integer(reals))

			
            # ## keep track of the best profile for this chain
            model$dsdt = sol$solbest
            model$rvec = sol$rvec	
            dsdt = sol$solbest
            rmat = sol$rvec
    
    		tab = sol$tab
    		tab = array(tab,c(nlines,(nparam+1),reals))        
            # ## call the routine that will write an R object data file for all the chains and report statistics on the best chain only (for now)

		for (iwalk in 1:reals) {
			boxplot.list[[iwalk]] <- mcmc.boxplot(tab=tab[,,iwalk],model=model, mydata=mydata)
			
		}
		
		for (iwalk in 1:reals) {
			
            mcmc.write.out <- mcmc.write(tab=tab[,,iwalk],model=model, ireal=iwalk,mydata=mydata,accept=sol$accept,log=logfile,verbose=FALSE, subDir = outputDir)
            
            mcmc.out       <- mcmc.write.out$mcmc
            
                        
            # ## write mcmc also randomly selects 100 values from each chain - this is needed for plotting at the end	
            
            mcmc.param[[iwalk]] <- mcmc.write.out$chain.param
            
            # ## Theses are all the walkers!
            
            mcmclist <- mcmc.out
            
            # ## record the mean value of -LLK for the best walker
            mean.llk[iwalk] <- mcmc.write.out$mean.llk

            # # plot the chain parameter history to a file 

   		switch (as.character(plot),
        	"FALSE" =,
        	"0" = {},
        	"TRUE" =,
        	"1" =
            	chain.plot <- plot.mcmc.chain(tab=tab[,,iwalk],model=model,ireal=iwalk,myName=mydata$dataName,device.name=ptab$device,log=logfile, subDir = outputDir),
        	"EXTERNAL" =,
        	"external" =,
        	"2" = 
            	chain.plot.external <- plot.mcmc.chain.external(tab=tab[,,iwalk],model=model,ireal=iwalk,myName=mydata$dataName,device.name=ptab$device,log=logfile, subDir = outputDir )
      	)
            
		}
	
      # # select the best chain
      
      iBest <- which.min(mean.llk)

      boxplot.data <- boxplot.list[[iBest]]
      
      myparam <- mcmc.param[[iBest]]
	   
      nRnd <- dim(myparam)[1]

      nparam <- dim(myparam)[2]
      rnd.dsdt <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      rnd.rvec <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      # # send the randomly selected set of parameters from the best chain to a routine that will calculate the profiles

      for (i in 1:nRnd) {
 
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),param=as.double(myparam[i,]),nparam = as.integer(length(model$base_ps)),pois=as.double(0.),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec),wweight=as.double(mydata$wweight))
            
            rnd.dsdt[,i] <- out$dsdt
            rnd.rvec[,i] <- out$rvec

       }

      
      # # all the results are plotted together-we plot results from the best chain, both random and the best one
      
   switch (as.character(ptab$plot),
        "FALSE" =,
        "0" = {},
        "TRUE" =,
        "1" = {
             plot.results.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,boxplot.data=boxplot.data,device.name=ptab$device,log=logfile, subDir = outputDir)
            plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt,rnd.dsdt=rnd.dsdt,device.name=ptab$device,log=logfile, subDir = outputDir) },
        "EXTERNAL" =,
        "external" =,
        "2" = {
            plot.results.mcmc.external(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,boxplot.data=boxplot.data,device.name=ptab$device,log=logfile, subDir = outputDir)
            plot.car.mcmc.external(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt,rnd.dsdt=rnd.dsdt,device.name=ptab$device,log=logfile, subDir = outputDir) }
      )
      
      # # stop the clock	
      cat("\n\nElapsed Time: \n\n")
      text <- paste(" Elapsed Time: ",sep="\n")
      writeLines(text=text,con=logfile)
      end.time = proc.time()
      run.time = end.time-start.time
      text.label = "user    system   elapsed"
      text.time  = paste(run.time[[1]],run.time[[2]],run.time[[3]],sep="   ")
      print(end.time - start.time)
      
      writeLines(text=c(text.label,text.time),con=logfile,sep="\n")
      close(logfile)
      
      # Need to think if there is anything else we want to return
      list(mcmc=mcmclist,model=model,chain.param=mcmc.param,llk=mean.llk)
      
}




runPMEDDS.WT <- function(mydata=mydata,shape=3.8,scale=8.4,ireal=1e4,iseed=NULL,debug=FALSE,verbose=FALSE,device="pdf",job.name="",plot=TRUE, outputDir = file.path(getwd(), "output")) {
	
      # start the clock
      start.time <- proc.time()
      # open a log file
      logfile <- file(description="log-wt.txt",open="wt")
      subDir <- outputDir
      
	  #set seed for random number generator - if NULL we will get a different sequence each time 
      
      if (is.null(iseed)) {
      	set.seed(seed=NULL)
      	iseed <-runif(1) * 1e4
      	iseed <- round(iseed)
      }


	  ncases = mydata$ncases
	  times  = mydata$times
	  case_number = mydata$case_number
	  
	
      # Print some information to the screen and to a text file 
      success <- print.wt.info(mydata=mydata,shape=shape,scale=scale,ireal=ireal,subDir=subDir,log=logfile)

	  # calculate the ncases x ncases matrix of relative likelihhod that case k has
	  # been infected by case l
	  pkl.mat = matrix(0.0,nrow=mydata$ncases,ncol=mydata$ncases)

	      
# the keyword 'norm' tells the weibull routine if it should normalize the matrix (norm=0) or not (norm!=0)
     out <- .Fortran("weibull",ncases=as.integer(ncases),pkl.mat=as.double(pkl.mat),times=as.double(times),shape=as.double(shape),scale=as.double(scale),norm=as.integer(0))
     
      pkl.mat = matrix(out$pkl.mat,nc=ncases)


	  Rl.expect <- rep(0.0,ncases)
	  for (l in 1:ncases){
		for (k in 1:ncases){
			Rl.expect[l] = Rl.expect[l]+pkl.mat[k,l]	
		}

	  }
	  
	  
	## This is what takes most of the time.  Need to see if there is a way to improve it. 
	# sample from a Bernoulli distribution with 
	nwrtf = 1000   # How often to print progress to the screen
	
	Rl <- matrix(0.0,nrow=ncases,ncol=ireal) #create a matrix to store the statistics

	# Here call the Fortran subroutine
	iverbose = 0
	if (verbose == TRUE) iverbose=1
	out <- .Fortran("binom",ireal=as.integer(ireal),ncases=as.integer(ncases),nwrtf=as.integer(nwrtf),pkl.mat=as.double(pkl.mat),Rl=as.numeric(Rl),iseed=as.integer(iseed),iverbose=as.integer(iverbose))
	
	# This holds all the realizations 
	
	Rl <- matrix(out$Rl,nc=ireal)

	# calculate some statistics on Rl
	
	probs=0.95
	stats <- wt.calc.stats(Rl=Rl,times=times,probs=probs)
	
	unique.t = stats$unique.t
	onset.n  = stats$onset.n
	Rl.qntl  = stats$Rl.qntl
	Rl.mean  = stats$Rl.mean
	Rl.median = stats$Rl.median
	
	nrow = length(case_number)
	import <- rep.int(0,nrow)
	weibull.par <- c(scale,shape)
	
	#Point estimate of Rt
	
	Xneg <-wallinga1_import(times = times,import = import,weibull.par = weibull.par)

	# R(t) - is the point estimate for Rt
	Rt <- Xneg / onset.n

	# Order the onset day and R(t) before plotting it

	o <-order(unique.t)
	Rt.order   <- rbind(unique.t[o],Rt[o])
	Rlq.order  <-rbind(unique.t[o],Rl.qntl[o])
	Rlm.order  <-rbind(unique.t[o],Rl.mean[o])
	Rlmd.order <-rbind(unique.t[o],Rl.median[o])
	cases.order<- rbind(unique.t[o],onset.n[o])

	#create a unique day vector for 2003
	dates.vec <- seq(as.Date("2003-01-01"),by="days",length=365)
	
	#Write an R object Data file with everything in it
	
	success <- wt.write.data(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,ireal=ireal,log=logfile,subDir=subDir)
	
	
	#write out the Results and realizations to csv files
	success <- wt.write.results(mycountry=mydata$mycountry,cases.order=cases.order,Rl=Rl,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,log=logfile,subDir=subDir)
	
	#plot the results
   switch (as.character(plot),
        "FALSE" =,
        "0" = {},
        "TRUE" =,
        "1" =
	    success <- wt.plot.results(mycountry=mydata$mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir),
        "EXTERNAL" =,
        "external" =,
        "2" =
	    success <- wt.plot.results.external(mycountry=mydata$mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir)
      )
	
	
      cat("\n\nElapsed Time: \n\n")
      text <- paste(" Elapsed Time: ",sep="\n")
      writeLines(text=text,con=logfile)
      end.time = proc.time()
      run.time = end.time-start.time
      text.label = "user    system   elapsed"
      text.time  = paste(run.time[[1]],run.time[[2]],run.time[[3]],sep="   ")
      print(end.time - start.time)
      
      writeLines(text=c(text.label,text.time),con=logfile,sep="\n")
      close(logfile)
      
      list(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order)
	
}


wt.write.data <- function(mydata=NULL,Rl=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,ireal=NULL,log=NULL,subDir=file.path(subDir,"output")) {

  	 # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }
      
      filename <- paste(subDir,"/",mydata$mycountry,"-",mydata$dateName,".RData",sep="")
      
      cat("\n Writing R object Data: ",filename,"\n")   
      text <- paste(" Writing R object Data file: ",filename,"\n",sep=" ") 
      writeLines(text=text,con=log)
      # save the complete chain here
      results = list(mydata=mydata,Rl=Rl,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,ireal=ireal)
      save(results,file=filename)      
	  save.image()
}


wt.calc.stats <- function(Rl=NULL,times=NULL,probs=0.95) {
#unique onset days and times

	unique.t<- unique(times)
	onset.n <- c(NA)

	for (i in 1:length(unique.t)){
		onset.n[i]<-length(times[which(times==unique.t[i])])
	}

	Rl.qntl <-rep(0.0,length(unique.t))
	Rl.mean <-rep(0.0,length(unique.t))
	Rl.median <-rep(0.0,length(unique.t))
	probs = 0.95

# calculate the 95% quantile and mean for each case Rl
	for (it in 1:length(unique.t)) {
	temp <- which(times == unique.t[it])
	Rl.temp <-rbind(Rl[temp,])
	Rl.mean[it] = mean(Rl.temp)
	Rl.median[it] = median(Rl.temp)
	Rl.qntl[it] = quantile(Rl.temp,probs=probs) 

	}	
	
	list(unique.t=unique.t,onset.n=onset.n,Rl.mean=Rl.mean,Rl.median=Rl.median,Rl.qntl=Rl.qntl)	
}
	
	
wt.write.results <- function(mycountry=NULL,cases.order=NULL,Rl=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,log=NULL,subDir=file.path(getwd(), "output")) {
	
	  # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }
      
      
	# write the sampled results so we can re-use it to calculate all sorts of statistics
	
	fileout = paste(subDir,"/Rl.",mycountry,".csv",sep="")
	write.csv(Rl,file=fileout)
	cat(" Writing the sampled results to file: ",fileout,"\n")
	text <- paste(" Writing the sampled results to file: ",fileout,"\n",sep="")
	writeLines(text=text,con=log)
	
#write all the ordered information to a file
	mytable = list()
	mytable$date    = dates[cases.order[1,]]
	mytable$day     = cases.order[1,]
	mytable$cases   = cases.order[2,]
	mytable$Rmean   = Rlm.order[2,]
	mytable$Rmedian = Rlmd.order[2,]
	mytable$R95     = Rlq.order[2,]
	fileout = paste(subDir,"/Rstats-",mycountry,".csv",sep="")
	write.csv(mytable,file=fileout)
	cat(" Writing ordered results to file: ",fileout,"\n")
	text <- paste(" Writing ordered results to file: ",fileout,"\n",sep="")
	writeLines(text=text,con=log)
	success = TRUE
	success
}

wt.plot.results <- function(mycountry=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,device="X11",log=NULL,subDir=file.path(getwd(), "output")) {
	
	  # check to see if "data" sub-directory exists, if not create it
      if (!dir.exists(subDir)) {
            dir.create(file.path(subDir))
            cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
            text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
            writeLines(text=text,con=log)
      }	

	
	if (device == "PDF" | device == "pdf") {
		pdfname = paste(subDir,"/W_T_",mycountry,".pdf",sep="")
		pdf(file=pdfname,onefile=TRUE,paper="a4r",width=11.5,height=8)
		cat(" Plotting Results to: ",pdfname,"\n")
		text <- paste(" Plotting Results to: ",pdfname,"\n",sep="")
		writeLines(text=text,con=log)
	}


	par(mfcol=c(2,1),mar=c(5,4,3,4))
	if (mycountry == "China") {
		title=paste("Probable cases of SARS by date of report - ",mycountry,sep="")
	} else {
		title=paste("Probable cases of SARS by date of onset - ",mycountry,sep="")
	}
	
	plot(cases.order[1,],cases.order[2,],type="h",col="red",xlim=range(cases.order[1,]),xlab='',ylab = "Cases",main=title,xaxt="n")
	axis(1,at=cases.order[1,],label=dates[cases.order[1,]],las=2,cex.axis=0.7)
	title="Estimated daily reproduction number (95% CI, mean-red and median-green)"
	plot(Rlq.order[1,],Rlq.order[2,],type="h",col="blue",xlim=range(cases.order[1,]),ylim=c(0,14),xlab = '',ylab = "Reproduction Number",main=title,xaxt="n")
	lines(Rlm.order[1,],Rlm.order[2,],type="p",col="red",xlab = '',ylab = "")
	lines(Rlmd.order[1,],Rlmd.order[2,],type="p",col="green",xlab = '',ylab = "")
	axis(1,at=cases.order[1,],label=dates[cases.order[1,]],las=2,cex.axis=0.7)
	abline(h=1.0,col="grey")

#
	if (device == "PDF" | device == "pdf") dev.off()
	
}
	
print.wt.info <- function(mydata=NULL,scale=NULL,shape=NULL,ireal=NULL,job.name=" ",subDir="output",log=NULL) {
      
	  mycountry = mydata$mycountry
	  ncases = mydata$ncases
	  subDir=paste(getwd(),"/",subDir,sep="")
      
      s <- Sys.getenv() # This will give a long list of environment variables. To see the naems use names(s)
      m <- Sys.info()   # This will give a lot of information on her hardware and OS
      cat("\n\n P-MEDDS Package Version 001 \n\n")
      cat(" Job Name: ",job.name)
      cat(" Job Started on: ",format(Sys.time(),"%a %b %d %X %Y"),"\n")
      cat(" Job Running on: ",m[["nodename"]],"\n")
      cat(" OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n")
      cat(" Job Running by User: ",s["USER"],"\n")
      cat(" Job Running in Directory: ",getwd(),"\n")
      cat(" All Data and Plots Saved in Sub-Directory: ",subDir,"\n\n")
      cat(" Modeling ", mycountry,"2003 SARS Data","\n")
	  cat(" Running ",ireal, ' Realizations for a Total of ',ncases," cases","\n")
      
      text <- paste("\n\n P-MEDDS Package Version 001\n",
                    " Job Name: ",job.name,"\n",
                    " Job Started on: ",format(Sys.time(),"%a %b %d %X %Y"),"\n",
                    " Job Running on: ",m[["nodename"]],"\n",
                    " OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n",
                    " Job Running by User: ",s["USER"],"\n",
                    " Job Running in Directory: ",getwd(),"\n",
                    " All Data and Plots Saved in Sub-Directory: ",subDir,"\n\n",
                    " Modeling ", mycountry,"2003 SARS Data","\n",
                    " Running ",ireal, ' Realizations for a Total of ',ncases," cases","\n")

		
      writeLines(text=text,con=log)
      return(TRUE)
}

get.data.wt <- function(mycountry="Singapore") {
	day1 = 37622 #This is a legacy item, due to the way MS saves January 1st 2003
	if (mycountry == "Singapore") {
         data(sarssingapore2003,package="pmedds.core")
         mydata <- sarssingapore2003
    } else if (mycountry == "Hong-Kong") {
         data(sarshongkong2003,package="pmedds.core")
         mydata <- sarshongkong2003
    } else if (mycountry == "Canada") {
         data(sarscanada2003,package="pmedds.core")
         mydata <- sarscanada2003
    } else if (mycountry == "Vietnam") {
         data(sarsvietnam2003,package="pmedds.core")
         mydata <- sarsvietnam2003
    } else if (mycountry == "China") {
         data(sarschina2003,package="pmedds.core")
         mydata <- sarschina2003         
    } else {
        cat(" Missing country name \n")
        cat(" Options are: Singapore, Hong-Kong, Canada, Vietnam and China\n")
        cat(" Code will exit \n")
        quit(save="no")         
    }
    
   	day1 = 37622 #This is a legacy item, due to the way MS saves January 1st 2003

	#Singapore it the only country for which the data is already in the format that the code can handle
	# for all other countires the columns are in the opposite order and it is a shorter list with unique days and the number of cases for that day
	# so we need to expand this list to day- case number 
	if (mycountry == "Singapore") {
		case_number <- mydata[,1]
    	times <-mydata[,2] - day1		
    	
	} else if (mycountry == "China") {
        cases = mydata[,2]
        days  = mydata[,1]
        ncaseOld=0
        
        case_number = rep(NA,sum(cases))
        times       = rep(NA,sum(cases))
        
        for (i in 1:length(days)) {
                ncase = cases[i]
                nstart = ncaseOld +1
                nend   = nstart + ncase - 1
                case_number[nstart:nend] = nstart:nend
                times[nstart:nend]       = days[i]
                ncaseOld = nend
        }
		
	} else {

        cases = mydata[,2]
        days  = mydata[,1]-day1
        ncaseOld=0
        
        case_number = rep(NA,sum(cases))
        times       = rep(NA,sum(cases))
        
        for (i in 1:length(days)) {
                ncase = cases[i]
                nstart = ncaseOld +1
                nend   = nstart + ncase - 1
                case_number[nstart:nend] = nstart:nend
                times[nstart:nend]       = days[i]
                ncaseOld = nend
        }
		
	}

    ncases = length(times)

	list(case_number=case_number,times=times,ncases=ncases,mycountry=mycountry)
	
}

# functions to give Wallinga's estimate of R
wallinga1_import <- function(times, import, weibull.par){

	#function to calculate the relative likelihoods that case k
	#has been infected by case l (output is a k x l matrix)
	#Wallinga 2004 AJE
	unique.t<- unique(times)
	n <- length(unique.t)
	import.n <- onset.n <- c(NA)
	Xneg <- c(NA)

	for (i in 1:length(unique.t)){
	onset.n[i]<-length(times[which(times==unique.t[i])])
	}

	for (i in 1:length(unique.t)){
	import.n[i]<-length(times[which(times==unique.t[i] & import==1)])
	}

	output.mat<- matrix(0.0, ncol=n, nrow=n)
	# the keyword 'norm' tells the weibull routine if it should normalize the matrix (norm=0) or not (norm!=0)

	out <- .Fortran("weibull",n=as.integer(n),output.mat=as.double(output.mat),times=as.double(unique.t),shape=as.double(weibull.par[2]),scale=as.double(weibull.par[1]),norm=as.integer(1))

	output <- matrix(out$output.mat,nc=n)


	for (i in 1:length(unique.t)){
	Xneg[i]<-0
	for (j in 1:length(unique.t)){
	temp <- as.numeric(((onset.n[j]-import.n[j])*output.mat[j,i]))/as.numeric((onset.n%*%(output.mat[j,])))
	Xneg[i] <- sum(Xneg[i], temp, na.rm=T)

	}
	Xneg[i]<- onset.n[i]* Xneg[i]

	}

	Xneg
} 
# end of function
#############################################


get.data <- function(dataType="MPZ",myMPZ="23708",iregion=1,national=FALSE,job.year=2009, wweight=NULL, wflag=0, week.start=NULL, week.end=NULL, year.end=NULL,nweeksFit=0,imodel=5)  {
	
	if (dataType=="MPZ") {
	  #retrieve epi/sh/school data for all bases 
	  #we get a list called mydata with elements epi, sh, school and pop			
	  # each one of these is a list by itself
      mydata <- get.MPZ.data()
      epi_data <- mydata$epi #cases for all bases
      zipnames <- mydata$epi$zipname #MPZ for all bases
      ibase <- which(zipnames == myMPZ) #find the base number using the zip code
      if (length(ibase) == 0) { #if not found set to base number one
      	ibase = 1
      	myMPZ = zipnames[1]
      	
      	cat("User provided MPZ does not exists in data base \n")
      	cat("Reverting to Default - First MPZ ",myMPZ,"\n")
      	txt= "User provided MPZ does not exists in data base"

      }
      #check to see that the user has set the job.year properly
      
      if (job.year != 2009) {
      	cat("\n\n For MPZ modeling we currently only model the pandemic year 2009-2010 \n")
      	cat(" Will reset to FY 2009-2010 \n\n")
      	job.year = 2009
      }
      
            
      nbase <- mydata$epi$nbase
      school_data  <- mydata$school
      sh_data      <- mydata$sh
      pop_data     <- mydata$pop
	  #Now select the base we are modeling 
	  epi=epi_data$curves[,ibase]
	  nweeks <- mydata$epi$nweeks
      weeks   <- mydata$epi$week
      dates   <- mydata$epi$date
      nweeksData = mydata$nweeksData
     
      zipname <- zipnames[ibase]
      sh <- sh_data$curves[,sh_data$zipname==zipname]
      sh[is.na(sh)] <- 0
      if(length(sh) == 0) sh[1:nweeks] = 0
      school <- school_data$curves[,school_data$zipname==zipname]
      school[is.na(school)] <- 0 #replace NAs with zero
      if(length(school) == 0) school[1:nweeks] = 0
      npop <- pop_data$pop[pop_data$zipname ==zipname]
      
      copyepi = epi
      gamaepi <- epi
	  # to save computational time we also calculate the gamma(epi+1)
      for (i in 1:nweeks) {
      	gamaepi[i] <- lgamma((copyepi[i]+1.))	
      }
      
      # If the user has NOT set nweeksFit to a value that is different than zero
      # all weeks will be fitted
      
      if(nweeksFit <= 0) {
      	nweeksFit = nweeks
      }
      
      if (nweeksFit > nweeks) {
      	nweeksFit = nweeks
      	cat("\n\n WARNING: nweeksFit > nweeks Resetting nweeksFit to the entire season \n\n ")
      }
      # The minimal number of weeks for fitting is ten
	  if (nweeksFit < 10) {
	  	nweeksFit = 10
	  	cat("\n\n WARNING: nweeksFit < 10 Resetting nweeksFit to it's minimal value of ten \n\n ")
	  }

	  # check to see for the current season that nweeksFit is NOT > than the number of weeks in the season
	  if (nweeksFit > mydata$nweeksData) {
	  	 cat("\n\n WARNING: nweeksFit > number of weeks in current season ")
	  	 cat("\n\n Resetting it to: ",mydata$nweeksData,' weeks\n\n')
	  	 nweeksFit = mydata$nweeksData
	  }

	  mydata=list()
	  mydata$cases=epi
	  mydata$sh=sh
	  mydata$school=school
	  mydata$gamaepi=gamaepi
	  
	  mydata$nweeks=nweeks
	  mydata$nweeksFit=nweeksFit
	  #mydata$nweeksData = nweeks
	  mydata$weeks=weeks
	  mydata$dates=dates
	  mydata$years= as.integer(format(dates,"%Y"))
	  mydata$zipname=zipname
	  mydata$npop=npop
	  mydata$base=ibase
	  mydata$dataName = paste(dataType,'_',job.year,'_',zipname,'_model',imodel,sep='')
	  
	  if (mydata$nweeksFit < mydata$nweeks) mydata$dataName = paste(dataType,'_',job.year,'_',zipname,'_model',imodel,"_WKFit",nweeksFit,sep='')
	  	  	  
	} else if (dataType=="CDC" | dataType=="GFT" | dataType=="GFTPlus") {
	  #Some set up work - need to change so this works with CDC EPI week numbers and not dates
      year = job.year
      reg  = iregion
      
      
      # for the 2009-2010 pandemic use this 3-30-2009 to 6-28-2010
      if (is.null(week.start) | is.null(week.end)) {
      	if (year==2002) {
      		week.start = 40
      		week.end   = 26            	
      	} else if(year==2009) {
      		week.start = 13
      		week.end   = 26
      	} else if(year<2002) {
      		week.start = 40
      		week.end   = 20
      	} else {
      		#This is the standard CDC definition of start/end of EPI year
      		week.start = 27
      		week.end   = 26 
      	}
      }
      
      # if no value for year.end, pull data for one school-year
      if (is.null(year.end)) year.end = year+1
          
      #Include (at most) one of the following:            
      
      #The features below are not ready yet
      state  = NULL      # Full state name or abbrievation (ie, 'CA' or 'California')
      county = NULL      # Full county name ('Orange County')
      city   = NULL      # Full city name
      
      #Retrieve population data
      npop = get.census.data(national = national, year=year, reg = reg)
                  
      #Retrieve epi data
      if(dataType=="GFT") {
      	mydata = get.GFT.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year, npop=npop, year.end=year.end)
      } else if (dataType == "GFTPlus"){
      	mydata = get.GFTPlus.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year, npop=npop, year.end=year.end)
      } else { #default is CDC data 
      	mydata = get.CDC.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year, npop=npop, year.end=year.end) 
      }
      # get the SH data 
      mydata$sh = get.SH.data(reg = reg, national = national, week.start=week.start, week.end=week.end, year.start = job.year, year.end=year.end)
	  
	  # get school data
	  mydata$school = get.School.data(state=state, reg=reg, national=national, week.start=week.start, week.end=week.end, year.start = job.year, year.end=year.end)
	  
      epi = mydata$cases
      copyepi = epi
      gamaepi = epi
	  nweeks=length(epi)
	  
      for (i in 1:nweeks) {
      	gamaepi[i] <- lgamma((copyepi[i]+1.))	
      } 
      
      mydata$nweeks = nweeks
      mydata$gamaepi = gamaepi
      
      # If the user has NOT set nweeksFit to a value that is different than zero
      # all weeks will be fitted
      
      if(nweeksFit <= 0) {
      	nweeksFit = nweeks
      }
      
      if (nweeksFit > nweeks) {
      	nweeksFit = nweeks
      	cat("\n\n WARNING: nweeksFit > nweeks Resetting nweeksFit to the entire season \n\n ")
      }
      
      # The minimal number of weeks for fitting is ten
	  if (nweeksFit < 10) {
	  	nweeksFit = 10
	  	cat("\n\n WARNING: nweeksFit < 10 Resetting nweeksFit to it's minimal value of ten \n\n ")
	  }
	  
	  # check to see for the current season that nweeksFit is NOT > than the number of weeks in the season
	  if (nweeksFit > mydata$nweeksData) {
	  	 cat("\n\n WARNING: nweeksFit > number of weeks in current season ")
	  	 cat("\n\n Resetting it to: ",mydata$nweeksData,' weeks\n\n')
	  	 nweeksFit = mydata$nweeksData
	  }

	  mydata$nweeksFit = nweeksFit
      
      if (national){
            mydata$dataName = paste(dataType,'_',year,'_national','_model',imodel,sep='')
            if (mydata$nweeksFit < mydata$nweeks) mydata$dataName = paste(dataType,'_',year,'_national','_model',imodel,"_WKFit",nweeksFit,sep='')
      } else if(!is.null(reg)){      
            mydata$dataName = paste(dataType,'_',year,'_reg',reg,'_model',imodel,sep='')
            if (mydata$nweeksFit < mydata$nweeks) mydata$dataName = paste(dataType,'_',year,'_reg',reg,'_model',imodel,"_WKFit",nweeksFit,sep='')
      }
      
      #this should give us CDC like week numbers
      weeks = mydata$weeks
      
      mydata$npop=as.double(npop)
      mydata$national=national
	  mydata$region = iregion
	  
	} else {
		cat("Error: dataType can only be set to MPZ/CDC/GFT/GFTPlus \n")
		cat("Currently dataType set to: ",dataType,"\n")
		cat("Code will exit \n")
		quit(save="no")
	}
	
	
    FY = paste(job.year,'-',(job.year+1),sep="")
    mydata$FY = FY
    
    
	# per request from Leidos - add date to RData files so lets's determine dateName
	mydata$dateName=Sys.Date()
	  
	# Get weekly objective function weights
	mydata$wweight = get.weights(mydata=mydata,dataType=dataType, wweightIN=wweight, wflag=wflag)
	mydata$wflag = wflag
	
	mydata
}


print.info <- function(dataType="MPZ",ptab=NULL,model=NULL,mydata=NULL,job.name=" ",job.year=2009,subDir=file.path(getwd(),"output"),log=NULL) {
      
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
      optTg = ptab$optTg
      zipname=mydata$zipname
      dataName=mydata$dataName
      N = mydata$npop
	  national = mydata$national
	  nweeks = mydata$nweeks
	  nweeksFit = mydata$nweeksFit
      vecopt=model$vecopt
      vecopt=paste(vecopt,collapse=" ",sep=" ")
      
      s <- Sys.getenv() # This will give a long list of environment variables. To see the naems use names(s)
      m <- Sys.info()   # This will give a lot of information on her hardware and OS
      cat("\n\n P-MEDDS Package Version 001 \n\n")
      cat(" Job Name: ",job.name)
      cat(" Job Started on: ",format(Sys.time(),"%a %b %d %X %Y"),"\n")
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
      if (nweeksFit < nweeks) {
      	cat(" Fitting ",nweeksFit," weeks for a season of ",nweeks,' weeks\n')
      }
      
      if (mydata$wflag)
      cat(" Using a Fixed Generation Time of: ",(Tg)," days\n")
      cat(" Optimizing the Following Parameters: ",vecopt,"\n")
      if (optTg) cat(' WARNING: Code is Optimizing both Tg and R0',"\n")
      cat(" Running ", reals, " MCMC Chains \n")
      cat(" Model Set to Number ",imodel,"\n") #Here more info must be printed
      cat(" Weekly Weights Set to Method ")
      cat(":",switch(EXPR=as.character(mydata$wflag),"0"="All Weeks Equal","1"='User Input',"2"='NOT supported',"3"='Use CDC Total Patients'),"\n") 
      cat(" Number of MCMC Steps in Each Chain Set to ",nMCMC,"\n")
      cat(" DEBUG set to ",debug,"\n")
      
      text <- paste("\n\n P-MEDDS Package Version 001\n",
                    " Job Name: ",job.name,"\n",
                    " Job Started on: ",format(Sys.time(),"%a %b %d %X %Y"),"\n",
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

      if (nweeksFit < nweeks) {
      	text <- paste("Fitting ",nweeksFit," weeks for a season of ",nweeks,' weeks\n',sep="")
      	writeLines(text=text,con=log)
      }
            
      text <- paste(text,
                    " Using a Fixed Generation Time of: ",(Tg)," days\n",
                    " Optimizing the Following Parameters: ",vecopt,"\n",
                    " Running ", reals, " MCMC Chains","\n",
                    " Model Set to Number ",imodel,"\n",
                    " Number of MCMC Steps in Each Chain Set to ",nMCMC,"\n",
                    " DEBUG set to ",debug,sep="")
      writeLines(text=text,con=log)
      if (optTg) {
      	text = paste(' WARNING: CODE is Optimizing both Tg and R0',"\n",sep="")
      	writeLines(text=text,con=log)
      }
      return(TRUE)
}


get.census.data <- function (year=2009, national = FALSE, reg = 1, state = NULL, county = NULL, city = NULL){
      
      data(region_state_census,package="pmedds.core")
      
      #mbn - updated to include the 2014 census data so there is no need to replace it with the 2013 data
      # if (year == 2014) year = 2013  #Use 2013 data for now. 2014 population will be available online 12/2014
            
      
      #If national is not null and not false
      if (!is.null(national) && national){
            #N = census_data['United States',paste('POP',year,sep='')] The census does not include Porto Rico so we will calculate the national number using the ten regions
            N=sum(census_data[paste(rep("Region",10),1:10,sep=""),paste('POP',year,sep='')])

      } else if (!is.null(reg)){
            N = census_data[paste('Region',reg,sep=''),paste('POP',year,sep='')]    
            
      } else if (!is.null(state)){
            
            if (nchar(state) == 2) {#If an abbreviation is supplied
                  state = toupper(state)
                  state = state.name[which(state.abb==state)] #Use R state dataset to convert abbreviation to full name
            } else{ #Assume full name supplied
                  state = paste0(toupper(substr(state,1,1)), tolower(substr(state,2,nchar(state)))) #Make sure only first letter is capitalized                
            }
            
            if (is.element(state,rownames(census_data))){
                  N = census_data[state,paste('POP',year,sep='')]
            } else{
                  stop('Invalid State Name: Provide either the full name or the 2-letter abbreviation of a state in the USA')
            }
      }
      npop = as.double(N)
      npop
}

get.School.data <- function(state=NULL, reg=1, national=FALSE, week.start=27, week.end=26, year.start=2009, year.end=2010) {
	
	# Determine row name
	if(!is.null(state)) {
		SchoolRowName = state
	} else if(national) {
		SchoolRowName = "National"
	} else {
		SchoolRowName = paste0("Region",reg)
	}
	
	# Ensure data is loaded
	data(school_region_nation,package="pmedds.core")
	
	# Determine col indices
	start.ind = which(school_region_nation$week==week.start & school_region_nation$year==year.start)
	end.ind = which(school_region_nation$week==week.end & school_region_nation$year==year.end)
	
	# pull data
	school = school_region_nation[start.ind:end.ind,SchoolRowName]
	return(school)
}


get.SH.data  <- function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009, year.end=NULL) {
      # reg        = index of particular USA region of interest (1-10)     
       
      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
   	  if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  } 
   	  date.start = CDCweek2date(week.start,year.start)
      date.end   = CDCweek2date(week.end,year.end) 
   	  
   	  # this loads a data frame named sh_region_state

	  data(sh_region_state,package="pmedds.core") 
	  	
	  
	  # first subset the rows that we need and then get the columns
	  subs = subset(sh_region_state, date >= date.start & date <= date.end)
	  # weeks = subs[,'week']
	  if (national == TRUE) {
	  	sh = subs[,"national"]
	  } else {
	  	sh = subs[,paste('Region',reg,sep="")]
	
	  }
	  
	  # list(sh=sh,weeks=weeks)
	  return(sh)
}


get.Temp.data  <- function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009, year.end=NULL) {
      # reg        = index of particular USA region of interest (1-10)     
       
      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
   	  if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  } 
   	  date.start = CDCweek2date(week.start,year.start)
      date.end   = CDCweek2date(week.end,year.end) 
   	   
   	  # this loads a data frame named sh_region_state
	  data(temp_region_state,package="pmedds.core") 
	  
	  # first subset the rows that we need and then get the columns
	  subs = subset(temp_region_state, date >= date.start & date <= date.end)
	  # weeks = subs[,'week']
	  if (national == TRUE) {
	  	temp = subs[,"national"]
	  } else {
	  	temp = subs[,paste('Region',reg,sep="")]

	  }
	  
	  # list(temp=temp,weeks=weeks)
	  return(temp)
}


get.Precip.data  <- function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009, year.end=NULL) {
      # reg        = index of particular USA region of interest (1-10)     
       
      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
   	  if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  } 
   	  date.start = CDCweek2date(week.start,year.start)
      date.end   = CDCweek2date(week.end,year.end) 

   	  # this loads a data frame named sh_region_state
	  data(prcp_region_state,package="pmedds.core") 
	  
	  # first subset the rows that we need and then get the columns
	  subs = subset(prcp_region_state, date >= date.start & date <= date.end)
	  weeks = subs[,'week']
	  if (national == TRUE) {
	  	precip = subs[,"national"]
	  } else {
	  	precip = subs[,paste('Region',reg,sep="")]
	  }
	  
	  # list(precip=precip,weeks=weeks)
	  return(precip)
}


get.CDC.data <- function(reg=1, national=FALSE, week.start=NULL, week.end=NULL, year.start=2009, year.end=NULL, npop=0){       
      # reg        = index of particular USA region of interest (1-10)     
       
      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
   	  if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  } 
      
      # For historic data
      
      if (year.start < 2015) {
      	
      
      data(cdc_region_nation,package="pmedds.core")

      
      if (national == TRUE){
            
            #Extract the data for the entire US. Output the data into a simple matrix (cases) containing week dates and incidence numbers     
            dat = subset(cdc_region_nation,REGION == 'National')            
            
      } else if (!is.null(reg)){
            
            #Extract the data for each region/year. Output the data into a simple matrix (cases) containing week dates and incidence numbers     
            dat = subset(cdc_region_nation,REGION == paste('Region',reg))   
            
      }
	
	
     # Determine index of week.start and week.end
      start.ind = which(dat$YEAR==year.start & dat$WEEK==week.start)
      
      if (length(start.ind)==0) stop('Error: start week/year combination is not present in the CDC data')
      end.ind   = which(dat$YEAR==year.end & dat$WEEK==week.end)
      if (length(end.ind)==0) stop('Error: end week/year combination is not present in the CDC data')
      
      weeks = dat$WEEK[start.ind:end.ind]
      weighted = dat$X..WEIGHTED.ILI[start.ind:end.ind]                                     
      patients = dat$TOTAL.PATIENTS[start.ind:end.ind]
      
      #JAT changed from percent*CDCpatients to percent*npop/2/52
      #cases = round((weighted/100)*patients)
      cases = round(weighted/100 * npop*2/52)
      
      # Initialize vectors
      dates    = as.Date("2000-01-01") + 1:length(weeks)
      years    = weeks
      
      # Generate a list of dates and years
	  curyear  = year.start
      years[1] = curyear
      dates[1] = CDCweek2date(weeks[1],curyear)
      for(ii in 2:length(weeks)) {
      	if (weeks[ii]==1) curyear = curyear + 1
      	years[ii] = curyear
      	dates[ii] = CDCweek2date(weeks[ii],curyear)
      }
      
      # Here we have all the data
	  nweeksData = length(weeks)


	  } else {
	  	    if (national == TRUE) {
	  	    	 dat = get_flu_data('national','ilinet',years=(year.start-1):year.end)  	
	  	    } else if (!is.null(reg)){
	  	    	dat = get_flu_data('hhs',reg,'ilinet',years=(year.start-1):year.end) 
	  	    }
	  	    
      	# Determine index of week.start and week.end
      	start.ind = which(dat$YEAR==year.start & dat$WEEK==week.start)	  	    
	  	nlines = dim(dat)[1] 
      	weeks = dat$WEEK[start.ind:nlines]
      	weighted = dat$X..WEIGHTED.ILI[start.ind:nlines]                                     
      	patients = dat$TOTAL.PATIENTS[start.ind:nlines]	
		nweeksData = length(weeks)
	    
      	# Need to pad the data here 
      	if (dat$YEAR[nlines]== (year.end) & weeks[nweeksData] < week.end) {
      		nAdd = length((weeks[nweeksData]+1):week.end)
      		weeks = append(weeks,(weeks[nweeksData]+1):week.end)
      		weighted = append(weighted,rep(0,nAdd))
      		patients = append(patients,rep(0,nAdd))
      		
      	}  	 
      cases = round(weighted/100 * npop*2/52)
      # Initialize vectors
      dates    = as.Date("2000-01-01") + 1:length(weeks)
      years    = weeks
      
      # Generate a list of dates and years
	  curyear  = year.start
      years[1] = curyear
      dates[1] = CDCweek2date(weeks[1],curyear)
      for(ii in 2:length(weeks)) {
      	if (weeks[ii]==1) curyear = curyear + 1
      	years[ii] = curyear
      	dates[ii] = CDCweek2date(weeks[ii],curyear)
      }

      	
      	  
	  }
	  
       
      list(cases=cases,weeks=weeks,weighted=weighted,patients=patients, dates=dates, years=years, nweeksData=nweeksData)  
      
}

get.GFTPlus.data <- function(reg=1,national = FALSE, week.start=13,week.end=26, year.start=2009, year.end=NULL, npop=0){       
      # reg        = index of particular USA region of interest (1-10)     

      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
      if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  }
            
      date.start = CDCweek2date(week.start,year.start)
      date.end   = CDCweek2date(week.end,year.end)
            
      data(gft_plus_region_nation,package="pmedds.core")
      
      dat = gft_plus_region_nation
      #dat$Date = as.Date(dat$Date,format='%Y-%m-%d')
      dat = subset(dat,dat$Date >= date.start & dat$Date <= date.end) 
      
      dates = dat$Date
	  years = as.integer(format(dates,"%Y"))

      #Import CDC data to get patient data and week numbers
      data(cdc_region_nation,package="pmedds.core")       
      if (national){ 
            
            cases = dat[,'United.States']      
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == 'National')            
            
      } else if (!is.null(reg)){
            
            region.ind = grep(paste('Region.',reg, '.',sep=''),names(dat),fixed=TRUE) #find column index in GFTPlus data corresponding to reg
            cases = dat[,region.ind]         
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == paste('Region',reg))            
      }
      
      # Determine index of week.start and week.end
      start.ind = which(subs$YEAR==year.start & subs$WEEK==week.start)
      end.ind   = which(subs$YEAR==year.end & subs$WEEK==week.end)
      
      weeks = subs$WEEK[start.ind:end.ind]                         
      patients = subs$TOTAL.PATIENTS[start.ind:end.ind]
          
      old = cases
      #JAT changed from percent*CDCpatients to percent*npop/2/52
      #cases = round((cases/100)*patients)
	  cases = round(cases/100 * npop*2/52)
      ineg = 0
      # Check to see that there are no negative values
      for (i in 1:length(cases)) {
      	if (cases[i] < 0) {
      	cases[i] = 0
      	ineg=ineg+1	
      	}
      	
      }
      if (ineg != 0) cat("\n\n","Warning: GFTPlus Data had negative values- they were set to zero","\n\n")
      
      nweeks = length(weeks)
      nweeksData = nweeks
      list(cases=cases,weeks=weeks,old=old,dates=dates,years=years,nweeksData=nweeksData)           
      
}

get.GFT.data <- function(reg=1,national = FALSE, week.start=13,week.end=26, year.start=2009, year.end=NULL, npop=0){       
      # reg        = index of particular USA region of interest (1-10)     

      #FY year.start - year.end
      if (is.null(year.end)){
            year.end = year.start + 1
      } 
      if (is.null(week.start) || is.null(week.end)) {
   	  		week.start = 27
   	  		week.end   = 26
   	  }      
      
      date.start = CDCweek2date(week.start,year.start)
      date.end   = CDCweek2date(week.end,year.end)
      
      data(gft_region_nation,package="pmedds.core")
      
      dat = gft_region_nation
      #dat$Date = as.Date(dat$Date,'%m/%d/%y')
      dat = subset(dat,dat$Date >= date.start & dat$Date <= date.end) 

	  dates = as.Date(dat$Date,'%Y-%m-%d')
	  years = as.integer(format(dates,"%Y"))

      #Import CDC data to get patient data and week numbers
      data(cdc_region_nation,package="pmedds.core") 
            
      if (national){ 
            
            cases = dat[,'United.States']      
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == 'National')            
            
      } else if (!is.null(reg)){
            
            region.ind = grep(paste('Region.',reg, '.',sep=''),names(dat),fixed=TRUE) #find column index in GFT data corresponding to reg
            cases = dat[,region.ind]         
            
            #Get patient data from CDC / ILINET
            subs = subset(cdc_region_nation,REGION == paste('Region',reg))            
      }
      
      # Determine index of week.start and week.end
      start.ind = which(subs$YEAR==year.start & subs$WEEK==week.start)
      end.ind   = which(subs$YEAR==year.end & subs$WEEK==week.end)
      
      weeks = subs$WEEK[start.ind:end.ind]                         
      patients = subs$TOTAL.PATIENTS[start.ind:end.ind]
             
      old = cases
      #JAT changed from percent*CDCpatients to percent*npop/2/52
      #cases = round(cases * (patients/100000.0))
      cases = round(cases/100.0 * npop*2/52)
      ineg = 0
      # Check to see that there are no negative values
      for (i in 1:length(cases)) {
      	if (cases[i] < 0) {
      	cases[i] = 0
      	ineg=ineg+1	
      	}
      	
      }
      nweeks = length(weeks)
      nweeksData = nweeks
      if (ineg != 0) cat("\n\n","Warning: GFT Data had negative values- they were set to zero","\n\n")
      list(cases=cases,weeks=weeks,old=old,dates=dates,years=years,nweeksData=nweeksData)           
      
}


get.weights <- function(wflag=0, mydata=NULL, dataType="MPZ", wweightIN=NULL) {
	
	# check for valid value of wflag
	if(!any(wflag==c(0,1,2,3))) {
		stop("wflag = ",wflag,", which is an invalid value.")
	}
	# Default to all ones
	if (wflag==0) {
		wweightIN = rep(1,mydata$nweeks)
	}
	# User-supplied wweight vector
	if (wflag==1) {
		if (is.null(wweightIN)) {
			cat("\n\n!!!WARNING!!! no user-weights entered. reverting to equal weights (wflag=0)\n\n")
			wweightIN=rep(1,mydata$nweeks)
		}else if (any(wweightIN<0)) {
			cat("\n\n!!!WARNING!!! Negative value(s) found in wweight. Weekly weights must be non-negative. reverting to equal weights (wflag=0)\n\n")
			wweightIN=rep(1,mydata$nweeks)
		}else if (sum(wweightIN)==0){
			cat("\n\n!!!WARNING!!! sum of weekly weights must be non-zero. reverting to equal weights (wflag=0)\n\n")
			wweightIN=rep(1,mydata$nweeks)
		}else if (length(wweightIN)!=mydata$nweeks) {
			cat("\n\n!!!WARNING!!! user entered weekly weights has length",length(wweightIN),", while epi-data has length",mydata$nweeks,". They are required to be equal for wflag=1. Reverting to equal weights (wflag=0)\n\n")
			wweightIN=rep(1,mydata$nweeks)
		} # else do nothing, user wweightIN is correct 
	} 
	# Get weights from some yet-to-be-created database
	if (wflag==2) {
		cat("\n\n!!!WARNING!!! wflag=2 is not yet functional, reverting to equal weights (wflag=0)\n\n")
		wweightIN=rep(1,mydata$nweeks)
	} 
	# Use CDC Total Patients as the weekly-weights
	if (wflag==3) {
		if (dataType=="CDC") {
			wweightIN = mydata$patients
		}else{
			cat("\n\n!!!WARNING!!! wflag=3 requires dataType='CDC'. Reverting to wflag=0 (default)\n\n")
			wweightIN=rep(1,mydata$nweeks)
		}
	}
	# Normalize weights s.t. sum(wweight) = nweeks
	wweightOUT = wweightIN*mydata$nweeks/sum(wweightIN)
	# for all data streams except MPZ - rescaling is needed to ensure that the number are not too large
	if (dataType != "MPZ") {
		wweightOUT = wweightOUT * 1./mydata$nweeksFit
	}
	return(wweightOUT)
}


CDCweek2date <- function(CDCweek,year) {
	# Convert CDC-week to a date (Sunday)
	# CDC week numbering: first Wednesday of the year is in week 1
	# R week numbering: first Sunday of the year is in week 1
	# R & CDC weeks: a week starts Sunday and ends Saturday
	
	# Determine if CDCweeks and Rweeks are the same using 1-Jan-year
	Jan1 = weekdays(as.Date(paste0(year,'-01-01')))
	if (any(Jan1==c('Monday','Tuesday','Wednesday'))) {
		# They are not, shift by 1
		Rweek = CDCweek-1
		if (Rweek==0) {
			date = as.Date(paste0('0-',1,'-',year),format='%w-%U-%Y')-7
		} else {
			date = as.Date(paste0('0-',Rweek,'-',year),format='%w-%U-%Y')
		}
	} else { # this year Rweeks and CDCweeks are the same
		Rweek = CDCweek
		if (Rweek==53) {
			date = as.Date(paste0('0-',52,'-',year),format='%w-%U-%Y')+7
		} else {
			date = as.Date(paste0('0-',Rweek,'-',year),format='%w-%U-%Y')
		}
	}
	return(date)	
}


## A Simple SIR model using either a Stochastic or determenistic procedure. 
##
psi.sim.modelA <- function(
R0 = 1.4,
Tg=2.6,
pC = 0.1,
t0=10.,
Background = 2,
seed=1,
N=1e4,
delta=-0.1,
ts = 12,
dur = 5, 
tps=(0:52)*7,
reals=100,
dt=0.1,
imodel=4,
stochastic=TRUE,
plot=TRUE) {


# convert to weeks
t0 = t0 * 7.
ts = ts * 7.
dur = dur * 7.

# in case of a fixed value R(t) must zero delta
if (imodel == 4) {
	dur = 0.
	delta = 0.
} 

# check that ts > t0 

if (imodel == 5) {
	if (ts <= t0) stop("Please set ts to be greater than 10 weeks")
}

if (imodel != 4 && imodel != 5) stop("Synthetic Data Can Only Be Used with imodel = 4 or 5")
noTPts <- length(tps) 
epsilon <- 1e-10
if (seed >= N) stop("psi.sim.modelA seed greater than N")

if (noTPts < 3) 
stop("simSIR: tps must be of length 2 or greater")

## For a determenistic profile - run only one realization
if (reals > 1 && stochastic==FALSE) reals = 1 


cat("\n CREATING  A SYNTHETIC PROFILE \n\n")
if (imodel == 4) cat("Using a fixed value model for the Force of Infection: R(t) = R0\n\n")
if (imodel == 5) cat("Using a two value model for the Force of Infection: R(t) = R0 * (1+delta)\n\n")


flush.console()

Baseline = Background 
rtn <- array(0,c(noTPts-1,reals))
Roft <- array(0,c(noTPts-1))
pb <- txtProgressBar(min = 0, max = reals, style = 3)

for (i in 1:reals) {
	
setTxtProgressBar(pb, i)

t_cur <- tps[1]
ind_t <- 2
t_next <- tps[ind_t]

## At time zero we have a fraction pS (which can be between 0-1) in the suscptible state and the complementary in the R stats is (1-pS)
## We are looking to see how changing pS changes the curves..

sS <- N - seed
sI <- 0
sR <- 0


blNotYetSeed <- TRUE

while (ind_t <= noTPts) {

if (t_cur >= t0 && blNotYetSeed) {
seedInf <- seed
blNotYetSeed <- FALSE
} else {
seedInf <- 0
}

Rt = R0
if (t_cur >= ts && t_cur <= (ts+dur)) Rt = R0 * (1.+delta)

lambda <- Rt * sI / Tg / N
pInf <- 1 - exp(-lambda*dt)
pRec 	<- 1 - exp(-dt/Tg)

if (stochastic) {
if (lambda > 0 || seedInf > 0 ) {
nInf <- rbinom(1,sS,pInf) + seedInf
} else {
nInf <- 0
}

nRec <- rbinom(1,sI,pRec)

} else {
	
if (lambda > 0 || seedInf > 0) {
nInf <- sS*pInf + seedInf
} else {
nInf <- 0
}

nRec <- sI*pRec

}

sS <- sS - nInf 
sI <- sI + nInf - nRec
sR <- sR + nRec


rtn[ind_t-1,i] <- rtn[ind_t-1,i] + nInf * pC

t_cur <- t_cur + dt

if (t_cur > (t_next - epsilon)) {
rtn[ind_t-1,i] = rtn[ind_t-1,i] + Baseline #add the background value
Roft[ind_t-1] = Rt
t_next <- tps[ind_t]
ind_t <- ind_t+1

}

}
}

# calculate also the mean incidence - only for realizations that took off

ave=rep(0,dim(rtn)[1])
allruns = rtn
index = NULL
for (i in 1:reals) {
	if (max(rtn[,i]) < (10)) {
		index = append(index,i)
	}
}

if(length(index) > 0) allruns = allruns[,-index]
itotal = dim(allruns)[2]

error = FALSE

if (stochastic && (itotal/reals) < 0.1) {
	
	stop("\n\n For this choice of Population,R0 and pC more than 90% of realizations did NOT take-off \n\n
	Please set R0 and/or pC to higher values and re-run \n\n")
	return(error=TRUE)
} 
for (i in 1:dim(allruns)[1]) ave[i] = mean(allruns[i,])


if (plot && stochastic == TRUE ) {
	title = 'Stochastic Synthetic Data'
plot(tps[2:(noTPts)],allruns[,1],type="l",col="grey",ylim=c(0,max(rtn)),xlab="Week Number",ylab="Weekly Incidence",xaxt='n',xlim=range(tps),main=title)
if (itotal > 1) {
for (j in 2:itotal) points(tps[2:(noTPts)],allruns[,j],type="l",col=rgb(0.3,0.3,0.3,alpha=0.2),xaxt='n')
}


points(tps[2:(noTPts)],ave,type='l',col='red',lwd=2,xaxt='n')
legend('topleft',legend=c('average','individual'),text.col=c('red','grey'),bty='n')
axis(1,at=tps,labels=(tps/7))
}


if (plot && stochastic == FALSE ) {
	title = 'Synthetic Data'
plot(tps[2:(noTPts)],rtn[,1],type="l",col="red",ylim=c(0,max(rtn)),xlab="Week Number",ylab="Weekly Incidence",xaxt='n',xlim=range(tps),main=title)
legend('topleft',legend='Data',text.col=c('red'),bty='n')
axis(1,at=tps,labels=(tps/7))	
}

par(new=TRUE)
plot(tps[2:(noTPts)],Roft,type='l',col='green',ylim=c(min(Roft)*0.6,max(Roft)*1.2),xlab="",ylab='',xlim=range(tps),xaxt='n',yaxt='n')

axis(4)

# round to integers and also calculate gamma function of (cases+1) 
cases = round(ave)

copyepi = cases
gamaepi <- copyepi
# to save computational time we also calculate the gamma(epi+1)
for (i in 1:length(copyepi)) {
   gamaepi[i] <- lgamma((copyepi[i]+1.))	
}

list(rtn=allruns,cases=ave,gamaepi=gamaepi,weeks=(tps[2:(noTPts)]/7),nweeks=length(ave),nweeksFit=length(ave),R0=R0,pC=pC,npop=N,Tg=Tg,Baseline=Baseline,t0=t0,ts=ts,delta=delta,dur=dur,Roft=Roft,imodel=imodel,stochastic=stochastic,error=error)

}



print.info.model <- function(dataType="MODEL",ptab=NULL,model=NULL,mydata=NULL,job.name=" ") {

      reals=ptab$reals
      nMCMC=ptab$nMCMC
      imodel=ptab$model
      plot=ptab$plot
      device=ptab$device
      Tg = ptab$Tg *7. #in ptab Tg is in days
      optTg = ptab$optTg

      N = mydata$npop
	  
      vecopt=model$vecopt
      vecopt=paste(vecopt,collapse=" ",sep=" ")
      
      s <- Sys.getenv() # This will give a long list of environment variables. To see the naems use names(s)
      m <- Sys.info()   # This will give a lot of information on her hardware and OS
      cat("\n\n P-MEDDS Package Version 001 \n\n")
      cat(" Job Name: ",job.name)
      cat(" Job Started on: ",format(Sys.time(),"%a %b %d %X %Y"),"\n")
      cat(" Job Running on: ",m[["nodename"]],"\n")
      cat(" OS Information: ",m[["sysname"]],"release",m[["release"]],"machine",m[["machine"]],"\n")
      cat(" Job Running by User: ",s["USER"],"\n")
      cat(" Job Running in Directory: ",getwd(),"\n")
	  cat(" Modeling Synthetic Data ","\n")
      cat(" Using a Fixed Generation Time of: ",Tg," days\n")
      cat(" Optimizing the Following Parameters: ",vecopt,"\n")
      if (optTg) cat(' WARNING: Code is Optimizing both Tg and R0',"\n")
      cat(" Model Set to Number ",imodel,"\n") #Here more info must be printed
      cat(" Number of MCMC Steps in Each Chain Set to ",nMCMC,"\n")
      
      return(TRUE)
}


setup.ini.demo <- function (model=NULL,mydata=NULL) {
	# Have to convert to weeks here 
	model$par.min['R0min']    = 1.
	model$par.min['t0']    = mydata$t0 / 7. * 0.5
	model$par.min['delta'] = mydata$delta * 0.5
  	model$par.min['dur']   = mydata$dur /7. * 0.5
  	model$par.min['ts']    = mydata$t0 / 7.
  	model$par.max["R0min"] = mydata$R0 * 1.5
  	model$par.max['t0']    = mydata$t0 / 7. * 1.5
  	model$par.max['delta'] = mydata$delta * 2.
  	model$par.max['dur']   = mydata$dur / 7. * 1.5
  	model$par.max['ts']	   = mydata$t0 / 7. + 4.
  	model$par.max['Baseline'] = mydata$Baseline * 5.0
  	nopt = length(model$vecopt)
  	model$param.default['R0min']  = mydata$R0
  	model$param.default['pC']  	  = mydata$pC
  	model$param.default['t0']     = mydata$t0 / 7.
  	model$param.default['Baseline'] = mydata$Baseline
  	model$param.default['ts']  	  	= mydata$ts / 7.  	
  	model$param.default['dur']  	= mydata$dur / 7.  	  
  	model$param.default['delta']  	= mydata$delta / 7. 
  	model$param.default['N']	  	= mydata$npop
  	model$param.default['Tg']	  	= mydata$Tg / 7.  	
  	#take care of possible negative value of delta:
  	if (mydata$delta < 0.) {
  		tmp = model$par.min['delta']
  		model$par.min['delta'] = model$par.max['delta']
  		model$par.max['delta'] = tmp
  	}
  	model$base_ps[model$vecopt]   =model$par.min[model$vecopt] + runif(nopt,min=0,max=1)*(model$par.max[model$vecopt]-model$par.min[model$vecopt]) #runif(nopt,min=model$par.min[model$vecopt],max=model$par.max[model$vecopt])
  	model$base_ps['t0'] = model$param.default['t0']
  	

	list(par.min=model$par.min,par.max=model$par.max,param.default=model$param.default,base_ps=model$base_ps)	
}

	
# Simplified version of runPMEDDS for demonstration purposes. User selects from included epi-data sets. Then model and optimization parameters are set such that a short MCMC optimization will produce a reasonable model-fit.  
runPMEDDS.model <- function(mydata=mydata) {
	
	if (mydata$error) stop()
	
	job.name='model.pmedds'
               
	imodel <- mydata$imodel

	Tg <- mydata$Tg
	optTg = FALSE
	RandInit = TRUE #FALSE
	nMCMC <- 1e5
	if (imodel == 5) nMCMC = 2e5
	nlines <- 1e3
	dr <- 0.07
	dataType = 'MODEL'

	debug   = FALSE
	verbose = FALSE
	plot    = TRUE
	device = "x11" # "pdf" "png" "X11"
	iseed = NULL #123456 #NULL

	nweeks = mydata$nweeks
    nweeksFit = mydata$nweeksFit
	wflag = 0
	wweight = rep(1,nweeks)
	
	mydata$sh = rep(0,nweeks)
	mydata$school = rep(0,nweeks)
        
      # start the clock
      start.time <- proc.time()
      
     # if optTg is not set - set it to FALSE
      if(is.null(optTg)) optTg = FALSE


      # pack all the parameters to a list
      ptab <- list(model=imodel, Tg=Tg, nMCMC=nMCMC, nlines=nlines, reals=1, seed=iseed, debug=debug, verbose=verbose, plot=plot, device=device, optTg=optTg)

            
      # set up all the parameters for the model and chains using the information given by the user - general real or synthetic data
      
      model <- setup(ptab=ptab,epi=mydata$cases,dataType=dataType,npop=mydata$npop,national=FALSE,dr=dr)

	  
      # set up a matrix for time series of the results

      dsdt <-rep(0.0,nr=mydata$nweeks)
            
      model$epi = mydata$cases
      
	  # set up initial conditions and min/max values for parameters - specific for synthetic data
	  
		tmp = setup.ini.demo(model=model,mydata=mydata)
		model$par.min = tmp$par.min
		model$par.max = tmp$par.max
		model$param.default = tmp$param.default
		model$base_ps       = tmp$base_ps

       # # Run the MCMC chain with Fortran code
		cat("\n\n ******** BEGINNING FITTING PROCEDURE ********\n\n")
       sol <-.Fortran("mcmc",data=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),nparam=as.integer(length(model$base_ps)),dt=as.double(model$dt), nstep=as.integer(model$nstep),nsamps=as.integer(model$nMCMC),logbase=as.integer(model$logbase),pmax=as.double(model$par.max),pmin=as.double(model$par.min),ilog=as.integer(model$logvec),step=as.double(model$par.dx),imask=as.integer(model$imask),pval=as.double(model$base_ps),iupdate=as.integer(model$iupdate),ithin=as.integer(model$ithin),iseed=as.integer(model$iseed),solbest=as.double(model$dsdt),rvec=as.double(model$rvec),tab=as.double(model$tab),iverbose=as.integer(1),accept=as.double(model$accept),wweight=as.double(wweight))

       ## keep track of the best profile for this chain
       model$dsdt = dsdt <- sol$solbest
	   model$rvec = rvec <- sol$rvec
            
        # convert MCMC output back to matrix and to an MCMC object
      	nparam <- length(model$base_ps)
     	tab <- matrix(sol$tab,nc=(nparam+1))
	
      	colnames(tab) <- c(model$vecnames,"AICc")
 
        nlines = dim(tab)[1]
        nlines2 = nlines/2
      myparam <- tab[nlines2:nlines,1:nparam]
      colnames(myparam) = c(model$vecnames)
      
		nlines = dim(myparam)[1]
      nRnd <- 100 
      
      nRnd = min(nRnd,nlines)

      rnd.dsdt <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)
      rvec.dsdt <- matrix(0.0,nr=mydata$nweeks,nc=nRnd)

      # send the randomly selected set of parameters from the best chain to a routine that will calculate the profiles

      for (i in 1:nRnd) {
 			irnd = runif(n=1,min=1,max=nlines)
 			irnd = round(irnd)
            out <- .Fortran("subprop",y=as.double(mydata$cases),gamay=as.double(mydata$gamaepi),sh=as.double(mydata$sh),school=as.double(mydata$school),dt=as.double(model$dt),nstep=as.integer(model$nstep),nweeks=as.integer(mydata$nweeks),nweeksFit=as.integer(mydata$nweeksFit),param=as.double(myparam[irnd,]),nparam = as.integer(length(model$base_ps)),pois=as.double(0.),dsdt=as.double(model$dsdt),rvec=as.double(model$rvec),wweight=as.double(wweight))
            
            rnd.dsdt[,i] <- out$dsdt
            rvec.dsdt[,i]  <- out$rvec
      }

	rtn = mydata$rtn
	
	rave = rep(0,mydata$nweeks)
	for (j in 1:mydata$nweeks) rave[j] = mean(rvec.dsdt[j,1:nRnd])
	
	
	dev.new()
	par(mar=c(5,5,2,5))
	title ='Synthetic Data-Fit'
	plot(mydata$weeks,mydata$cases,col='red',ylim=c(0,max(mydata$cases,mydata$rtn,rnd.dsdt,dsdt)),xlab="Week Number",ylab="Weekly Incidence",xaxt='n',xlim=range(mydata$weeks),lwd=2,type='l',main=title)
	mycolor = col2rgb('seagreen4')
	for (j in 1:nRnd) lines(mydata$weeks,rnd.dsdt[,j],type="l",col=rgb(0.18,0.5,0.34,alpha=0.3),xaxt='n')
		
	if (mydata$stochastic == TRUE) {
	for (j in 1:dim(rtn)[2]) lines(mydata$weeks,rtn[,j],type="l",col=rgb(0.3,0.3,0.3,alpha=0.2),xaxt='n')
	legend('topright',legend=c('data-average','data-individual','best-fit','individual-fits'),text.col=c('red','grey','blue',rgb(0.18,0.5,0.34,alpha=0.6)),bty='n')
	} else {
	legend('topright',legend=c('data-average','best-fit','individual-fits'),text.col=c('red','blue',rgb(0.18,0.5,0.34,alpha=0.6)),bty='n')		
	}

	lines(mydata$weeks,dsdt,type='l',col='blue',lwd=2,xaxt='n')
	lines(mydata$weeks,mydata$cases,type='l',col='red',lwd=2,xaxt='n')	
	
	axis(1,at=mydata$weeks,labels=mydata$weeks)

	par(new=TRUE)
	par(mar=c(5,5,2,5))

	plot(mydata$weeks,rvec,type='l',col=rgb(0.18,0.5,0.34,alpha=0.3),lwd=2,xaxt='n',yaxt='n',xlab='',ylab='',ylim=c(min(mydata$Roft)*0.6,max(mydata$Roft)*1.2))
	for (j in 1:nRnd) lines(mydata$weeks,rvec.dsdt[,j],type="l",col=rgb(0.18,0.5,0.34,alpha=0.3),xaxt='n')
	lines(mydata$weeks,mydata$Roft,type="l",col="red",xaxt='n')
	#lines(mydata$weeks,rave,type="l",col="green",xaxt='n')
	axis(4)
	mtext(text="R(t)",side=4,line=2.5)
	pval = sol$pval
	
	names(pval) = model$vecnames

	if (imodel == 4) {
		param = c("R0",'pC','Background')
		input = c(mydata$R0,mydata$pC,mydata$Baseline)
		par.mean  = c(mean(myparam[,'R0min']),mean(myparam[,'pC']),mean(myparam[,'Baseline']))
		par.sd    = c(sd(myparam[,'R0min'])  ,sd(myparam[,'pC'])  ,sd(myparam[,'Baseline']))
	} else if (imodel == 5) {
		param  = c("R0",'pC','ts','dur','delta','Background')
		input = c(mydata$R0,mydata$pC,mydata$ts/7.,mydata$dur/7.,mydata$delta,mydata$Baseline)
		par.mean  = c(mean(myparam[,'R0min']),mean(myparam[,'pC']),mean(myparam[,'ts']),mean(myparam[,'dur']),mean(myparam[,'delta']),mean(myparam[,'Baseline']))
		par.sd    = c(sd(myparam[,'R0min'])  ,sd(myparam[,'pC'])  ,sd(myparam[,'ts'])  ,sd(myparam[,'dur'])  ,sd(myparam[,'delta'])  ,sd(myparam[,'Baseline']))		
	}
			
	results = data.frame(parameters=param,input=input,mean=par.mean,sd=par.sd)
	cat("\n\n******** Results of Fitting Procedure: ********\n")
	print.data.frame(results,digits=2)
	cat('\n\n JOB COMPLETED \n\n')
	cat('\n\n')

}




