rm(list=ls())

graphics.off()

pdf = TRUE

year.start = 2016
year.end   = 2017

FY = paste(year.start,'-',year.end,sep="")

nreal = 3

models = 1:4
prior  = c(0,1, 2,3, 3)
Temp   = c(1,1,10,1,10)

nmodels = length(prior)

nmodels = length(models)
nprior = length(prior)
nTemp  = length(Temp)

priorLegend = c("No Prior",'Prior','Heated Prior','Augment Data',"Augmented Data Heated")
modelLegend = c("SH", "SV", "SH+SV", "Fixed")

library(DICE)

mod_level = 2
fit_level = 3
dataType = 'cdc'

## get the user name
username = Sys.getenv("USER") # change as needed here

seasonDir = paste0("~/work/dice-results/",FY,"-season/")

dataDir = paste0("~/work/dice-results/",FY,"-season/data/")

csv_and_pdfDir = paste0("~/work/dice-results/",FY,"-season/csv_and_pdf/")


mydata = get.subset(mod_level=mod_level,fit_level=fit_level,dataType=dataType,start.year = year.start,end.year=year.end)

nweeksData = mydata$nweeksData

nweeksFit = nweeksData

sh = mydata$fit$sh

school = mydata$fit$school

onset = mydata$fit$onset

nregion = mydata$fit$nregion

nregion1 = nregion + 1

if (pdf == TRUE) {
	pdfName = paste0(csv_and_pdfDir, "compare-models-nweeksFit-", nweeksFit, ".pdf", sep = "")
	pdf(file = pdfName, width = 12, height = 11)
}

llk.mean = array(999999, c(nregion, nreal))
llk.best = array(999999, c(nregion, nreal))
iBest    = array(1, c(nregion, nmodels,nprior))
llk.table.mean = array(0, c(nregion, nmodels,nprior))
llk.table.best = array(0, c(nregion, nmodels,nprior))

dimnames(iBest)[[1]] = mydata$fit$name
dimnames(iBest)[[2]] = paste('model',models,sep='')
dimnames(iBest)[[3]] = paste('prior',prior,sep='')

dimnames(llk.table.mean)[[1]] = mydata$fit$name
dimnames(llk.table.mean)[[2]] = paste('model',models,sep='')
dimnames(llk.table.mean)[[3]] = paste('prior',prior,sep='')

dimnames(llk.table.best)[[1]] = mydata$fit$name
dimnames(llk.table.best)[[2]] = paste('model',models,sep='')
dimnames(llk.table.best)[[3]] = paste('prior',prior,sep='')


for (imodel in 1:nmodels) {

	for (ip in 1:nprior) {
		if (Temp[ip] == 1) {
			subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-nweeks-", nweeksFit)
		} else {
			subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-sig", Temp[ip], "-nweeks-", nweeksFit)
		}
		
		myDir = subDir
		setwd(myDir)

		dataName = paste(dataType, "-", mydata$model$name, "-uncpl-", FY, "-", imodel, "-", nweeksFit, sep = "")
		llk.mean = array(999999, c(nregion, nreal))
		llk.best = array(999999, c(nregion, nreal))
		for (ireal in 1:nreal) {
			filename = paste("mcmc-", dataName, "-", ireal, ".RData", sep = "")
			if(!file.exists(filename)) {
				llk.mean[iregion,ireal] = 999999
				llk.best[iregion,ireal] = 999999
				next
				}
			
			load(filename)
			# This loads a list of mcmc objects with nregion + 1 elements
			for (iregion in 1:nregion) {
				tmp = results.list[[iregion]]
				nlines = dim(tmp)[1]
				nparam1 = dim(tmp)[2]
				nlines2 = nlines/2
				llk.mean[iregion, ireal] = mean(tmp[nlines2:nlines, nparam1])
				llk.best[iregion, ireal] = min(tmp[nlines2:nlines, nparam1])
			}


		} # End of loop on realizations - for now use the best 'mean' to select the chain.  Can change to using llk.best

		for (iregion in 1:nregion) {
			iBest[iregion, imodel,ip] = which.min(llk.mean[iregion, ])
			llk.table.mean[iregion,imodel,ip] = min(llk.mean[iregion,])
			llk.table.best[iregion,imodel,ip] = min(llk.best[iregion,])			
		}


	}


}



filename = paste0(csv_and_pdfDir,'choices-nweeksData-',nweeksData,'.csv')

write.csv(file=filename,iBest)

cat('\n For a csv file with Best chain Info see: ',filename,'\n')

filename = paste0(csv_and_pdfDir,'llk-mean-nweeksData-',nweeksData,'.csv')

write.csv(file=filename,round(llk.table.mean))

cat('\n For a csv file with mean LLK values see: ',filename,'\n')

filename = paste0(csv_and_pdfDir,'llk-best-nweeksData-',nweeksData,'.csv')

write.csv(file=filename,round(llk.table.best))

cat('\n For a csv file with best LLK values see: ',filename,'\n\n')

weeks = mydata$weeks

nweeks = mydata$nweeks

epi = mydata$fit$raw

## Now we can plot everything 

par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))

for (iregion in 1:nregion) {
	cat("Processing Region ",iregion,'\n')
	for (imodel in 1:nmodels) {

		for (ip in 1:nprior) {
			
			if (Temp[ip] == 1) {
				subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-nweeks-", nweeksFit)
			} else {
				subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-sig", Temp[ip], "-nweeks-", nweeksFit)
			}
			myDir = subDir
			setwd(myDir)
			
			dataName = paste0(dataType, "-", mydata$model$name, "-uncpl-", FY, "-", imodel, "-", nweeksFit)

			filename = paste0("profiles-", dataName, "-", iBest[iregion, imodel, ip], ".RData")

			load(filename)

			## get all the results we need to make the frames
			##
			rtn = dump$rtn_ili[, iregion]

			profile = dump$profile_ili[, , iregion]

			profile.mean = rep(NA,nweeks)
			
			for (i in 1:nweeks) profile.mean[i] = mean(profile[,i])
			
			np = dim(profile)[1]

			# Now we can plot
			ymax = max(as.numeric(onset[iregion]) * 5, epi[iregion])
			ymax = min(ymax, 6)

			myEpi = epi[, iregion]

			myEpi[(nweeksData + 1):nweeks] = NA

			myName = mydata$fit$name[iregion]

			myPrior = priorLegend[ip]

			tit = ""
			if (imodel == 1) 
				tit = priorLegend[ip]

			plot(1:nweeks, myEpi[1:nweeks], type = "l", col = "red", lwd = 2, xlab = "", ylab = "%ILI", xaxt = "n", ylim = c(0, ymax), main = tit)

			for (i in seq(from = 1, to = np, by = 10)) lines(1:nweeksData, profile[i, 1:nweeksData], type = "l", col = "grey", lwd = 1, lty = 1)

			for (i in seq(from = 1, to = np, by = 10)) lines((nweeksData):nweeks, profile[i, (nweeksData):nweeks], type = "l", col = "grey", lwd = 1, lty = 2)

			lines(1:nweeksData, rtn[1:nweeksData], type = "l", col = "blue", lwd = 2, lty = 1)

			#lines(nweeksData:nweeks, rtn[(nweeksData):nweeks], type = "l", col = "blue", lwd = 2, lty = 2)

			lines(1:nweeksData, profile.mean[1:nweeksData], type = "l", col = "purple", lwd = 2, lty = 1)

			lines(nweeksData:nweeks, profile.mean[(nweeksData):nweeks], type = "l", col = "purple", lwd = 2, lty = 2)			
			
			lines(1:nweeks, myEpi[1:nweeks], type = "l", col = "red", lwd = 2)

			abline(h = onset[iregion], col = "black", lty = 2)

			rect(xleft = (nweeksData + 1), ybottom = 0, xright = (nweeksData + 4), ytop = (ymax * 1.2), col = rgb(0.15, 0.1, 0.1, alpha = 0.1), border = NA)

			## Add the SH and SV if needed
			if (imodel != 4) {
				if (imodel == 1 | imodel == 3) {

					par(new = TRUE)
					plot(sh[, iregion], lwd = 1, col = "coral", xaxt = "n", yaxt = "n", xlab = "", ylab = "",type='l')
				}
				if (imodel == 2 | imodel == 3) {
					
					par(new = TRUE)
					mySV = school[,iregion]
					mySV[mySV==0] = NA
					plot(mySV, col = "coral", xaxt = "n", yaxt = "n", xlab = "", ylab = "",type = "p", pch = 22, lwd = 2,ylim=c(0,6))

				}
			}
			myChain = paste0('Chain #: ',iBest[iregion, imodel, ip])
			pk_wk_best  = weeks[which.max(rtn)]			
			pk_val_best = round(max(rtn),digits=1)
			pk_best = paste0('P-WK ',pk_wk_best,'/P-Val ',pk_val_best,'%')
			
			profile_pk_wk = rep(0,np)
			for (i in 1:np)profile_pk_wk[i] = which.max(profile[i,])
			groups= cut(profile_pk_wk,breaks=1:nweeks,right=FALSE)
			freq = as.data.frame(table(groups))
			ind = which.max(freq$Freq)
			pk_wk_prfl = weeks[ind]
			pk_val_prfl = max(profile.mean)
			pk_val_prfl = round(pk_val_prfl,digits=1)
			pk_mp = paste0('P-WK ',pk_wk_prfl,'/P-Val ',pk_val_prfl,'%')
			
			
			#legend("topleft", c(myName, modelLegend[imodel],myChain,pk_best,pk_mp), text.col=c('black','black','black','blue','purple'),bty = "n")
			legend("topleft", c(myName, modelLegend[imodel],myChain,pk_mp), text.col=c('black','black','black','purple'),bty = "n")
			#legend("topright", c(paste("Best llk=", round(llk.table.best[iregion, imodel, ip])), paste("Mean llk=", round(llk.table.mean[iregion, imodel, ip]))), bty = "n",text.col=c('blue','purple'))
			legend("topright", paste("Mean llk=", round(llk.table.mean[iregion, imodel, ip])), bty = "n",text.col='purple')
			axis(1, at = 1:nweeks, label = weeks)


		}


	}
}

if (pdf == TRUE) dev.off()

cat("\n For a plot of the results see: ",pdfName,'\n\n')



