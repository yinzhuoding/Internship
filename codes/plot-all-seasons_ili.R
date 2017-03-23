rm(list = ls())

graphics.off()

library(DICE)

library(RColorBrewer)

mod_level = 2
fit_level = 3
dataType = "cdc"
year.start.vec = 2004:2016
year.end.vec = 2005:2017

nyears = length(year.start.vec)

col = colorRampPalette(brewer.pal(11, "Spectral"))(nyears-1)

epiList = list()
epiNatl  = list()
weeksList = list()
nweeks.vec = rep(0, nyears)
FY.vec = rep(0, nyears)
pk_valMAT = array(0,c(nyears,10))
pk_wkMAT  = array(0,c(nyears,10))
for (iyear in 1:nyears) {
	year.start = year.start.vec[iyear]
	year.end = year.end.vec[iyear]
	if (year.start == 2009) 
		next
	mydata = get.subset(mod_level = mod_level, fit_level = fit_level, dataType = dataType, start.year = year.start, end.year = year.end)

	weeksList[[iyear]] = mydata$weeks
	nweeks.vec[iyear] = length(weeks)
	epiList[[iyear]] = mydata$fit$raw
	epiNatl[[iyear]] = mydata$model$raw
	FY.vec[iyear] = FY = paste0(year.start, "-", year.end)
	epi = epiList[[iyear]]
	weeks = weeksList[[iyear]]
	nregion = dim(epi)[2]
	for (iregion in 1:nregion) {
		pk_valMAT[iyear,iregion] = max(epi[, iregion])
		pk_wkMAT[iyear,iregion] = which.max(epi[, iregion])
		
	}
}

pdf(file=paste0("all-regions-EW",mydata$weeks[mydata$nweeksData],".pdf"), width=14, height=8)
par(mar = c(4, 3, 2, 1), mfrow = c(2, 5))

nregion = dim(epiList[[1]])[2]

col[nyears] = 'black'

for (iregion in 1:nregion) {
	pk_val = rep(NA, nyears)
	pk_wk  = rep(NA, nyears)
	for (iyear in 1:nyears) {
		year.start = year.start.vec[iyear]
		year.end = year.end.vec[iyear]
		if (year.start == 2009) 
			next

		epi = epiList[[iyear]]
		weeks = weeksList[[iyear]]
		nweeks = length(weeks)
		if (iyear == nyears) {
			index = which(epi[,iregion] == 0)
			epi[index,iregion] = NA	
		}
		
		FY = paste0(year.start, "-", year.end)

		if (iyear == 1) {
			myName = mydata$fit$name[iregion]
			plot(1:nweeks,epi[1:nweeks, iregion], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2,ylim=c(0,max(pk_valMAT[,iregion])),main=myName)
		} else {
			lines(1:nweeks,epi[1:nweeks, iregion], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2)
		}
		
		
		pk_wk[iyear] = which.max(epi[, iregion])
		pk_wk[iyear] = weeks[pk_wk[iyear]]
		pk_val[iyear] = max(epi[, iregion],na.rm=TRUE)
		pk_val[iyear] = round(pk_val[iyear], digits = 1)

	}
	pk_wk = pk_wk[!is.na(pk_wk)]
	pk_val = pk_val[!is.na(pk_val)]
	n = length(pk_wk)
	nm1 = n-1
	pk_wk = pk_wk[1:nm1]
	pk_val = pk_val[1:nm1]
	
	abline(h = mydata$fit$onset[, iregion], lwd = 2, lty = 2, col = "grey")
	## Last three years averagge
	pk_wk_ave  = mean(pk_wkMAT[(nyears-3):(nyears-1),iregion])
	pk_wk_ave = weeks[round(pk_wk_ave)]
	pk_val_ave = round(mean(pk_valMAT[(nyears-3):(nyears-1),iregion]),digits=1)
	legend("topleft",legend=c(pk_wk,pk_wk_ave), bty = "n",text.col=c(col[1:nm1],'black'))
	legend("topright", legend=c(pk_val,pk_val_ave), bty = "n",text.col=c(col[1:nm1],'black'))
	axis(1, at = 1:nweeks, weeks)	
	
}

dev.off()
#dev.new()
pdf(file=paste0("national-EW",mydata$weeks[mydata$nweeksData],".pdf"))

	for (iyear in 1:nyears) {
		year.start = year.start.vec[iyear]
		year.end = year.end.vec[iyear]
		if (year.start == 2009) 
			next

		epi = epiNatl[[iyear]]
		weeks = weeksList[[iyear]]
		nweeks = length(weeks)
		if (iyear == nyears) {
			index = which(epi == 0)
			epi[index] = NA
		}

		FY = paste0(year.start, "-", year.end)

		if (iyear == 1) {
			myName = mydata$model$name
			plot(1:nweeks, epi[1:nweeks], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2, ylim = c(0, 6.5), main = myName)
		} else {
			lines(1:nweeks, epi[1:nweeks], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2)
		}


		pk_wk[iyear] = which.max(epi)
		pk_wk[iyear] = weeks[pk_wk[iyear]]
		pk_val[iyear] = max(epi, na.rm = TRUE)
		pk_val[iyear] = round(pk_val[iyear], digits = 1)

	}
	
	pk_wk = pk_wk[!is.na(pk_wk)]
	pk_val = pk_val[!is.na(pk_val)]
	n = length(pk_wk)
	nm1 = n-1
	pk_wk = pk_wk[1:nm1]
	pk_val = pk_val[1:nm1]
	abline(h = mydata$model$onset, lwd = 2, lty = 2, col = "grey")
	legend("topleft", legend = pk_wk, bty = "n",text.col=col[1:nm1])
	legend("topright", legend = pk_val, bty = "n",text.col=col[1:nm1])
	axis(1, at = 1:nweeks, weeks)

dev.off()