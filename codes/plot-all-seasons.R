rm(list = ls())

graphics.off()

library(DICE)

library(RColorBrewer)

pdf = TRUE

mod_level = 2
fit_level = 3
dataType = "cdc"
year.start.vec = 2004:2016
year.end.vec = 2005:2017

nyears = length(year.start.vec)

col = colorRampPalette(brewer.pal(11, "Spectral"))(nyears - 1)

epiList = list()
epiNatl = list()
weeksList = list()
nweeks.vec = rep(0, nyears)
FY.vec = rep(0, nyears)

nregion = 10
nregion1 = nregion + 1
nweeks = 52
epi_mean = array(0, c(nweeks, nregion1))
pk_valMAT = array(0, c(nyears, nregion1))
pk_wkMAT = array(0, c(nyears, nregion1))

for (iyear in 1:nyears) {
	year.start = year.start.vec[iyear]
	year.end = year.end.vec[iyear]
	if (year.start == 2009) 
		next
	mydata = get.subset(mod_level = mod_level, fit_level = fit_level, dataType = dataType, start.year = year.start, end.year = year.end)
	print(mydata$nweeksData)
	weeksList[[iyear]] = mydata$weeks
	nweeks.vec[iyear] = length(weeks)
	epiList[[iyear]] = mydata$fit$raw
	epiNatl[[iyear]] = mydata$model$raw
	FY.vec[iyear] = FY = paste0(year.start, "-", year.end)
	epi = epiList[[iyear]]
	weeks = weeksList[[iyear]]
	nregion = dim(epi)[2]
	for (iregion in 1:nregion) {
		pk_valMAT[iyear, iregion] = max(epi[, iregion])
		pk_wkMAT[iyear, iregion] = which.max(epi[, iregion])
	}
	pk_valMAT[iyear, nregion1] = max(epiNatl[[iyear]])
	pk_wkMAT[iyear, nregion1] = which.max(epiNatl[[iyear]])
	if (iyear != nyears) {
		epi_mean[1:nweeks, 1:nregion] = epi_mean[1:nweeks, 1:nregion] + mydata$fit$raw[1:52, ]
		epi_mean[1:nweeks, nregion1] = epi_mean[1:nweeks, nregion1] + mydata$model$raw[1:52]
	}
}

epi_mean = epi_mean/(nyears - 2)
nweeksData = mydata$nweeksData
if (pdf == TRUE) {
	file = paste0("regions-all-seasons-week-", nweeksData, ".pdf")
	pdf(file, width = 15, height = 9)
}

par(mar = c(4, 3, 2, 1), mfrow = c(2, 5))

nregion = dim(epiList[[1]])[2]

col[nyears] = "black"

for (iregion in 1:nregion) {
	pk_val = rep(NA, nyears)
	pk_wk = rep(NA, nyears)
	for (iyear in 1:nyears) {
		year.start = year.start.vec[iyear]
		year.end = year.end.vec[iyear]
		if (year.start == 2009) 
			next

		epi = epiList[[iyear]]
		weeks = weeksList[[iyear]]
		nweeks = length(weeks)
		if (iyear == nyears) {
			index = which(epi[, iregion] == 0)
			epi[index, iregion] = NA
		}

		FY = paste0(year.start, "-", year.end)

		if (iyear == 1) {
			myName = mydata$fit$name[iregion]
			plot(1:nweeks, epi[1:nweeks, iregion], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2, ylim = c(0, max(pk_valMAT[, iregion])), main = myName)
		} else {
			lines(1:nweeks, epi[1:nweeks, iregion], xaxt = "n", xlab = "", ylab = "% ILI", type = "l", col = col[iyear], lwd = 2)
		}


		pk_wk[iyear] = which.max(epi[, iregion])
		pk_wk[iyear] = weeks[pk_wk[iyear]]
		pk_val[iyear] = max(epi[, iregion], na.rm = TRUE)
		pk_val[iyear] = round(pk_val[iyear], digits = 1)

	}
	# This is the histroc average - not including the current season
	lines(1:52, epi_mean[, iregion], col = "black", lwd = 1, lty = 2)
	pk_wk = pk_wk[!is.na(pk_wk)]
	pk_val = pk_val[!is.na(pk_val)]
	n = length(pk_wk)
	nm1 = n - 1
	pk_wk = pk_wk[1:nm1]
	pk_val = pk_val[1:nm1]

	abline(h = mydata$fit$onset[, iregion], lwd = 2, lty = 2, col = "grey")
	## Last three years averagge
	pk_wk_ave = mean(pk_wkMAT[(nyears - 3):(nyears - 1), iregion])
	pk_wk_ave = weeks[round(pk_wk_ave)]
	pk_val_ave = round(mean(pk_valMAT[(nyears - 3):(nyears - 1), iregion]), digits = 1)
	pk_wk_hst = which.max(epi_mean[, iregion])
	pk_wk_hst = weeks[pk_wk_hst]
	pk_val_hst = max(epi_mean[, iregion])
	pk_val_hst = round(pk_val_hst, digits = 1)
	legend("topleft", legend = c(pk_wk, pk_wk_ave, pk_wk_hst), bty = "n", text.col = c(col[1:nm1], "black", "black"))
	legend("topright", legend = c(pk_val, pk_val_ave, pk_val_hst), bty = "n", text.col = c(col[1:nm1], "black", "black"))
	axis(1, at = 1:nweeks, weeks)

}

if (pdf == TRUE) {
	dev.off()
	file = paste0("natl-all-seasons-week-", nweeksData, ".pdf")
	pdf(file = file)
} else {
	dev.new()
}

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

# This is the histroc average - not including the current season
lines(1:52, epi_mean[, nregion1], col = "black", lwd = 1, lty = 2)
pk_wk = pk_wk[!is.na(pk_wk)]
pk_val = pk_val[!is.na(pk_val)]

pk_wk_ave = mean(pk_wkMAT[(nyears - 3):(nyears - 1), nregion1])
pk_wk_ave = weeks[round(pk_wk_ave)]
pk_val_ave = round(mean(pk_valMAT[(nyears - 3):(nyears - 1), nregion1]), digits = 1)

n = length(pk_wk)
nm1 = n - 1
pk_wk = pk_wk[1:nm1]
pk_val = pk_val[1:nm1]
pk_wk_hst = which.max(epi_mean[, nregion1])
pk_wk_hst = weeks[pk_wk_hst]
pk_val_hst = max(epi_mean[, nregion1])
pk_val_hst = round(pk_val_hst, digits = 1)

abline(h = mydata$model$onset, lwd = 2, lty = 2, col = "grey")
legend("topleft", legend = c(pk_wk, pk_wk_ave, pk_wk_hst), bty = "n", text.col = c(col[1:nm1], "black", "black"))
legend("topright", legend = c(pk_val, pk_val_ave, pk_val_hst), bty = "n", text.col = c(col[1:nm1], "black", "black"))
axis(1, at = 1:nweeks, weeks)

if (pdf == TRUE) 
	dev.off()
	
