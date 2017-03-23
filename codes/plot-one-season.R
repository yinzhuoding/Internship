rm(list=ls())

library(DICE)

mod_level = 2
fit_level = 3
dataType = 'cdc'
year.start = 2015
year.end = 2016

mydata = get.subset(mod_level=mod_level,fit_level=fit_level,dataType=dataType,start.year = year.start,end.year=year.end)

weeks = mydata$weeks
nweeks = length(weeks)
epi = mydata$fit$raw
nregion = dim(epi)[2]
FY = paste0(year.start,'-',year.end)

par(mar = c(4, 3, 1, 1), mfrow = c(2,5))

for (iregion in 1:nregion) {
	plot(epi[,iregion],xaxt='n',xlab='week #',ylab = '% ILI',type='l',col='blue',lwd=2)
	myName = mydata$fit$name[iregion]
	pk_wk = which.max(epi[,iregion])
	pk_wk = weeks[pk_wk]
	pk_val = max(epi[,iregion])
	pk_val = round(pk_val,digits=1)
	pk_leg = paste0('P-WK ',pk_wk,'/P-Val ',pk_val,'%')
	legend('topleft',c(myName,FY,pk_leg),bty='n')
	axis(1,at=1:nweeks,weeks)
	abline(h=mydata$fit$onset[,iregion],lwd=2,lty=2,col='grey')
}