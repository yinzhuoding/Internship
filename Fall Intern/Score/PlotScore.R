rm(list=ls())
start.year = 2016
end.year  = 2017
mod_level = 2
fit_level = 3
dataType = 'cdc'

require(DICE)
require(httr)
mydata = get.subset(mod_level=mod_level,fit_level=fit_level,dataType=dataType,
                    start.year = start.year,end.year=end.year, name = c(NAME_2 = "USA"))


RepDir = "~/FluFore/"
source(paste0(RepDir,"codes/ScoreCsv.R"))
#dataDir = paste0("~/Dropbox/LEPR02/CDC-Flu-Challenge-2016-2017/weekly_submission")
#setwd(dataDir)
weeknum = seq(43,50,1)
teamname = "PSI"
year = 2016
dateset = c("11-07", "11-14", "11-21", "11-29", "12-05", "12-12", "12-19", "12-28")
week = mydata$weeks

nregion = mydata$fit$nregion
score.st = array(NA, c(nregion,length(weeknum)))
score.pw = array(NA, c(nregion,length(weeknum)))
score.pi = array(NA, c(nregion,length(weeknum)))
score.fc = array(NA, c(nregion,length(weeknum), 4))
nat.st = rep(NA,length(weeknum))
nat.pw = rep(NA,length(weeknum))
nat.pi = rep(NA,length(weeknum))
nat.fc = array(NA,c(length(weeknum),4))
for(i in 1:length(weeknum)) {
  wk = weeknum[i]
  #date = dateset[i]
  filename = list.files(path=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",wk,".*\\.csv"))
  #filename = paste("EW",wk,"-",teamname,"-",year,"-",date,".csv", sep = '')
  est = read.csv(file=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/",filename))
  nweeksFit = which(mydata$weeks==wk)
  peak = TRUE
  #if(i >= 5) {
    #peak = TRUE
  #}
  ScoreCsv(mydata,est,nweeksFit, peak = peak)
  nat.score = read.csv(paste("Nationalscore",nweeksFit,".csv", sep = ""))
  reg.score = read.csv(paste("Regscore",nweeksFit,".csv", sep = ""))
  nat.st[i] = subset(nat.score, Target == "Start week")$Log.score
  nat.pw[i] = subset(nat.score, Target == "Peak week")$Log.score
  nat.pi[i] = subset(nat.score, Target == "Peak intensity")$Log.score
  nat.fc[i, ] = subset(nat.score, Target == "Forecast")$Log.score
  for (iregion in 1:nregion) {
    score.st[iregion,i] = subset(reg.score, (Location == iregion) & (Target == "Start week"))$Log.score
    score.pw[iregion,i] = subset(reg.score, (Location == iregion) & (Target == "Peak week"))$Log.score
    score.pi[iregion,i] = subset(reg.score, (Location == iregion) & (Target == "Peak intensity"))$Log.score
    score.fc[iregion,i, ] = subset(reg.score, (Location == iregion) & (Target == "Forecast"))$Log.score
  }
  nat.real = nat.score[,c(1,2,4)]
  reg.real = reg.score[,c(1,2,4)]
}

rawdata = mydata$model$raw
rawdata[rawdata == 0] = NA
nat.onset = mydata$model$onset
reg.onset = as.numeric(mydata$fit$onset)

y.min = -10
y.max = 0
#xrange = c(weeknum[1], weeknum[length(weeknum)])
xrange = c(1,52)
# start week
wk = weeknum[length(weeknum)]
date = dateset[length(dateset)]
plotname = paste("EW",wk,"-",teamname,"-",year,"-",date,".pdf", sep = '')
pdf(file = paste("Score-plot-OnsetWeek-", plotname, sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[weeknum], nat.st, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = nat.real[1,3], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[weeknum], score.st[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[1,3]], col = "grey")
  title(main = paste("Region",iregion))
}
dev.off()
# peak week
pdf(file = paste("Score-plot-PeakWeek-", plotname, sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[weeknum], nat.pw, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[weeknum], score.pw[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[2,3]], col = "grey")
  title(main = paste("Region",iregion))
}
plot(1:52,rawdata,type = "l", col = "blue", xaxt = "n", xlab = "EW")
axis(1, at = 1:52, label = week)
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "Data")
dev.off()
# peak intensity
pdf(file = paste("Score-plot-PeakIntensity-", plotname, sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[weeknum], nat.pi, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[weeknum], score.pi[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[2,3]], col = "grey")
  title(main = paste("Region",iregion))
}
plot(1:52,rawdata,type = "l", col = "blue", xaxt = "n", xlab = "EW")
axis(1, at = 1:52, label = week)
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "Data")
dev.off()
# forecast
pdf(file = paste("Score-plot-Forecast-", plotname, sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
p.col = rainbow(4)
for (i in 1:4) {
  points(week[weeknum], nat.fc[ ,i], type = "b", pch = 16, col = alpha(p.col[i],0.5))
}
legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead"), text.col = p.col, bty = "n",xjust = 0)
title(main = "National")
# hhs region
p.col = rainbow(4)
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  for (i in 1:4) {
    points(week[weeknum], score.fc[iregion, ,i], type = "b", pch = 16, col = alpha(p.col[i],0.5))
  }
  legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead"), text.col = p.col, bty = "n",xjust = 0)
  title(main = paste("Region",iregion))
}
dev.off()  
