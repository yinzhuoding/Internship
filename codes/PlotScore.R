rm(list=ls())
require(DICE)
start.year = 2016
forecast.week = seq(43,52,1)
mydata = get.subset(start.year=start.year,end.year=start.year+1, mod_level = 2, fit_level=3)


dataDir = paste0("~/Dropbox/LEPR02/CDC-Flu-Challenge-2016-2017/weekly_submission")
RepDir = paste0("~/Dropbox/LEPR02/")
source("~/FluFore/codes/ScoreCsv.R")


nregion = mydata$fit$nregion
score.st = array(NA, c(nregion,length(forecast.week)))
score.pw = array(NA, c(nregion,length(forecast.week)))
score.pi = array(NA, c(nregion,length(forecast.week)))
score.fc = array(NA, c(nregion,length(forecast.week), 4))
nat.st = rep(NA,length(forecast.week))
nat.pw = rep(NA,length(forecast.week))
nat.pi = rep(NA,length(forecast.week))
nat.fc = array(NA,c(length(forecast.week),4))
for(i in 1:length(forecast.week)) {
  week = forecast.week[i]
  nweeksFit = which(mydata$weeks==week)
  filename = list.files(path=paste0(RepDir,"CDC-Flu-Challenge-",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",week,".*\\.csv"))
  est = read.csv(file=paste0(RepDir,"CDC-Flu-Challenge-",start.year,"-",start.year+1,"/weekly_submission/",filename))
  ScoreCsv(mydata,est,nweeksFit,peak = TRUE)
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
xrange = c(1,52)
week = mydata$weeks
wk = forecast.week[length(forecast.week)]
filename = list.files(path=paste0(RepDir,"CDC-Flu-Challenge-",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",wk,".*\\.csv"))
plotname = strsplit(filename,"[.]")[[1]][1]
# start week
pdf(file = paste("Score-plot-OnsetWeek-", plotname, ".pdf", sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[forecast.week], nat.st, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = nat.real[1,3], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[forecast.week], score.st[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[1,3]], col = "grey")
  title(main = paste("Region",iregion))
}
dev.off()
# peak week
pdf(file = paste("Score-plot-PeakWeek-", plotname, ".pdf", sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[forecast.week], nat.pw, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[forecast.week], score.pw[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[2,3]], col = "grey")
  title(main = paste("Region",iregion))
}
plot(1:52,rawdata,type = "l", col = "blue", xaxt = "n", xlab = "EW")
axis(1, at = 1:52, label = week)
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "Data")
dev.off()
# peak intensity
pdf(file = paste("Score-plot-PeakIntensity-", plotname, ".pdf", sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
points(week[forecast.week], nat.pi, type = "b", pch = 16, col = alpha("blue",0.5))
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "National")
# hhs region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  points(week[forecast.week], score.pi[iregion,], type = "b", pch = 16, col = alpha("blue",0.5))
  abline(v = week[subset(reg.real, Location == iregion)[2,3]], col = "grey")
  title(main = paste("Region",iregion))
}
plot(1:52,rawdata,type = "l", col = "blue", xaxt = "n", xlab = "EW")
axis(1, at = 1:52, label = week)
abline(v = week[nat.real[2,3]], col = "grey")
title(main = "Data")
dev.off()
# forecast
pdf(file = paste("Score-plot-Forecast-", plotname, ".pdf", sep = ""), width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
# national
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, at = 1:52, label = week)
p.col = rainbow(4)
for (i in 1:4) {
  points(week[forecast.week], nat.fc[ ,i], type = "b", pch = 16, col = alpha(p.col[i],0.5))
}
legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead"), text.col = p.col, bty = "n",xjust = 0)
title(main = "National")
# hhs region
p.col = rainbow(4)
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, at = 1:52, label = week)
  for (i in 1:4) {
    points(week[forecast.week], score.fc[iregion, ,i], type = "b", pch = 16, col = alpha(p.col[i],0.5))
  }
  legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead"), text.col = p.col, bty = "n",xjust = 0)
  title(main = paste("Region",iregion))
}
dev.off()  
