rm(list=ls())
require(DICE)
start.year = 2016
forecast.week = seq(43,52,1)
mydata = get.subset(start.year = start.year,end.year = start.year+1,fit_level = 3)
#start.year = c(rep(2016,10),rep(2017,9))
#forecast.week = c(seq(43,52,1),seq(1,9,1))
#mydata1 = get.subset(start.year = 2016, end.year = 2017, fit_level = 3)
#mydata2 = get.subset(start.year = 2017)


#RepDir = "~/GitReps/FluFore/"
RepDir = "~/FluFore/"

# load Submission reading and plotting codes
source(paste0(RepDir,"codes/ScoreCsv.R"))
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
  filename = list.files(path=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",week,".*\\.csv"))
  est = read.csv(file=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/",filename))
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
y.min = -10
y.max = 0
xrange = c(forecast.week[1],forecast.week[length(forecast.week)] + 2)
#xrange = c(1,52)
week = mydata$weeks
target.name = c("Onset","Peak week","Peak intensity","1-week","2-week","3-week","4-week")
pdf(file = "sample.pdf", width = 12, height = 11)
par(mar = c(4, 3, 1, 1), mfrow = c(3, 4))
## National
plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
axis(1, forecast.week)
color = rainbow(7)
points(forecast.week, nat.st, type = "l", lwd = 2, col = color[1])
points(forecast.week, nat.pw, type = "l", lwd = 2, col = color[2])
points(forecast.week, nat.pi, type = "l", lwd = 2, col = color[3])
for (i in 1:4) {
  points(forecast.week, nat.fc[ ,i], type = "l", lwd = 2, col = color[i+3])
}
nat.sum = c(sum(nat.st),sum(nat.pw),sum(nat.pi),apply(nat.fc,2,sum,na.rm = TRUE))
legend("bottomright", paste(target.name,": ",round(nat.sum,1),sep = ""), lty = rep(1,7), col = color, bty = "n",xjust = 1)
title(main = "National")

## HHS Region
for (iregion in 1:nregion) {
  plot(0, type = "n",xlab = "EW", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, forecast.week)
  points(forecast.week, score.st[iregion,], type = "l", lwd = 2, col = color[1])
  points(forecast.week, score.pw[iregion,], type = "l", lwd = 2, col = color[2])
  points(forecast.week, score.pi[iregion,], type = "l", lwd = 2, col = color[3])
  for (i in 1:4) {
    points(forecast.week, score.fc[iregion, ,i], type = "l", lwd = 2, col = color[i+3])
  }
  reg.sum = array(NA,c(10,7))
  reg.sum[,1] = apply(score.st, 1, sum)
  reg.sum[,2] = apply(score.pw, 1, sum)
  reg.sum[,3] = apply(score.pi, 1, sum)
  reg.sum[,4:7] = apply(score.fc,c(1,3),sum,na.rm = TRUE)
  legend("bottomright", paste(target.name,": ",round(reg.sum[iregion,],1),sep = ""), lty = rep(1,7), col = color, bty = "n",xjust = 1)
  title(main = paste("Region",iregion, sep = ""))
}
dev.off()


