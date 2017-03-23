rm(list = ls())
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
library(tidyr)
week = qdata$week
year = qdata$year
n = length(week)
total.raw = qdata$Total
states = qdata$states
var = colnames(total.raw)
total.state = total.raw[, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))] 
date = as.Date(paste(rep("0",n),week,year,sep = "-"),format = "%w-%W-%Y")
geoinfo.state = read.csv("geoinfo_state.csv")
lon.state = geoinfo.state$lon
lat.state = geoinfo.state$lat
total.state.std = as.data.frame(apply(total.state,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
map.total.state = cbind.data.frame(states,lon.state, lat.state, t(total.state.std))
colnames(map.total.state) = c("state", "lon", "lat",paste(year,week,sep = "-"))
long.total.state = gather(map.total.state,date,value,4:ncol(map.total.state))
long.total.state$date = as.Date(paste(long.total.state$date,rep(0,nrow(long.total.state)),sep = '-'), format = "%Y-%W-%w")
#long.total.state$value = (long.total.state$value - min(long.total.state$value))/(max(long.total.state$value) - min(long.total.state$value))
p1 = gvisMotionChart(long.total.state,idvar = "state",timevar = "date")
plot(p1)
