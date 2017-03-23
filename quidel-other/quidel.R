rm(list = ls())
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
week = qdata$week
year = qdata$year
n = length(week)
library(googleVis)
## Preparation of data
# all
total.raw = qdata$Total
# nation
total.nation = qdata$Total$USA 
# region
regions = qdata$regions
total.region = total.raw[,paste("USA",regions,sep = ".")] 

# state 
states = qdata$states
var = colnames(total.raw)
total.state = total.raw[, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))] 

# county
othercounty = c("Baltimore City","Anchorage Borough","Nome Census Area","Parish")
county.index = which(grepl('County',var))
for(county in othercounty) {
  county.index = union(county.index, which(grepl(county,var)))
}
total.county = total.raw[,sort(county.index)]

# city
cities = qdata$cities[,2]
city.index = NULL
for (city in cities) {
  city.index = union(city.index, which(grepl(city,var)))
}
city.index = setdiff(city.index, which(grepl('County',var)))
for (county in othercounty) {
  city.index = setdiff(city.index, which(grepl(county,var)))
}
total.city = total.raw[,sort(city.index)]

# zip
zips = qdata$zips
zip.index = NULL
for (zip in zips) {
  zip.index = union(zip.index, which(grepl(zip,var)))
}
zip.index = setdiff(zip.index, city.index)
total.zip = total.raw[,sort(zip.index)]

########################################
date = as.Date(paste(rep("0",n),week,year,sep = "-"),format = "%w-%W-%Y")
#data.nat = cbind.data.frame(Date = date, Total = total.nat)

# Geochart for the state level
geoinfo.nation = read.csv("geoinfo_nation.csv")
geoinfo.region = read.csv("geoinfo_region.csv")
geoinfo.state = read.csv("geoinfo_state.csv")
geoinfo.county = read.csv("geoinfo_county.csv")
geoinfo.city = read.csv("geoinfo_city.csv")
geoinfo.zip = read.csv("geoinfo_zip.csv")

lon.state = geoinfo.state$lon
lat.state = geoinfo.state$lat

total.state.std = as.data.frame(apply(total.state,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
map.total.state = cbind.data.frame(states,lon.state, lat.state, t(total.state.std))
colnames(map.total.state) = c("state", "lon", "lat",paste(year,week,sep = "-"))
# geochart_state= gvisGeoChart(map.total.state,locationvar = 'state', colorvar = '2015-38',
#                   options = list(region = 'US',displayMode='markers', backgroundColor = "lightgrey"))
# plot(geochart_state)
# saveHTML(plot(geochart_state))

dev.off()
print(geochart_state, file = "plot3.html")
library(tidyr)
long.total.state = gather(map.total.state,date,value,4:ncol(map.total.state))
long.total.state$date = as.Date(paste(long.total.state$date,rep(0,nrow(long.total.state)),sep = '-'), format = "%Y-%W-%w")
#long.total.state$value = (long.total.state$value - min(long.total.state$value))/(max(long.total.state$value) - min(long.total.state$value))
p1 = gvisMotionChart(long.total.state,idvar = "state",timevar = "date")
plot(p1)

print(p1,'html',file = "MotionChart.html")
cat(p1$html$chart, file="temp.html")
# plot2 = gvisGeoChart(map.total.state,locationvar = 'key', colorvar = '2015-38',
#                     options = list(region = 'US',displayMode='regions',resolution="provinces"))
# plot(gvisMerge(plot1,plot2))
# plot(plot2)
demo(AnimatedGeoChart)


sample = rbind.data.frame(map.total.state[1:5,1:4],map.total.county[1:5,1:4])
library(tidyr)
sample = cbind(c(rep("State",5), rep("County",5)), sample)
colnames(sample)[1] = "location"
samplelong = gather(sample,date,value,3:5)
sample2long = samplelong[,-1]
samplelong$date = as.Date(paste(samplelong$date,rep(0,10),sep = '-'), format = "%Y-%W-%w")
plot(gvisMotionChart(samplelong,idvar = "key",timevar = "date"))
#plot(gvisGeoMap(map.total.state,'state', '2015-35',
#                   options = list(region = 'US',dataMode='markers')))

counties = as.character(qdata$counties[,2])
map.total.county = cbind.data.frame(rep(0.1,length(counties)), counties,t(total.county))
colnames(map.total.county) = c("size","key",paste(year,week,sep = "-"))
p1 = gvisGeoChart(map.total.county,locationvar = 'key',  colorvar = '2015-36',
                  options = list(region = 'US',displayMode='markers',resolution="metros"))
plot(p1)
sample = map.total.county[1,]
sample$key = "Washington County,AR"
p2 = gvisGeoChart(map.total.county,locationvar = 'key', colorvar = '2015-36',
                  options = list(region = 'US-AR',displayMode='regions',resolution="metros"))
p2 = gvisGeoChart(sample,locationvar = 'key', colorvar = '2015-36',
                  options = list(region = 'US-AR',displayMode='regions',resolution="metros"))
plot(p2)
plot(gvisMerge(p1,p2))
plot(gvisGeoMap(map.total.county,'county', '2016-40',
                options = list(region = 'US',dataMode='markers')))


saveGIF(plot(p1))
cities = as.character(qdata$cities[,2])
map.total.city = cbind.data.frame(cities,t(total.city))
colnames(map.total.city) = c("key",paste(year,week,sep = "-"))
p1 = gvisGeoChart(map.total.city,locationvar = 'key',colorvar = '2015-36',
                  options = list(region = 'US',displayMode='markers',resolution="metros"))
p2 = gvisGeoChart(map.total.city,locationvar = 'key', colorvar = '2015-36',
                  options = list(region = 'US',displayMode='regions',resolution="metros"))
plot(p1)
plot(p2)

p = gvisGeoChart(total.region.std,locationvar = 'LatLong', colorvar = '1',hovervar = "Region",
                 options = list(region = 'US', displayMode='markers'))
plot(p)

