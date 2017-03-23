library(raster)
library(rgeos)
library(dplyr)
library(ggmap)
library(animation)

dataType = "cdc"
year = 2015
nweeksFit = 52
model = 5
isingle = 0
nMCMC = 1000
nreal = 1

mydata = get.DICE.data(dataType = dataType, year = year, mod_level = 2, fit_level = 3, RegState = "BRA",model = model, nweeksFit = nweeksFit, isingle=isingle)
country = mydata$model$attr$ABBV_2
region_num = length(mydata$fit$factor)

port1 = suppressMessages(getData('GADM', country = country, level = 1))
port1$NAME_1 = as.factor(as.character(port1$NAME_1))
name = port1$NAME_1
port1 = gSimplify(port1, tol = 0.01, topologyPreserve = TRUE)

map1 = fortify(port1) 
map1$id = as.integer(map1$id)
dat = data.frame(id = 1:(length(name)), state = name)
map1.df = inner_join(map1, dat, by = "id")

#nation_long = suppressMessages(geocode(country)$lon)
#nation_lat = suppressMessages(geocode(country)$lat)

longitude = mydata$fit$attr$lon
latitude = mydata$fit$attr$lat
output = runDICE(dataType = dataType, year = year, mod_level = 2, fit_level = 3, model = model, isingle = isingle, nMCMC = nMCMC, nreal = nreal)
onset = output$rtn
for(i in 1:ncol(output$rtn)) {
  onset[,i] = output$rtn[,i]/as.numeric(mydata$fit$factor[i])
}

region_r = onset
for(i in 1:ncol(onset)) {
  region_r[,i] = onset[,i] / min(onset[,i])
}

col = rainbow(region_num)
week = mydata$weeks
## Make a movie
mainDir = getwd()
subDir = "output"
err = makeDir(subDir = subDir)
setwd(paste(mainDir,subDir,sep = "/"))
frame = nrow(onset)

for(i in 1:frame) {
  if(i < 10) {
    name = paste('000',i,'plot.png',sep = '')
  }
  if(i >= 10) {
    name = paste('00',i,'plot.png',sep = '')
  }
  p = ggplot() +
    geom_map(data = map1.df, map = map1.df,
             aes(map_id = id, x = long, y = lat, group = group),
             size = 0.25, fill = "grey") +
    coord_map() +
    #geom_point(aes(x = nation_long, y = nation_lat), size = 2 * nation_r[i], col = "grey", show.legend = FALSE, alpha = 0.5) +
    scale_x_continuous(name = "",limits = c(-130,-60)) +
    scale_y_continuous(name = "",limits = c(25,50)) +
    ggtitle(paste("USA map colored by CDC region - week ", week[i], sep = '')) +
    theme_bw() + 
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 15, family = "serif", face = "bold"))
  for(j in 1:10) {
    p = p + geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 4 * region_r[i,j],  col = col[j], show.legend = FALSE,alpha = 0.8)
  }
  suppressMessages(ggsave(filename = name))
}

moviename = "map.gif"
system("convert -delay 20 *.png map.gif")
file.remove(list.files(pattern = ".png"))
setwd(mainDir)
cat("For movie see: ", subDir, "/", moviename, sep = "")



p = ggplot() +
  geom_map(data = map1.df, map = map1.df,
           aes(map_id = id, x = long, y = lat, group = group),
           size = 0.125, fill = "grey") +
  coord_map() +
  #geom_point(aes(x = nation_long, y = nation_lat), size = 2 * nation_r[i], col = "grey", show.legend = FALSE, alpha = 0.5) +
  scale_x_continuous(name = "",limits = c(-130,-60)) +
  scale_y_continuous(name = "",limits = c(25,50)) +
  ggtitle(paste("USA map colored by CDC region - week ", week[i], sep = '')) +
  #theme_bw() + 
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 15, family = "serif", face = "bold"))
for(j in 1:region_num) {
  p = p + geom_point(aes_string(x = longitude[j], y = latitude[j]), col = col[j], size = 4 * region_r[i,j], show.legend = FALSE,alpha = 0.8)
}
