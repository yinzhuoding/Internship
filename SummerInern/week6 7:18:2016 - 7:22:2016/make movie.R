#require(DICE)
#library(ggplot2)
#library(sp)
library(raster)
library(rgeos)
library(dplyr)
#library(maptools)
#require(RColorBrewer)

#library(maps)
library(ggmap)
library(animation)

port1 = getData('GADM', country = 'USA', level = 1)
port1$NAME_1 = as.factor(as.character(port1$NAME_1))
name = port1$NAME_1
port1 = gSimplify(port1, tol=0.01, topologyPreserve=TRUE)
## fills the map basing on variable NAME_1
region_list = list(Region0 = c("Maine","New Hampshire","Massachusetts","Rhode Island","Connecticut","Vermont"),
                   Region1 = c("New York","New Jersey","Puerto Rico"),
                   Region2 = c("Pennsylvania","Delaware","Maryland","West Virginia","Virginia","District of Columbia"),
                   Region3 = c("Kentucky","Tennessee","North Carolina","South Carolina","Georgia","Florida","Alabama","Mississippi"),
                   Region4 = c("Minnesota","Wisconsin","Illinois","Indiana","Michigan","Ohio"),
                   Region5 = c("New Mexico","Texas","Oklahoma","Arkansas","Louisiana"),
                   Region6 = c("Nebraska","Kansas","Iowa","Missouri"),
                   Region7 = c("Utah","Colorado","Wyoming","Montana","South Dakota","North Dakota"),
                   Region8 = c("California","Nevada","Arizona","Hawaii"),
                   Region9 = c("Oregon","Washington","Idaho","Alaska"))
col = rainbow(length(region_list))
labels = c("Region0", "Region1","Region2","Region3","Region4",
           "Region5","Region6","Region7","Region8","Region9")
province.list = name
nprovince = length(province.list)
for(i in nprovince) {
  k = regexpr(":", province.list[i])
  if(k != -1)
    province.list[i] = substr(province.list[i], start = 1, stop = (k-1))
}
prov.col = NULL
prov.label = NULL
nregion = length(region_list)
for(j in 1:nregion) {
  myregion = region_list[[j]]
  index = which(as.character(province.list) %in% myregion)
  prov.col[index] = col[j]
  prov.label[index] = labels[j]
}


map1 = fortify(port1) # change to data.frame
map1$id = as.integer(map1$id)
dat = data.frame(id = 1:(length(name)), state = name)
map1.df = inner_join(map1, dat, by = "id")
center = data.frame(gCentroid(port1, byid = TRUE))
center$state = dat$state

labels = c("Region1","Region2","Region3","Region4","Region5",
           "Region6","Region7","Region8","Region9","Region10")
map1.df$col = prov.col[map1.df$id]
map1.df$col = factor(map1.df$col, 
                     levels = unique(prov.col)[order(unique(prov.label))],
                     labels = labels)

mydata = get.DICE.data()
longitude = mydata$fit$attr$lon
latitude = mydata$fit$attr$lat
output = runDICE(dataType = 'cdc', year = 2015, mod_level = 2, fit_level = 3, nMCMC = 1000)
onset = output$rtn
for(i in 1:ncol(output$rtn)) {
  onset[,i] = output$rtn[,i]/as.numeric(mydata$fit$factor[i])
}

nation_long = geocode("USA")$lon
nation_lat = geocode("USA")$lat
region_r = onset
# Method1: divided by minimum value
for(i in 1:ncol(onset)) {
  region_r[,i] = onset[,i] / min(onset[,i])
}
nation_r = apply(region_r,1,sum)


week = mydata$weeks
#### make a movie
mainDir = getwd()
subDir = "output"
#if (is.null(subDir))
err = makeDir(subDir = subDir)
setwd(paste(mainDir,subDir,sep = "/"))
#if(!file.exists(subDir)) {
# dir.create(file.path(subDir))
#}

#subDir = paste(mainDir,"/movie/",sep = "")
#if(!file.exists(subDir)) {
# dir.create(file.path(subDir))
#}

frame = nrow(onset)
map.animate = function() {
  for(i in 11:20) {
    if(i < 10) {
      name = paste('000',i,'plot.png',sep = '')
    }
    if(i >= 10) {
      name = paste('00',i,'plot.png',sep = '')
    }
    
    p = ggplot() +
      geom_map(data = map1.df, map = map1.df,
               aes(map_id = id, x = long, y = lat, group = group, fill = col),
               size = 0.25) +
      coord_map() +
      geom_point(aes(x = nation_long, y = nation_lat), size = 2 * nation_r[i], col = "grey", show.legend = FALSE, alpha = 0.5) +
      scale_x_continuous(name = "",limits = c(-130,-60)) +
      scale_y_continuous(name = "",limits = c(25,50)) +
      ggtitle(paste("USA map colored by CDC region - week ", week[i], sep = '')) +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 15, family = "serif", face = "bold"))
    for(j in 1:10) {
      p = p + geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 4 * region_r[i,j],  col = "#24576D", show.legend = FALSE,alpha = 0.8)
    }
    print(p)
  }
}
moviename = "map.gif"
saveGIF(map.animate(),interval = 0.2, movie.name = moviename)
setwd(mainDir)
cat("For movie see: ", subDir, "/", moviename, sep = "")
