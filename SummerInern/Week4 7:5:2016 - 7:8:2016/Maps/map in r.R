############## Maps plotting in R ################

############## sp package ###################
library(sp)
library(maptools)
require(RColorBrewer)
library(raster)
library(maps)
library(ggplot2)
port1 = getData('GADM', country = 'CHN', level = 1)
table(port1$NAME_1)
port1$NAME_1 = as.factor(as.character(port1$NAME_1))
## fills the map basing on variable NAME_1
region_list = list(Region1 = c("Heilongjiang","Jilin","Liaoning"),
                   Region2 = c("Beijing","Tianjin","Henan","Hebei","Shandong","Shanxi"),
                   Region3 = c("Jiangsu","Zhejiang","Shanghai","Anhui"),
                   Region4 = c("Fujian","Guangdong","Guangxi","Jiangxi","Hainan"),
                   Region5 = c("Shaanxi","Gansu","Ningxia Hui","Nei Mongol"),
                   Region6 = c("Sichuan","Chongqing","Qinghai"),
                   Region7 = c("Hunan","Hubei","Guizhou","Yunnan"),
                   Region8 = "Xinjiang Uygur",
                   Region9 = "Xizang")
col = rainbow(9)
labels = c("Region1","Region2","Region3","Region4","Region5",
           "Region6","Region7","Region8","Region9","Region10")
province.list = port1$NAME_1
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


spplot(port1,"NAME_1",
       col.regions = prov.col,
       col = "white",
       asp = 0.8,colorkey = FALSE,
       #key.space = list(x = 0.2,y = 0.2, corner = c(0,0)),
       scales = list(draw = T))

########## using ggplot to plot the map ########
library(dplyr)
library(rgeos)
port1 = getData('GADM', country = 'CHN', level = 1)
map1 = fortify(port1) # change to data.frame
map1$id = as.integer(map1$id)
dat = data.frame(id = 1:(length(port1$NAME_1)), state = port1$NAME_1)
map1.df = inner_join(map1, dat, by = "id")
center = data.frame(gCentroid(port1, byid = TRUE))
center$state = dat$state

map1.df$col = prov.col[map1.df$id]
map1.df$col = factor(map1.df$col, 
                     levels = unique(prov.col)[order(unique(prov.label))],
                     labels = sort(unique(prov.label)))

ggplot() +
  geom_map(data = map1.df, map = map1.df,
           aes(map_id = id, x = long, y = lat, group = group, fill = col),
           size = 0.25) +
  geom_text(data = center, aes(label = state, x=x, y=y), size = 2) +
  coord_map() +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  theme(legend.title = element_blank())

##################


