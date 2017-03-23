require(DICE)
library(sp)
library(raster)
library(rgeos)
library(dplyr)
library(ggmap)
library(animation)
library(simpleboot)

## Get map from GADM and simplify it
port1 = getData('GADM', country = 'USA', level = 1)
port1$NAME_1 = as.factor(as.character(port1$NAME_1))
name = port1$NAME_1
port1 = gSimplify(port1, tol=0.01, topologyPreserve=TRUE)
## Fill the map basing on variable NAME_1
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

mydata = get.DICE.data(dataType = 'cdc', year = 2015, mod_level = 2, fit_level = 3)

longitude = mydata$fit$attr$lon
latitude = mydata$fit$attr$lat
output = runDICE(dataType = 'cdc', year = 2015, mod_level = 2, fit_level = 3, nMCMC = 10000)

nation_long = geocode("USA")$lon
nation_lat = geocode("USA")$lat

## Use quantile to calculate the 0.75 confidence interval
quanci = function(x) {
  ub = quantile(x, 0.875)
  lb = quantile(x, 0.125)
  return(c(lb,ub))
}
## Data for region
profile = output$profile
region.r = output$rtn
quan.out = apply(profile,c(2,3),quanci)
region.lb = quan.out[1, , ]
region.ub  =quan.out[2, , ]

for(i in 1:ncol(output$rtn)) {
  fac = as.numeric(mydata$fit$factor[i])
  region.r[,i] = region.r[,i]/fac
  region.lb[,i] = region.lb[,i]/fac
  region.ub[,i] = region.ub[,i]/fac
}
region_r = region.r
region_lb = region.lb
region_ub = region.ub
## Standardization by divided by minimum
for(i in 1:ncol(region.r)) {
  dnt = min(region.r[,i])
  region_r[,i] = region.r[,i] / dnt
  region_lb[,i] = region.lb[,i] / dnt
  region_ub[,i] = region.ub[,i] / dnt
}
## Data for nation
nation.r = output$model_rtn
model_profile = output$model_profile
nation.out = apply(model_profile,2,quanci)
nation.lb = nation.out[1,]
nation.ub = nation.out[2,]
nation_r = nation.r/min(nation.r)
nation_lb = nation.lb/min(nation.r)
nation_ub = nation.ub/min(nation.r)
## Plot the radius of different circle
par(mfrow = c(3,4))
for(i in 1:10) {
  plot(region_ub[,i], type = "n", xlab = "week", ylab = "radius", main = paste("region",i, sep  =""))
  lines(region_ub[,i], col = "dodgerblue")
  lines(region_r[,i], col = "black")
  lines(region_lb[,i], col = "deeppink")
  legend("topleft", c(" - basic circle", " - upper bound", " - lower bound"), text.col = c("black", "dodgerblue","deeppink"), bty = "n",cex = 0.5)
}
plot(nation_ub, type = "n", xlab = "week", ylab = "radius", main = "nation")
lines(nation_ub, col = "dodgerblue")
lines(nation_r, col = "black")
lines(nation_lb, col = "deeppink")
legend("topleft", c(" - basic circle", " - upper bound", " - lower bound"), text.col = c("black", "dodgerblue","deeppink"), bty = "n",cex = 0.5)


#### make a movie
week = mydata$weeks
mainDir = getwd()
subDir = "output"
err = makeDir(subDir = subDir)
setwd(paste(mainDir,subDir,sep = "/"))

frame = nrow(region.r)
for(i in 1:frame) {
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
    geom_point(aes_string(x = nation_long, y = nation_lat), size = 15 * nation_ub[i], col = "mediumpurple", show.legend = FALSE, alpha = 0.5) +
    geom_point(aes_string(x = nation_long, y = nation_lat), size = 15 * nation_r[i], col = "grey", show.legend = FALSE, alpha = 0.5) +
    geom_point(aes_string(x = nation_long, y = nation_lat), size = 15 * nation_lb[i], col = "khaki", show.legend = FALSE, alpha = 0.5) +
    scale_x_continuous(name = "",limits = c(-130,-60)) +
    scale_y_continuous(name = "",limits = c(25,50)) +
    ggtitle(paste("USA map colored by CDC region - week ", week[i], sep = '')) +
    theme(legend.title = element_blank(),
          plot.title = element_text(size = 15, family = "serif", face = "bold"))
  for(j in 1:10) {
    p = p +
      geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 2.5 * region_ub[i,j],  col = "forestgreen", show.legend = FALSE,alpha = 0.5) +
      geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 2.5 * region_lb[i,j],  col = "deeppink", show.legend = FALSE,alpha = 0.5) +
      geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 2.5 * region_r[i,j],  col = "#24576D", show.legend = FALSE,alpha = 0.5)
  }
  suppressMessages(ggsave(filename = name))
}
moviename = "map.gif"
system("convert -delay 20 *.png map.gif")
file.remove(list.files(pattern = ".png"))
setwd(mainDir)
cat("For movie see: ", subDir, "/", moviename, sep = "")



  



