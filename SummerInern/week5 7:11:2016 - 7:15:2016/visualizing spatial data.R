## chapter 3 -  visualizing spatial data
library(sp)
data(meuse)
par(mfrow = c(1,2))
# plot - points
coordinates(meuse) = c("x", "y")
plot(meuse)
title("points")
# plot - grids
data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
meuse.grid = as(meuse.grid, "SpatialPixels")
image(meuse.grid, col = "grey")
title("grid")

library(maptools)
library(maps)
wrld = map("world", interior = FALSE, 
           xlim = c(-179,179), ylim = c(-89,89), plot = FALSE)
wrld_p = pruneMap(wrld, xlim = c(-179,179))
llCRS = CRS("+proj=longlat +ellps=WGS84")
wrld_sp = map2SpatialLines(wrld_p, proj4string = llCRS)
prj_new = CRS("+proj=moll")
library(rgdal)
wrld_proj = spTransform(wrld_sp, prj_new)
wrld_grd = gridlines(wrld_sp, 
                     easts = c(-179, seq(-150,150,50)),
                     norths = seq(-75, 75, 15), ndiscr = 100)
wrld_grd_proj = spTransform(wrld_grd, prj_new)
at_sp = gridat(wrld_sp, easts = 0, norths = seq(-75, 75, 15), offset = 0.3)
at_proj = spTransform(at_sp, prj_new)
par(mfrow = c(1,1))
plot(wrld_proj, col = "grey60")
plot(wrld_grd_proj, add = TRUE, lty = 3, col = "grey70")

library(maptools)
data(meuse.grid)
coordinates(meuse.grid) = c("x","y")
meuse.grid = as(meuse.grid, "SpatialPixelsDataFrame")
im = as.image.SpatialGridDataFrame(meuse.grid["dist"])
cl = ContourLines2SLDF(contourLines(im))
spplot(cl)


##############
library(dplyr)
library(rgeos)
port1 = getData('GADM', country = 'CHN', level = 1)
port1 = gSimplify(port1, tol=0.01, topologyPreserve=TRUE)
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


