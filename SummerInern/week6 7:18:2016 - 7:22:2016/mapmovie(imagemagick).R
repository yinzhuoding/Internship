#library(sp)
library(raster)
library(rgeos)
library(dplyr)
library(ggmap)
library(animation)

mapMovie = function(dataType = "cdc", year = 2015, nweeksFit = 52, model = 5, isingle = 0, nMCMC = 1000, nreal = 1) {

#' Make a map movie for HHS regions indicating the trend of %ILI in a certain flu year for each region and nation.
#' 
#' @param dataType either 'cdc' or 'gft'.
#' @param year Integer - start year of the flu season
#' @param nweeksFit Integer - Number of weeks that will be fitted.  Default is to fit all the data.
#' @param model Integer - The model number, see manual for more details (1-5 are supported)
#' @param isingle Integer - 0: couple the fit spatial regions; 1: no coupling
#' @param nMCMC Integer - number of steps/trials in the MCMC procedure
#' @param nreal Integer - number of chains 
#' 
#' @return a map movie with format .gif
  
  ## Data Type: cdc or gft
  if (is.null(dataType)) 
    dataType = "cdc"
  
  ## Start year of the flu season
  if (is.null(year)) 
    year = 2015
  
  ## Number of weeks of data that are fitted 
  if (is.null(nweeksFit)) 
    nweeksFit = 52
  
  ## Model Number
  if (is.null(model)) 
    model = 5
  
  ## Number of steps/trials in each MCMC chain
  if (is.null(nMCMC)) 
    nMCMC = 1000
  
  ## Number of MCMC chains
  if (is.null(nreal)) 
    nreal = 1
  
  ## Coupled (isingle = 0) or Uncoupled (isingle = 1) fit for the spatial regions 
  if (is.null(isingle)) 
    isingle = 0
  
  ## Get map-data from GADM and simplify it
  port1 = suppressMessages(getData('GADM', country = 'USA', level = 1))
  port1$NAME_1 = as.factor(as.character(port1$NAME_1))
  name = port1$NAME_1
  port1 = gSimplify(port1, tol = 0.01, topologyPreserve = TRUE)
  
  ## fills the map basing on different region
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
  state.list = name
  nstate = length(state.list)
  for(i in nstate) {
    k = regexpr(":", state.list[i])
    if(k != -1)
      state.list[i] = substr(state.list[i], start = 1, stop = (k-1))
  }
  state.col = NULL
  state.label = NULL
  nregion = length(region_list)
  for(j in 1:nregion) {
    myregion = region_list[[j]]
    index = which(as.character(state.list) %in% myregion)
    state.col[index] = col[j]
    state.label[index] = labels[j]
  }
  
  ## change to data.frame
  map1 = fortify(port1) 
  map1$id = as.integer(map1$id)
  dat = data.frame(id = 1:(length(name)), state = name)
  map1.df = inner_join(map1, dat, by = "id")
  
  labels = c("Region1","Region2","Region3","Region4","Region5",
             "Region6","Region7","Region8","Region9","Region10")
  map1.df$col = state.col[map1.df$id]
  map1.df$col = factor(map1.df$col, 
                       levels = unique(state.col)[order(unique(state.label))],
                       labels = labels)
  
  mydata = get.DICE.data(dataType = dataType, year = year, mod_level = 2, fit_level = 3, model = model, nweeksFit = nweeksFit, isingle=isingle)
  longitude = mydata$fit$attr$lon
  latitude = mydata$fit$attr$lat
  output = runDICE(dataType = dataType, year = year, mod_level = 2, fit_level = 3, model = model, isingle = isingle, nMCMC = nMCMC, nreal = nreal)
  onset = output$rtn
  for(i in 1:ncol(output$rtn)) {
    onset[,i] = output$rtn[,i]/as.numeric(mydata$fit$factor[i])
  }
  
  nation_long = suppressMessages(geocode("USA")$lon)
  nation_lat = suppressMessages(geocode("USA")$lat)
  region_r = onset
  
  ## Normailization: divided by minimum value
  for(i in 1:ncol(onset)) {
    region_r[,i] = onset[,i] / min(onset[,i])
  }
  nation_r = apply(region_r,1,sum)

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
    suppressMessages(ggsave(filename = name))
  }
  
  moviename = "map.gif"
  system("convert -delay 20 *.png map.gif")
  file.remove(list.files(pattern = ".png"))
  setwd(mainDir)
  cat("For movie see: ", subDir, "/", moviename, sep = "")
}

