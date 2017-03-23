require(DICE)
load("input-gft-United.States-uncpl-2013-2014-3-52-1.RData")
load("profiles-gft-United.States-uncpl-2013-2014-3-52-1.RData")
mydata = mydata
rtn = dump$rtn_ili
profile = dump$profile_ili
run.list = input$run.list

# call:
MakeStateMovieFrames(rtn, profile, mydata, convert = FALSE)

MakeStateMovieFrames = function(rtn = NULL, profile = NULL, mydata = NULL, ireal = 1, run.list=NULL, convert =TRUE ) {
  # Preparation
  FY = mydata$FY
  weeks = mydata$weeks
  nweeks = mydata$nweeks
  nRnd = dim(profile)[1]
  n.fit = mydata$fit$nregions
  fit_coef = mydata$fit$coef
  
  ## check to see if output directory exists and if not create it
  subDir = run.list$subDir
  if (is.null(subDir))
    subDir = 'output'
  err = makeDir(subDir = subDir)
  myName = mydata$dataName
  
  # Extract the country name from data
  iso = unique(mydata$fit$attr$ABBV_2)
  # get data from GADM
  port1 = suppressMessages(getData('GADM', country = iso, level = 1))
  port1$NAME_1 = as.factor(as.character(port1$NAME_1))
  state.name = as.character(port1$NAME_1)
  port1 = gSimplify(port1, tol=0.01, topologyPreserve=TRUE)
  col = rainbow(length(state.name))
  
  map1 = fortify(port1) # change to data.frame
  map1$id = as.integer(map1$id)
  dat = data.frame(id = 1:(length(state.name)), state = state.name)
  map1.df = inner_join(map1, dat, by = "id")
  center = data.frame(gCentroid(port1, byid = TRUE))
  center$state = dat$state
  map1.df$col = col[map1.df$id]
  map1.df$col = factor(map1.df$col)
  
  longitude = mydata$fit$attr$lon
  latitude = mydata$fit$attr$lat
  factor = as.numeric(mydata$fit$factor)
  model_factor = mydata$model$factor
  ## This is the fit and model %ILI data
  fit_ili = mydata$fit$raw
  model_ili = mydata$model$raw
  ## If needed, convert the fit results to %ILI
  ## When this routine is running after a run is complete the loaded profiles are alreadty %ILI then there is no need to convert.
  rtn_ili = rtn
  profile_ili = profile
  if (convert == TRUE) {
    for(i in 1:n.fit) {
      rtn_ili[,i] = rtn_ili[,i]/factor[i]
      profile_ili[, , i] = profile_ili[, , i]/factor[i]
    }
  }
  
  ## Build the indirect model results
  fit_model = rep(NA,nweeks)
  fit_model_profile = array(0, dim = c(nRnd, nweeks))
  for(i in 1:nweeks) {
    fit_model[i] = sum(rtn_ili[i, 1:n.fit] * fit_coef[1:n.fit])
    for(irnd in 1:nRnd) {
      for(k in 1:n.fit) {
        fit_model_profile[irnd,i] = fit_model_profile[irnd,i] + profile_ili[irnd,i,k] * fit_coef[k]
      }
    }
  }
  
  region_r = rtn_ili
  nation_long = mydata$model$attr$lon
  nation_lat = mydata$model$attr$lat
  nation_r = fit_model
  
  ## Quantiles for regions and nation
  quan.out = apply(profile_ili, c(2,3), quanci)
  region.lb = quan.out[1, , ]
  region.ub = quan.out[2, , ]
  nation.out = apply(fit_model_profile, 2, quanci)
  nation.lb = nation.out[1,]
  nation.ub = nation.out[2,]
  
  maxrtn = max(region_r, nation_r, na.rm = TRUE)
  
  ## create a directory for the frames
  tmp <- getwd()  # get the current working directory
  setwd(subDir)  # set the working directory to the output dir
  fname <- paste0("frames-", ireal)
  # create the directory name
  err <- makeDir(subDir = fname)   # create the new directory
  setwd(tmp)  # return to the application working directory
  subDir2 <- file.path(subDir, fname) 
  
  for(i in 1:nweeks) {
    cat("making frame number: ",i,'\n')
    
    if(i < 10) {
      p.name = paste(subDir2,'/000',i,'plot.png',sep = '')
    }
    if(i >= 10) {
      p.name = paste(subDir2,'/00',i,'plot.png',sep = '')
    }
    p = ggplot() +
      geom_map(data = map1.df, map = map1.df, aes(map_id = id, x = long, y = lat, group = group, fill = col), size = 0.25) +
      coord_map() +
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation.ub[i]/maxrtn), col = "mediumpurple", show.legend = FALSE, alpha = 0.5) +
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation_r[i]/maxrtn), col = "grey", show.legend = FALSE, alpha = 0.5) +
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation.lb[i]/maxrtn), col = "khaki", show.legend = FALSE, alpha = 0.5) +           
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (model_ili[i] /maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE) + 
      ggtitle(paste(FY, " Season - Epidemic Week ", weeks[i], sep = "")) +
      theme(legend.title = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 15, family = "serif", face = "bold"))
    if(iso == "USA") {
      p = p + scale_x_continuous(name = "",limits = c(-140,-65)) +
        scale_y_continuous(name = "",limits = c(25,50))
    }
    for(j in 1:ncol(region_r)) {
      p = p + 
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 10 * (region.ub[i,j]/maxrtn),  col = "forestgreen", show.legend = FALSE,alpha = 0.5, na.rm = TRUE) +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 10 * (region_r[i,j]/maxrtn), col = "#24576D", show.legend = FALSE, alpha = 0.5, na.rm = TRUE) +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 10 * (region.lb[i,j] /maxrtn),  col = "deeppink", show.legend = FALSE,alpha = 0.5, na.rm = TRUE) 
      geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 10 * (fit_ili[i,j]  /maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE)
    }
    # If USA map, plot "Alaska" and "Hawaii" alone
    if(iso == "USA") {
      name = mydata$fit$name
      s1 = which(name == "Alaska")
      s2 = which(name == "Hawaii")
      p = p + geom_point(aes_string(x = -138, y = 47), size = 10 * (region.ub[i, s1]/maxrtn), col = "forestgreen", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 47), size = 10 * (region_r[i, s1]/maxrtn), col = "#24576D", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 47), size = 10 * (region.lb[i, s1]/maxrtn), col = "deeppink", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 47), size = 10 * (fit_ili[i,s1]/maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE) +
        geom_point(aes_string(x = -138, y = 30), size = 10 * (region.ub[i, s2]/maxrtn), col = "forestgreen", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 30), size = 10 * (region_r[i, s2]/maxrtn), col = "#24576D", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 30), size = 10 * (region.lb[i, s2]/maxrtn), col = "deeppink", show.legend = FALSE, alpha = 0.5) +
        geom_point(aes_string(x = -138, y = 30), size = 10 * (fit_ili[i,s2]/maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE) +
        annotate("text", x = -140, y = 50, label = "Alaska", hjust = 0, vjust = 2.5, col = "black", family = "serif", size = 3) +
        annotate("text", x = -140, y = 30, label = "Hawaii", hjust = 0, vjust = 2.5, col = "black", family = "serif", size = 3)
    }
    suppressMessages(ggsave(filename = p.name,device = "png"))
  }
  
  cat("  png frames were saved at: ",subDir2,'\n')
  cat(" They can be used to make a movie using for example: convert -delay 20 *png my_movie.mov",'\n')
  
  err = 0
  return(err)	
}











