makeMovieFrames.ggplot2 <- function(rtn = NULL, profile = NULL, mydata = NULL, ireal = 1, run.list=NULL, convert =TRUE ){
  
  #' For a CDC or GFT run of USA create frames of weekly incidence 
  #'
  #' For a fit_level=3 and mod_level =3 run of the US create weekly maps of ILI incidence using ggplot2.  The function creates a sub-directory where all the frames are written. Default name is frames-ireal where ireal is the MCMC chain number.  A frame is written for each week.  The user can then string these frmaes to create a movie.  Each frames shows a map of CONUS colord by the ten HHS regions and overlayed on it are circles whos radius is proportional to the \%ILI.  At each HHS region at a that week.  The circles are drawn at the (population density weighted) centroid of each HHS region.  We also show a circle for the national results - drawn at the (population density weighted) centroid of the continental US (CONUS).  Each circle show the best result along with the 75\% CI. The thin black circle denotes the data.
  #' 
  #' @param rtn A 1D numeric array with the best in-direct prediction to the model region
  #' @param mydata A list with the entire data set of this \code{DICE} run
  #' @param ireal Integer - the MCMC chain number
  #' @param run.list  A list with various run parameters
  #' @param convert Logical - tells the code if we need to convert the profiles to \%ILI (TRUE)  or not (FALSE)
  #' 
  #' @return  Returns \eqn{err = 0} if successful
  #' 
  #' @examples
  #' makeMovieFrames.ggplot2{rtn = rtn, profile = profile, mydata = mydata, ireal = ireal}
  
  
  FY = mydata$FY
  model = mydata$imodel
  weeks = mydata$weeks
  nweeks = mydata$nweeks
  nweeksFit = mydata$nweeksFit
  nweeksData = mydata$nweeksData
  reg.fit.name = mydata$fit$name
  reg.model.name = mydata$model$name
  
  nRnd = dim(profile)[1]
  
  n.model = 1
  n.fit = mydata$fit$nregions
  nRnd = dim(profile)[1]
  
  fit_coef = mydata$fit$coef
  fit_onset = mydata$fit$onset
  model_coef = mydata$model$coef
  model_onset = mydata$model$onset
  weeks = mydata$weeks
  
  ## check to see if output directory exists and if not create it
  subDir = run.list$subDir
  if (is.null(subDir))
    subDir = 'output'
  err = makeDir(subDir = subDir)
  myName = mydata$dataName
  
  
  # Make a movie
  
  ## Get map-data from GADM and simplify it
  ##port1 = suppressMessages(getData('GADM', country = 'USA', level = 1))
  port1$NAME_1 = as.factor(as.character(port1$NAME_1))
  name = port1$NAME_1
  port1 = gSimplify(port1, tol = 0.01, topologyPreserve = TRUE)
  
  ## fills the map based on different regions
  region_list = list(Region1 = c("Maine","New Hampshire","Massachusetts","Rhode Island","Connecticut","Vermont"),
                     Region2 = c("New York","New Jersey","Puerto Rico"),
                     Region3 = c("Pennsylvania","Delaware","Maryland","West Virginia","Virginia","District of Columbia"),
                     Region4 = c("Kentucky","Tennessee","North Carolina","South Carolina","Georgia","Florida","Alabama","Mississippi"),
                     Region5 = c("Minnesota","Wisconsin","Illinois","Indiana","Michigan","Ohio"),
                     Region6 = c("New Mexico","Texas","Oklahoma","Arkansas","Louisiana"),
                     Region7 = c("Nebraska","Kansas","Iowa","Missouri"),
                     Region8 = c("Utah","Colorado","Wyoming","Montana","South Dakota","North Dakota"),
                     Region9 = c("California","Nevada","Arizona","Hawaii"),
                     Region10 = c("Oregon","Washington","Idaho","Alaska"))
  col = rainbow(n.fit)
  
  #labels = mydata$fit$attr$NAME_3
  labels = mydata$fit$name
  
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
  
  
  labels = mydata$fit$attr$NAME_3
  
  map1.df$col = state.col[map1.df$id]
  map1.df$col = factor(map1.df$col, 
                       levels = unique(state.col)[order(unique(state.label))],
                       labels = labels)
  
  longitude = mydata$fit$attr$lon
  latitude = mydata$fit$attr$lat
  
  factor = as.numeric(mydata$fit$factor)
  model_factor = mydata$model$factor
  
  ## This is the fit and model %ILI data
  fit_ili = mydata$fit$raw
  model_ili = mydata$model$raw
  
  ## If needed Convert the fit results to % ILI
  ## When this routine is running after a run is complete the loaded profiles are already %ILI and there is no need to convert
  
  rtn_ili = rtn
  profile_ili = profile
  
  if (convert == TRUE) {
    for (i in 1:n.fit) {
      rtn_ili[, i] = rtn[, i]/factor[i]
      profile_ili[, , i] = profile[, , i]/factor[i]
    }
    
  }
  
  
  ## Build the indirect model results 
  fit_model = rep(NA, nweeks)
  fit_model_profile = array(0, dim = c(nRnd, nweeks))
  for (i in 1:nweeks) {
    fit_model[i] = sum(rtn_ili[i, 1:n.fit] * fit_coef[1:n.fit])
    for (irnd in 1:nRnd) {
      for (k in 1:n.fit) fit_model_profile[irnd,i] = fit_model_profile[irnd,i] + profile_ili[irnd,i,k]* fit_coef[k]	
    } 
    
  }
  
  region_r = rtn_ili
  
  ## Quantiles for regions and nation
  
  quan.out = apply(profile_ili, c(2, 3), quanci)
  region.lb = quan.out[1, , ]
  region.ub = quan.out[2, , ]
  
  
  nation_long = mydata$model$attr$lon 
  nation_lat =  mydata$model$attr$lat 
  
  nation_r = fit_model
  
  nation.out = apply(fit_model_profile,2,quanci)
  nation.lb = nation.out[1,]
  nation.ub = nation.out[2,]
  
  maxrtn = max(region_r,nation_r,na.rm=TRUE)
  
  
  ## create a directory for the frames
  tmp <- getwd()  # get the current working directory
  setwd(subDir)  # set the working directory to the output dir
  fname <- paste0("frames-", ireal)
  # create the directory name
  err <- makeDir(subDir = fname)                # create the new directory
  setwd(tmp)  # return to the application working directory
  subDir2 <- file.path(subDir, fname) 
  
  for(i in 1:nweeks) {
    cat("making frame number: ",i,'\n')
    
    if(i < 10) {
      name = paste(subDir2,'/000',i,'plot.png',sep = '')
    }
    if(i >= 10) {
      name = paste(subDir2,'/00',i,'plot.png',sep = '')
    }
    p = ggplot() +
      geom_map(data = map1.df, map = map1.df,
               aes(map_id = id, x = long, y = lat, group = group, fill = col),
               size = 0.25) +
      coord_map() +        
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation.ub[i]/maxrtn), col = "mediumpurple", show.legend = FALSE, alpha = 0.5) +
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation.lb[i]/maxrtn), col = "khaki", show.legend = FALSE, alpha = 0.5) +           
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (nation_r[i] /maxrtn), col = "grey", show.legend = FALSE, alpha = 0.5) +
      geom_point(aes_string(x = nation_long, y = nation_lat), size = 8 * (model_ili[i] /maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE) +        
      scale_x_continuous(name = "",limits = c(-130,-60)) +
      scale_y_continuous(name = "",limits = c(25,50)) +
      ggtitle(paste(FY," Seaon - Epidemic Week ", weeks[i], sep = '')) +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 15, family = "serif", face = "bold"))
    for(j in 1:n.fit) {
      p = p +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 8 * (region.ub[i,j]/maxrtn),  col = "forestgreen", show.legend = FALSE,alpha = 0.5) +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 8 * (region.lb[i,j]/maxrtn),  col = "deeppink", show.legend = FALSE,alpha = 0.5) +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 8 * (region_r[i,j] /maxrtn),  col = "#24576D", show.legend = FALSE,alpha = 0.5) +
        geom_point(aes_string(x = longitude[j], y = latitude[j]), size = 8 * (fit_ili[i,j]  /maxrtn), shape=1, col = "black", show.legend = FALSE, alpha =0.6,na.rm=TRUE)
    }
    suppressMessages(ggsave(filename = name))
  }
  
  cat("  png frames were saved at: ",subDir2,'\n')
  cat(" They can be used to make a movie using for example: convert -delay 20 *png my_movie.mov",'\n')
  
  
  err = 0
  return(err)	
  
  
}