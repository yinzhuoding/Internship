plotFitCDCPercentILI.ggplot2 <- function(rtn = NULL, profile = NULL, model_rtn = NULL, model_profile = NULL, mydata = NULL, ireal = 1, run.list=NULL, idevice = 1) {
  
  #' Plot the results of a \code{DICE} Run
  #'
  #' Plot the results of an a coupled or ucoupled  \code{DICE} run. For each of the fit regions we plot  the  \% ILI of the region along with
  #' our predictions for it based on randomly selected results from the  history of the MCMC chain of each region.  Using the predictions
  #' for the fit regions we then show the results for the model  region as a weighted sum of the fit regions.  The last panel
  #' shows our prediction for the model region using a direct fit to the model data.  The function also writes a binary RData file with 
  #' all the profile predictions for the model and fit regions. Note that in the case of a coupled run the fit regions are never individually
  #' optimized. It is their weighted sum that is optimized, with the weights given by the relative population of each fit region.
  #' @param rtn A 1D numeric array with the best in-direct prediction to the model region
  #' @param profile A 3D numeric array holding random predictions for each of the fit regions based on the history of their MCMC chains.
  #' @param model_rtn A 1D numeric array with the best direct prediction to the model region
  #' @param model_profile A 2D numeric array with randomly chosen predicted profiles obtained by fitting the model region directly.
  #' @param mydata A list with the entire data set of this \code{DICE} run
  #' @param ireal Integer - the MCMC chain number
  #' @param run.list  A list with various run parameters
  #' @param idevice Integer - the index of the device in the device array. Default is 1 - make only one format of plot results
  #' @return  Returns \eqn{err = 0} if successful
  #' @examples
  #' plotFitCDCPercentILI{ rtn = rtn, profile = profile, model_rtn = model_rtn, model_profile = model_profile, 
  #' mydata = mydata, ireal = ireal, device = device, idevice = 1}
  
  
  device = run.list$device[idevice]
  if (is.null(device)) 
    device = "pdf"
  if (is.null(profile)) 
    return
  if (is.null(rtn)) 
    return
  
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
  tps = mydata$weeks
  
  ## check to see if output directory exists and if not create it
  subDir = run.list$subDir
  if (is.null(subDir))
    subDir = 'output'
  err = makeDir(subDir = subDir)
  myName = mydata$dataName
  
  if (tolower(device) == "pdf") {
    pdfName = paste(subDir, "/results-", myName,"-", nweeksFit, "-", ireal, ".pdf", sep = "")
    cat("\n\n For a plot of %ILI See: ", pdfName, "\n\n")
    pdf(file = pdfName, onefile = TRUE, width = 15, height = 9)
  } else if (tolower(device) == 'png') {
    pngName  = paste(subDir, "/results-", myName,"-", nweeksFit, "-", ireal, ".png", sep = "")
    cat("\n\n For a plot of  %ILI  See: ", pngName, "\n\n")
    png(file=pngName,width=1200,height=900)
  } else {
    dev.next()
    dev.new()
  }
  
  # convert the fit and model results to  %ILI
  # calculate the national
  
  # convert the model to  %ILI
  # calculate the national
  factor = as.numeric(mydata$fit$factor)
  model_factor = mydata$model$factor
  
  ## This is the fit and model %ILI data
  fit_ili = mydata$fit$raw
  model_ili = mydata$model$raw
  
  rtn_ili = rtn
  profile_ili = profile
  for (i in 1:n.fit) {
    rtn_ili[, i] = rtn[, i]/factor[i]
    profile_ili[, , i] = profile[, , i]/factor[i]
  }
  
  model_rtn_ili = NULL
  model_profile_ili = NULL
  
  if (!is.null(model_rtn)) 
    model_rtn_ili = model_rtn/model_factor
  if (!is.null(model_profile)) {
    model_profile_ili = model_profile
    model_profile_ili = model_profile_ili/model_factor
  }
  
  ## for plotting - replace zero's with NA
  if (nweeksFit < nweeks) {
    index <- which(fit_ili[, 1] == 0)
    
    if (length(index) >= 1) {
      fit_ili[index, 1:n.fit] = NA
      model_ili[index] = NA
    }
  }
  
  colnames(fit_ili) = mydata$fit$attr$NAME_3
  colnames(rtn_ili) = mydata$fit$attr$NAME_3
  
  colvec = rainbow(n.fit)
  lwd = rep(2, n.fit)
  
  # Plot one region at a time
  plot_region = list()
  for (i in 1:n.fit) {
    ymax = max(fit_ili[1:nweeksData, i], rtn_ili[1:nweeksData, i], profile_ili[,1:nweeksData , i], unlist(fit_onset[i]), na.rm = TRUE)
    breaks = seq(from = 1, to = nweeks,by = 4)
    labels = weeks[breaks]
    plot_region[[i]] = ggplot(data = NULL) +
      scale_x_continuous(name = "EW #", limit = c(1,nweeks),
                         breaks = breaks, labels = labels) +
      scale_y_continuous(name = "% ILI") +
      coord_cartesian(ylim=c(0, ymax)) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
    
    step = max(1,nRnd/100)
    irnd.set = seq(from = 1, to = nRnd, by = step)
    dat.rnd = t(profile_ili[irnd.set, 1:nweeksFit, i])
    dat.rnd.pred = t(profile_ili[irnd.set, nweeksFit:nweeks, i])
    data.rnd = melt(dat.rnd)
    data.rnd.pred = melt(dat.rnd.pred)
    plot_region[[i]] = plot_region[[i]] + 
      geom_line(aes_string(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = colvec[i], size = 1.5, alpha = 0.4) +
      geom_line(aes_string(x = 1:nweeksFit, y = rtn_ili[1:nweeksFit, i]), col = colvec[i]) +
      geom_line(aes_string(x = 1:nweeksFit, y = fit_ili[1:nweeksFit, i]), col = "black", linetype = 2,na.rm = TRUE) +
      geom_point(aes_string(x = 1:nweeksFit, y = fit_ili[1:nweeksFit, i]), col = "black", pch = 20)
    
    if (nweeksFit < nweeks) {
      plot_region[[i]] = plot_region[[i]] +
        geom_line(aes_string(x = data.rnd.pred[,1] + nweeksFit - 1, y = data.rnd.pred[,3],group = data.rnd.pred[,2]), col = colvec[i], size = 1.5, linetype = 2,alpha = 0.4) +
        geom_line(aes_string(x = nweeksFit:nweeks, y =rtn_ili[nweeksFit:nweeks, i]), col = colvec[i], linetype = 2) +
        geom_rect(aes_string(xmin = nweeksFit, xmax = min(nweeksFit + 4,nweeks), ymin = 0, ymax = Inf), fill = "#D497D3",alpha = 0.7)
    }
    if (length(fit_onset[i]) > 0) {
      y = rep(as.numeric(fit_onset[i]),nweeks)
      plot_region[[i]] = plot_region[[i]] + 
        geom_line(aes_string(x = 1:nweeks, y = y), col = "#D497D3", size = 1, linetype = 2)
    }
    reg.name = reg.fit.name[i]
    rel.pop = fit_coef[i]
    rel.pop = round(rel.pop, digits = 3)
    plot_region[[i]] = plot_region[[i]] +
      annotate("text", x = rep(-Inf,3), y = rep(Inf,3), 
               label = c(paste("   ", mydata$FY, sep = ""), paste("   ", reg.name, sep = ""), paste("   ", rel.pop, sep = "")), 
               hjust = rep(0,3), vjust = c(2.5,4,5.5),
               col = c("black",colvec[i],colvec[i]),family = "serif", size = 3.5)
    if (model == 2 | model == 3) {
      school = mydata$fit$school[, i]
      school[school == 0] = NA
      plot_region[[i]] = plot_region[[i]] +
        geom_point(aes(x = 1:nweeks, y = school*(ymax/5)), fill = "grey50", col = "grey", size = 3, pch = 22, na.rm = TRUE)
    }
    if (model == 1 || model == 3) {
      sh = mydata$fit$sh[, i]
      plot_region[[i]] = plot_region[[i]] +
        geom_line(aes(x = 1:nweeks, y = sh * (ymax/max(sh))), col = "black")
    }
  }
  
  ## now do the national
  fit_model = rep(NA, nweeks)
  fit_model_mean = rep(NA, nweeks)
  fit_model_profile = array(0, dim = c(nRnd, nweeks))
  for (i in 1:nweeks) {
    fit_model[i] = sum(rtn_ili[i, 1:n.fit] * fit_coef[1:n.fit])
    tmp = rep(0, n.fit)
    for (k in 1:n.fit) tmp[k] = mean(profile_ili[, i, k])
    fit_model_mean[i] = sum(tmp[1:n.fit] * fit_coef[1:n.fit])
    for (irnd in 1:nRnd) {
      for (k in 1:n.fit) fit_model_profile[irnd,i] = fit_model_profile[irnd,i] + profile_ili[irnd,i,k]* fit_coef[k]	
    } 
  }
  
  ymax = max(fit_model[1:nweeksData],fit_model_mean[1:nweeksData], fit_model_profile[1:nweeksData], model_ili[1:nweeksData], na.rm = TRUE)
  breaks = seq(from = 1, to = nweeks,by = 4)
  labels = weeks[breaks]
  plot_national = ggplot(data = NULL) +
    scale_x_continuous(name = "EW #", limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "% ILI") +
    coord_cartesian(ylim=c(0, ymax)) +
    theme(text = element_text(size = 10, color = "gray20", face = "italic"),
          axis.text.x = element_text(face = "plain"),
          axis.text.y = element_text(face = "plain"))
  step = max(1,nRnd/100)
  irnd.set = seq(from = 1, to = nRnd, by = step)
  dat.rnd = t(fit_model_profile[irnd.set, 1:nweeksFit])
  dat.rnd.pred = t(fit_model_profile[irnd.set, nweeksFit:nweeks])
  data.rnd = melt(dat.rnd)
  data.rnd.pred = melt(dat.rnd.pred)
  plot_national = plot_national + 
    geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "#E495A5", size = 2, alpha = 0.4) +
    geom_line(aes(x = 1:nweeksFit, y = fit_model[1:nweeksFit]), col = "#39BEB1", size = 1) +
    geom_line(aes(x = 1:nweeksFit, y = fit_model_mean[1:nweeksFit]), col = "#099DD7", size = 0.8) +
    geom_line(aes(x = 1:nweeks, y = model_ili), col = "black",na.rm = TRUE) +
    geom_point(aes(x = 1:nweeksFit, y = model_ili[1:nweeksFit]), col = "#24576D", size = 1) 
  
  if (nweeksFit < nweeks) {
    plot_national = plot_national +
      geom_line(aes(x = data.rnd.pred[,1] + nweeksFit - 1, y = data.rnd.pred[,3],group = data.rnd.pred[,2]), col = "#E495A5", size = 2, linetype = 2,alpha = 0.4) + 
      geom_line(aes(x = nweeksFit:nweeks, y = fit_model[nweeksFit:nweeks]), col = "#39BEB1", size = 1, linetype = 2) +
      geom_line(aes(x = nweeksFit:nweeks, y = fit_model_mean[nweeksFit:nweeks]), col = "#099DD7", size = 0.8, linetype = 2) +
      geom_rect(aes(xmin = nweeksFit, xmax = min(nweeksFit + 4, nweeks), ymin = 0, ymax = Inf), fill = "#D497D3",alpha = 0.7)
  }
  
  if (length(model_onset) > 0) {
    plot_national = plot_national + 
      geom_hline(yintercept = model_onset, col = "#D497D3", size = 1, linetype = 2) 
  }
  reg.name = paste("   ", mydata$model$name, c("-Data", "-Best", "-Mean", "-Random"), sep="")
  plot_national = plot_national + 
    annotate("text", x = rep(-Inf,5), y = rep(Inf,5), 
             label = c(paste("   ", mydata$FY, sep = ""), reg.name), 
             hjust = rep(0,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
             col = c("black","black","#39BEB1","#099DD7","#E495A5"),family = "serif", size = 3.5)
  
  if (model == 2 || model == 3) {
    school = mydata$model$school
    school[school == 0] = NA
    plot_national = plot_national +
      geom_point(aes(x = 1:nweeks, y = school*(ymax/5)), fill = "grey50", col = "grey", size = 3, pch = 22, na.rm = TRUE)
  }
  
  if (model == 1 || model == 3) {
    sh = mydata$model$sh
    plot_national = plot_national +
      geom_line(aes(x = 1:nweeks, y = sh * (ymax/max(sh))), col = "black")
  }
  plot_region[[(n.fit + 1)]] = plot_national
  
  ## Repeat with the direct fit of the national data - if it was done!
  
  if (!is.null(model_rtn) && !is.null(model_profile)) {
    model_mean = rep(0,nweeks)
    for (iweek in 1:nweeks) model_mean[iweek] = mean(model_profile_ili[, iweek])
    ymax = max(model_rtn_ili[1:nweeksData], model_profile_ili[,1:nweeksData], model_ili[1:nweeksData], na.rm = TRUE)
    breaks = seq(from = 1, to = nweeks,by = 4)
    labels = weeks[breaks]
    plot_direct = ggplot(data = NULL) +
      scale_x_continuous(name = "EW #", limits = c(1,nweeks),
                         breaks = breaks, labels = labels) +
      scale_y_continuous(name = "% ILI") +
      coord_cartesian(ylim=c(0, ymax)) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
    
    step = max(1, nRnd/100)
    irnd.set = seq(from = 1, to = nRnd, by = step)
    dat_rnd = t(model_profile_ili[irnd.set, 1:nweeksFit])
    dat_rnd_pred = t(model_profile_ili[irnd.set,nweeksFit:nweeks])
    data_rnd = melt(dat_rnd)
    data_rnd_pred = melt(dat_rnd_pred)
    plot_direct = plot_direct + 
      geom_line(aes(x = data_rnd[,1], y = data_rnd[,3], group = data_rnd[,2]), col = "#E495A5", size = 2, alpha = 0.4) +
      geom_line(aes(x = 1:nweeksFit, y = model_rtn_ili[1:nweeksFit]), col = "#39BEB1", size = 1) +
      geom_line(aes(x = 1:nweeksFit, y = model_mean[1:nweeksFit]), col = "#099DD7", size = 0.8) +
      geom_line(aes(x = 1:nweeks, y = model_ili), col = "black",na.rm = TRUE) +
      geom_point(aes(x = 1:nweeksFit, y = model_ili[1:nweeksFit]), col = "#24576D", size = 1)
    
    if (nweeksFit < nweeks) {
      plot_direct = plot_direct +
        geom_line(aes(x = data_rnd_pred[,1] + nweeksFit - 1, y = data_rnd_pred[,3],group = data_rnd_pred[,2]), col = "#E495A5", size = 2, linetype = 2,alpha = 0.4) + 
        geom_line(aes(x = nweeksFit:nweeks, y = model_rtn_ili[nweeksFit:nweeks]), col = "#39BEB1", size = 1, linetype = 2) +
        geom_line(aes(x = nweeksFit:nweeks, y = model_mean[nweeksFit:nweeks]), col = "#099DD7", size = 0.8, linetype = 2) +
        geom_rect(aes(xmin = nweeksFit, xmax = min(nweeksFit + 4, nweeks), ymin = 0, ymax = Inf), fill = "#D497D3",alpha = 0.7)
    }
    
    if (length(model_onset) > 0) {
      plot_direct = plot_direct + 
        geom_hline(yintercept = model_onset, col = "#D497D3", size = 1, linetype = 2)
    }
    reg.name = paste(mydata$model$name, c("-Data", "-Direct-Best", "-Direct-Mean", "-Direct-Random"), sep = "")
    plot_direct = plot_direct + 
      annotate("text", x = rep(-Inf,5), y = rep(Inf,5), 
               label = c(paste("   ", mydata$FY, sep = ""), paste("   ", reg.name, sep = "")), 
               hjust = rep(0,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
               col = c("black","black","#39BEB1","#099DD7","#E495A5"),family = "serif", size = 3.5)
    
    if (model == 2 || model == 3) {
      school = mydata$model$school
      school[school == 0] = NA
      plot_direct = plot_direct +
        geom_point(aes(x = 1:nweeks, y = school*(ymax/5)), fill = "grey50", col = "grey", size = 3, pch = 22, na.rm = TRUE)
    }
    
    if (model == 1 || model == 3) {
      sh = mydata$model$sh
      plot_direct = plot_direct +
        geom_line(aes(x = 1:nweeks, y = sh * (ymax/max(sh))), col = "black")
    }
  }
  plot_region[[(n.fit+2)]] = plot_direct
  layout = matrix(seq(1,12,1), nrow = 3, byrow = TRUE)
  multiplot(plotlist = plot_region, layout = layout)
  
  if (tolower(device) == "pdf" | tolower(device) == 'png') 
    dev.off()
  
  # Make a movie
  if ((mydata$model$level == 2) && (mydata$fit$level == 3)) {
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
    
    #mydata = get.DICE.data(dataType = dataType, year = year, mod_level = 2, fit_level = 3, model = model, nweeksFit = nweeksFit, isingle=isingle)
    longitude = mydata$fit$attr$lon
    latitude = mydata$fit$attr$lat
    #output = runDICE(dataType = dataType, year = year, mod_level = 2, fit_level = 3, model = model, isingle = isingle, nMCMC = nMCMC, nreal = nreal)
    #onset = output$rtn
    factor = as.numeric(mydata$fit$factor)
    region_r = rtn
    for(i in 1:ncol(rtn)) {
      region_r[,i] = rtn[,i]/factor[i]
    }
    
    nation_long = suppressMessages(geocode("USA")$lon)
    nation_lat = suppressMessages(geocode("USA")$lat)
    #region_r = onset
    
    ## Normailization: divided by minimum value
    for(i in 1:ncol(region_r)) {
      region_r[,i] = region_r[,i] / min(region_r[,i])
    }
    nation_r = apply(region_r,1,sum)
    
    #week = mydata$weeks
    ## Make a movie
    mainDir = getwd()
    err = makeDir(subDir = subDir)
    setwd(paste(mainDir,subDir,sep = "/"))
    frame = nrow(region_r)
    
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
        ggtitle(paste("USA map colored by CDC region - week ", weeks[i], sep = '')) +
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
  
  
  # now dump all the profiles we have to a file
  dump = list()
  dump = list(FY = mydata$FY, weeks = weeks, nweeks = nweeks, nweeksFit = nweeksFit, fit_ili = fit_ili, rtn_ili = rtn_ili, profile_ili = profile_ili, model_ili = model_ili,model_rtn_ili = model_rtn_ili, model_profile_ili = model_profile_ili, fit_model = fit_model, fit_model_mean = fit_model_mean, fit_model_profile = fit_model_profile, fit_onset = fit_onset, fit_coef = fit_coef, fit_school = mydata$fit$school, fit_sh = mydata$fit$sh, model_onset= model_onset, model_school = mydata$model$school, model_sh = mydata$model$sh)
  
  filename = pdfName = paste(subDir, "/profiles-", myName,"-", nweeksFit, "-", ireal, ".RData", sep = "")
  save(dump, file = filename)
  
  
  # and write a csv file with the LLK information for the national and regional from nweeksFit  to nweeksData
  
  if (is.null(model_rtn) || is.null(model_profile)) {
    meanLLK = array(0, c(nweeksData, (n.fit + 2)))
    colnames(meanLLK) = c("CDC EW",reg.fit.name, paste(reg.model.name,'-indirect',sep=''))		
  } else {
    meanLLK = array(0, c(nweeksData, (n.fit + 3)))
    colnames(meanLLK) = c("CDC EW",reg.fit.name, paste(reg.model.name,'-indirect',sep=''),paste(reg.model.name,'-direct',sep=''))		
  }
  
  meanLLK[, 1] = weeks[1:nweeksData]
  
  profile_LLK = array(0,c(nRnd,nweeksData))
  for (i in 1:n.fit) {
    profile_LLK[1:nRnd,1:nweeksData] = 0.
    for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = calcLLK(profile[k,1:nweeksData,i],mydata$fit$epi[1:nweeksData,i])
    for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = cumsum(profile_LLK[k,1:nweeksData])
    for(j in 1:nweeksData) meanLLK[j,(i+1)] = mean(profile_LLK[,j])
    
  }
  
  profile_LLK[k,1:nweeksData] = calcLLK(sum(profile[k,1:nweeksData,1:n.fit]*fit_coef[1:n.fit]),mydata$model$epi[1:nweeksData])
  for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = cumsum(profile_LLK[k,1:nweeksData])
  for(j in 1:nweeksData) meanLLK[j,(n.fit+2)] = mean(profile_LLK[,j])	
  
  if (!is.null(model_profile)) {
    profile_LLK[k,1:nweeksData] = calcLLK(model_profile[k,1:nweeksData],mydata$model$epi[1:nweeksData])
    for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = cumsum(profile_LLK[k,1:nweeksData])
    for(j in 1:nweeksData) meanLLK[j,(n.fit+3)] = mean(profile_LLK[,j])			
  }
  meanLLK = meanLLK[nweeksFit:nweeksData, ]
  meanLLK = round(meanLLK,digits=2)
  
  if (nweeksFit == nweeksData) 
    meanLLK = as.matrix(t(meanLLK))
  filename = paste(subDir, "/llk-", myName,"-", nweeksFit, "-", ireal, ".csv", sep = "")
  write.csv(meanLLK, file = filename)
  err = 0
  return(err)
}

