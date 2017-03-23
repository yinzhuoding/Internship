plotHists.ggplot2 <- function(rtn = NULL, profile = NULL, model_rtn=NULL, model_profile=NULL, mydata = NULL, ireal = 1, run.list = NULL, idevice = 1) {
  
  #' Plot Histograms of the predicted and observed peak value and peak week
  #' 
  #' Plots two hitogram files one for the observed and predicted peak week and the other for the
  #' observed and predicted \% ILI value for all weeks included in the range of the number of weeks fitted
  #' to the number of weeks of data.  The \% ILI is presented in bins of 0.5\%.The default is to
  #' fit all the avialbel data.  In each case we first plot the results for all the fit regions, followed by 
  #' the results for an indirect (coupled or uncoupled) fit to the model region, and the last panel is for
  #' the direct fit to the model region.
  #' @param rtn A 1D numeric array with the best in-direct prediction to the model region
  #' @param profile A 3D numeric array holding random predictions for each of the fit regions based on the history of their MCMC chains.
  #' @param model_rtn A 1D numeric array with the best direct prediction to the model region
  #' @param model_profile A 2D numeric array with randomly chosen predicted profiles obtained by fitting the model region directly.
  #' @param mydata A list with the entire data set of this \code{DICE} run
  #' @param ireal Integer - the MCMC chain number
  #' @param run.list a list with various parameters for the run 
  #' @param idevice Integer - the index of the device in the device array. Default is 1 - make only one format of plot results
  #' @return  Returns \eqn{err = 0} if successful
  
  
  device = run.list$device[idevice]
  
  if (is.null(device)) 
    device = "pdf"
  if (is.null(mydata)) 
    return
  if (is.null(profile)) 
    return
  
  FY = mydata$FY
  model = mydata$imodel
  weeks = mydata$weeks
  nweeks = mydata$nweeks
  nweeksFit = mydata$nweeksFit
  nweeksData = mydata$nweeksData
  reg.fit.name = mydata$fit$name
  reg.model.name = mydata$model$name
  
  n.model = 1
  n.fit = mydata$fit$nregions
  nRnd = dim(profile)[1]
  nRndiceData = nRnd + 1
  n.fit1 = n.fit + 1
  
  # the coefficients are given by the relative populations
  ## for safety normalize it
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
  factor = as.numeric(mydata$fit$factor)
  model_factor = mydata$model$factor
  model_ili = mydata$model$raw
  fit_ili = mydata$fit$raw
  
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
    model_profile_ili = model_profile/model_factor
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
  model_profile_indr = array(data = 0, dim = c(nRnd, nweeks))
  
  # this is the indirect modeling
  for (i in 1:nweeks) {
    for (j in 1:nRnd) {
      model_profile_indr[j, i] = sum(profile_ili[j, i, 1:n.fit] * fit_coef[1:n.fit])
    }
  }
  
  # direct fitting of n.fit regions and indirect of the model
  if (tolower(device) == "pdf") {
    pdfName = paste(subDir, "/hist-prfl-", myName,"-", nweeksFit, "-", ireal, ".pdf", sep = "")
    cat("\n\n For a Histogram Plot of Predicted Profiles See: ", pdfName, "\n\n")
    pdf(file = pdfName, onefile = TRUE, width = 15, height = 9)
  } else if (tolower(device) == 'png') {
    pngName  = paste(subDir, "/hist-prfl-", myName,"-", nweeksFit, "-", ireal, ".png", sep = "")
    cat("\n\n For a Histogram Plot of Predicted Profiles See: ", pngName, "\n\n")
    png(file=pngName,width=1200,height=900)
  } else {
    dev.next()
    dev.new()
  }	
  
  
  nrow = 3 
  ncol = length(nweeksFit:min(nweeks, nweeksFit + 4)) 
  ## a histogram plot of binned data 
  ylab = "Probability Density"
  if (mydata$dataType == 'cdc' | mydata$dataType == 'gft') {
    xlab = '%ILI'
  } else {
    xlab = '# Cases'
  }
  plotlist = list()
  plot_hist = list()
  for (iregion in 1:n.fit) {
    plot_hist[[iregion]] = list()
    for (iweek in nweeksFit:min(nweeks, nweeksFit + 4)) {
      min.val = 0
      if (iweek <= nweeksData) {
        max.val = ceiling(max(fit_ili[iweek, iregion], profile_ili[, iweek, iregion], na.rm = TRUE))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        data = data.frame(x = c(fit_ili[iweek,iregion], profile_ili[,iweek,iregion]),
                          y = c(rep(0,length(fit_ili[iweek,iregion])),rep(1,length(profile_ili[,iweek,iregion]))))
        if (!is.na(fit_ili[iweek,iregion])) {
          plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = ggplot(data = data, aes(x = x)) +
            geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                           breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
            geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                           breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7)
        } else {
          plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = ggplot(data = data, aes(x = x)) +
            geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                           breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7)
        }
        plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] +
          scale_x_continuous(name = xlab, limits = range(breaks),
                             breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                             oob = rescale_none) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        leg.text = c(mydata$FY)
        reg.name = reg.fit.name[iregion]
        rel.pop = round(fit_coef[iregion], digits = 3)
        my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
        leg.text = c(leg.text, reg.name, rel.pop, my.week, "Data", "Direct")
        plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] +
          annotate("text", x = rep(Inf,6), y = rep(Inf,6), 
                   label = paste(leg.text, "   ", sep = ""), 
                   hjust = rep(1,6), vjust = seq(from = 2.5, to = 10, by = 1.5),
                   col = c("black","black","black","black","dodgerblue","forestgreen"), family = "serif", size = 3.5)
      }
      else {
        max.val = ceiling(max(profile_ili[, iweek, iregion], na.rm = TRUE))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = ggplot(data = NULL,aes_string(profile_ili[, iweek, iregion])) +
          geom_histogram(aes(y = ..density..),
                         breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7) +
          scale_x_continuous(name = xlab, limits = range(breaks),
                             breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                             oob = rescale_none) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        leg.text = c(mydata$FY)
        reg.name = reg.fit.name[iregion]
        rel.pop = round(fit_coef[iregion], digits = 3)
        my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
        leg.text = c(leg.text, reg.name, rel.pop, my.week, "Direct")
        plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] = plot_hist[[iregion]][[(iweek - nweeksFit + 1)]] +
          annotate("text", x = rep(Inf,5), y = rep(Inf,5), 
                   label = paste(leg.text, "   ", sep = ""), 
                   hjust = rep(1,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
                   col = c("black","black","black","black","forestgreen"), family = "serif", size = 3.5)
      }
    }
    plotlist = append(plotlist,plot_hist[[iregion]])
  }
  
  plot_dr = list()
  for (iweek in nweeksFit:min(nweeks,nweeksFit+4)) {
    min.val = 0
    if (iweek <= nweeksData) {
      max.val = ceiling(max(model_ili[iweek], model_profile_indr[, iweek]))
      max.val = 2 * max.val
      breaks = seq(from = min.val, to = max.val, by = 0.5)
      data = data.frame(x = c(model_ili[iweek], model_profile_indr[, iweek]),
                        y = c(rep(0,length(model_ili[iweek])),rep(1,length(model_profile_indr[, iweek]))))
      plot_dr[[(iweek - nweeksFit + 1)]] = ggplot(data = data, aes(x = x)) +
        geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                       breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
        geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                       breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7) +
        scale_x_continuous(name = xlab, limits = range(breaks),
                           breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                           oob = rescale_none) +
        scale_y_continuous(name = ylab) +
        theme(text = element_text(size = 10, color = "gray20", face = "italic"),
              axis.text.x = element_text(face = "plain"),
              axis.text.y = element_text(face = "plain"))
      leg.text = c(mydata$FY)
      model.name = mydata$model$name
      my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
      gsub(model.name, ".", " ", model.name)
      leg.text = c(leg.text, model.name, my.week, "Data", "Indirect")
      plot_dr[[(iweek - nweeksFit + 1)]] = plot_dr[[(iweek - nweeksFit + 1)]] +
        annotate("text", x = rep(Inf,5), y = rep(Inf,5), 
                 label = paste(leg.text, "   ", sep = ""), 
                 hjust = rep(1,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
                 col = c("black","black","black","dodgerblue","forestgreen"), family = "serif", size = 3.5)
    } 
    else {
      max.val = ceiling(max(model_profile_indr[, iweek]))
      max.val = 2 * max.val
      breaks = seq(from = min.val, to = max.val, by = 0.5)
      plot_dr[[(iweek - nweeksFit + 1)]] = ggplot(data = NULL,aes_string(model_profile_indr[, iweek])) +
        geom_histogram(aes(y = ..density..),
                       breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7) +
        scale_x_continuous(name = xlab, limits = range(breaks),
                           breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                           oob = rescale_none) +
        scale_y_continuous(name = ylab) +
        theme(text = element_text(size = 10, color = "gray20", face = "italic"),
              axis.text.x = element_text(face = "plain"),
              axis.text.y = element_text(face = "plain"))
      leg.text = c(mydata$FY)
      model.name = mydata$model$name
      my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
      gsub(model.name, ".", " ", model.name)
      
      leg.text = c(leg.text, model.name, my.week, "Indirect")
      plot_dr[[(iweek - nweeksFit + 1)]] = plot_dr[[(iweek - nweeksFit + 1)]] +
        annotate("text", x = rep(Inf,4), y = rep(Inf,4), 
                 label = paste(leg.text, "   ", sep = ""), 
                 hjust = rep(1,4), vjust = seq(from = 2.5, to = 7, by = 1.5),
                 col = c("black","black","black","forestgreen"), family = "serif", size = 3.5)
    }
  }
  plotlist = append(plotlist,plot_dr)
  
  if (!is.null(model_profile_ili)) {
    plot_idr = list()
    for (iweek in nweeksFit:min(nweeks, nweeksFit + 4)) {
      if (iweek <= nweeksData) {
        max.val = ceiling(max(model_ili[iweek], model_profile_ili[, iweek]))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        data = data.frame(x = c(model_ili[iweek], model_profile_ili[, iweek]),
                          y = c(rep(0,length(model_ili[iweek])),rep(1,length(model_profile_ili[, iweek]))))
        plot_idr[[(iweek - nweeksFit + 1)]] = ggplot(data = data, aes(x = x)) +
          geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                         breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
          geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                         breaks = breaks, fill = "mediumpurple", col = "black", alpha = 0.7) +
          scale_x_continuous(name = xlab, limits = range(breaks),
                             breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                             oob = rescale_none) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        
        leg.text = c(mydata$FY)
        model.name = mydata$model$name
        my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
        gsub(model.name, ".", " ", model.name)
        leg.text = c(leg.text, model.name, my.week, "Data", "Direct")
        plot_idr[[(iweek - nweeksFit + 1)]] = plot_idr[[(iweek - nweeksFit + 1)]] +
          annotate("text", x = rep(Inf,5), y = rep(Inf,5), 
                   label = paste(leg.text, "   ", sep = ""), 
                   hjust = rep(1,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
                   col = c("black","black","black","dodgerblue","mediumpurple"), family = "serif", size = 3.5)
      } 
      else {
        max.val = ceiling(max(model_profile_ili[, iweek]))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        plot_idr[[(iweek - nweeksFit + 1)]] = ggplot(data = NULL,aes_string(model_profile_ili[, iweek])) +
          geom_histogram(aes(y = ..density..),
                         breaks = breaks, fill = "mediumpurple", col = "black", alpha = 0.7) +
          scale_x_continuous(name = xlab, limits = range(breaks),
                             breaks = seq(min.val,max.val, by = (max.val-min.val)/4),
                             oob = rescale_none) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        
        leg.text = c(mydata$FY)
        model.name = mydata$model$name
        my.week = paste("%ILI for EW # ", weeks[iweek], sep = "")
        gsub(model.name, ".", " ", model.name)
        leg.text = c(leg.text, model.name, my.week, "Direct")
        plot_idr[[(iweek - nweeksFit + 1)]] = plot_idr[[(iweek - nweeksFit + 1)]] +
          annotate("text", x = rep(Inf,4), y = rep(Inf,4), 
                   label = paste(leg.text, "   ", sep = ""), 
                   hjust = rep(1,4), vjust = seq(from = 2.5, to = 7, by = 1.5),
                   col = c("black","black","black","mediumpurple"), family = "serif", size = 3.5)
      }
    }
  }
  
  plotlist = append(plotlist,plot_idr)
  s = ncol*nrow
  for(i in 1:ceiling(length(plotlist)/s)) {
    layout = matrix(seq(1,s,1), nrow = 3, byrow = TRUE)
    if(i == ceiling(length(plotlist))/s) {
      multiplot(plotlist = plotlist[((i-1)*s+1):length(plotlist)], layout = layout)
    } 
    else{
      multiplot(plotlist = plotlist[((i-1)*s+1):(i*s)], layout = layout)
    }
  }
  
  if (tolower(device) == "pdf" | tolower(device) == "png") 
    dev.off()
  
  colvec = rainbow(n.fit)
  lwd = rep(2, n.fit)
  
  # Plot  the individual fits along with the model fit
  
  if (tolower(device) == "pdf") {
    pdfName = paste(subDir, "/hist-week-max-", myName, "-", nweeksFit, "-", ireal, ".pdf", sep = "")
    cat("\n\n For a Histogram Plot of Peak Week: ", pdfName, "\n\n")
    pdf(file = pdfName, onefile = TRUE, width = 16, height = 12)
    
  } else if (tolower(device) == "png") {
    pdfName = paste(subDir, "/hist-week-max-", myName, "-", nweeksFit, "-", ireal, ".png", sep = "")
    cat("\n\n For a Histogram Plot of Peak Week: ", pngName, "\n\n")
    png(file = pngName, width = 1200, height = 900)
  } else {
    dev.next()
    dev.new()
  }
  
  nrow = 3
  ncol = 4
  
  # maximum week for model and fit in the data 
  
  dat_model_wk_max = which.max(model_ili)
  
  if (mydata$fit$level > mydata$model$level) {
    dat_fit_wk_max = rep(0, n.fit)	
    for (i in 1:n.fit) dat_fit_wk_max[i] = which.max(fit_ili[, i])	
  }
  
  # maximum fit in direct modeling 
  
  drct_model_wk_max = rep(0, nRnd)
  
  if (!is.null(model_profile_ili))
    for (i in 1:nRnd) drct_model_wk_max[i] = which.max(model_profile_ili[i, ])
  
  
  indrct_model_wk_max = rep(0, nRnd)
  for (i in 1:nRnd) indrct_model_wk_max[i] = which.max(model_profile_indr[i, ])
  
  sim_fit_wk_max = array(0, c(nRnd, n.fit))
  
  for (i in 1:n.fit) {
    for (j in 1:nRnd) {
      sim_fit_wk_max[j, i] = which.max(profile_ili[j, , i])
    }
  }
  
  # Continue plotting if fitted at a higher resolution
  wk.min = round(min(dat_fit_wk_max,sim_fit_wk_max))
  wk.max = round(max(dat_fit_wk_max,sim_fit_wk_max))
  
  wk.min = round(0.5*wk.min)
  wk.max = round(1.5*wk.max)
  wk.max = min(wk.max,nweeks)
  wk.min = max(1,wk.min)
  
  breaks = seq(from = wk.min, to = wk.max, by = 1)
  breaks_x = seq(from = wk.min, to = wk.max, by = 4)
  labels = weeks[breaks_x]
  ylab = "Probability Density"
  xlab = "EW #"
  
  plotlist2 = list()
  plot_region = list()
  for (iregion in 1:n.fit) {
    data = data.frame(x = c(dat_fit_wk_max[iregion], sim_fit_wk_max[, iregion]),
                      y = c(rep(0,length(dat_fit_wk_max[iregion])),rep(1,length(sim_fit_wk_max[, iregion]))))
    plot_region[[iregion]] = ggplot(data = data, aes(x = x)) +
      geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                     breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
      geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                     breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7) +
      scale_x_continuous(name = xlab, limits = range(breaks),
                         breaks = breaks_x,
                         labels = labels) +
      scale_y_continuous(name = ylab) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
    
    reg.name = reg.fit.name[iregion]
    rel.pop = round(fit_coef[iregion], digits = 3)
    leg.text = c('Peak Week',"Data", "Direct")
    plot_region[[iregion]]  = plot_region[[iregion]] +
      annotate("text", x = c(rep(-Inf,3),rep(Inf,3)), y = rep(Inf,6), 
               label = c(paste("   ", mydata$FY, sep = ""), paste("   ", reg.name, sep = ""), paste("   ", rel.pop, sep = ""), paste(leg.text, "   ", sep = "")), 
               hjust = c(0,0,0,1,1,1), vjust = c(2.5, 4, 5.5, 2.5, 4, 5.5),
               col = c("black","black","black","black","dodgerblue","forestgreen"), family = "serif", size = 3.5)
  }
  plotlist2 = append(plotlist2, plot_region)
  
  # Plot the national
  wk.min = round(min(dat_model_wk_max,indrct_model_wk_max))
  wk.max = round(max(dat_model_wk_max,indrct_model_wk_max))
  if (!is.null(model_profile_ili)) {
    wk.min = round(min(wk.min,drct_model_wk_max))
    wk.max = round(max(wk.max,drct_model_wk_max))
  }
  wk.min = round(0.5*wk.min)
  wk.max = round(1.5*wk.max)
  wk.max = min(wk.max,nweeks)	
  wk.min = max(1,wk.min)
  
  breaks = seq(from = wk.min, to = wk.max, by = 1)
  breaks_x = seq(from = wk.min, to = wk.max, by = 4)
  labels = weeks[breaks_x]
  plot_nation = list()
  data = data.frame(x = c(dat_model_wk_max, indrct_model_wk_max),
                    y = c(rep(0,length(dat_model_wk_max)),rep(1,length(indrct_model_wk_max))))
  plot_nation[[1]] = ggplot(data = data, aes(x = x)) +
    geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                   breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
    geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                   breaks = breaks, fill = "forestgreen", col = "black", alpha = 0.7) +
    scale_x_continuous(name = xlab, limits = range(breaks),
                       breaks = breaks_x,
                       labels = labels) +
    scale_y_continuous(name = ylab) +
    theme(text = element_text(size = 10, color = "gray20", face = "italic"),
          axis.text.x = element_text(face = "plain"),
          axis.text.y = element_text(face = "plain"))
  
  model.name = mydata$model$name
  gsub(model.name, ".", " ", model.name)
  leg.text = c('Peak Week',"Data", "Indirect")
  plot_nation[[1]] = plot_nation[[1]] + 
    annotate("text", x = c(rep(-Inf,2),rep(Inf,3)), y = rep(Inf,5), 
             label = c(paste("   ", mydata$FY, sep = ""), paste("   ", model.name, sep = ""), paste(leg.text, "   ", sep = "")), 
             hjust = c(0,0,1,1,1), vjust = c(2.5, 4, 2.5, 4, 5.5),
             col = c("black","black","black","dodgerblue","forestgreen"), family = "serif", size = 3.5)
  
  if (!is.null(model_profile_ili)) {
    data = data.frame(x = c(dat_model_wk_max, drct_model_wk_max),
                      y = c(rep(0,length(dat_model_wk_max)),rep(1,length(drct_model_wk_max))))
    plot_nation[[2]] = ggplot(data = data, aes(x = x)) +
      geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                     breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
      geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                     breaks = breaks, fill = "mediumpurple", col = "black", alpha = 0.7) +
      scale_x_continuous(name = xlab, limits = range(breaks),
                         breaks = breaks_x,
                         labels = labels) +
      scale_y_continuous(name = ylab) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
  }
  
  model.name = mydata$model$name
  gsub(model.name, ".", " ", model.name)
  leg.text = c('Peak Week',"Data", "Direct")
  plot_nation[[2]] = plot_nation[[2]] + 
    annotate("text", x = c(rep(-Inf,2),rep(Inf,3)), y = rep(Inf,5), 
             label = c(paste("   ", mydata$FY, sep = ""), paste("   ", model.name, sep = ""), paste(leg.text, "   ", sep = "")), 
             hjust = c(0,0,1,1,1), vjust = c(2.5, 4, 2.5, 4, 5.5),
             col = c("black","black","black","dodgerblue","mediumpurple"), family = "serif", size = 3.5)
  
  plotlist2 = append(plotlist2, plot_nation)
  s = nrow*ncol
  layout = matrix(seq(1,s,1), nrow = nrow, byrow = TRUE)
  multiplot(plotlist = plotlist2, layout = layout)
  
  
  if (tolower(device) == "pdf" | tolower(device) == "png") 
    dev.off()
  
  return(err=0)
  
}