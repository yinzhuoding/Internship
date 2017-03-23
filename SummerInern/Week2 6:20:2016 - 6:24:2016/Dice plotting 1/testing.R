plotFitOnePatch.ggplot2 <- function(model_rtn = NULL, model_profile = NULL, mydata = NULL, ireal = 1, run.list=NULL, idevice = 1) {
  
  #' Plot the results of a \code{DICE} Run - Single Region
  #'
  #' Plot the results of \code{DICE} run for a single region/patch. We show the observed %ILI (or number of cases) along with our fits and
  #' if appropriate predictions. We show the best result and randomly selected results from the MCMC chain. This is the ggplot2 version.
  #' @param model_rtn A 1D numeric array with the best direct prediction to the region
  #' @param model_profile A 2D numeric array with randomly chosen predicted profiles obtained by fitting the data
  #' @param mydata A list with the entire data set of this \code{DICE} run
  #' @param ireal Integer - the MCMC chain number
  #' @param run.list  A list with various run parameters
  #' @param idevice Integer - the index of the device in the device array. Default is 1 - make only one format of plot results
  #' @return  Returns \eqn{err = 0} if successful
  #' @examples
  #' plotFitOnePatch{model_rtn = model_rtn, model_profile = model_profile, 
  #' mydata = mydata, ireal = ireal, device = device}
  
  device = run.list$device[1]
  if (is.null(device)) 
    device = "x11"
  if (is.null(model_profile)) 
    return
  device = run.list$device[1]
  if (is.null(device)) 
    device = "x11"
  if (is.null(model_profile)) 
    return
  
  FY = mydata$FY 
  model = mydata$imodel 
  weeks = mydata$weeks 
  nweeks = mydata$nweeks 
  nweeksFit = mydata$nweeksFit 
  nweeksData = mydata$nweeksData 
  reg.model.name = mydata$model$name 
  
  nRnd = dim(model_profile)[1] 
  n.model = 1
  
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
    cat("\n\n For a Plot of the Results See: ", pdfName, "\n\n")
    pdf(file = pdfName, onefile = TRUE, width = 15, height = 9)
  } else if (tolower(device) == 'png') {
    pngName  = paste(subDir, "/results-", myName,"-", nweeksFit, "-", ireal, ".png", sep = "")
    cat("\n\n For a Plot of the Results See: ", pngName, "\n\n")
    png(file=pngName,width=1200,height=900)
  } else {
    dev.next()
    dev.new()
  }
  
  # convert the fit and model results to  %ILI
  # calculate the national
  
  # convert the model to  %ILI
  # calculate the national
  model_factor = mydata$model$factor 
  
  ## This model raw data
  model_ili = mydata$model$raw
  
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
    index <- which(model_ili == 0)
    if (length(index) >= 1) 
      model_ili[index] = NA	
  }
  
  ## Determine ylabel based on dataType
  if (mydata$dataType == 'cdc' | mydata$dataType == 'gft') {
    ylab = '%ILI'
  } else {
    ylab = '# Cases'
  }
  
  # Plot the data and fits/predictions 
  plotlist = list() # For saving all the plots
  if (!is.null(model_rtn) && !is.null(model_profile)) {
    model_mean = rep(0,nweeks)
    for (iweek in 1:nweeks) model_mean[iweek] = mean(model_profile_ili[, iweek])
    ymax = max(model_rtn_ili[1:nweeksData], model_profile_ili[,1:nweeksData], model_ili[1:nweeksData], na.rm = TRUE)
    
    breaks = seq(from = 1, to = nweeks, by = 4)
    labels = weeks[breaks]
    
    plotlist[[1]] = ggplot(data = NULL) + 
      scale_x_continuous(name = "EW #", limits = c(1,nweeks),
                         breaks = breaks, labels = labels) +
      scale_y_continuous(name = ylab, limits = c(0, ymax)) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
    
    step = max(1,nRnd/100)
    irnd.set = seq(from = 1, to = nRnd, by = step)
    dat.rnd = t(model_profile[irnd.set, 1:nweeksFit])
    dat.rnd.pred = t(model_profile_ili[irnd.set,nweeksFit:nweeks])
    data.rnd = melt(dat.rnd)
    data.rnd.pred = melt(dat.rnd.pred)
    
    plotlist[[1]] = plotlist[[1]] + geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "#E495A5", size = 2, alpha = 0.4) +
      geom_line(aes(x = 1:nweeksFit, y = model_rtn_ili[1:nweeksFit]), col = "#39BEB1", size = 1) +
      geom_line(aes(x = 1:nweeksFit, y = model_mean[1:nweeksFit]), col = "#099DD7", size = 0.8) +
      geom_line(aes(x = 1:nweeks, y = model_ili), col = "black", na.rm = TRUE) +
      geom_point(aes(x = 1:nweeksFit, y = model_ili[1:nweeksFit]), col = "#24576D", size = 1,na.rm = TRUE) 
    
    if (nweeksFit < nweeks) {     	
      plotlist[[1]] = plotlist[[1]] +  
        geom_line(aes(x = (data.rnd.pred[,1] + nweeksFit - 1), y = data.rnd.pred[,3], group = data.rnd.pred[,2]), col = "#E495A5", size = 2, linetype = 2,alpha = 0.4) +     
        geom_line(aes(x = nweeksFit:nweeks, y = model_mean[nweeksFit:nweeks]), col = "#099DD7", size = 0.8, linetype = 2) +
        geom_line(aes(x = nweeksFit:nweeks, y = model_rtn_ili[nweeksFit:nweeks]), col = "#39BEB1", size = 1, linetype = 2) +      
        geom_rect(aes(xmin = nweeksFit, xmax = min(nweeksFit + 4, nweeks), ymin = 0, ymax = ymax), fill = "#D497D3",alpha = 0.7)
    }
    
    if (length(model_onset) > 0) {
      plotlist[[1]] = plotlist[[1]] + 
        geom_hline(yintercept = model_onset, col = "#D497D3", size = 1, linetype = 2) 
      
    }
    
    reg.name = paste("   ", mydata$model$name, c("-Data", "-Model-Best", "-Model-Mean", "-Model-Random"), sep = "")
    plotlist[[1]] = plotlist[[1]] + annotate("text", x = rep(-Inf,5), y = rep(Inf,5), 
                                             label = c(paste("   ", mydata$FY, sep = ""), reg.name), 
                                             hjust = rep(0,5), vjust = seq(from = 2.5, to = 8.5, by = 1.5),
                                             col = c("black","black","#39BEB1","#099DD7","#E495A5"),
                                             family = "serif", size = 3.5)
    # The following two blocks have not been tested
    if (model == 2 || model == 3) {
      school = mydata$model$school
      school[school == 0] = NA
      plotlist[[1]] = plotlist[[1]] + geom_point(aes(x = 1:nweeks, y = school*(ymax/5)), fill = "grey50", col = "grey", size = 3, pch = 22, na.rm = TRUE)
    }
    if (model == 1 || model == 3) {
      sh = mydata$model$sh
      plotlist[[1]] = plotlist[[1]] + geom_line(aes(x = 1:nweeks, y = sh * (ymax/max(sh))), col = "black") 
    }
  }
  
  # maximum week in data 
  dat_model_wk_max = which.max(model_ili)
  
  # maximum week in model
  drct_model_wk_max = rep(0, nRnd)
  
  if (!is.null(model_profile_ili))
    for (i in 1:nRnd) drct_model_wk_max[i] = which.max(model_profile_ili[i, ])
  
  if (!is.null(model_profile_ili)) {
    wk.min = round(min(dat_model_wk_max,drct_model_wk_max))
    wk.max = round(max(dat_model_wk_max,drct_model_wk_max))
  }
  wk.min = round(0.5*wk.min)
  wk.max = round(1.5*wk.max)
  wk.max = min(wk.max,nweeks)	
  wk.min = max(1,wk.min)
  ylab = "Probability Density"
  xlab = "EW #"
  
  # Plot histograms of maximum week in model
  if (!is.null(model_profile_ili)) {
    
    breaks = seq(from = wk.min, to = wk.max, by = 1)
    breaks_x = seq(from = wk.min, to = wk.max, by = 4)
    labels = weeks[breaks_x]
    data = data.frame(x = c(dat_model_wk_max, drct_model_wk_max),
                      y = c(rep(0,length(dat_model_wk_max)),rep(1,length(drct_model_wk_max))))
    plotlist[[2]] = ggplot(data = data, aes(x = x)) +
      geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                     breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
      geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                     breaks = breaks, fill = "deeppink", col = "black", alpha = 0.7) +
      scale_x_continuous(name = xlab, limits = range(breaks),
                         breaks = breaks_x, labels = labels) +
      scale_y_continuous(name = ylab) +
      theme(text = element_text(size = 10, color = "gray20", face = "italic"),
            axis.text.x = element_text(face = "plain"),
            axis.text.y = element_text(face = "plain"))
  }
  
  model.name = mydata$model$name
  gsub(model.name, ".", " ", model.name)	
  leg.text = c(mydata$FY, model.name,"Data", "Model")
  plotlist[[2]] = plotlist[[2]] + 
    annotate("text", x = c(rep(-Inf,2), rep(Inf,4)), y = rep(Inf,6), 
             label = c(paste("   ", "Observed/Predicted", sep = ""), paste("   ", "Peak Week", sep = ""), leg.text), 
             hjust = c(0,0,1,1,1,1), vjust = c(2.5,4,seq(from = 2.5, to = 7, by = 1.5)),
             col = c("black","black","black","black","dodgerblue","deeppink"),
             family = "serif", size = 3.5)
  
  ## Histogram plots - of %ILI binned
  ylab = "Probability Density"
  if (mydata$dataType == 'cdc' | mydata$dataType == 'gft') {
    xlab = '%ILI'
  } else {
    xlab = '# Cases'
  }
  if (!is.null(model_profile_ili)) {
    for (iweek in nweeksFit:min(nweeks,nweeksFit+4)) {
      min.val = 0
      if (iweek <= nweeksData) {
        max.val = ceiling(max(model_ili[iweek], model_profile_ili[, iweek], na.rm = TRUE))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        if (max.val <= 20) step = 2
        else if (max.val <= 40) step = 4
        else if (max.val <= 150) step = 10
        else step = 50
        data = data.frame(x = c(model_ili[iweek], model_profile_ili[,iweek]),
                          y = c(rep(0,length(model_ili[iweek])),rep(1,length(model_profile_ili[,iweek]))))
        if (!is.na(model_ili[iweek])) {
          plotlist[[iweek - nweeksFit + 3]] = ggplot(data = data, aes(x = x)) +
            geom_histogram(data = subset(data, y == 0), aes(y = ..density..), 
                           breaks = breaks, fill = "dodgerblue",col = "black", alpha = 0.7) +
            geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                           breaks = breaks, fill = "deeppink", col = "black", alpha = 0.7) 
          
        } else {
          plotlist[[iweek - nweeksFit + 3]] = ggplot(data = data, aes(x = x)) + 
            geom_histogram(data = subset(data, y == 1), aes(y = ..density..),
                           breaks = breaks, fill = "deeppink", col = "black", alpha = 0.7)
        }
        plotlist[[iweek - nweeksFit + 3]] = plotlist[[iweek - nweeksFit + 3]] +
          scale_x_continuous(name = xlab, limits = range(breaks),
                             breaks = seq(from = min.val, to = max.val, by = step)) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        
        model.name = mydata$model$name
        my.week = paste("EW # ", weeks[iweek], sep = "")
        gsub(model.name, ".", " ", model.name)
        leg.text = c(mydata$FY, model.name, "Data", "Model")
        plotlist[[iweek - nweeksFit + 3]] = plotlist[[iweek - nweeksFit + 3]] + 
          annotate("text", x = c(rep(-Inf,2), rep(Inf,4)), y = rep(Inf,6), 
                   label = c(paste("   ", "Observed/Predicted", sep = ""), paste("   %ILI for ", my.week, sep = ""), leg.text), 
                   hjust = c(0,0,1,1,1,1), vjust = c(2.5,4,seq(from = 2.5, to = 7, by = 1.5)),
                   col = c("black","black","black","black","dodgerblue","deeppink"),
                   family = "serif", size = 3.5)
      }
      else {
        max.val = ceiling(max(model_profile_ili[, iweek], na.rm = TRUE))
        max.val = 2 * max.val
        breaks = seq(from = min.val, to = max.val, by = 0.5)
        plotlist[[iweek - nweeksFit + 3]] = ggplot(data = NULL) +
          geom_histogram(aes(model_profile_ili[,iweek], y = ..density..),
                         fill = "dodgerblue",col = "black",breaks = breaks, alpha = 0.7) +
          scale_x_continuous(name = xlab, limits = c(breaks[1], breaks[length(breaks)]),
                             breaks = seq(min.val,max.val, by = 2)) +
          scale_y_continuous(name = ylab) +
          theme(text = element_text(size = 10, color = "gray20", face = "italic"),
                axis.text.x = element_text(face = "plain"),
                axis.text.y = element_text(face = "plain"))
        
        model.name = mydata$model$name
        my.week = paste("EW # ", weeks[iweek], sep = "")
        gsub(model.name, ".", " ", model.name)
        leg.text = c(mydata$FY, model.name, "Model")
        plotlist[[iweek - nweeksFit + 3]] = plotlist[[iweek - nweeksFit + 3]] + 
          annotate("text", x = c(rep(-Inf,2), rep(Inf,3)), y = rep(Inf,5), 
                   label = c(paste("   ", "Observed/Predicted", sep = ""), paste("   %ILI for ", my.week, sep = ""), leg.text), 
                   hjust = c(0,0,1,1,1), vjust = c(2.5,4,2.5,4,5.5),
                   col = c("black","black","black","black","dodgerblue"),
                   family = "serif", size = 3.5)
      }
    }
  }
  layout = matrix(seq(1,9,1), nrow = 3, byrow = TRUE)
  multiplot(plotlist = plotlist, layout = layout)
  
  if (tolower(device) == "pdf") 
    dev.off()
  
  # now dump all the profiles we have to a file
  dump = list()
  dump = list(FY = mydata$FY, weeks = weeks, nweeks = nweeks, nweeksFit = nweeksFit, model_ili = model_ili,model_rtn_ili = model_rtn_ili, model_profile_ili = model_profile_ili, model_onset= model_onset, model_school = mydata$model$school, model_sh = mydata$model$sh)
  
  filename = pdfName = paste(subDir, "/profiles-", myName,"-", nweeksFit, "-", ireal, ".RData", sep = "")
  save(dump, file = filename)
  
  
  # and write a csv file with the LLK information for the national and regional from nweeksFit  to nweeksData
  
  
  meanLLK = array(0, c(nweeksData, 2))
  colnames(meanLLK) = c("CDC EW",reg.model.name)		
  
  
  meanLLK[, 1] = weeks[1:nweeksData]
  
  profile_LLK = array(0,c(nRnd,nweeksData))
  
  if (!is.null(model_profile)) {
    for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = calcLLK(model_profile[k,1:nweeksData],mydata$model$epi[1:nweeksData])
    for(k in 1:nRnd) profile_LLK[k,1:nweeksData] = cumsum(profile_LLK[k,1:nweeksData])
    for(j in 1:nweeksData) meanLLK[j,2] = mean(profile_LLK[,j])			
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