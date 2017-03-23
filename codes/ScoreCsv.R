### Score estimation from csv file

ScoreStartWeek <- function(data,onset,sweek,pot,prob) {
  #' Score the estimation of start week
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param onset The baseline 
  #' @param sweek Season week in DICE data
  #' @prarm pot Point Estimation of start week
  #' @param prob Prob Estimation of start week
  #' @return 1*3 vector with actual value, log score and absolute error
  pot = as.numeric(pot)
  season.week = sweek
  temp = which(data >= onset)
  # If there is no onset week currently, then we will not score our estimates
  if(length(temp) <= 2) {
    sw = NA
    score.sw = NA
    ae.sw = NA
  }else {
    for(ck in 1:(length(temp) - 2)) {
      if(temp[ck] + 2 == temp[ck + 2]) {
        sw = season.week[temp[ck]]
        break
      }
    }
    # Log score
    if(sw == 52) {sw.range = c(51,52,1)}
    else if(sw == 1) {sw.range = c(52,1,2)}
    else {
      sw.range = c(sw - 1, sw, sw + 1)
    }
    match = which(prob[, ncol(prob)-1] %in% sw.range)
    if(length(match) == 0) {
      #score.sw = log(0)
      score.sw = -10
    }else {
      score.sw = log(sum(prob[match,ncol(prob)]))
    }
    # Absolute error
    if(!is.na(pot)) {
      ae.sw = abs(which(season.week == sw) - which(season.week == pot))
    }else{
      ae.sw = NA
    }
  }
  return(c(sw,score.sw,ae.sw))
}

ScorePeakWeek <- function(data,sweek,pot,prob,peak = TRUE) {
  #' Score the estimation of peak week
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param sweek Seanson week in DICE data
  #' @prarm pot Point Estimation of peak week
  #' @param prob Prob Estimation of peak week
  #' @param peak Boolean variable indicating whether there exists a peak week 
  #' @return 1*3 vector with actual value, log score and absolute error
  if(peak == FALSE) {
    return(c(NA, NA, NA))
  }
  if(peak == TRUE) {
    pot = as.numeric(pot)
    season.week = sweek
    temp = which(data == max(data))
    pw = season.week[temp]
    # Log score
    pw.range = NULL
    for(wk in pw) {
      if(wk == 52) {wk.range = c(51,52,1)}
      else if(wk == 1) {wk.range = c(52,1,2)} 
      else {
        wk.range = c(wk - 1, wk, wk + 1)
      }
      pw.range = union(pw.range, wk.range)
    }
    match = which(prob[,ncol(prob)-1] %in% pw.range)
    if(length(match) == 0) {
      score.pw = -10
    }else {
      score.pw = log(sum(prob[match,ncol(prob)]))
    }
    # Absolue error
    if(length(pw) == 1) {
      ae.pw = abs(which(season.week == pw) - which(season.week == pot))
      return(c(pw, score.pw, ae.pw))
    } else {
      return(c(NA, score.pw, NA))
    }
  }
}

ScorePeakInty <- function(data,pot,prob,peak = TRUE) {
  #' Score the estimation of peak intensity
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @prarm pot Point Estimation of peak intensity
  #' @param prob Prob Estimation of peak intensity
  #' @param peak Boolean variable indicating whether there exists a peak week 
  #' @return *3 vector with actual value, log score and absolute error
  if(peak == FALSE) {
    return(c(NA,NA,NA))
  }
  if(peak == TRUE) {
    pot = as.numeric(pot)
    pi = max(data,na.rm = TRUE)
    bin.pi = seq(from = 0, to = 13, by = 0.1)
    temp = which(pi >= bin.pi - 0.05)
    bin.num = temp[length(temp)]
    m = length(bin.pi)
    # Log score
    lb = max(1, bin.num - 5)
    ub = min(m, bin.num + 5)
    pi.range = seq(lb, ub, 1)
    est.bin.num = which(prob[,ncol(prob)] != 0)
    match = which(est.bin.num %in% pi.range)
    if(length(match) == 0) {
      score.pi = -10
    }else {
      score.pi = log(sum(prob[est.bin.num[match],ncol(prob)]))
    }
    # Absolute error
    ae.pi = abs(pot - pi)
    return(c(pi, score.pi, ae.pi))
  }
}

ScoreForecast <- function(data,nweekfit, nweekdata, pot,prob) {
  #' Score the estimation of peak intensity
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param nweekfit A numeric value of the number of fitted weeks, so from nweekfit + 1, the data is a prediction
  #' @param nweekdata Number of weeks data we have
  #' @prarm pot Point Estimation of peak intensity
  #' @param prob Prob Estimation of peak intensity
  #' @return 4*3 dataframe with actual value, log score and absolute error
  pot = as.numeric(pot)
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi)
  score.fc = rep(NA,4)
  ae.fc = rep(NA,4)
  forecast = data[(nweekfit+1):(nweekfit+4)]
  # if there is no data to compare, then score = NA
  if((nweekdata - nweekfit) > 0) {
    nfc = min(4,nweekdata - nweekfit)
    # Log score/Absolute error 
    for(i in 1:nfc) {
      temp = which(forecast[i] >= bin.pi - 0.05)
      bin.num = temp[length(temp)]
      lb = max(1, bin.num - 5)
      ub = min(m, bin.num + 5)
      fc.range = seq(lb, ub, 1)
      est.bin.num = which(prob[,i+1] != 0)
      match = which(est.bin.num %in% fc.range)
      if(length(match) == 0) {
        score.fc[i] = -10
      }else {
        score.fc[i] = log(sum(prob[est.bin.num[match], i+1]))
      }
      ae.fc[i] = abs(pot[i] - forecast[i])
    }
  }
  return(cbind(forecast,score.fc,ae.fc))
}

ScoreCsv <- function(mydata, est, nweeksFit, peak = TRUE) {
  #' Give the log score and absolute error of our estimation compared with cdc data
  #' @param mydata DICE data for comparison
  #' @param est Submission .csv file
  #' @param nweeksFit Current available weeks of data
  #' @param peak Boolean variable indicating whether there exists a peak week 
  #' @return return nothing, create two .csv files saving the score for national and regional seperately.
  #' 
  nat.onset = mydata$model$onset
  reg.onset = as.numeric(mydata$fit$onset)
  nweeksData = mydata$nweeksData
  week = mydata$weeks
  
  target.name = as.character(unique(est$Target))
  location.name = as.character(unique(est$Location))
  ### National Score
  nat.cdc.data = mydata$model$raw
  bins = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Bin"))$Bin_start_incl
  bins = suppressWarnings(as.numeric(levels(bins))[bins])
  # start week
  nat.pot.st = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Point"))$Value
  temp = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Bin"))$Value
  index = which(temp !=0)
  nat.prob.st = cbind(as.numeric(bins[index]), temp[index])
  nat.res.sw = ScoreStartWeek(nat.cdc.data, nat.onset, week, nat.pot.st, nat.prob.st)
  # peak week
  nat.pot.pw = subset(est, (Location == "US National")&(Target == target.name[2])&(Type == "Point"))$Value
  temp = subset(est, (Location == "US National")&(Target == target.name[2])&(Type == "Bin"))$Value
  index = which(temp !=0)
  nat.prob.pw = cbind(as.numeric(bins[index]), temp[index])
  nat.res.pw = ScorePeakWeek(nat.cdc.data, week, nat.pot.pw, nat.prob.pw,peak)
  # peak intensity
  nat.pot.pi = subset(est, (Location == "US National")&(Target == target.name[3])&(Type == "Point"))$Value
  temp = subset(est, (Location == "US National")&(Target == target.name[3])&(Type == "Bin"))$Value
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  nat.prob.pi = cbind(bin.pi, temp)
  nat.res.pi = ScorePeakInty(nat.cdc.data, nat.pot.pi,nat.prob.pi, peak)
  # 4 week ahead forecast
  nat.pot.fc = subset(est, (Location == "US National")&(Target %in% target.name[4:7])&(Type == "Point"))$Value
  nat.prob.fc = bin.pi
  for(wk in 1:4) {
    nat.prob.fc = cbind(nat.prob.fc, subset(est, (Location == "US National")&(Target %in% target.name[wk+3])&(Type == "Bin"))$Value)
  }
  nat.res.fc = ScoreForecast(nat.cdc.data, nweeksFit, nweeksData, nat.pot.fc, nat.prob.fc)
  
  # Create a csv file for the score of national
  nat.est = as.numeric(c(nat.pot.st,nat.pot.pw,nat.pot.pi,as.numeric(nat.pot.fc)))
  nat.score = rbind(nat.res.sw, nat.res.pw, nat.res.pi, nat.res.fc)
  nat.score = cbind(nat.est,nat.score)
  target = c("Start week", "Peak week", "Peak intensity", rep("Forecast",4))
  nat.score = cbind(rep("US National",nrow(nat.score)), target, nat.score)
  row.names(nat.score) = seq(1,nrow(nat.score),1)
  nat.score = as.data.frame(nat.score)
  names(nat.score) = c("Location","Target","Point Est","Actual Value", "Log score", "Absolute error")
  filename = paste("Nationalscore",nweeksFit, ".csv", sep = "")
  write.csv(nat.score, file = filename, row.names = FALSE)
  cat("See National score at: 'Nationalscore.csv'\n")
  
  ### HHS region
  reg.sw = NULL
  reg.pw = NULL
  reg.pi = NULL
  reg.fc = NULL

  for (reg.num in 1:10) {
    reg.data = mydata$fit$raw[,reg.num]
    # start week
    pot.st = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[1])&(Type == "Point"))$Value
    temp = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[1])&(Type == "Bin"))$Value
    index = which(temp !=0)
    prob.st = cbind(as.numeric(bins[index]), temp[index])
    sw.res = ScoreStartWeek(reg.data, reg.onset[reg.num], week, pot.st, prob.st)
    reg.sw = rbind(reg.sw, sw.res)
    # peak week
    pot.pw = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[2])&(Type == "Point"))$Value
    temp = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[2])&(Type == "Bin"))$Value
    index = which(temp !=0)
    prob.pw = cbind(as.numeric(bins[index]), temp[index])
    pw.res = ScorePeakWeek(reg.data, week, pot.pw, prob.pw,peak)
    reg.pw = rbind(reg.pw, pw.res)
    # peak intensity
    pot.pi = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[3])&(Type == "Point"))$Value
    temp = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[3])&(Type == "Bin"))$Value
    prob.pi = cbind(bin.pi, temp)
    pi.res = ScorePeakInty(reg.data, pot.pi,prob.pi, peak)
    reg.pi = rbind(reg.pi, pi.res)
    # forecast
    pot.fc = subset(est, (Location == location.name[reg.num + 1])&(Target %in% target.name[4:7])&(Type == "Point"))$Value
    prob.fc = bin.pi
    for(wk in 1:4) {
      prob.fc = cbind(prob.fc, subset(est, (Location == location.name[reg.num + 1])&(Target %in% target.name[wk+3])&(Type == "Bin"))$Value)
    }
    fc.res = ScoreForecast(reg.data, nweeksFit, nweeksData, pot.fc,prob.fc)
    fc.res = cbind(rep(reg.num,4), fc.res)
    reg.fc = rbind(reg.fc, fc.res)
  }
  row.names(reg.sw) = seq(1,10,1)
  reg.sw = data.frame(seq(1,10,1),reg.sw)
  names(reg.sw) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  row.names(reg.pw) = seq(1,10,1)
  reg.pw = data.frame(seq(1,10,1),reg.pw)
  names(reg.pw) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  row.names(reg.pi) = seq(1,10,1)
  reg.pi = data.frame(seq(1,10,1),reg.pi)
  names(reg.pi) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  row.names(reg.fc) = seq(1,nrow(reg.fc),1)
  reg.fc = as.data.frame(reg.fc)
  names(reg.fc) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  # Create a csv file for the score of regional
  reg.pot.st = subset(est, (Location %in% location.name[-1])&(Target == target.name[1])&(Type == "Point"))$Value
  a = data.frame(rep("Start week", 10), reg.pot.st, reg.sw[,-1])
  names(a)[1:2] = c("Target","Point Est")
  hhs = seq(1,10,1)
  a = cbind(hhs,a)
  
  reg.pot.pw = subset(est, (Location %in% location.name[-1])&(Target == target.name[2])&(Type == "Point"))$Value
  b = data.frame(rep("Peak week", 10), reg.pot.pw, reg.pw[,-1])
  names(b)[1:2] = c("Target","Point Est")
  b = cbind(hhs,b)
  
  reg.pot.pi = subset(est, (Location %in% location.name[-1])&(Target == target.name[3])&(Type == "Point"))$Value
  c = data.frame(rep("Peak intensity", 10), reg.pot.pi, reg.pi[,-1])
  names(c)[1:2] = c("Target", "Point Est")
  c = cbind(hhs,c)
  
  reg.pot.fc = subset(est, (Location %in% location.name[-1])&(Target %in% target.name[4:7])&(Type == "Point"))$Value
  
  d = data.frame(rep("Forecast", length(reg.pot.fc)), reg.pot.fc, reg.fc[-1])
  names(d)[1:2] = c("Target","Point Est")
  hhs = NULL
  for(i in 1:10){
    hhs = c(hhs,rep(i,4))
  }
  d = cbind(hhs,d)
  score = rbind(a,b,c,d)
  names(score)[1] = "Location"
  score = as.matrix(score)
  filename = paste("Regscore",nweeksFit, ".csv", sep = "")
  write.csv(score, file = filename,row.names = FALSE)
  cat("See Regional score at: 'Regscore.csv'\n")
}


ReadCDCSubmission <- function(est) {
  #' Give the log score and absolute error of our estimation compared with cdc data
  #' @param mydata DICE data for comparison
  #' @param est Submission .csv file
  #' @param nweeksFit Current available weeks of data
  #' @param peak Boolean variable indicating whether there exists a peak week 
  #' @return return nothing, create two .csv files saving the score for national and regional seperately.
  #' 

  out = list()
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  
  target.name = as.character(unique(est$Target))
  location.name = as.character(unique(est$Location))
  ### National Score
  # nat.cdc.data = mydata$model$raw
  bins = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Bin"))$Bin_start_incl
  bins = suppressWarnings(as.numeric(levels(bins))[bins])
  
  out$prob.st = data.frame(weeks = bins)
  out$prob.pw = data.frame(weeks = bins[!is.na(bins)])
  out$prob.pi = data.frame(intensity = bin.pi)
  out$pot.fc = data.frame(row.names=c("week1","week2","week3","week4"))
  out$prob.fc$intensity = bin.pi
  out$prob.fc$week1 = data.frame(row.names = bin.pi)
  out$prob.fc$week2 = data.frame(row.names = bin.pi)
  out$prob.fc$week3 = data.frame(row.names = bin.pi)
  out$prob.fc$week4 = data.frame(row.names = bin.pi)
  
  # start week
  out$pot.st$national = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Point"))$Value
  out$prob.st$national = subset(est, (Location == "US National")&(Target == target.name[1])&(Type == "Bin"))$Value

  # peak week
  out$pot.pw$national = subset(est, (Location == "US National")&(Target == target.name[2])&(Type == "Point"))$Value
  out$prob.pw$national = subset(est, (Location == "US National")&(Target == target.name[2])&(Type == "Bin"))$Value

  # peak intensity
  out$pot.pi$national = subset(est, (Location == "US National")&(Target == target.name[3])&(Type == "Point"))$Value
  out$prob.pi$national = subset(est, (Location == "US National")&(Target == target.name[3])&(Type == "Bin"))$Value

  # 4 week ahead forecast
  out$pot.fc$national = subset(est, (Location == "US National")&(Target %in% target.name[4:7])&(Type == "Point"))$Value
  for(wk in 1:4) {
    out$prob.fc[[paste0("week",wk)]]$national = subset(est, (Location == "US National")&(Target %in% target.name[wk+3])&(Type == "Bin"))$Value
  }

  ### HHS region
  for (reg.num in 1:10) {
    # start week
    out$pot.st[paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[1])&(Type == "Point"))$Value
    out$prob.st[,paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[1])&(Type == "Bin"))$Value

    # peak week
    out$pot.pw[paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[2])&(Type == "Point"))$Value
    out$prob.pw[,paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[2])&(Type == "Bin"))$Value

    # peak intensity
    out$pot.pi[paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[3])&(Type == "Point"))$Value
    out$prob.pi[,paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target == target.name[3])&(Type == "Bin"))$Value

    # forecast
    out$pot.fc[paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target %in% target.name[4:7])&(Type == "Point"))$Value

    for(wk in 1:4) {
      out$prob.fc[[paste0("week",wk)]][,paste0("Region",reg.num)] = subset(est, (Location == location.name[reg.num + 1])&(Target %in% target.name[wk+3])&(Type == "Bin"))$Value
    }

  }
  
  out$pot.st = unlist(out$pot.st)
  out$pot.pw = unlist(out$pot.pw)
  out$pot.pi = unlist(out$pot.pi)
  
  return(out)
}


PlotSubmission <- function(SubDat=NULL, dump=NULL, OutDir=NULL) {
  
  intoff = .045
  weekoff = .45
  nweeksFit = dump$nweeksFit
  nint = length(SubDat$prob.pi$intensity)
  start.year = as.integer(strsplit(dump$FY,split="-")[[1]][1])
  
  # First retrieve current CDC data
  require(DICE)
  mydata = get.subset(start.year=start.year,end.year=start.year+1)
  mydata$model$raw[mydata$model$raw==0] = NA
  mydata$fit$raw[mydata$fit$raw==0] = NA
  
  weeks = 1:(mydata$nweeks)
  prob.weeks = (which(mydata$weeks==SubDat$prob.st$weeks[1])):which(mydata$weeks==SubDat$prob.st$weeks[length(SubDat$prob.st$weeks)-1])
  prob.weeks.onset = c(prob.weeks,1)
  
  # this version makes transparency proportional to CDC-challenge-score
  trans1 <- function(input) {
    # this works for start week and peak week
    week = SubDat$prob.pw$weeks
    nint = length(input)
    trans = rep(NA,nint)
    for(wk in week) {
      if(is.na(wk)) {wk.range = wk}
      else if(wk == 1) {wk.range = c(52,1,2)}
      else if(wk == 52) {wk.range = c(51,52,1)}
      else{wk.range = c(wk-1, wk, wk + 1)}
      match = which(week %in% wk.range)
      if(length(match) == 0) {
        trans[which(week == wk)] = -10
      }else{
        trans[which(week == wk)] = log(sum(input[match]))
      }
    }
    if(nint != length(week)) {
      trans[nint] = log(input[nint])
    }
    trans[trans < -10] = -10
    trans = trans/10 + 1
    return(trans)
  }
  
  trans2 <- function(input) {
    # this works for peak intensity and forecast
    nint = length(input)
    trans = rep(NA,nint)
    for(bin.num in 1:nint) {
      lb = max(1, bin.num - 5)
      ub = min(nint, bin.num + 5)
      range = seq(lb, ub, 1)
      trans[bin.num] = log(sum(input[range]))
    }
    trans[trans < -10] = -10
    trans = trans/10 + 1
    return(trans)
  }
  
  pdf(file=paste0(OutDir,"Submission_EW",mydata$weeks[nweeksFit],"_EWdata",mydata$weeks[mydata$nweeksData],".pdf"), width=12, height=9)
  par(mfrow=c(3,4))
  
  
  # First Plot each region
  for(region in 1:10) {
    colName = paste0("Region",region)
    
    # Plot data
    MaxInt = max(which(SubDat$prob.pi[,colName] > 0))
    plot(mydata$fit$raw[,region], col="blue", type="l", lwd=2, ylab="%ILI", xlab="EW", xaxt='n', ylim=c(0,SubDat$prob.pi$intensity[MaxInt] + 0.5), xlim=c(1,mydata$nweeks), main=colName)
    axis(1,at=c(1:(mydata$nweeks)),labels=mydata$weeks)
    lines(c(1,mydata$nweeks),c(mydata$fit$onset[region],mydata$fit$onset[region]), lty=2, col=alpha("gray",0.5), lwd=2)
    # Mean Forecast trajectory
    mean_traj = apply(dump$profile_ili[,,region],2,mean)
    lines((nweeksFit+1):mydata$nweeks, mean_traj[(nweeksFit+1):mydata$nweeks], lwd=2, lty=2, col=alpha("blue",alpha=.5))
    # 1-4 weeks ahead forecast
    rect(rep(nweeksFit+1,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+1,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week1[,colName])), border=NA)
    rect(rep(nweeksFit+2,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+2,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week2[,colName])), border=NA)
    rect(rep(nweeksFit+3,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+3,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week3[,colName])), border=NA)
    rect(rep(nweeksFit+4,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+4,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week4[,colName])), border=NA)
    
    # Onset week forecast
    rect(prob.weeks.onset-weekoff,rep(0.1,mydata$nweeks)-intoff, prob.weeks.onset+weekoff,rep(0.1, mydata$nweeks)+intoff, col=alpha("black",trans1(SubDat$prob.st[,colName])), border=NA)
    
    maxweek = prob.weeks[which.max(SubDat$prob.pw[,colName])]
    maxint = SubDat$prob.pi$intensity[which.max(SubDat$prob.pi[,colName])]
    # Peak week forecast
    rect(prob.weeks-weekoff,rep(maxint,mydata$nweeks)-intoff, prob.weeks+weekoff,rep(maxint,mydata$nweeks)+intoff, col=alpha("forestgreen",trans1(SubDat$prob.pw[,colName])), border=NA)
    # Peak intensity forecast
    rect(rep(maxweek,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(maxweek,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("forestgreen",trans2(SubDat$prob.pi[,colName])), border=NA)
    # replot data over the top of distributions
    lines(mydata$fit$raw[,region], col="blue", lwd=2)
    # legend
    legend("topleft",legend=c("Data","Mean Sol","1-4 Week","Peak","Onset"), col=c("blue",alpha("blue",.5),alpha("red",.6),alpha("forestgreen",.6),alpha("black",.6)), lty=c(1,2,NA,NA,NA), lwd=c(2,2,1,1,1), pch=c(NA,NA,15,15,15))
  }
 
 
   # Now Plot National
   # Plot data
  MaxInt = max(which(SubDat$prob.pi[,"national"] > 0))
  plot(mydata$model$raw, col="blue", type="l", lwd=2, ylab="%ILI", xlab="EW", xaxt='n', ylim=c(0,SubDat$prob.pi$intensity[MaxInt] + 0.5), xlim=c(1,mydata$nweeks), main="National")
  axis(1,at=c(1:(mydata$nweeks)),labels=mydata$weeks)
  lines(c(1,mydata$nweeks),c(mydata$model$onset,mydata$model$onset), lty=2, col=alpha("gray",0.5), lwd=2)
  # Mean Forecast trajectory
  lines((nweeksFit+1):mydata$nweeks, dump$model_mean[(nweeksFit+1):mydata$nweeks], lwd=2, lty=2, col=alpha("blue",alpha=.5))
  # 1-4 weeks ahead forecast
  rect(rep(nweeksFit+1,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+1,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week1[,"national"])), border=NA)
  rect(rep(nweeksFit+2,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+2,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week2[,"national"])), border=NA)
  rect(rep(nweeksFit+3,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+3,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week3[,"national"])), border=NA)
  rect(rep(nweeksFit+4,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(nweeksFit+4,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("red",trans2(SubDat$prob.fc$week4[,"national"])), border=NA)
  
  # Onset week forecast
  rect(prob.weeks.onset-weekoff,rep(0.1,mydata$nweeks)-intoff, prob.weeks.onset+weekoff,rep(0.1, mydata$nweeks)+intoff, col=alpha("black",trans1(SubDat$prob.st[,"national"])), border=NA)
  
  maxweek = prob.weeks[which.max(SubDat$prob.pw[,"national"])]
  maxint = SubDat$prob.pi$intensity[which.max(SubDat$prob.pi[,"national"])]
  # Peak week forecast
  rect(prob.weeks-weekoff,rep(maxint,mydata$nweeks)-intoff, prob.weeks+weekoff,rep(maxint,mydata$nweeks)+intoff, col=alpha("forestgreen",trans1(SubDat$prob.pw[,"national"])), border=NA)
  # Peak intensity forecast
  rect(rep(maxweek,nint)-weekoff, SubDat$prob.pi$intensity-intoff, rep(maxweek,nint)+weekoff, SubDat$prob.pi$intensity+intoff, col=alpha("forestgreen",trans2(SubDat$prob.pi[,"national"])), border=NA)
  # re-plot data on top of distributions
  lines(mydata$model$raw, col="blue", lwd=2)
  # legend
  legend("topleft",legend=c("Data","Mean Sol","4 Week","Peak","Onset"), col=c("blue",alpha("blue",.5),alpha("red",.6),alpha("forestgreen",.6),alpha("black",.6)), lty=c(1,2,NA,NA,NA), lwd=c(2,2,1,1,1), pch=c(NA,NA,15,15,15))

 
  legend_image <- as.raster(matrix(alpha("red",(10:0)/10), ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Bin Score')
  text(x=1.25, y = (0:10)/10, labels = -10:0)
  rasterImage(legend_image, 0, 0, 1,1)
  
  dev.off()
  
}


