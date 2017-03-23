### Call:
# Fill table
FillTable(dump,table)
# Score the estimation
ScoreEst(dump)
a = read.csv("Nationalscore.csv")
b = read.csv("Regscore.csv")

### Load files:
# load the simulation data
load("profiles-cdc-2015-2016-3-44-1.RData")
load("profiles-cdc-United.States-cpl-2015-2016-5-46-1.RData")
# load the submission table
table = read.csv("Long_Flu_Submission_Template_update.csv")


### Functions definition: 
StartWeekEst = function(data, onset, week) {
  #' Calculate the first week of three consecutive weeks that ILInet reaches/exceeds baseline
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param onset The baseline 
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation
  n = nrow(data)
  st = rep(0,n)
  for(i in 1:n) {
    temp = which(data[i,] >= onset)
    if(length(temp) > 2) {
      for(ck in 1:(length(temp) - 2)) {
        if(temp[ck] + 2 == temp[ck + 2]) {
          st[i] = week[temp[ck]]
          break
        }
      }
    }
  }
  uni.st = sort(unique(st))
  prob.st = NULL
  for(i in uni.st) {
    prob.st = c(prob.st, sum(st == i)/n)
  }
  prob.st = cbind(uni.st, prob.st) 
  prob.st = as.data.frame(prob.st) # prob estimation for start week
  names(prob.st) = c("Start Week", "Prob") 
  pot.st = prob.st[which.max(prob.st[,2]),1] # point estimation for start week
  return(list(pot.st,prob.st))
}

PeakWeekEst = function(data) {
  #' Calculate the peak week
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation
  n = nrow(data)
  pw = NULL
  for(i in 1: n) {
    temp = which(data[i,] == max(data[i,]))
    pw = rbind(pw,cbind(temp,1/length(temp)))
  }
  uni.pw = sort(unique(pw[,1]))
  prob.pw = NULL
  for(j in uni.pw) {
    temp = which(pw[,1] == j)
    prob.pw = c(prob.pw, sum(pw[temp,2])/n)
  }
  prob.pw = cbind(uni.pw, prob.pw)
  prob.pw = as.data.frame(prob.pw) # prob estimation for peak week
  names(prob.pw) = c("Peak week", "Prob")
  pot.pw = prob.pw[which.max(prob.pw[,2]),1] # point estimation for peak week
  return(list(pot.pw, prob.pw))
}

PeakIntyEst = function(data) {
  #' Calculate the peak intensity
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation for the full bin
  n = nrow(data)
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi)
  
  pi = apply(data, 1, max)
  prob.pi = rep(NA,m)
  for(i in 1: (m-1)) {
    prob.pi[i] = sum((pi >= bin.pi[i] - 0.05) & (pi < bin.pi[i] + 0.05)) / n 
  }
  prob.pi[m] = sum(pi >= bin.pi[m] - 0.05) / n
  prob.pi = cbind(bin.pi, prob.pi)
  prob.pi = as.data.frame(prob.pi) # prob estimation for peak intensity
  names(prob.pi) = c("Bin", "Prob")
  pot.pi = mean(pi) # point estimation for peak intensity
  return(list(pot.pi, prob.pi))
}

ForecastEst = function(data,nweekfit) {
  #' Calculate one-to-four-week ahead forecasts
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param nweekfit A numeric value of the number of fitted weeks, so from nweekfit + 1, the data is a prediction
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation for the full bin
  n = nrow(data)
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi)
  
  pred = data[,(nweekfit+1): (nweekfit+4)]
  pred.fc = bin.pi
  for(wk in 1:4) {
    prob.fc = NULL
    for(i in 1:(m-1)) {
      prob.fc[i] = sum((pred[, wk] >= bin.pi[i] - 0.05) & (pred[, wk] < bin.pi[i] + 0.05)) / n
    }
    prob.fc[m] = sum((pred[, wk] >= bin.pi[m] - 0.05) & (pred[, wk] < 100)) / n
    # scale to sum = 1
    if(sum(prob.fc) != 1) {
      prob.fc = prob.fc/sum(prob.fc)
    }
    pred.fc = cbind(pred.fc, prob.fc)
  }
  pred.fc = as.data.frame(pred.fc) # prob estimation for national forecast
  names(pred.fc) = c("Bin","1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  pot.fc = apply(pred, 2, mean) # point estimation for national forecast
  names(pot.fc) = c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  return(list(pot.fc,pred.fc))
}

FillStartWeek = function(pot,prob) {
  #' Fill the table for target - start week
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  
  if(pot == 0) {
    pot = NA
  }
  bin.st = table[(table$Location == "US National") & (table$Target == "Season onset") & (table$Type == "Bin"), ]
  l.st = nrow(bin.st)
  fill = rep(0,l.st)
  nc = ncol(prob)
  p.st = prob[,nc-1]
  temp = NULL
  for(i in p.st) {
    temp = c(temp, which(bin.st$Bin_start_incl == i))
  }
  if(length(temp) == length(p.st)) {
    fill[temp] = prob[,nc]
  }else {
    r = NULL
    for(i in bin.st$Bin_start_incl[temp]) {
      r = c(r, which(p.st == i))
    }
    #r = which(p.st %in% bin.st$Bin_start_incl[temp])
    fill[temp] = prob[r,nc]
    fill[length(fill)] = sum(prob[-r,nc])
  }
  return(c(pot,fill))
}

FillPeakWeek = function(pot,prob) {
  #' Fill the table for target - peak week
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  bin.pw = table[(table$Location == "US National") & (table$Target == "Season peak week") & (table$Type == "Bin"), ]
  l.pw = nrow(bin.pw)
  
  fill = rep(0, l.pw)
  nc = ncol(prob)
  p.pw = prob[,nc-1]
  temp = NULL
  for(i in p.pw) {
    temp = c(temp, which(bin.pw$Bin_start_incl == i))
  }
  if(length(temp) == length(p.pw)) {
    fill[temp] = prob[,nc]
  }else {
    r = NULL
    for(i in bin.pw$Bin_start_incl[temp]) {
      r = c(r, which(p.pw == i))
    }
    fill[temp] = prob[r,nc]
    fill[length(fill)] = sum(prob[-r,nc])
  }
  return(c(pot,fill))
}


FillPeakInty = function(pot,prob) {
  #' Fill the table for target - peak intensity
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  return(c(round(pot,1),prob$Prob))
}

FillForecast = function(pot,prob) {
  #' Fill the table for target - one-to-four-week ahead forecasts
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  fill = NULL
  for(i in 1:4) {
    fill = c(fill, round(pot[i],1))
    fill = c(fill, prob[, i+1])
  }
  return(fill)
}

ScoreStartWeek = function(data,onset,sweek,pot,prob) {
  #' Score the estimation of start week
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param onset The baseline 
  #' @param sweek Seanson week in cdc data
  #' @prarm pot Point Estimation of start week
  #' @param prob Prob Estimation of start week
  #' @return 1*3 vector with actual value, log score and absolute error
  season.week = sweek
  temp = which(data >= onset)
  for(ck in 1:(length(temp) - 2)) {
    if(temp[ck] + 2 == temp[ck + 2]) {
      sw = season.week[temp[ck]]
      break
    }
  }
  # Log score
  if((sw == 52) || (sw == 1)) {
    sw.range = sw
  }else {
    sw.range = c(sw - 1, sw, sw + 1)
  }
  match = which(prob[, ncol(prob)-1] %in% sw.range)
  if(length(match) == 0) {
    score.sw = log(0)
  }else {
    score.sw = log(sum(prob[match,ncol(prob)]))
  }
  # Absolute error
  if(pot != 0) {
    ae.sw = abs(which(season.week == sw) - which(season.week == pot))
  }else{
    ae.sw = NA
  }
  return(c(sw,score.sw,ae.sw))
}

ScorePeakWeek = function(data,sweek,pot,prob) {
  #' Score the estimation of peak week
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param sweek Seanson week in cdc data
  #' @prarm pot Point Estimation of peak week
  #' @param prob Prob Estimation of peak week
  #' @return 1*3 vector with actual value, log score and absolute error
  season.week = sweek
  temp = which(data == max(data))
  pw = season.week[temp]
  # Log score
  pw.range = NULL
  for(wk in pw) {
    if((wk == 1) || (wk == 52)) {
      wk.range = wk
    }else {
      wk.range = c(wk - 1, wk, wk + 1)
    }
    pw.range = union(pw.range, wk.range)
  }
  match = which(prob[,ncol(prob)-1] %in% pw.range)
  if(length(match) == 0) {
    score.pw = log(0)
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

ScorePeakInty = function(data,pot,prob) {
  #' Score the estimation of peak intensity
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @prarm pot Point Estimation of peak intensity
  #' @param prob Prob Estimation of peak intensity
  #' @return *3 vector with actual value, log score and absolute error
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
    score.pi = log(0)
  }else {
    score.pi = log(sum(prob[est.bin.num[match],ncol(prob)]))
  }
  # Absolute error
  ae.pi = abs(pot - pi)
  return(c(pi, score.pi, ae.pi))
}

ScoreForecast = function(data,week,sweek,pot,prob) {
  #' Score the estimation of peak intensity
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param week vector in our simulation
  #' @param season week in cdc data
  #' @prarm pot Point Estimation of peak intensity
  #' @param prob Prob Estimation of peak intensity
  #' @return 4*3 dataframe with actual value, log score and absolute error
  season.week = sweek
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi)
  season.fc = seq(week + 1, week + 4) 
  week.num = which(season.week %in% season.fc)
  forecast = data[week.num]
  score.fc = NULL
  ae.fc = NULL
  # Log score/Absolute error 
  for(i in 1:4) {
    temp = which(forecast[i] >= bin.pi - 0.05)
    bin.num = temp[length(temp)]
    lb = max(1, bin.num - 5)
    ub = min(m, bin.num + 5)
    fc.range = seq(lb, ub, 1)
    est.bin.num = which(prob[,i+1] != 0)
    match = which(est.bin.num %in% fc.range)
    if(length(match) == 0) {
      score.fc[i] = log(0)
    }else {
      score.fc[i] = log(sum(prob[est.bin.num[match], i+1]))
    }
    ae.fc[i] = abs(pot[i] - forecast[i])
  }
  return(cbind(forecast,score.fc,ae.fc))
}

GetEst = function(dump) {
  #' Calculate all the estimation
  #' @param dump Simulation data
  #' @return A list saving all the national/regional point/prob estimation
  # Extract information
  reg.prof = dump$profile_ili
  week = dump$weeks
  if("onset" %in% names(dump)) {
    nat.prof = dump$natl_profile
    onset = dump$onset 
    reg.onset = onset[1:10]
    nat.onset = onset[11]
    nwfit = dump$nweeks.fit
  }else {
    nat.prof = dump$model_profile_ili
    reg.onset = as.numeric(dump$fit_onset)
    nat.onset = dump$model_onset
    nwfit = dump$nweeksFit
  }

  n = nrow(nat.prof) # number of simulation
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi) # number of bins
  
  # National
  result = list() # saving all the estimation
  nat.st = StartWeekEst(nat.prof, nat.onset,week)
  result$nat.pot.st = nat.st[[1]]
  #nat.st[[2]][,1] = week[nat.st[[2]][,1]]
  result$nat.prob.st = nat.st[[2]]
  
  nat.pw = PeakWeekEst(nat.prof)
  result$nat.pot.pw = week[nat.pw[[1]]]
  nat.pw[[2]][,1] = week[nat.pw[[2]][,1]]
  result$nat.prob.pw = nat.pw[[2]]
  
  nat.pi = PeakIntyEst(nat.prof)
  result$nat.pot.pi = nat.pi[[1]]
  result$nat.prob.pi = nat.pi[[2]]
  
  nat.fc = ForecastEst(nat.prof,nwfit)
  result$nat.pot.fc = nat.fc[[1]]
  result$nat.prob.fc = nat.fc[[2]]
  
  # HHS Regional
  prob.st = NULL
  prob.pw = NULL
  prob.pi = NULL
  pot.st = NULL
  pot.pw = NULL
  pot.pi = NULL
  prob.fc = NULL
  pot.fc = NULL
  for(reg.num in 1:10) {    # 10 HHS region
    data = reg.prof[, , reg.num]
    # Extract regional data
    # Start week
    reg.st = StartWeekEst(data, reg.onset[reg.num], week)
    reg.pot.st = reg.st[[1]]
    reg.prob.st = reg.st[[2]]
    pot.st = c(pot.st,reg.pot.st)
    reg.prob.st = data.frame(rep(reg.num, nrow(reg.prob.st)), reg.prob.st)
    prob.st = rbind(prob.st, reg.prob.st)
    
    # Peak week
    reg.pw = PeakWeekEst(data)
    reg.pot.pw = reg.pw[[1]]
    reg.prob.pw = reg.pw[[2]]
    pot.pw = c(pot.pw, reg.pot.pw)
    reg.prob.pw = data.frame(rep(reg.num, nrow(reg.prob.pw)), reg.prob.pw)
    prob.pw = rbind(prob.pw, reg.prob.pw)
    
    # Peak intensity
    reg.pi = PeakIntyEst(data)
    reg.pot.pi = reg.pi[[1]]
    reg.prob.pi = reg.pi[[2]]
    pot.pi = c(pot.pi, reg.pot.pi)
    reg.prob.pi = cbind(rep(reg.num, m), reg.prob.pi)
    prob.pi = rbind(prob.pi, reg.prob.pi)
    
    # 1-4 week ahead forecast
    reg.fc = ForecastEst(data,nwfit)
    reg.pot.fc = reg.fc[[1]]
    reg.prob.fc = reg.fc[[2]]
    reg.pot.fc = c(reg.num, reg.pot.fc)
    pot.fc = rbind(pot.fc, reg.pot.fc)
    reg.prob.fc = cbind(rep(reg.num, m), reg.prob.fc)
    prob.fc = rbind(prob.fc, reg.prob.fc)
  }
  
  pot.st = as.data.frame(cbind(seq(1,10,1), pot.st)) # point estimation of regional start week
  names(pot.st) = c("HHS Region", "Point estimation of start week")
  prob.st[,2] = prob.st[,2]
  names(prob.st)[1] = "HHS Region"
  result$reg.pot.st = pot.st
  result$reg.prob.st = prob.st
  
  pot.pw = as.data.frame(cbind(seq(1,10,1), week[pot.pw])) # point estimation of regional peak week
  names(pot.pw) = c("HHS Region", "Point estimation of peak week")
  prob.pw[,2] = week[prob.pw[,2]]
  names(prob.pw)[1] = "HHS Region"
  result$reg.pot.pw = pot.pw
  result$reg.prob.pw = prob.pw
  
  pot.pi = as.data.frame(cbind(seq(1,10,1), pot.pi)) # point estimation of regional peak intensity
  names(pot.pi) = c("HHS Region", "Point estimation of peak intensity")
  names(prob.pi)[1] = "HHS Region"
  result$reg.pot.pi = pot.pi
  result$reg.prob.pi = prob.pi
  
  rownames(pot.fc) = seq(1,10,1) # point estimation of regional forecast
  pot.fc = as.data.frame(pot.fc)
  names(pot.fc)[1] = "HHS Region"
  names(prob.fc)[1] = "HHS Region"
  result$reg.pot.fc = pot.fc
  result$reg.prob.fc = prob.fc
  return(result)
}
  
FillTable = function(dump,table) {
  #' Fill the submission form
  #' @param dump Simulation data
  #' @param table Submission table
  #' @return Return nothing, just create a .csv file for submission
  est = GetEst(dump)
  # National
  fill.st = FillStartWeek(est$nat.pot.st, est$nat.prob.st)
  fill.pw = FillPeakWeek(est$nat.pot.pw, est$nat.prob.pw)
  fill.pi = FillPeakInty(est$nat.pot.pi, est$nat.prob.pi)
  fill.fc = FillForecast(est$nat.pot.fc, est$nat.prob.fc)
  fill.national = c(fill.st,fill.pw,fill.pi,fill.fc)
  
  # Regional
  fill.regional = NULL
  pot.st = est$reg.pot.st
  prob.st = est$reg.prob.st
  pot.pw = est$reg.pot.pw
  prob.pw = est$reg.prob.pw
  pot.pi = est$reg.pot.pi
  prob.pi = est$reg.prob.pi
  pot.fc = est$reg.pot.fc
  prob.fc = est$reg.prob.fc
  for(reg.num in 1:10) {
    fill.st.reg = FillStartWeek(pot.st[reg.num,2], subset(prob.st, prob.st[,1] == reg.num))
    fill.pw.reg = FillPeakWeek(pot.pw[reg.num,2], subset(prob.pw, prob.pw[,1] == reg.num))
    fill.pi.reg = FillPeakInty(pot.pi[reg.num,2], subset(prob.pi, prob.pi[,1] == reg.num))
    fill.fc.reg = FillForecast(pot.fc[reg.num,-1], subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
    fill.reg = as.numeric(c(fill.st.reg, fill.pw.reg, fill.pi.reg, fill.fc.reg))
    fill.regional = c(fill.regional, fill.reg)
  }
  
  v = ncol(table)
  table[,v] = c(fill.national, fill.regional)
  write.csv(table, file = "Submission.csv", row.names = FALSE)
  cat("See Regional score at: 'Submission.csv'\n")
}


ScoreEst = function(dump) {
  #' Give the log score and absolute error of our estimation compared with cdc data
  #' @param dump Simulation data
  #' @return return nothing, create two .csv files saving the score for national and regional seperately.
  est = GetEst(dump)
  week = dump$weeks
  if("onset" %in% names(dump)) {
    onset = dump$onset
    nat.onset = onset[11]
    reg.onset = onset[1:10]
    nwfit = dump$nweeks.fit
  }else{
    reg.onset = as.numeric(dump$fit_onset)
    nat.onset = dump$model_onset
    nwfit = dump$nweeksFit
  }

  # Get the cdc data
  require(cdcfluview)
  regionflu = get_flu_data(region = "HHS", sub_region = 1:10, "ilinet", year = 2015)
  nationflu = get_flu_data(region = "national", "ilinet", year = 2015)
  season.week = nationflu$WEEK
  
  # National Score
  # if rounded
  #nat.data = round(nationflu$X..WEIGHTED.ILI,1)
  # if no rounded
  nat.data = nationflu$X..WEIGHTED.ILI
  nat.res.sw = ScoreStartWeek(nat.data, nat.onset, season.week, est$nat.pot.st,est$nat.prob.st)
  nat.res.pw = ScorePeakWeek(nat.data, season.week, est$nat.pot.pw, est$nat.prob.pw)
  nat.res.pi = ScorePeakInty(nat.data, est$nat.pot.pi,est$nat.prob.pi)
  nat.res.fc = ScoreForecast(nat.data, week[nwfit], season.week, est$nat.pot.fc,est$nat.prob.fc)
  # Create a csv file for the score of national
  nat.est = c(est$nat.pot.st,est$nat.pot.pw,est$nat.pot.pi,est$nat.pot.fc)
  nat.score = rbind(nat.res.sw, nat.res.pw, nat.res.pi, nat.res.fc)
  nat.score = cbind(nat.est,nat.score)
  target = c("Start week", "Peak week", "Peak intensity", rep("Forecast",4))
  nat.score = cbind(rep("US National",7), target, nat.score)
  nat.score = as.data.frame(nat.score)
  names(nat.score) = c("Location","Target","Point Est","Actual Value", "Log Score", "Absolute error")
  rownames(nat.score) = NULL
  write.csv(nat.score, file = "Nationalscore.csv",row.names = FALSE)
  cat("See National score at: 'Nationalscore.csv'\n")
  
  ## Regional
  pot.st = est$reg.pot.st
  prob.st = est$reg.prob.st
  pot.pw = est$reg.pot.pw
  prob.pw = est$reg.prob.pw
  pot.pi = est$reg.pot.pi
  prob.pi = est$reg.prob.pi
  pot.fc = est$reg.pot.fc
  prob.fc = est$reg.prob.fc
  reg.sw = NULL
  reg.pw = NULL
  reg.pi = NULL
  reg.fc = NULL
  for(reg.num in 1:10) {
    reg.data = subset(regionflu, REGION == paste("Region", reg.num, sep = " "))
    cdc.data = reg.data$X..WEIGHTED.ILI
    # Start week
    sw.res = ScoreStartWeek(cdc.data, reg.onset[reg.num], season.week,
                            pot.st[reg.num,2], subset(prob.st, prob.st[,1] == reg.num))
    reg.sw = rbind(reg.sw, sw.res)
    # Peak week
    pw.res = ScorePeakWeek(cdc.data, season.week, pot.pw[reg.num,2],
                           subset(prob.pw, prob.pw[,1] == reg.num))
    reg.pw = rbind(reg.pw, pw.res)
    # Peak intensity
    pi.res = ScorePeakInty(cdc.data, pot.pi[reg.num,2],
                           subset(prob.pi, prob.pi[,1] == reg.num))
    reg.pi = rbind(reg.pi, pi.res)
    # 1-4 week ahead forecast
    fc.res = ScoreForecast(cdc.data,week[nwfit], season.week, pot.fc[reg.num,-1],
                           subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
    fc.res = cbind(rep(reg.num,4), fc.res)
    reg.fc = rbind(reg.fc, fc.res)
  }
  rownames(reg.sw) = seq(1,10,1)
  reg.sw = data.frame(seq(1,10,1),reg.sw)
  names(reg.sw) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  rownames(reg.pw) = seq(1,10,1)
  reg.pw = data.frame(seq(1,10,1),reg.pw)
  names(reg.pw) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  rownames(reg.pi) = seq(1,10,1)
  reg.pi = data.frame(seq(1,10,1),reg.pi)
  names(reg.pi) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  
  rownames(reg.fc) = seq(1,nrow(reg.fc),1)
  reg.fc = as.data.frame(reg.fc)
  names(reg.fc) = c("HHS Region", "Actual Value", "Log score", "Absolute error")
  # Create a csv file for the score of regional
  a = data.frame(rep("Start week", 10), pot.st[,2], reg.sw[,-1])
  names(a)[1:2] = c("Target","Point Est")
  hhs = seq(1,10,1)
  a = cbind(hhs,a)
  b = data.frame(rep("Peak week", 10), pot.pw[,2], reg.pw[,-1])
  names(b)[1:2] = c("Target","Point Est")
  b = cbind(hhs,b)
  c = data.frame(rep("Peak intensity", 10), pot.pi[,2], reg.pi[,-1])
  names(c)[1:2] = c("Target", "Point Est")
  c = cbind(hhs,c)
  fc = NULL
  for(i in 1:10) {
    for(j in 2:5) {
      fc = c(fc, pot.fc[i,j])
    }
  }
  d = data.frame(rep("Forecast", 40), fc, reg.fc[-1])
  names(d)[1:2] = c("Target","Point Est")
  hhs = NULL
  for(i in 1:10){
    hhs = c(hhs,rep(i,4))
  }
  d = cbind(hhs,d)
  score = rbind(a,b,c,d)
  names(score)[1] = "Location"
  score = as.matrix(score)
  write.csv(score, file = "Regscore.csv",row.names = FALSE)
  cat("See Regional score at: 'Regscore.csv'\n")
}



