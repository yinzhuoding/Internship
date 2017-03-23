###### Flusight Chanllenge ######
##### Calculate the values for the table #####
load("profiles-cdc-2015-2016-3-44-1.RData")
reg.prof = dump$profile_ili # 
nat.prof = dump$natl_profile
onset = dump$onset 
reg.onset = onset[1:10]
nat.onset = onset[11]
week = dump$weeks
nwfit = dump$nweeks.fit
n = nrow(nat.prof) # number of realization
bin.pi = seq(from = 0, to = 13, by = 0.1)
m = length(bin.pi) # number of bins

### Functions definition: 
# Start week - the first week of three consecutive weeks that ILInet reaches/exceeds baseline
StartWeekEst = function(data, onset) { 
  # data is n*52 matrix/dataframe
  # onset is numeric
  st = NULL
  for(i in 1:n) {
    temp = which(data[i,] >= onset)
    for(ck in 1:(length(temp) - 2)) {
      if(temp[ck] + 2 == temp[ck + 2]) {
        st[i] = week[temp[ck]]
        break
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

# Peak week
PeakWeekEst = function(data) {
  pw = NULL
  for(i in 1: n) {
    temp = which.max(data[i,])
    pw[i] = week[temp]
  }
  uni.pw = sort(unique(pw))
  prob.pw = NULL
  for(i in uni.pw) {
    prob.pw = c(prob.pw, sum(pw == i)/n)
  }
  prob.pw = cbind(uni.pw, prob.pw)
  prob.pw = as.data.frame(prob.pw) # prob estimation for peak week
  names(prob.pw) = c("Peak week", "Prob")
  pot.pw = prob.pw[which.max(prob.pw[,2]),1] # point estimation for peak week
  return(list(pot.pw, prob.pw))
}

PeakWeekEstR = function(data) {
  pw = NULL
  for(i in 1:n) {
    temp = which(data[i,] == max(data[i,]))
    pw = rbind(pw,cbind(week[temp],1/length(temp)))
  }
  uni.pw = sort(unique(pw[,1]))
  prob.pw = NULL
  for(i in uni.pw)
}

# Peak intensity
PeakIntyEst = function(data) {
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

# 1-4 week ahead forecast
ForecastEst = function(data,nweekfit) {
  pred = data[,(nweekfit+1): (nweekfit+4)]
  pred.fc = bin.pi
  for(wk in 1:4) {
    prob.fc = NULL
    for(i in 1:(m-1)) {
      prob.fc[i] = sum((pred[, wk] >= bin.pi[i] - 0.05) & (pred[, wk] < bin.pi[i] + 0.05)) / n
    }
    prob.fc[m] = sum(pred[, wk] >= bin.pi[m] - 0.05) / n
    pred.fc = cbind(pred.fc, prob.fc)
  }
  pred.fc = as.data.frame(pred.fc) # prob estimation for national forecast
  names(pred.fc) = c("Bin","1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  pot.fc = apply(pred, 2, mean) # point estimation for national forecast
  names(pot.fc) = c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  return(list(pot.fc,pred.fc))
}

### National Analysis
nat.st = StartWeekEst(nat.prof, nat.onset)
nat.pot.st = nat.st[[1]]
nat.prob.st = nat.st[[2]]

nat.pw = PeakWeekEst(nat.prof)
nat.pot.pw = nat.pw[[1]]
nat.prob.pw = nat.pw[[2]]

nat.pi = PeakIntyEst(nat.prof)
nat.pot.pi = nat.pi[[1]]
nat.prob.pi = nat.pi[[2]]

nat.fc = ForecastEst(nat.prof,nwfit)
nat.pot.fc = nat.fc[[1]]
nat.prob.fc = nat.fc[[2]]

## if data is rounded to one decimal
nat.prof = round(nat.prof,1)
# start week is the same



## Print the estimation
PrintNational = function() {
  cat("US National:\n")
  cat("  Season onset - Point\n")
  cat(nat.pot.st, "\n")
  cat("  Season onset - Bin\n")
  print(nat.prob.st)
  cat("  Season peak week - Point\n")
  cat(nat.pot.pw, "\n")
  cat("  Season peak week - Bin\n")
  print(nat.prob.pw)
  cat("  Season peak intensity - Point\n")
  cat(nat.pot.pi, "\n")
  cat("  Season peak intensity - Bin\n")
  print(nat.prob.pi[which(nat.prob.pi[,2] !=0),])
  cat("  Season forecast - Point\n")
  print(nat.pot.fc)
  cat("  Season forecast - Bin\n")
  row.num = NULL
  for(i in 2:5) {
    row.num = c(row.num, which(nat.prob.fc[,i] != 0))
  }
  row.num = sort(unique(row.num))
  print(nat.prob.fc[row.num,])
}
PrintNational()

### Regional Analysis
prob.st = NULL
prob.pw = NULL
prob.pi = NULL
pot.st = NULL
pot.pw = NULL
pot.pi = NULL
prob.fc = NULL
pot.fc = NULL
for(reg.num in 1:10) {    # 10 HHS region
  data = reg.prof[, , reg.num] # Extract regional data
  # Start week
  reg.st = StartWeekEst(data, reg.onset[reg.num])
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
names(prob.st)[1] = "HHS Region"

pot.pw = as.data.frame(cbind(seq(1,10,1), pot.pw)) # point estimation of regional peak week
names(pot.pw) = c("HHS Region", "Point estimation of peak week")
names(prob.pw)[1] = "HHS Region"

pot.pi = as.data.frame(cbind(seq(1,10,1), pot.pi)) # point estimation of regional peak intensity
names(pot.pi) = c("HHS Region", "Point estimation of peak intensity")
names(prob.pi)[1] = "HHS Region"

rownames(pot.fc) = seq(1,10,1) # point estimation of regional forecast
pot.fc = as.data.frame(pot.fc)
names(pot.fc)[1] = "HHS Region"
names(prob.fc)[1] = "HHS Region"

##### Score the results #####
# Get the cdc data
require(cdcfluview)
regionflu = get_flu_data(region = "HHS", sub_region = 1:10, "ilinet", year = 2015)
nationflu = get_flu_data(region = "national", "ilinet", year = 2015)
season.week = nationflu$WEEK

# Start week
ScoreStartWeek = function(data,onset,pot,prob) {
  # data - a numeric vector from cdc data indicates the weighted ILINet
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
  ae.sw = abs(which(season.week == sw) - which(season.week == pot))
  return(c(sw,score.sw,ae.sw))
}

# Peak week
ScorePeakWeek = function(data,pot,prob) {
  temp = which.max(data)
  pw = season.week[temp]
  # Log score 
  if((pw == 52) || (pw == 1)) {
    pw.range = pw
  }else {
    pw.range = c(pw - 1, pw, pw + 1)
  }
  match = which(prob[,ncol(prob)-1] %in% pw.range)
  if(length(match) == 0) {
    score.pw = log(0)
  }else {
    score.pw = log(sum(prob[match,ncol(prob)]))
  }
  # Absolue error
  ae.pw = abs(which(season.week == pw) - which(season.week == pot))
  return(c(pw, score.pw, ae.pw))
}

# Peak intensity
ScorePeakInty = function(data,pot,prob) {
  pi = max(data,na.rm = TRUE)
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

## 1-4 week ahead forecast
ScoreForecast = function(data,nweekfit,pot,prob) {
  season.fc = seq(week[nweekfit] + 1, week[nweekfit] + 4) 
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

## National
nat.res.sw = ScoreStartWeek(nationflu$X..WEIGHTED.ILI,nat.onset,nat.pot.st,nat.prob.st)
nat.res.pw = ScorePeakWeek(nationflu$X..WEIGHTED.ILI, nat.pot.pw,nat.prob.pw)
nat.res.pi = ScorePeakInty(nationflu$X..WEIGHTED.ILI, nat.pot.pi,nat.prob.pi)
nat.res.fc = ScoreForecast(nationflu$X..WEIGHTED.ILI,nwfit,nat.pot.fc,nat.prob.fc)

PrintScore
## Regional
reg.sw = NULL
reg.pw = NULL
reg.pi = NULL
reg.fc = NULL
for(reg.num in 1:10) {
  reg.data = subset(regionflu, REGION == paste("Region", reg.num, sep = " "))
  # Start week
  sw.res = ScoreStartWeek(reg.data$X..WEIGHTED.ILI, reg.onset[reg.num],
                          pot.st[reg.num,2], subset(prob.st, prob.st[,1] == reg.num))
  reg.sw = rbind(reg.sw, sw.res)
  # Peak week
  pw.res = ScorePeakWeek(reg.data$X..WEIGHTED.ILI, pot.pw[reg.num,2],
                         subset(prob.pw, prob.pw[,1] == reg.num))
  reg.pw = rbind(reg.pw, pw.res)
  # Peak intensity
  pi.res = ScorePeakInty(reg.data$X..WEIGHTED.ILI, pot.pi[reg.num,2],
                         subset(prob.pi, prob.pi[,1] == reg.num))
  reg.pi = rbind(reg.pi, pi.res)
  # 1-4 week ahead forecast
  fc.res = ScoreForecast(reg.data$X..WEIGHTED.ILI,nwfit,pot.fc[reg.num,-1],
                         subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
  fc.res = cbind(rep(reg.num,4), fc.res)
  reg.fc = rbind(reg.fc, fc.res)
}
rownames(reg.sw) = seq(1,10,1)
reg.sw = data.frame(seq(1,10,1),reg.sw)
names(reg.sw) = c("HHS Region", "Actual Value", "Log score", "Absolute value")

rownames(reg.pw) = seq(1,10,1)
reg.pw = data.frame(seq(1,10,1),reg.pw)
names(reg.pw) = c("HHS Region", "Actual Value", "Log score", "Absolute value")

rownames(reg.pi) = seq(1,10,1)
reg.pi = data.frame(seq(1,10,1),reg.pi)
names(reg.pi) = c("HHS Region", "Actual Value", "Log score", "Absolute value")

rownames(reg.fc) = seq(1,nrow(reg.fc),1)
reg.fc = as.data.frame(reg.fc)
names(reg.fc) = c("HHS Region", "Actual Value", "Log score", "Absolute value")

# print the score
a = data.frame(rep("Start week", 10), reg.sw)
names(a)[1] = "Target"
b = data.frame(rep("Peak week", 10), reg.pw)
names(b)[1] = "Target"
c = data.frame(rep("Peak intensity", 10), reg.pi)
names(c)[1] = "Target"
d = data.frame(rep("Forecast", 10), reg.fc)
names(d)[1] = "Target"
score = rbind(a,b,c,d)
rownames(score) = seq(1,nrow(score),1)
score = data.frame(lapply(score, as.numeric), stringsAsFactors=FALSE)
write.csv(score, file = "score.csv")

##### Automatically fill the table #####
# load the csv file
table = read.csv("Long_Flu_Submission_Template_v5.csv") 
# Preparation
bin.st = table[(table$Location == "US National") & (table$Target == "Season onset") & (table$Type == "Bin"), ]
l.st = nrow(bin.st)
bin.pw = table[(table$Location == "US National") & (table$Target == "Season peak week") & (table$Type == "Bin"), ]
l.pw = nrow(bin.pw)

# Function definitions
FillStartWeek = function(pot,prob) {
  fill = rep(0,l.st)
  nc = ncol(prob)
  p.st = prob[,nc-1]
  temp = which(bin.st$Bin_start_incl %in% p.st)
  if(length(temp) == length(p.st)) {
    fill[temp] = prob[,nc]
  }else {
    r = which(bin.st[temp] %in% p.st)
    fill[temp] = prob[r,nc]
    fill[length(fill)] = sum(prob[-r,nc])
  }
  return(c(pot,fill))
}

FillPeakWeek = function(pot,prob) {
  fill = rep(0, l.pw)
  nc = ncol(prob)
  p.pw = prob[,nc-1]
  temp = which(bin.pw$Bin_start_incl %in% p.pw)
  if(length(temp) == length(p.pw)) {
    fill[temp] = prob[,nc]
  }else {
    r = which(bin.st[temp] %in% p.pw)
    fill[temp] = prob[r,nc]
  }
  return(c(pot,fill))
}

FillPeakInty = function(pot,prob) {
  return(c(pot,prob$Prob))
}

FillForecast = function(pot,prob) {
  fill = NULL
  for(i in 1:4) {
    fill = c(fill, pot[i])
    fill = c(fill, prob[, i+1])
  }
  return(fill)
}
# National
fill.st = FillStartWeek(nat.pot.st, nat.prob.st)
fill.pw = FillPeakWeek(nat.pot.pw, nat.prob.pw)
fill.pi = FillPeakInty(nat.pot.pi, nat.prob.pi[,2])
fill.fc = FillForecast(nat.pot.fc, nat.prob.fc[,2])
fill.national = c(fill.st,fill.pw,fill.pi,fill.fc)

# check the length 
v = ncol(table)
nat.num = length(which(table$Location == "US National"))
if(nat.num == length(fill.national)) {
  print("Length check pass!")
}else {
  print("Length check fail: please check!")
}

# Regional
fill.regional = NULL
for(reg.num in 1:10) {
  fill.st.reg = FillStartWeek(pot.st[reg.num,2], subset(prob.st, prob.st[,1] == reg.num))
  fill.pw.reg = FillPeakWeek(pot.pw[reg.num,2], subset(prob.pw, prob.pw[,1] == reg.num))
  fill.pi.reg = FillPeakInty(pot.pi[reg.num,2], subset(prob.pi, prob.pi[,1] == reg.num))
  fill.fc.reg = FillForecast(pot.fc[reg.num,-1], subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
  fill.reg = as.numeric(c(fill.st.reg, fill.pw.reg, fill.pi.reg, fill.fc.reg))
  fill.regional = c(fill.regional, fill.reg)
}

# check the length 
reg.n = nrow(table) - nat.num
if(reg.n == length(fill.regional)) {
  print("Length check pass!")
}else {
  print("Length check fail: please check!")
}

# fill the table
table[,v] = c(fill.national, fill.regional)
