### Call:
# Fill table
#FillTable(dump,table)
# Score the estimation
#ScoreEst(dump)

### Load files:
# load the simulation data
##load("profiles-cdc-2015-2016-3-44-1.RData")
#load("profiles-cdc-United.States-cpl-2015-2016-5-46-1.RData")
#load("CDC-Flu-Challenge-2016-2017-week-16-AggregateProfile.RData")
# load the submission table
#table = read.csv("Long_Flu_Submission_Template_update.csv")


### Functions definition: 

FindOnset <- function(data=NULL,onset=NULL,CDCweeks=NULL) {
  sw = NA
  temp = which(data >= onset)
  if(length(temp)>2) {
    for(ck in 1:(length(temp) - 2)) {
      if(temp[ck] + 2 == temp[ck + 2]) {
        sw = CDCweeks[temp[ck]]
        break
      }
    }
  }
  return(sw)
}


RemoveSmallProb <- function(est) {
  # Set any probabilities <exp(-10) to zero. Then renormalize the distribution.
  # National Onset Week
  est$nat.prob.st$Prob[est$nat.prob.st$Prob<exp(-10)] = 0
  est$nat.prob.st$Prob = est$nat.prob.st$Prob/sum(est$nat.prob.st$Prob)
  # National Peak Week
  est$nat.prob.pw$Prob[est$nat.prob.pw$Prob<exp(-10)] = 0
  est$nat.prob.pw$Prob = est$nat.prob.pw$Prob/sum(est$nat.prob.pw$Prob)
  # National Peak Intensity
  est$nat.prob.pi$Prob[est$nat.prob.pi$Prob<exp(-10)] = 0
  est$nat.prob.pi$Prob = est$nat.prob.pi$Prob/sum(est$nat.prob.pi$Prob)
  # National Forecase Weeks
  est$nat.prob.fc[,2:5][est$nat.prob.fc[,2:5]<exp(-10)] = 0 
  for (ii in 1:4) {
    est$nat.prob.fc[,ii+1] = est$nat.prob.fc[,ii+1]/sum(est$nat.prob.fc[,ii+1])
  }
  # Regions
  for(region in 1:10) {
    # Onset Week
    temp.prob = est$reg.prob.st$Prob[est$reg.prob.st$`HHS Region`==region]
    temp.prob[temp.prob < exp(-10)] = 0
    temp.prob = temp.prob/sum(temp.prob)
    est$reg.prob.st$Prob[est$reg.prob.st$`HHS Region`==region] = temp.prob
    # Peak Week
    temp.prob = est$reg.prob.pw$Prob[est$reg.prob.pw$`HHS Region`==region]
    temp.prob[temp.prob < exp(-10)] = 0
    temp.prob = temp.prob/sum(temp.prob)
    est$reg.prob.pw$Prob[est$reg.prob.pw$`HHS Region`==region] = temp.prob
    # Peak Intensity
    temp.prob = est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region]
    temp.prob[temp.prob < exp(-10)] = 0
    temp.prob = temp.prob/sum(temp.prob)
    est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region] = temp.prob
    # Forecast Weeks
    RegInd = est$reg.prob.fc$`HHS Region`==region
    for (ii in 1:4) {
      est$reg.prob.fc[RegInd,ii+2][est$reg.prob.fc[RegInd,ii+2]<exp(-10)] = 0
      est$reg.prob.fc[RegInd,ii+2] = est$reg.prob.fc[RegInd,ii+2]/sum(est$reg.prob.fc[RegInd,ii+2])
    }
    return(est)
  }
}


MixOnsetWeek <- function(est_prob=NULL,baseprob=NULL,CDC_weeks=NULL,base_factor=0) {
  
  NewWeeks = unique(c(est_prob$'Start.Week', na.omit(CDC_weeks[baseprob>0])))
  new_est = data.frame('Start.Week'=NewWeeks, Prob=0)
  for (week in est_prob$Start.Week) {
    new_est[new_est$Start.Week==week,"Prob"] = est_prob[est_prob$Start.Week==week,"Prob"]
  }
  for (week in NewWeeks) {
    if (week==0) {
      new_est[new_est$Start.Week==week,"Prob"] = (1-base_factor)*new_est[new_est$Start.Week==week,"Prob"]
    } else {
      new_est[new_est$Start.Week==week,"Prob"] = (1-base_factor)*new_est[new_est$Start.Week==week,"Prob"] + base_factor*baseprob[CDC_weeks==week]
    }
  }
  new_est = new_est[new_est$Start.Week<=20 | new_est$Start.Week>=40,]
  return(new_est)
}


MixPeakWeek <- function(est_prob=NULL,baseprob=NULL,CDC_weeks=NULL,base_factor=0) {
  NewWeeks = unique(c(est_prob$'Peak.Week', na.omit(CDC_weeks[baseprob>0])))
  new_est = data.frame('Peak.Week'=NewWeeks, Prob=0)
  for (week in est_prob$`Peak.Week`) {
    new_est[new_est$Peak.Week==week,"Prob"] = est_prob[est_prob$`Peak.Week`==week,"Prob"]
  }
  for (week in NewWeeks) {
    new_est[new_est$Peak.Week==week,"Prob"] = (1-base_factor)*new_est[new_est$Peak.Week==week,"Prob"] + base_factor*baseprob[CDC_weeks==week]
  }
  new_est = new_est[new_est$Peak.Week<=20 | new_est$Peak.Week>26,]
  return(new_est)
}


MixInt <- function(est_prob=NULL,baseprob=NULL, base_factor=NULL) {
  # adjust length of baseprob to match est_prob
  baseprob = c(baseprob,0)
  
  est_new = (1-base_factor)*est_prob + base_factor*baseprob
  return(est_new)
}


MixEstBase <- function(est=NULL,base=NULL,select=NULL,CDC_weeks=NULL, nat_baseline=NULL) {
  ## national
  names(est$nat.prob.st)[1] = "Start.Week"
  names(est$nat.prob.pw)[1] = "Peak.Week"
  est$nat.prob.st = MixOnsetWeek(est_prob=est$nat.prob.st,baseprob=base$OnsetGaus[11,], CDC_weeks=CDC_weeks, base_factor=nat_baseline)
  est$nat.prob.pw = MixPeakWeek(est_prob=est$nat.prob.pw,baseprob=base$PeakWeekGaus[11,], CDC_weeks=CDC_weeks, base_factor=nat_baseline)
  
  est$nat.prob.pi$Prob = MixInt(est_prob=est$nat.prob.pi$Prob,baseprob=base$PeakIntGaus[11,], base_factor=nat_baseline)
  est$nat.prob.fc$`1 wk ahead` = MixInt(est_prob=est$nat.prob.fc$`1 wk ahead`,baseprob=base$Week1ln[11,], base_factor=nat_baseline)
  est$nat.prob.fc$`2 wk ahead` = MixInt(est_prob=est$nat.prob.fc$`2 wk ahead`,baseprob=base$Week2ln[11,], base_factor=nat_baseline)
  est$nat.prob.fc$`3 wk ahead` = MixInt(est_prob=est$nat.prob.fc$`3 wk ahead`,baseprob=base$Week3ln[11,], base_factor=nat_baseline)
  est$nat.prob.fc$`4 wk ahead` = MixInt(est_prob=est$nat.prob.fc$`4 wk ahead`,baseprob=base$Week4ln[11,], base_factor=nat_baseline)
  
  #names(est$reg.prob.st)[1] = "Start.Week"
  names(est$reg.prob.pw)[2] = "Peak.Week"
  for (region in 1:10) {
    # --- Onset Week
    temp.prob.st = MixOnsetWeek(est_prob=est$reg.prob.st[est$reg.prob.st$`HHS Region`==region,2:3],baseprob=base$OnsetGaus[region,], CDC_weeks=CDC_weeks, base_factor=select$baseline[region])
    temp.prob.st$`HHS Region` = region
    temp.prob.st = temp.prob.st[,c("HHS Region","Start.Week","Prob")]
    # merge new prob.st into est
    if (region==1) {
      est$reg.prob.st = rbind(temp.prob.st,est$reg.prob.st[est$reg.prob.st$`HHS Region`>region,])
    } else if (region==10) {
      est$reg.prob.st = rbind(est$reg.prob.st[est$reg.prob.st$`HHS Region`<region,],temp.prob.st)
    } else {
      est$reg.prob.st = rbind(est$reg.prob.st[est$reg.prob.st$`HHS Region`<region,],temp.prob.st,est$reg.prob.st[est$reg.prob.st$`HHS Region`>region,])
    }
    
    # --- Peak Week
    temp.prob.pw = MixPeakWeek(est_prob=est$reg.prob.pw[est$reg.prob.pw$`HHS Region`==region,2:3],baseprob=base$PeakWeekGaus[region,], CDC_weeks=CDC_weeks, base_factor=select$baseline[region])
    temp.prob.pw$`HHS Region` = region
    temp.prob.pw = temp.prob.pw[,c("HHS Region","Peak.Week","Prob")]
    # names(temp.prob.pw) = c("HHS Region","Peak.week","Prob")
    # merge new prob.pw into est
    if (region==1) {
      est$reg.prob.pw = rbind(temp.prob.pw,est$reg.prob.pw[est$reg.prob.pw$`HHS Region`>region,])
    } else if (region==10) {
      est$reg.prob.pw = rbind(est$reg.prob.pw[est$reg.prob.pw$`HHS Region`<region,],temp.prob.pw)
    } else {
      est$reg.prob.pw = rbind(est$reg.prob.pw[est$reg.prob.pw$`HHS Region`<region,],temp.prob.pw,est$reg.prob.pw[est$reg.prob.pw$`HHS Region`>region,])
    }
    
    # --- Peak Intensity
    est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region] = MixInt(est_prob=est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region],baseprob=base$PeakIntGaus[region,], base_factor=select$baseline[region])
    # --- Forecast Weeks
    est$reg.prob.fc$`1 wk ahead`[est$reg.prob.fc$`HHS Region`==region] = MixInt(est_prob=est$reg.prob.fc$`1 wk ahead`[est$reg.prob.fc$`HHS Region`==region],baseprob=base$Week1ln[region,], base_factor=select$baseline[region])
    est$reg.prob.fc$`2 wk ahead`[est$reg.prob.fc$`HHS Region`==region] = MixInt(est_prob=est$reg.prob.fc$`2 wk ahead`[est$reg.prob.fc$`HHS Region`==region],baseprob=base$Week2ln[region,], base_factor=select$baseline[region])
    est$reg.prob.fc$`3 wk ahead`[est$reg.prob.fc$`HHS Region`==region] = MixInt(est_prob=est$reg.prob.fc$`3 wk ahead`[est$reg.prob.fc$`HHS Region`==region],baseprob=base$Week3ln[region,], base_factor=select$baseline[region])
    est$reg.prob.fc$`4 wk ahead`[est$reg.prob.fc$`HHS Region`==region] = MixInt(est_prob=est$reg.prob.fc$`4 wk ahead`[est$reg.prob.fc$`HHS Region`==region],baseprob=base$Week4ln[region,], base_factor=select$baseline[region])
  }
  
  return(est)
}


GetHistoric <- function(curYear=2016,nwindow=3,nweeksFit=NULL) {
  #' Calculate historically-correct distributions for peak-week, peak-intensity, onset-week, and 1,2,3,4-week linear forecasts.  Taylored for CDC competition (assumes mod_level="United.States" and fit_level=HHS Regions).
  #' @param curYear The current year
  #' @param nwindom The number of weeks to use for the linear-fit.  Used to make the 1,2,3,4-week forecasts.
  #' @param nweeksFit Number of weeks into the season (counted from week 27) that we are forecasting forward from  (week=27 corresponds to nweeksFit=1, week=40 to nweeksFit=14, etc).  First forecast week will be nweeksFit+1.  Usually nweeksFit is the most up-to-date week of data. However, it also works on previous years for the purpose of testing. 
  #' @return A list that includes the recorded historical data as well as probability distributions for each CDC category.
  require(DICE)
  require(MASS)
  
  # Create a vector of historic years (remove 2009)
  years = c(2004:(curYear-1))
  years = years[!(years==2009)]
  
  # Initialize data-recording matrices
  PeakWeek = matrix(0,nrow=length(years),ncol=11)
  rownames(PeakWeek) = years
  colnames(PeakWeek) = c(paste0("R",1:10),"Nat")
  PeakInt  = PeakWeek
  OnsetWk  = PeakWeek
  Week1For = PeakWeek
  Week2For = PeakWeek
  Week3For = PeakWeek
  Week4For = PeakWeek
  
  # For each year record peak-week, peak-intensity, onset-week, and deviation from linear model for 1,2,3,4 weeks ahead
  for (ii in 1:length(years)) {
    mydata = get.subset(name=c(NAME_2="USA"),start.year=years[ii],end.year=years[ii]+1)
    
    TempWeek = which.max(mydata$model$raw)
    PeakInt[ii,11] = mydata$model$raw[TempWeek]
    PeakWeek[ii,11] = TempWeek
    
    fit = lm(y~x,list(x=1:nwindow,y=mydata$model$raw[(nweeksFit-nwindow+1):nweeksFit]))
    LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
    LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
    LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
    LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]
    
    Week1For[ii,11] = mydata$model$raw[nweeksFit+1] - LinPred1
    Week2For[ii,11] = mydata$model$raw[nweeksFit+2] - LinPred2
    Week3For[ii,11] = mydata$model$raw[nweeksFit+3] - LinPred3
    Week4For[ii,11] = mydata$model$raw[nweeksFit+4] - LinPred4
    
    for (week in 1:(mydata$nweeksData-2)) {
      if (all(mydata$model$raw[week:(week+2)] > as.numeric(mydata$model$onset))) {
        # Record onset week
        OnsetWk[ii,11] = week
        break
      }
    }
    # if no onset week is found, set to NA
    if (OnsetWk[ii,11]==0) OnsetWk[ii,11] = NA
    
    for (region in 1:10) {
      TempWeek = which.max(mydata$fit$raw[,region])
      PeakInt[ii,region]  = mydata$fit$raw[TempWeek,region]
      PeakWeek[ii,region] = TempWeek
      # Regions only have onset weeks for 2007+
      if (years[ii] >= 2007) {
        for (week in 1:(mydata$nweeksData-2)) {
          if (all(mydata$fit$raw[week:(week+2),region] > as.numeric(mydata$fit$onset[region]))) {
            # Record onset week
            OnsetWk[ii,region] = week
            break
          }
        }
      }
      # if no onset week is found, set to NA
      if (OnsetWk[ii,region]==0) OnsetWk[ii,region] = NA
      
      fit = lm(y~x,list(x=1:nwindow,y=mydata$fit$raw[(nweeksFit-nwindow+1):nweeksFit,region]))
      LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
      LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
      LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
      LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]
      
      Week1For[ii,region] = mydata$fit$raw[nweeksFit+1,region] - LinPred1
      Week2For[ii,region] = mydata$fit$raw[nweeksFit+2,region] - LinPred2
      Week3For[ii,region] = mydata$fit$raw[nweeksFit+3,region] - LinPred3
      Week4For[ii,region] = mydata$fit$raw[nweeksFit+4,region] - LinPred4
    }
  }
  
  # In 2008 Region 2 and 9 had late H1N1-pandemic peaks
  PeakWeek[years==2008,2] = NA
  PeakInt[years==2008,2] = NA
  PeakWeek[years==2008,9] = NA
  PeakInt[years==2008,9] = NA
  
  # These peaks are just weird
  PeakWeek[years==2006,8] = NA
  PeakInt[years==2006,8] = NA
  PeakWeek[years==2011,3] = NA
  PeakInt[years==2011,3] = NA
  
  ##
  ## Calculate distributions
  mydata = get.subset(name=c(NAME_2="USA"),start.year=curYear,end.year=curYear+1)
  # weeks = mydata$weeks
  weeks = 1:mydata$nweeks
  intensity = seq(from=0.05,to=12.95,by=.1)
  
  PeakWeekGaus = matrix(0,nrow=11,ncol=mydata$nweeks)
  PeakIntGaus = matrix(0,nrow=11,ncol=length(intensity))
  OnsetGaus = PeakWeekGaus
  Week1ln = PeakIntGaus; Week2ln = PeakIntGaus; Week3ln = PeakIntGaus; Week4ln = PeakIntGaus
  Week1Gaus = PeakIntGaus; Week2Gaus = PeakIntGaus; Week3Gaus = PeakIntGaus; Week4Gaus = PeakIntGaus
  
  
  PeakWeekMean = apply(PeakWeek,MARGIN=2,mean,na.rm=TRUE)
  PeakIntMean = apply(PeakInt,MARGIN=2,mean,na.rm=TRUE)
  OnsetMean = apply(OnsetWk,MARGIN=2,mean,na.rm=TRUE)
  
  PeakWeekVar = apply(PeakWeek,MARGIN=2,var,na.rm=TRUE)
  PeakIntVar = apply(PeakInt,MARGIN=2,var,na.rm=TRUE)
  OnsetVar = apply(OnsetWk,MARGIN=2,var,na.rm=TRUE)
  
  Week1Var = apply(Week1For,MARGIN=2,var,na.rm=TRUE)
  Week2Var = apply(Week2For,MARGIN=2,var,na.rm=TRUE)
  Week3Var = apply(Week3For,MARGIN=2,var,na.rm=TRUE)
  Week4Var = apply(Week4For,MARGIN=2,var,na.rm=TRUE)
  Week1Mean = apply(Week1For,MARGIN=2,mean,na.rm=TRUE)
  Week2Mean = apply(Week2For,MARGIN=2,mean,na.rm=TRUE)
  Week3Mean = apply(Week3For,MARGIN=2,mean,na.rm=TRUE)
  Week4Mean = apply(Week4For,MARGIN=2,mean,na.rm=TRUE)
  
  ## Apply linear-deviation to linear prediction then fit log-normal distribution
  log_Int = log(intensity)
  for (region in 1:11) {
    if (region==11) {
      fit = lm(y~x,list(x=1:nwindow,y=mydata$model$raw[(nweeksFit-nwindow+1):nweeksFit]))
      onset = mydata$model$onset
    } else {
      fit = lm(y~x,list(x=1:nwindow,y=mydata$fit$raw[(nweeksFit-nwindow+1):nweeksFit,region]))
      onset = mydata$fit$onset[[region]]
    }
    LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
    LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
    LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
    LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]

    Dat1toFit = Week1For[,region] + LinPred1
    Dat2toFit = Week2For[,region] + LinPred2
    Dat3toFit = Week3For[,region] + LinPred3
    Dat4toFit = Week4For[,region] + LinPred4
    
    # Dat1toFit = Dat1toFit[Dat1toFit>0]
    # Dat2toFit = Dat2toFit[Dat2toFit>0]
    # Dat3toFit = Dat3toFit[Dat3toFit>0]
    # Dat4toFit = Dat4toFit[Dat4toFit>0]
    Dat1toFit[Dat1toFit<=onset/10] = onset/10
    Dat2toFit[Dat2toFit<=onset/10] = onset/10
    Dat3toFit[Dat3toFit<=onset/10] = onset/10
    Dat4toFit[Dat4toFit<=onset/10] = onset/10
    
    Week1fit = fitdistr(Dat1toFit,densfun="log-normal")
    Week2fit = fitdistr(Dat2toFit,densfun="log-normal")
    Week3fit = fitdistr(Dat3toFit,densfun="log-normal")
    Week4fit = fitdistr(Dat4toFit,densfun="log-normal")
    
    Week1ln[region,] = .1*exp(-(log_Int-Week1fit$estimate[1])^2/(2*Week1fit$estimate[2]^2))/(intensity*Week1fit$estimate[2]*sqrt(2*pi))
    Week2ln[region,] = .1*exp(-(log_Int-Week2fit$estimate[1])^2/(2*Week2fit$estimate[2]^2))/(intensity*Week2fit$estimate[2]*sqrt(2*pi))
    Week3ln[region,] = .1*exp(-(log_Int-Week3fit$estimate[1])^2/(2*Week3fit$estimate[2]^2))/(intensity*Week3fit$estimate[2]*sqrt(2*pi))
    Week4ln[region,] = .1*exp(-(log_Int-Week4fit$estimate[1])^2/(2*Week4fit$estimate[2]^2))/(intensity*Week4fit$estimate[2]*sqrt(2*pi))
  }
  
  
  
  for (ii in 1:11) {
    if (ii==11) {
      fit = lm(y~x,list(x=1:nwindow,y=mydata$model$raw[(nweeksFit-nwindow+1):nweeksFit]))
    } else {
      fit = lm(y~x,list(x=1:nwindow,y=mydata$fit$raw[(nweeksFit-nwindow+1):nweeksFit,ii]))
    }
    LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
    LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
    LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
    LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]
    
    PeakWeekGaus[ii,] = exp(-(weeks-PeakWeekMean[ii])^2/(2*PeakWeekVar[ii]))/sqrt(2*PeakWeekVar[ii]*pi)
    PeakIntGaus[ii,] = .1*exp(-(intensity-PeakIntMean[ii])^2/(2*PeakIntVar[ii]))/sqrt(2*PeakIntVar[ii]*pi)
    OnsetGaus[ii,] = exp(-(weeks-OnsetMean[ii])^2/(2*OnsetVar[ii]))/sqrt(2*OnsetVar[ii]*pi)
    Week1Gaus[ii,] = .1*exp(-(intensity-LinPred1-Week1Mean[ii])^2/(2*Week1Var[ii]))/sqrt(2*Week1Var[ii]*pi)
    Week2Gaus[ii,] = .1*exp(-(intensity-LinPred2-Week2Mean[ii])^2/(2*Week2Var[ii]))/sqrt(2*Week2Var[ii]*pi)
    Week3Gaus[ii,] = .1*exp(-(intensity-LinPred3-Week3Mean[ii])^2/(2*Week3Var[ii]))/sqrt(2*Week3Var[ii]*pi)
    Week4Gaus[ii,] = .1*exp(-(intensity-LinPred4-Week4Mean[ii])^2/(2*Week4Var[ii]))/sqrt(2*Week4Var[ii]*pi)
  }
  
  ## taylor the distributions for CDC submission
  
  # remove probability of past weeks
  PeakWeekGaus[,weeks<=nweeksFit] = 0
  OnsetGaus[,weeks<=nweeksFit] = 0
  
  # remove probability of intensities less than max ILI thus far
  for (ii in 1:11) {
    if (ii==11) {
      MaxILI = max(mydata$model$raw[weeks<=nweeksFit])
    } else {
      MaxILI = max(mydata$fit$raw[weeks<=nweeksFit,ii])
    }
    PeakIntGaus[ii,intensity<MaxILI] = 0
  }
  
  # remove any bin-probabilities p where log(p) < -10
  PeakWeekGaus[PeakWeekGaus<exp(-10)] = 0
  PeakIntGaus[PeakIntGaus<exp(-10)] = 0
  OnsetGaus[OnsetGaus<exp(-10)] = 0
  Week1Gaus[Week1Gaus<exp(-10)] = 0
  Week2Gaus[Week2Gaus<exp(-10)] = 0
  Week3Gaus[Week3Gaus<exp(-10)] = 0
  Week4Gaus[Week4Gaus<exp(-10)] = 0
  Week1ln[Week1ln<exp(-10)] = 0
  Week2ln[Week2ln<exp(-10)] = 0
  Week3ln[Week3ln<exp(-10)] = 0
  Week4ln[Week4ln<exp(-10)] = 0
  
  # renormalize distributions
  for (ii in 1:11) {
    PeakWeekGaus[ii,] = PeakWeekGaus[ii,]/sum(PeakWeekGaus[ii,])
    PeakIntGaus[ii,] = PeakIntGaus[ii,]/sum(PeakIntGaus[ii,])
    OnsetGaus[ii,] = OnsetGaus[ii,]/sum(OnsetGaus[ii,])
    Week1Gaus[ii,] = Week1Gaus[ii,]/sum(Week1Gaus[ii,])
    Week2Gaus[ii,] = Week2Gaus[ii,]/sum(Week2Gaus[ii,])
    Week3Gaus[ii,] = Week3Gaus[ii,]/sum(Week3Gaus[ii,])
    Week4Gaus[ii,] = Week4Gaus[ii,]/sum(Week4Gaus[ii,])
    Week1ln[ii,] = Week1ln[ii,]/sum(Week1ln[ii,])
    Week2ln[ii,] = Week2ln[ii,]/sum(Week2ln[ii,])
    Week3ln[ii,] = Week3ln[ii,]/sum(Week3ln[ii,])
    Week4ln[ii,] = Week4ln[ii,]/sum(Week4ln[ii,])
  }
  
  out = list(intensity=intensity,weeks=weeks,PeakWeek=PeakWeek, PeakWeekGaus=PeakWeekGaus, PeakInt=PeakInt, PeakIntGaus=PeakIntGaus, OnsetWk=OnsetWk, OnsetGaus=OnsetGaus, Week1For=Week1For, Week1Gaus=Week1Gaus, Week1ln=Week1ln, Week2For=Week2For, Week2Gaus=Week2Gaus, Week2ln=Week2ln, Week3For=Week3For, Week3Gaus=Week3Gaus, Week3ln=Week3ln, Week4For=Week4For, Week4Gaus=Week4Gaus, Week4ln=Week4ln )
  
  return(out)
}

StartWeekEst <- function(data,rtn,onset,week) {
  #' Calculate the first week of three consecutive weeks that ILInet reaches/exceeds baseline
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param rtn Best estimation
  #' @param onset The baseline 
  #' @param week A vector saving the season week
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
  prob.st = as.data.frame(prob.st) 
  names(prob.st) = c("Start Week", "Prob")
  # Best estimation from rtn
  temp = which(rtn >= onset)
  best.st = 0
  if(length(temp) > 2) {
    for(ck in 1:(length(temp)-2)) {
      if(temp[ck] + 2 == temp[ck + 2]) {
        best.st = week[temp[ck]]
        break
      }
    }
  }
  pot.st = c(prob.st[which.max(prob.st[,2]),1], mean(st), median(st), best.st) 
  names(pot.st) = c("Most probable", "Mean", "Median", "Best")
  return(list(pot.st,prob.st))
}

PeakWeekEst <- function(data, rtn, week) {
  #' Calculate the peak week
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param rtn Best estimation
  #' @param week A vector saving the season week
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation
  n = nrow(data)
  pw = NULL
  for(i in 1: n) {
    temp = which(data[i,] == max(data[i,]))
    pw = rbind(pw,cbind(week[temp],1/length(temp)))
  }
  uni.pw = sort(unique(pw[,1]))
  prob.pw = NULL
  for(j in uni.pw) {
    temp = which(pw[,1] == j)
    prob.pw = c(prob.pw, sum(pw[temp,2])/n)
  }
  prob.pw = cbind(uni.pw, prob.pw)
  prob.pw = as.data.frame(prob.pw) 
  names(prob.pw) = c("Peak week", "Prob")
  # Best estimation
  best.pw = week[which(rtn == max(rtn))]
  mp.pw = prob.pw[which.max(prob.pw[,2]),1] 
  pot.pw = c(mp.pw, mean(pw[,1]), median(pw[,1]), best.pw)
  names(pot.pw) = c("Most probable", "Mean", "Median", "Best")
  return(list(pot.pw, prob.pw))
}

PeakIntyEst <- function(data,rtn) {
  #' Calculate the peak intensity
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param rtn Best estimation
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
  mp.pi = bin.pi[which.max(prob.pi)]
  prob.pi = cbind(bin.pi, prob.pi)
  prob.pi = as.data.frame(prob.pi) 
  names(prob.pi) = c("Bin", "Prob")
  best.pi = max(rtn)
  pot.pi = c(mp.pi, mean(pi), median(pi), best.pi) 
  names(pot.pi) = c("Most probable", "Mean", "Median", "Best")
  return(list(pot.pi, prob.pi))
}

ForecastEst <- function(data, rtn, nweekfit) {
  #' Calculate one-to-four-week ahead forecasts
  #' @param data A n*52 matrix/dataframe with n simulation
  #' @param rtn Best estimation
  #' @param nweekfit A numeric value of the number of fitted weeks, so from nweekfit + 1, the data is a prediction
  #' @return A list with 2 elements: first is the point estimation, second is the prob estimation for the full bin
  n = nrow(data)
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi)
  
  pred = data[,(nweekfit+1): (nweekfit+4)]
  # median
  median.fc = apply(pred,2,median)
  # mean
  mean.fc = apply(pred, 2, mean) 
  # best
  best.fc = rtn[(nweekfit + 1):(nweekfit + 4)]
  # prob estimation
  pred.fc = bin.pi
  for(wk in 1:4) {
    prob.fc = NULL
    for(i in 1:(m-1)) {
      prob.fc[i] = sum((pred[, wk] >= bin.pi[i] - 0.05) & (pred[, wk] < bin.pi[i] + 0.05)) / n
    }
    prob.fc[m] = sum((pred[, wk] >= bin.pi[m] - 0.05) & (pred[, wk] < 100)) / n
    # scale to sum = 1
    if((sum(prob.fc) < 1) & (sum(prob.fc) > 0)) {
      prob.fc = prob.fc/sum(prob.fc)
    }
    pred.fc = cbind(pred.fc, prob.fc)
  }
  pred.fc = as.data.frame(pred.fc) 
  names(pred.fc) = c("Bin","1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  # most probable
  mp.fc = NULL
  for(i in 1:4) {
    mp.fc[i] = bin.pi[which.max(pred.fc[,i+1])]
  }
  pot.fc = rbind(mp.fc, mean.fc, median.fc, best.fc)
  pot.fc = as.data.frame(pot.fc)
  names(pot.fc) = c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  rownames(pot.fc) = c("Most probable", "Mean", "Median", "Best")
  return(list(pot.fc,pred.fc))
}

FillStartWeek <- function(pot,prob) {
  #' Fill the table for target - start week
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  
  pot = as.numeric(pot)
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
  }else if(length(temp) == 0) {
    fill[length(fill)] = 1
  }else {
    r = NULL
    for(i in bin.st$Bin_start_incl[temp]) {
      r = c(r, which(p.st == i))
    }
    fill[temp] = prob[r,nc]
    fill[length(fill)] = sum(prob[-r,nc])
  }
  return(c(pot,fill))
}

FillPeakWeek <- function(pot,prob) {
  #' Fill the table for target - peak week
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  bin.pw = table[(table$Location == "US National") & (table$Target == "Season peak week") & (table$Type == "Bin"), ]
  l.pw = nrow(bin.pw)
  pot = as.numeric(pot)
  fill = rep(0, l.pw)
  nc = ncol(prob)
  p.pw = prob[,nc-1]
  temp = NULL
  for(i in p.pw) {
    temp = c(temp, which(bin.pw$Bin_start_incl == i))
  }
  if(length(temp) == length(p.pw)) {
    fill[temp] = prob[,nc]
  }else if(length(temp) != 0) {
    r = NULL
    for(i in bin.pw$Bin_start_incl[temp]) {
      r = c(r, which(p.pw == i))
    }
    fill[temp] = prob[r,nc]
    fill[length(fill)] = sum(prob[-r,nc])
  }
  return(c(pot,fill))
}


FillPeakInty <- function(pot,prob) {
  #' Fill the table for target - peak intensity
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  pot = as.numeric(pot)
  return(c(round(pot,1),prob$Prob))
}

FillForecast <- function(pot,prob) {
  #' Fill the table for target - one-to-four-week ahead forecasts
  #' @param pot Point estimation - a numeric value
  #' @param prob Prob estimation - a matrix/dataframe
  #' @return A 1-columon vector where the first element is the point estimation
  pot = as.numeric(pot)
  fill = NULL
  for(i in 1:4) {
    fill = c(fill, round(pot[i],1))
    fill = c(fill, prob[, i+1])
  }
  return(fill)
}

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
    if((sw == 52) || (sw == 1)) {
      sw.range = sw
    }else {
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

ScorePeakWeek = function(data,sweek,pot,prob,peak = TRUE) {
  #' Score the estimation of peak week
  #' @param data A numeric vector from cdc data indicates the weighted ILINet
  #' @param sweek Seanson week in DICE data
  #' @prarm pot Point Estimation of peak week
  #' @param prob Prob Estimation of peak week
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
      if((wk == 1) || (wk == 52)) {
        wk.range = wk
      }else {
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

GetEst <- function(dump) {
  #' Calculate all the estimation
  #' @param dump Simulation data
  #' @return A list saving all the national/regional point/prob estimation
  # Extract information
  reg.prof = dump$profile_ili

  week = dump$weeks
  if("onset" %in% names(dump)) {
    nat.prof = dump$natl_profile
    nat.rtn = dump$natl_model
    reg.rtn = dump$rtn_ili
    onset = dump$onset 
    reg.onset = onset[1:10]
    nat.onset = onset[11]
    nwfit = dump$nweeks.fit
  }else {
    nat.prof = dump$model_profile_ili
    reg.rtn = dump$rtn_ili
    nat.rtn = dump$model_rtn_ili
    reg.onset = as.numeric(dump$fit_onset)
    nat.onset = dump$model_onset
    nwfit = dump$nweeksFit
  }

  #n = nrow(nat.prof) # number of simulation
  bin.pi = seq(from = 0, to = 13, by = 0.1)
  m = length(bin.pi) # number of bins
  
  # National
  result = list() # saving all the estimation
  nat.st = StartWeekEst(nat.prof, nat.rtn, nat.onset, week)
  result$nat.pot.st = nat.st[[1]]
  result$nat.prob.st = nat.st[[2]]
  
  nat.pw = PeakWeekEst(nat.prof, nat.rtn, week)
  result$nat.pot.pw = nat.pw[[1]]
  result$nat.prob.pw = nat.pw[[2]]
  
  nat.pi = PeakIntyEst(nat.prof, nat.rtn)
  result$nat.pot.pi = nat.pi[[1]]
  result$nat.prob.pi = nat.pi[[2]]
  
  nat.fc = ForecastEst(nat.prof, nat.rtn, nwfit)
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
    rtn = reg.rtn[,reg.num]
    # Extract regional data
    # Start week
    reg.st = StartWeekEst(data, rtn, reg.onset[reg.num], week)
    reg.pot.st = reg.st[[1]]
    reg.prob.st = reg.st[[2]]
    pot.st = rbind(pot.st,reg.pot.st)
    reg.prob.st = data.frame(rep(reg.num, nrow(reg.prob.st)), reg.prob.st)
    prob.st = rbind(prob.st, reg.prob.st)
    
    # Peak week
    reg.pw = PeakWeekEst(data,rtn, week)
    reg.pot.pw = reg.pw[[1]]
    reg.prob.pw = reg.pw[[2]]
    pot.pw = rbind(pot.pw, reg.pot.pw)
    reg.prob.pw = data.frame(rep(reg.num, nrow(reg.prob.pw)), reg.prob.pw)
    prob.pw = rbind(prob.pw, reg.prob.pw)
    
    # Peak intensity
    reg.pi = PeakIntyEst(data,rtn)
    reg.pot.pi = reg.pi[[1]]
    reg.prob.pi = reg.pi[[2]]
    pot.pi = rbind(pot.pi, reg.pot.pi)
    reg.prob.pi = cbind(rep(reg.num, m), reg.prob.pi)
    prob.pi = rbind(prob.pi, reg.prob.pi)
    
    # 1-4 week ahead forecast
    reg.fc = ForecastEst(data, rtn, nwfit)
    reg.pot.fc = reg.fc[[1]]
    reg.prob.fc = reg.fc[[2]]
    #reg.pot.fc= c(reg.num, reg.pot.fc)
    #pot.fc = rbind(pot.fc, reg.pot.fc)
    pot.fc[[reg.num]] =  reg.pot.fc
    reg.prob.fc = cbind(rep(reg.num, m), reg.prob.fc)
    prob.fc = rbind(prob.fc, reg.prob.fc)
  }
  
  rownames(pot.st) = seq(1,10,1)
  pot.st = as.data.frame(cbind(seq(1,10,1), pot.st)) # point estimation of regional start week
  names(pot.st)[1] = "HHS Region"
  names(prob.st)[1] = "HHS Region"
  result$reg.pot.st = pot.st
  result$reg.prob.st = prob.st
  
  rownames(pot.pw) = seq(1,10,1)
  pot.pw = as.data.frame(cbind(seq(1,10,1), pot.pw)) # point estimation of regional peak week
  names(pot.pw)[1] = "HHS Region"
  names(prob.pw)[1] = "HHS Region"
  result$reg.pot.pw = pot.pw
  result$reg.prob.pw = prob.pw
  
  rownames(pot.pi) = seq(1,10,1)
  pot.pi = as.data.frame(cbind(seq(1,10,1), pot.pi)) # point estimation of regional peak intensity
  names(pot.pi)[1] = "HHS Region"
  names(prob.pi)[1] = "HHS Region"
  result$reg.pot.pi = pot.pi
  result$reg.prob.pi = prob.pi
  
  # point estimation of regional forecast
  names(prob.fc)[1] = "HHS Region"
  result$reg.pot.fc = pot.fc
  result$reg.prob.fc = prob.fc
  return(result)
}


PrintEst <- function(dump){
  est = GetEst(dump)
  cat("US National:\n")
  nat.est = rbind(est$nat.pot.st, est$nat.pot.pw, est$nat.pot.pi, t(est$nat.pot.fc))
  rownames(nat.est) = seq(1, nrow(nat.est), 1)
  nat.est = as.data.frame(nat.est)
  rownames(nat.est) = c("Onset week", "Peak week", "Peak intensity", "1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead")
  print(nat.est)
  cat("\n")
  for(i in 1:10) {
    cat("HHS Region ",i, ": \n")
    reg.est = rbind(est$reg.pot.st[i,-1], est$reg.pot.pw[i,-1], est$reg.pot.pi[i,-1], t(est$reg.pot.fc[[i]]))
    rownames(reg.est) = seq(1, nrow(reg.est), 1)
    reg.est = as.data.frame(reg.est)
    rownames(reg.est) = c("Onset week", "Peak week", "Peak intensity", "1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead")
    print(reg.est)
    cat("\n")
  }
}

  
FillTable <- function(dump=NULL,table=NULL,EW=NULL,SelectFile=NULL, OutDir) {

  #' Fill the submission form
  #' @param dump Simulation data
  #' @param table Submission table
  #' @return Return nothing, just create a .csv file for submission
  est = GetEst(dump)

  # Mix DICE distributions 'est' with baseline distributions 'base'
  select = read.csv(file=SelectFile)
  select$peak_week[select$peak_week==""] = NA
  
  curYear = as.integer(strsplit(dump$FY,split="-")[[1]][1])
  nwindow = select$nwindow[1]
  mydata = get.DICE.data(year=curYear)
  if(is.na(select[select$region=="national","baseline"])) {
    # if national baseline is not set, aggregate the region baselines
    nat_baseline = sum(select$baseline[1:10]*mydata$fit$coef)
  } else {
    nat_baseline = select[select$region=="national","baseline"]
  }
  
  # Calculate historic baselines
  base = GetHistoric(curYear=curYear,nwindow=nwindow,nweeksFit=dump$nweeksFit)
  
  est_old = est
  est = MixEstBase(est=est_old,base=base,select=select,CDC_weeks=dump$weeks, nat_baseline=nat_baseline)
  
  # Check if national onset has already occurred
  nat.onset = FindOnset(data=mydata$model$raw, onset=mydata$model$onset ,CDCweeks=mydata$weeks)
  if(!is.na(nat.onset)) {
    # replace probabilities/point forecast with actual onset week
    est$nat.pot.st = nat.onset
    est$nat.prob.st = data.frame(Start.Week=nat.onset,Prob=1)
  }
  
  # If peak-week is manually set
  if(!is.na(select$peak_week[11])) {
    peak_week = as.integer(strsplit(as.character(select$peak_week[11]),";")[[1]])
    peak_weight = as.double(strsplit(as.character(select$peak_weight[11]),";")[[1]])
    
    # determine which peak_weeks are data
    model_pi_weight = 1
    for (ii in 1:length(peak_week)) {
      peak_int = dump$model_ili[dump$weeks==peak_week[ii]]
      if (!is.na(peak_int)) {
        model_pi_weight = model_pi_weight - peak_weight[ii]
      }
    }
    
    est$nat.prob.pw$Prob = (1-sum(peak_weight))*est$nat.prob.pw$Prob
    est$nat.prob.pi$Prob = model_pi_weight*est$nat.prob.pi$Prob
    for (ii in 1:length(peak_week)) {
      
      if (peak_week[ii] %in% est$nat.prob.pw$Peak.Week) {
        est$nat.prob.pw$Prob[est$nat.prob.pw$Peak.Week==peak_week[ii]] = est$nat.prob.pw$Prob[est$nat.prob.pw$Peak.Week==peak_week[ii]] + peak_weight[ii]
      } else {
        est$nat.prob.pw[length(est$nat.prob.pw$Prob)+1,] = c(peak_week[ii], peak_weight[ii])
      }
      
      peak_int = dump$model_ili[dump$weeks==peak_week[ii]]
      if (!is.na(peak_int)) {
        int_ind = (which(peak_int<est$nat.prob.pi$Bin))[1] - 1
        est$nat.prob.pi$Prob[int_ind] = est$nat.prob.pi$Prob[int_ind] + peak_weight[ii]
      }
    }
    
    # Update point forecasts
    est$nat.pot.pw = est$nat.prob.pw$Peak.Week[which.max(est$nat.prob.pw$Prob)]
    est$nat.pot.pi = est$nat.prob.pi$Bin[which.max(est$nat.prob.pi$Prob)]
    
  }
  
  # Region onsets
  for(region in 1:10) {
    reg.onset = FindOnset(data=mydata$fit$raw[,region], onset=as.double(mydata$fit$onset[region]), CDCweeks=mydata$weeks)
    if(!is.na(reg.onset)) {
      # replace probabilities/point forecast with actual onset week
      est$reg.pot.st[region,2] = reg.onset
      
      PreDat = est$reg.prob.st[est$reg.prob.st[,1]<region,]
      PostDat = est$reg.prob.st[est$reg.prob.st[,1]>region,]
      NewDat = data.frame(HHS.Region=region, Start.Week=reg.onset, Prob=1)
      names(NewDat)[1] = "HHS Region"
      est$reg.prob.st = rbind(PreDat,NewDat,PostDat)
    }
    # If peak-week is manually set
    if(!is.na(select$peak_week[region])) {
      peak_week = as.integer(strsplit(as.character(select$peak_week[region]),";")[[1]])
      peak_weight = as.double(strsplit(as.character(select$peak_weight[region]),";")[[1]])
      
      # determine which peak_weeks are data
      model_pi_weight = 1
      for (ii in 1:length(peak_week)) {
        peak_int = dump$fit_ili[dump$weeks==peak_week[ii],region]
        if (!is.na(peak_int)) {
          model_pi_weight = model_pi_weight - peak_weight[ii]
        }
      }
      
      reg_ind = est$reg.prob.pw$`HHS Region`==region
      est$reg.prob.pw$Prob[reg_ind] = (1-sum(peak_weight))*est$reg.prob.pw$Prob[reg_ind]
      est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region] = model_pi_weight*est$reg.prob.pi$Prob[est$reg.prob.pi$`HHS Region`==region]
      for (ii in 1:length(peak_week)) {
        if (peak_week[ii] %in% est$reg.prob.pw$Peak.Week[reg_ind]) {
          est$reg.prob.pw$Prob[est$reg.prob.pw$Peak.Week==peak_week[ii] & reg_ind] = est$reg.prob.pw$Prob[est$reg.prob.pw$Peak.Week==peak_week[ii] & reg_ind] + peak_weight[ii]
        } else {
          r_ind = max(which(est$reg.prob.pw$`HHS Region`<=region)) 
          if (r_ind < length(est$reg.prob.pw$Prob)) {
            est$reg.prob.pw = rbind(est$reg.prob.pw[1:r_ind,], c(region,peak_week[ii], peak_weight[ii]), est$reg.prob.pw[(r_ind+1):length(est$reg.prob.pw$`HHS Region`),])
          } else if (r_ind == length(est$reg.prob.pw$Prob)) {
            est$reg.prob.pw = rbind(est$reg.prob.pw, c(region,peak_week[ii], peak_weight[ii]))
          }
          reg_ind = est$reg.prob.pw$`HHS Region`==region
        }
        
        peak_int = dump$fit_ili[dump$weeks==peak_week[ii],region]
        if (!is.na(peak_int)) {
          pi.reg_ind = est$reg.prob.pi$`HHS Region`==region
          int_ind = (which(peak_int<est$reg.prob.pi$Bin[pi.reg_ind]))[1] - 1
          est$reg.prob.pi$Prob[pi.reg_ind][int_ind] = est$reg.prob.pi$Prob[pi.reg_ind][int_ind] + peak_weight[ii]
        }
      }
      # Update point forecasts
      est$reg.pot.pw$`Most probable`[region] = est$reg.prob.pw$Peak.Week[reg_ind][which.max(est$reg.prob.pw$Prob[reg_ind])]
      est$reg.pot.pi$`Most probable`[region] = est$reg.prob.pi$Bin[pi.reg_ind][which.max(est$reg.prob.pi$Prob[pi.reg_ind])]
    }
  }
  
  # Remove any extremely small probabilities
  est = RemoveSmallProb(est)
  
  
  
  ## Begin Filling the .csv
  # National
  fill.st = FillStartWeek(est$nat.pot.st[1], est$nat.prob.st)
  fill.pw = FillPeakWeek(est$nat.pot.pw[1], est$nat.prob.pw)
  fill.pi = FillPeakInty(est$nat.pot.pi[2], est$nat.prob.pi)
  fill.fc = FillForecast(est$nat.pot.fc[2,], est$nat.prob.fc)
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
    fill.pi.reg = FillPeakInty(pot.pi[reg.num,3], subset(prob.pi, prob.pi[,1] == reg.num))
    fill.fc.reg = FillForecast(pot.fc[[reg.num]][2,], subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
    fill.reg = as.numeric(c(fill.st.reg, fill.pw.reg, fill.pi.reg, fill.fc.reg))
    fill.regional = c(fill.regional, fill.reg)
  }
  
  v = ncol(table)
  table[,v] = c(fill.national, fill.regional)
  filename = paste0(OutDir,'/EW',EW,'-PSI-',Sys.Date(),'.csv')
  write.csv(table, file = filename, row.names = FALSE)
  cat("For CDC Submission Results:",filename,"\n")

}



ScoreEst <- function(dump, mydata, peak) {
  #' Give the log score and absolute error of our estimation compared with cdc data
  #' @param dump Simulation data
  #' @param mydata DICE data for comparison
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
  
  # National Score
  nat.data = mydata$model$raw
  nat.res.sw = ScoreStartWeek(nat.data, nat.onset, week, est$nat.pot.st[1], est$nat.prob.st)
  nat.res.pw = ScorePeakWeek(nat.data, week, est$nat.pot.pw[1], est$nat.prob.pw, peak)
  nat.res.pi = ScorePeakInty(nat.data, est$nat.pot.pi[2],est$nat.prob.pi, peak)
  nat.res.fc = ScoreForecast(nat.data, nwfit, mydata$nweeksData, est$nat.pot.fc[2,], est$nat.prob.fc)
  # Create a csv file for the score of national
  nat.est = as.numeric(c(est$nat.pot.st[1],est$nat.pot.pw[1],est$nat.pot.pi[2],as.numeric(est$nat.pot.fc[2,])))
  nat.score = rbind(nat.res.sw, nat.res.pw, nat.res.pi, nat.res.fc)
  nat.score = cbind(nat.est,nat.score)
  target = c("Start week", "Peak week", "Peak intensity", rep("Forecast",4))
  nat.score = cbind(rep("US National",nrow(nat.score)), target, nat.score)
  row.names(nat.score) = seq(1,nrow(nat.score),1)
  nat.score = as.data.frame(nat.score)
  names(nat.score) = c("Location","Target","Point Est","Actual Value", "Log score", "Absolute error")
  write.csv(nat.score, file = "Nationalscore.csv",row.names = FALSE)
  cat("See National score at: 'Nationalscore.csv'\n")
  
  ## Regional
  pot.st = est$reg.pot.st[,2]
  prob.st = est$reg.prob.st
  pot.pw = est$reg.pot.pw[,2]
  prob.pw = est$reg.prob.pw
  pot.pi = est$reg.pot.pi[,3]
  prob.pi = est$reg.prob.pi
  pot.fc = est$reg.pot.fc
  prob.fc = est$reg.prob.fc
  reg.sw = NULL
  reg.pw = NULL
  reg.pi = NULL
  reg.fc = NULL
  #nfc = min(4,abs(mydata$nweeksData - nwfit))
  for(reg.num in 1:10) {
    reg.data = mydata$fit$raw[,reg.num]
    # Start week
    sw.res = ScoreStartWeek(reg.data, reg.onset[reg.num], week,
                            pot.st[reg.num], subset(prob.st, prob.st[,1] == reg.num))
    reg.sw = rbind(reg.sw, sw.res)
    # Peak week
    pw.res = ScorePeakWeek(reg.data, week, pot.pw[reg.num],
                           subset(prob.pw, prob.pw[,1] == reg.num), peak)
    reg.pw = rbind(reg.pw, pw.res)
    # Peak intensity
    pi.res = ScorePeakInty(reg.data, pot.pi[reg.num],
                           subset(prob.pi, prob.pi[,1] == reg.num), peak)
    reg.pi = rbind(reg.pi, pi.res)
    # 1-4 week ahead forecast
    fc.res = ScoreForecast(reg.data, nwfit, mydata$nweeksData, pot.fc[[reg.num]][2,],
                           subset(prob.fc, prob.fc[,1] == reg.num)[,-1])
    
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
  a = data.frame(rep("Start week", 10), pot.st, reg.sw[,-1])
  names(a)[1:2] = c("Target","Point Est")
  hhs = seq(1,10,1)
  a = cbind(hhs,a)
  b = data.frame(rep("Peak week", 10), pot.pw, reg.pw[,-1])
  names(b)[1:2] = c("Target","Point Est")
  b = cbind(hhs,b)
  c = data.frame(rep("Peak intensity", 10), pot.pi, reg.pi[,-1])
  names(c)[1:2] = c("Target", "Point Est")
  c = cbind(hhs,c)
  fc = NULL
  for(i in 1:10) {
    for(j in 1:4) {
      fc = c(fc, pot.fc[[i]][2,j])
    }
  }
  d = data.frame(rep("Forecast", length(fc)), fc, reg.fc[-1])
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



