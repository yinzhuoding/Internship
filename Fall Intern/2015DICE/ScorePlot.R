rm(list=ls())
library(httr)
require(devtools)
install_github("hrbrmstr/cdcfluview")


graphics.off()
pdf = TRUE

year.start = 2016
year.end   = 2017
FY = paste(year.start,'-',year.end,sep="")

nreal = 3
models = 1:4
prior  = c(0,1,2,3,3)
#prior = c(0,1,2)
Temp   = c(1,1,10,1,10)
#Temp = c(1,1,10)
nwfit = seq(17,23,1) 

nmodels = length(models)
nprior = length(prior)
nTemp  = length(Temp)

priorLegend = c("No Prior",'Prior','Heated Prior','Augment Data',"Augmented Data Heated")
#priorLegend = c("No Prior",'Prior','Heated Prior')
modelLegend = c("SH", "SV", "SH+SV", "Fixed")

library(DICE)

mod_level = 2
fit_level = 3
dataType = 'cdc'

## get the user name
username = Sys.getenv("USER") # change as needed here

## Later use
dataDir = paste0("~/Dropbox/Share (1)/2016/")
csv_and_pdfDir = paste0("~/Dropbox/Share (1)/2016/csv_and_pdf/")

mydata = get.subset(mod_level=mod_level,fit_level=fit_level,dataType=dataType,start.year = year.start,end.year=year.end, name = c(NAME_2 = "USA"))
#mydata = get_flu_data(region = "hhs")

nweeksData = mydata$nweeksData

sh = mydata$fit$sh

school = mydata$fit$school

onset = mydata$fit$onset

nregion = mydata$fit$nregion

nregion1 = nregion + 1

for (nweeksFit in nwfit) {
  llk.mean = array(0, c(nregion, nreal))
  llk.best = array(0, c(nregion, nreal))
  iBest    = array(0, c(nregion, nmodels,nprior))
  
  dimnames(iBest)[[1]] = mydata$fit$name
  dimnames(iBest)[[2]] = paste('model',models,sep='')
  dimnames(iBest)[[3]] = paste('prior',prior,sep='')
  
  for (imodel in 1:nmodels) {
    for (ip in 1:nprior) {
      if (Temp[ip] == 1) {
        subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-nweeks-", nweeksFit)
      } else {
        subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-sig", Temp[ip], "-nweeks-", nweeksFit)
      }
      myDir = subDir
      setwd(myDir)
      
      dataName = paste(dataType, "-", mydata$model$name, "-uncpl-", FY, "-", imodel, "-", nweeksFit, sep = "")
      if(nweeksFit == 20) {
        if((prior[ip] == 1) | (prior[ip] == 2)) {
          nreal = 2
          llk.mean = array(0, c(nregion, nreal))
          llk.best = array(0, c(nregion, nreal))
        }
      }
      for (ireal in 1:nreal) {
        filename = paste("mcmc-", dataName, "-", ireal, ".RData", sep = "")
        load(filename)
        # This loads a list of mcmc objects with nregion + 1 elements
        for (iregion in 1:nregion) {
          tmp = results.list[[iregion]]
          nlines = dim(tmp)[1]
          nparam1 = dim(tmp)[2]
          nlines2 = nlines/2
          llk.mean[iregion, ireal] = mean(tmp[nlines2:nlines, nparam1])
          llk.best[iregion, ireal] = min(tmp[nlines2:nlines, nparam1])
        }
      } # End of loop on realizations - for now use the best 'mean' to select the chain.  Can change to using llk.best
      
      for (iregion in 1:nregion) {
        iBest[iregion, imodel,ip] = which.min(llk.mean[iregion, ])
      }
    }
  }
  filename = paste0(csv_and_pdfDir,'choices-nweeksData-',nweeksData,'-nweeksFit-',nweeksFit,'.csv')
  write.csv(file=filename,iBest)
  cat('\n For a csv file with Best chain Info see: ',filename,'\n')
}

weeks = mydata$weeks
nweeks = mydata$nweeks

## Now we can plot everything 
score.st = array(NA, c(nregion,length(nwfit),nmodels,nprior))
score.pw = array(NA, c(nregion,length(nwfit),nmodels,nprior))
score.pi = array(NA, c(nregion,length(nwfit),nmodels,nprior))
score.fc = array(NA, c(nregion,length(nwfit),nmodels,nprior,4))
nat.st = array(NA,c(length(nwfit),nmodels,nprior))
nat.pw = array(NA,c(length(nwfit),nmodels,nprior))
nat.pi = array(NA,c(length(nwfit),nmodels,nprior))
nat.fc = array(NA,c(length(nwfit),nmodels,nprior,4))
dimnames(score.st)[[1]] = seq(1,nregion,1)
dimnames(score.st)[[2]] = nwfit
dimnames(score.st)[[3]] = paste('model',models,sep='')
dimnames(score.st)[[4]] = paste('prior',prior,sep='')

for (nweeksFit in nwfit) {
  for (imodel in 1:nmodels) {
    for (ip in 1:nprior) {
      if (Temp[ip] == 1) {
        subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-nweeks-", nweeksFit)
      } else {
        subDir <- paste0(dataDir,"cdc-uncpl-prior", prior[ip], "-sig", Temp[ip], "-nweeks-", nweeksFit)
      }
      myDir = subDir
      setwd(myDir)
      
      dataName = paste0(dataType, "-", mydata$model$name, "-uncpl-", FY, "-", imodel, "-", nweeksFit)
      filename = paste0("profiles-", dataName, "-", iBest[iregion, imodel, ip], ".RData")
      load(filename)
      ScoreEst(dump,mydata,peak = TRUE)
      nat.score = read.csv("Nationalscore.csv")
      reg.score = read.csv("Regscore.csv")
      nat.st[nweeksFit-nwfit[1]+1,imodel,ip] = subset(nat.score, Target == "Start week")$Log.score
      nat.pw[nweeksFit-nwfit[1]+1,imodel,ip] = subset(nat.score, Target == "Peak week")$Log.score
      nat.pi[nweeksFit-nwfit[1]+1,imodel,ip] = subset(nat.score, Target == "Peak intensity")$Log.score
      nat.fc[nweeksFit-nwfit[1]+1,imodel,ip, ] = subset(nat.score, Target == "Forecast")$Log.score
      for (iregion in 1:nregion) {
        score.st[iregion,nweeksFit-nwfit[1]+1,imodel,ip] = subset(reg.score, (Location == iregion) & (Target == "Start week"))$Log.score
        score.pw[iregion,nweeksFit-nwfit[1]+1,imodel,ip] = subset(reg.score, (Location == iregion) & (Target == "Peak week"))$Log.score
        score.pi[iregion,nweeksFit-nwfit[1]+1,imodel,ip] = subset(reg.score, (Location == iregion) & (Target == "Peak intensity"))$Log.score
        score.fc[iregion,nweeksFit-nwfit[1]+1,imodel,ip, ] = subset(reg.score, (Location == iregion) & (Target == "Forecast"))$Log.score
      }
      nat.real = nat.score[,c(1,2,4)]
      reg.real = reg.score[,c(1,2,4)]
    }
  }
}

y.min = -10
y.max = 0
p.pch = seq(15,15+nmodels-1,1)
p.col = rainbow(nmodels)
xrange = c(nwfit[1],nwfit[length(nwfit)])
xaxis = nwfit
#xrange = c(1,52)
#xaxis = seq(1,52,1)
## Start week
if (pdf == TRUE) {
  pdfName = paste0(csv_and_pdfDir, "compare-models-score-StartWeek-", nwfit[length(nwfit)],".pdf",sep = "")
  pdf(file = pdfName, width = 12, height = 11)
}

par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))
# National
for(ip in 1:nprior) {
  plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1,xaxis)
  for(imodel in 1:nmodels) {
    points(nwfit, nat.st[ ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
  }
  #abline(v = nat.real[1,3], col = "grey")
  legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
  legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
  title(main = "National")
}

# HHS Region
for(iregion in 1:nregion) {
  for(ip in 1:nprior) {
    plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
    axis(1,xaxis)
    for(imodel in 1:nmodels) {
      points(nwfit, score.st[iregion, ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
    }
    #abline(v = subset(reg.real, Location == iregion)[1,3], col = "grey")
    legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
    legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
    title(main = mydata$fit$name[iregion])
  }
}
if (pdf == TRUE) dev.off()

## Peak week
if (pdf == TRUE) {
  pdfName = paste0(csv_and_pdfDir, "compare-models-score-PeakWeek-",nwfit[length(nwfit)],".pdf", sep = "")
  pdf(file = pdfName, width = 12, height = 11)
}

par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))
# National
for(ip in 1:nprior) {
  plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1,xaxis)
  for(imodel in 1:nmodels) {
    points(nwfit, nat.pw[ ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
  }
  #abline(v = nat.real[2,3], col = "grey")
  legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
  legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
  title(main = "National")
}

# HHS region
for(iregion in 1:nregion) {
  for(ip in 1:nprior) {
    plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
    axis(1,xaxis)
    for(imodel in 1:nmodels) {
      points(nwfit, score.pw[iregion, ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
    }
    #abline(v = subset(reg.real, Location == iregion)[2,3], col = "grey")
    legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
    legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
    title(main = mydata$fit$name[iregion])
  }
}
if (pdf == TRUE) dev.off()

## Peak intensity
if (pdf == TRUE) {
  pdfName = paste0(csv_and_pdfDir, "compare-models-score-PeakIntensity-",nwfit[length(nwfit)],".pdf", sep = "")
  pdf(file = pdfName, width = 12, height = 11)
}

par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))
# National
for(ip in 1:nprior) {
  plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
  axis(1, xaxis)
  for(imodel in 1:nmodels) {
    points(nwfit, nat.pi[ ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
  }
  #bline(v = nat.real[2,3], col = "grey")
  legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
  legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
  title(main = "National")
}
# HHS region
for(iregion in 1:nregion) {
  for(ip in 1:nprior) {
    plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
    axis(1,xaxis)
    for(imodel in 1:nmodels) {
      points(nwfit, score.pi[iregion, ,imodel,ip], type = "b", pch = p.pch[imodel], col = alpha(p.col[imodel],0.5))
    }
    #abline(v = subset(reg.real, Location == iregion)[2,3], col = "grey")
    legend("topleft",priorLegend[ip], text.col = "blue", bty = "n", xjust = 0)
    legend("topright", modelLegend, text.col = p.col, bty = "n", xjust = 1)
    title(main = mydata$fit$name[iregion])
  }
}
if (pdf == TRUE) dev.off()

## Forecast
if (pdf == TRUE) {
  pdfName = paste0(csv_and_pdfDir, "compare-models-score-Forecast-", nwfit[length(nwfit)],".pdf",sep = "")
  pdf(file = pdfName, width = 12, height = 11)
}
par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))
# National
for(imodel in 1:nmodels) {
  for(ip in 1:nprior) {
    nfc = length(score.fc[iregion,nweeksFit-nwfit[1]+1,imodel,ip, ])
    p.col = rainbow(4)
    p.pch = seq(15,15+4-1,1)
    plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
    axis(1,xaxis)
    for (i in 1:4) {
      points(nwfit, nat.fc[ ,imodel,ip,i], type = "b", pch = p.pch[i], col = alpha(p.col[i],0.5))
    }
    #abline(v = nat.real[2,3], col = "grey")
    legend("topleft",c(modelLegend[imodel], priorLegend[ip]), text.col = c("black","blue"), bty = "n", xjust = 0)
    legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead")[1:nfc], text.col = p.col, bty = "n",xjust = 0)
    title(main = "National")
  }
}

for(iregion in 1:nregion) {
  for(imodel in 1:nmodels) {
    for(ip in 1:nprior) {
      nfc = length(score.fc[iregion,nweeksFit-nwfit[1]+1,imodel,ip, ])
      plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(y.min,y.max), xaxt = "n")
      axis(1,xaxis)
      p.col = rainbow(nfc)
      p.pch = seq(15,15+nfc-1,1)
      for (i in 1:nfc) {
        points(nwfit, score.fc[iregion, ,imodel,ip,i], type = "b", pch = p.pch[i], col = alpha(p.col[i],0.5))
      }
      #abline(v = subset(reg.real, Location == iregion)[2,3], col = "grey")
      legend("topleft",c(modelLegend[imodel], priorLegend[ip]), text.col = c("black","blue"), bty = "n", xjust = 0)
      legend("topright",c("1wk ahead", "2wk ahead", "3wk ahead", "4wk ahead")[1:nfc], text.col = p.col, bty = "n", xjust = 1)
      title(main = mydata$fit$name[iregion])
    }
  }
}
if (pdf == TRUE) dev.off()


if (pdf == TRUE) {
  pdfName = paste0(csv_and_pdfDir, "compare-models-score-SumForecast-",nwfit[length(nwfit)],".pdf", sep = "")
  pdf(file = pdfName, width = 12, height = 11)
}
par(mar = c(4, 3, 1, 1), mfrow = c(nmodels, nprior))
# National
for(imodel in 1:nmodels) {
  for(ip in 1:nprior) {
    nfc = length(score.fc[iregion,nweeksFit-nwfit[1]+1,imodel,ip, ])
    p.col = rainbow(4)
    p.pch = seq(15,15+4-1,1)
    plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(-40,y.max), xaxt = "n")
    axis(1,xaxis)
    points(nwfit,apply(nat.fc[,imodel,ip,],1,sum,na.rm = TRUE), col = alpha("red",0.8), pch = 16)
    legend("topleft",c(modelLegend[imodel], priorLegend[ip]), text.col = c("black","blue"), bty = "n", xjust = 0)
    title(main = "National")
  }
}

for(iregion in 1:nregion) {
  for(imodel in 1:nmodels) {
    for(ip in 1:nprior) {
      nfc = length(score.fc[iregion,nweeksFit-nwfit[1]+1,imodel,ip, ])
      plot(0, type = "n",xlab = "nweeksFit", ylab = "Log score", xlim = xrange, ylim = c(-40,y.max), xaxt = "n")
      axis(1,xaxis)
      p.col = rainbow(nfc)
      p.pch = seq(15,15+nfc-1,1)
      points(nwfit,apply(score.fc[iregion, ,imodel,ip,],1,sum,na.rm = TRUE), col = alpha("red",0.8), pch = 16)
      legend("topleft",c(modelLegend[imodel], priorLegend[ip]), text.col = c("black","blue"), bty = "n", xjust = 0)
      title(main = mydata$fit$name[iregion])
    }
  }
}
if (pdf == TRUE) dev.off()


cat("\n For a plot of the results see: ",pdfName,'\n\n')



