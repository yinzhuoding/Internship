rm(list = ls())

require(DICE)

dataType = 'cdc'

year = 2015

nweeksFit = 50

RegState = 'United.States'

model = 5

ireal = 1

idevice = 1

myName = paste(dataType,'-',RegState,'-cpl-',year,'-',(year+1),'-',model,'-',nweeksFit,'-',ireal,sep="")

## Loading the data (into mydata) and run.list

filename = paste("input-",myName,'.RData',sep="")

## This loads the object mydata - with ALL the data 
load(filename)

run.list = input$run.list

filename = paste('profiles-',myName,'.RData',sep ="")

## This load an object called dump and from it we will 
load(filename)


model_rtn = dump$model_rtn

model_profile = dump$model_profile

profile = dump$profile

rtn = dump$rtn

mydata$model$factor = 1

nregions = mydata$fit$nregions
mydata$fit$factor[1:nregions] = 1

err <- plotFitCDCPercentILI.ggplot2(rtn = rtn, profile = profile, model_rtn = model_rtn, model_profile = model_profile, mydata = mydata, ireal = ireal, run.list = run.list, idevice = 1)

err <- plotHists.ggplot2(rtn = rtn, profile = profile, model_rtn = model_rtn, model_profile = model_profile, mydata = mydata, ireal = ireal, run.list =run.list, idevice = 1)
