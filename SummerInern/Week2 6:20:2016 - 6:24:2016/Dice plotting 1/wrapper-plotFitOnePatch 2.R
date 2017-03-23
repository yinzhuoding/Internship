rm(list = ls())

require(DICE)

dataType = 'cdc'

year = 2010

nweeksFit = 52

RegState = "AZ"

model = 3

ireal = 1

idevice = 1

myName = "arizona-maricopa-2010-2011-3-52-1" #paste(dataType,'-','Region',RegState,'-',year,'-',(year+1),'-',model,'-',nweeksFit,'-',ireal,sep="")

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

mydata$model$factor = 1

err <- plotFitOnePatch.ggplot2(model_rtn = model_rtn, model_profile = model_profile, mydata = mydata, ireal = ireal, run.list=run.list, idevice = 1)
