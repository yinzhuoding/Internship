
rm(list = ls())

require(DICE)

dataType = 'cdc'

year = 2015

nweeksFit = 32

RegState = 9

model = 5

ireal = 1

idevice = 1

myName = paste(dataType,'-','Region',RegState,'-',year,'-',(year+1),'-',model,'-',nweeksFit,'-',ireal,sep="")

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

err <- plotFitOnePatch(model_rtn = model_rtn, model_profile = model_profile, mydata = mydata, ireal = ireal, run.list=run.list, idevice = 1)

