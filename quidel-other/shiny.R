rm(list = ls())
require(googleVis)
require(shiny)
library(tidyr)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
week = qdata$week
year = qdata$year
total.raw = qdata$Total
n = length(week)
states = qdata$states
var = colnames(total.raw)
total.state = total.raw[, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))] 
total.state = cbind.data.frame(states, t(total.state))
#Date = as.Date(paste(rep("0",n),week,year,sep = "-"),format = "%w-%W-%Y")
#colnames(total.state) = c("state", Date)
colnames(total.state)[1] = "state"
long.total.state = gather(total.state,date,value,2:ncol(total.state))
long.total.state$value = (long.total.state$value - min(long.total.state$value))/(max(long.total.state$value)-min(long.total.state$value))

#da = rep(Date,each = 2)
dataminmax = data.frame(state=rep(c("Min", "Max"),69), 
                        value=rep(c(0,1),69),
                        date=rep(seq(1,69,1),each = 2))
data = rbind(long.total.state, dataminmax)
#data$date = as.Date(as.numeric(data$date), origin = "1970-01-01")
shinyServer(function(input, output) {
  myDate <- reactive({
    input$date
  })
  output$date <- renderText({
    paste("Quidel", myDate())
  })
  output$gvis <- renderGvis({
    myData <- subset(data, 
                     date == myDate())
    gvisGeoChart(myData,
                 locationvar="state", colorvar="value",
                 options=list(region="US", displayMode="regions",
                              resolution="provinces",
                              width=500, height=400,
                              colorAxis="{colors:['green', 'red']}"
                 ))     
  })
})

require(googleVis)
require(shiny)
runGist('https://gist.github.com/yinzhuoding/cf5cca87d31bd79e7f54fa1f1a6dc3bc')
runGist(4642963)
