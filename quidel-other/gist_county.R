rm(list = ls())
require(googleVis)
require(shiny)
library(tidyr)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
GeoAnimation.county = function(qdata,showyear = 2016,state) {
  week = qdata$week
  year = qdata$year
  total.raw = qdata$Total
  split.index = which(year == 2016 & week == 26)
  if(showyear == 2015) {
    year.index = seq(1,split.index,1)
  }
  if(showyear == 2016) {
    year.index = seq(split.index+1,length(week),1)
  }
  n = length(week)
  var = colnames(total.raw)
  othercounty = c("Baltimore City","Anchorage Borough","Nome Census Area","Parish","NULL")
  county.index = which(grepl('County',var))
  for(county in othercounty) {
    county.index = union(county.index, which(grepl(county,var)))
  }
  total.county = total.raw[year.index,sort(county.index)]
  # normalization
  total.county.std = as.data.frame(apply(total.county,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))

  county.list = strsplit(colnames(total.county),"[.]")
  counties.state = sapply(county.list, function(x) {x[3]})
  counties = paste(as.character(qdata$counties[,2]), counties.state, sep = ",")
  
  total.county.std = cbind.data.frame(counties, t(total.county.std))
  total.county.std.state = total.county.std[which(counties.state == state),]
  colnames(total.county.std.state) = c("region",seq(1,length(year.index),1))
  
  p1 = gvisGeoChart(total.county.std.state,locationvar = 'region',  colorvar = '1',
                    options = list(region = paste('US',state,sep = "-"),displayMode='markers',resolution="metros"))
  plot(p1)
  long.total.county = gather(total.county.std.state,date,value,2:ncol(total.county.std))
  write.csv(long.total.county, file = "data.csv",row.names = FALSE)
  myColourAxis <- paste("{values: [0, 0.5, 1.0 ], ",
                        "colors: [\'blue\', \'purple\', \'red\']}")
  
  gvisData <- by(long.total.county, list(date=long.total.county$date), function(x){
    date <- x$date[1]	
    g <- gvisGeoChart(x,  locationvar = "region", colorvar = "value",
                      options=list(region=paste("US",state,sep = "-"), displayMode="markers", resolution="metros"),
                      chartid=paste("[", date, "]", sep=""))
    .data <- g$html$chart["jsData"]
    .data <-gsub("function ", "", .data)
    .data <- sub("\\] ()", "\\] = function ", .data)
    return(.data)	
  }
  )
  currentYear = showyear
  startDate = 1
  endDate = length(year.index)
  myRegion <- as.character(paste("US",state, sep = "-"))
  animation <- sprintf("
                       var gvisData = {};
                       var Animation = {};
                       Animation.startDate = %s;
                       Animation.endDate = %s;
                       Animation.currentYear = %s;
                       Animation.currentDate = Animation.startDate;
                       Animation.divCharts = {};
                       
                       Animation.playAnimation = function() {
                       if (Animation.currentDate > Animation.endDate) {
                       return;
                       }
                       document.getElementById('chart-header').innerHTML =
                       'Quidel data in year <b>' + Animation.currentYear + '</b> of week <b>'
                       + Animation.currentDate;
                       if (Animation.currentDate > Animation.startDate) {
                       Animation.divCharts[Animation.currentDate-1].style.display = 'none';
                       }
                       Animation.divCharts[Animation.currentDate++].style.visibility = 'visible';
                       setTimeout(Animation.playAnimation, 500);
                       };
                       
                       ", startDate, endDate, currentYear)
  
  
  gvisChart <- sprintf('
                       
                       // jsDrawChart
                       function drawChart() {
                       var chart = {};
                       var options = {};
                       options["displayMode"] = "markers";
                       options["resolution"] = "metros";
                       options["width"] =   556;
                       options["height"] =  347;
                       options["datalessRegionColor"]="white";
                       options["colorAxis"] = %s;
                       options["region"] = %s;
                       
                       for (var i = Animation.startDate; i<=Animation.endDate; i++) {
                       Animation.divCharts[i] = document.createElement("div");
                       Animation.divCharts[i].className = "pop-chart";
                       document.body.appendChild(Animation.divCharts[i]);
                       chart[i] = new google.visualization.GeoChart(Animation.divCharts[i]);
                       
                       var data = gvisData[(i+1-%s)]();
                       options["title"] = i;
                       chart[i].draw(data,options);
                       }
                       
                       // Animation.playAnimation();
                       setTimeout(Animation.playAnimation, 6000);
                       }
                       
                       
                       // jsDisplayChart 
                       function displayChart() {
                       google.load("visualization", "1", { packages:["geochart"] }); 
                       google.setOnLoadCallback(drawChart);
                       }
                       // jsChart 
                       displayChart()
                       
                       ', myColourAxis, myRegion, startDate)
  
  
  htmlHead <- '
  
  <html>
  <meta http-equiv="content-type" content="text/html;charset=utf-8" />
  <head>
  <style type="text/css">
  body {
  color: #444444;
  font-family: Arial,Helvetica,sans-serif;
  font-size: 75%;
  }
  a {
  color: #4D87C7;
  text-decoration: none;
  }
  .pop-chart {
  position: absolute;
  top: 50;
  left: 10;
  display: block;
  visibility: hidden;
  }
  </style>
  
  <title>Quidel data Animation</title>
  
  <script type="text/javascript" src="http://www.google.com/jsapi"></script>
  <script type="text/javascript">
  
  '
  htmlFoot <-'
  
  </script>
  </head>
  <body>
  
  <div id="chart-header"></div>
  
  <FORM>
  <INPUT TYPE="button" onClick="history.go(0)" VALUE="Replay">
  </FORM>
  
  </body>
  </html>
  
  '
  Animated.Geo.Chart <- structure(
    list(type="AnimatedGeoChart",
         chartid="Quidel.Animation",
         html=list(
           header=htmlHead,
           chart=c(animation, gvisData, gvisChart),
           caption="",
           footer=htmlFoot)
    ),
    class = c("gvis", "list")
  )
  plot(Animated.Geo.Chart)
  return(Animated.Geo.Chart)
}
plot(GeoAnimation.county(qdata,showyear = 2016))
