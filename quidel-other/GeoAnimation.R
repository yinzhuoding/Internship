rm(list = ls())
require(googleVis)
require(shiny)
library(tidyr)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
GeoAnimation = function(qdata,level) {
  week = qdata$week
  year = qdata$year
  total.raw = qdata$Total
  n = length(week)
  if(level == "nation") {
    total.nation = qdata$Total$USA
    total.nation.std = (total.nation-min(total.nation))/(max(total.nation)-min(total.nation))
    total.nation.std = cbind.data.frame(rep("USA",n), total.nation.std)
    long.total.nation = cbind(total.nation.std,seq(1,n,1))
    names(long.total.nation) = c("location","value","date")
    long.data = long.total.nation
  }else if (level == "state") {
    states = qdata$states
    var = colnames(total.raw)
    total.state = total.raw[, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))]
    # normalization
    total.state.std = as.data.frame(apply(total.state,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
    total.state.std = cbind.data.frame(states, t(total.state.std))
    colnames(total.state.std)[1] = "location"
    long.total.state = gather(total.state.std,date,value,2:ncol(total.state.std))
    long.data = long.total.state
  }

  myColourAxis <- paste("{values: [0, 0.05, 0.1, 0.2, 0.4, 0.6, 1.0 ], ",
                        "colors: [\'red\', \'#D0D1E6\', \'#A6BDDB\', \'#74A9CF\',",
                        " \'#3690C0\', \'#0570B0\', \'green\']}")
  
  gvisData <- by(long.data, list(date=long.data$date), function(x){
    date <- x$date[1]	
    g <- gvisGeoChart(x,  locationvar = "location", colorvar = "value",
                      options=list(region="US", displayMode="regions", resolution="provinces"),
                      chartid=paste("[", date, "]", sep=""))
    .data <- g$html$chart["jsData"]
    .data <-gsub("function ", "", .data)
    .data <- sub("\\] ()", "\\] = function ", .data)
    return(.data)	
  }
  )
  
  startDate = 1
  endDate = n
  animation <- sprintf("
                       var gvisData = {};
                       
                       var Animation = {};
                       Animation.startDate = %s;
                       Animation.endDate = %s;
                       Animation.currentDate = Animation.startDate;
                       Animation.divCharts = {};
                       
                       Animation.playAnimation = function() {
                       if (Animation.currentDate > Animation.endDate) {
                       return;
                       }
                       document.getElementById('chart-header').innerHTML =
                       'Life expectancy at birth in <b>'
                       + Animation.currentDate + '</b> (Countries with no data shown in white)';
                       if (Animation.currentDate > Animation.startDate) {
                       Animation.divCharts[Animation.currentDate-1].style.display = 'none';
                       }
                       Animation.divCharts[Animation.currentDate++].style.visibility = 'visible';
                       setTimeout(Animation.playAnimation, 500);
                       };
                       
                       ", startDate, endDate)
  
  
  gvisChart <- sprintf('
                       
                       // jsDrawChart
                       function drawChart() {
                       var chart = {};
                       var options ={};
                       options["region"] = "US";
                       options["displayMode"] = "regions";
                       options["resolution"] = "provinces";
                       options["width"] =   556;
                       options["height"] =  347;
                       options["datalessRegionColor"]="white";
                       options["colorAxis"] = %s;
                       
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
                       setTimeout(Animation.playAnimation, 5000);
                       }
                       
                       
                       // jsDisplayChart 
                       function displayChart() {
                       google.load("visualization", "1", { packages:["geochart"] }); 
                       google.setOnLoadCallback(drawChart);
                       }
                       // jsChart 
                       displayChart()
                       
                       ', myColourAxis, startDate)
  
  
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
  
  <title>Life expectancy by country</title>
  
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
         chartid="Life.Expectancy",
         html=list(
           header=htmlHead,
           chart=c(animation, gvisData, gvisChart),
           caption="",
           footer=htmlFoot)
    ),
    class = c("gvis", "list")
  )
  return(Animated.Geo.Chart)
}

animation.state = GeoAnimation(qdata,level = "state")
animation.nation = GeoAnimation(qdata,level = "nation")
plot(animation.state)
plot(animation.nation)
