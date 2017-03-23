## Copyright Markus Gesmann, March 2011
## Distributed under GPL 2 or later
## Animated Geo Charts based on ideas and code by:
## Manoj Ananthapadmanabhan and Anand Ramalingam

## Here we animate the life expectancy by country from 1960
## This demo requires the 'WDI' and 'Hmisc' packages

library(WDI)
library(Hmisc)
library(googleVis)

## Get life expectancy and population figures from the World Bank
rm(list = ls())
wbData <- WDI(country="all", indicator=c("SP.DYN.LE00.IN",
                               "SP.POP.TOTL"), start=1960, end=2011,
              extra=FALSE)
names(wbData) <- c("iso2c", "country", "year", "life.expectancy",
                   "population")

## Get country mappings from World Bank
getWorldBankCountries <- function(){
  wbCountries <-
    fromJSON("http://api.worldbank.org/countries?per_page=12000&format=json") 

  wbCountries <- data.frame(t(sapply(wbCountries[[2]], unlist)))
  wbCountries$longitude <- as.numeric(wbCountries$longitude)
  wbCountries$latitude <- as.numeric(wbCountries$latitude)
  levels(wbCountries$region.value) <- gsub(" \\(all income levels\\)",
                                           "", levels(wbCountries$region.value))
  return(wbCountries)
}

## Get country mappings
wbCountries <- getWorldBankCountries()

## Merge the two data sets
wbData <- merge(wbData, wbCountries[c("iso2Code", "region.value",                                      
                                      "incomeLevel.value", "name")],
                by.x="iso2c", by.y="iso2Code")


## Round life.expectancy 
wbData <- transform(wbData,  life.expectancy=round(life.expectancy,1))


## Filter out the aggregates and country id column
cntryData <- subset(wbData, !region.value %in% "Aggregates" )

## Filter out the aggregates and country id column
aggData <- subset(wbData, region.value %in% "Aggregates" )


startYear <- min(na.omit(wbData)$year)
endYear <- max(na.omit(wbData)$year)
minExpectancy <- floor(min(wbData$life.expectancy, na.rm=TRUE))
maxExpectancy <- ceiling(max(wbData$life.expectancy, na.rm=TRUE))

myColourAxis <- paste("{values: [0, 100, 200, 500, 1000, 2500, 5000], ",
                      "colors: [\'red\', \'#D0D1E6\', \'#A6BDDB\', \'#74A9CF\',",
                      " \'#3690C0\', \'#0570B0\', \'green\']}")


## Create Geo Charts for each year
gvisData <- by(cntryData, list(year=cntryData$year), function(x){
    
    year <- x$year[1]	
    g <- gvisGeoChart(x,  "iso2c", "life.expectancy",
                      options=list(displayMode="regions"),
                      chartid=paste("[", year - startYear +1, "]", sep=""))
    .data <- g$html$chart["jsData"]
    .data <-gsub("function ", "", .data)
    .data <- sub("\\] ()", "\\] = function ", .data)
    return(.data)	
}
)

gvisData <- by(long.total.state, list(date=long.total.state$date), function(x){
	date <- x$date[1]	
	g <- gvisGeoChart(x,  locationvar = "state", colorvar = "value",
		options=list(region="US", displayMode="regions", resolution="provinces"),
		 chartid=paste("[", date, "]", sep=""))
	.data <- g$html$chart["jsData"]
	.data <-gsub("function ", "", .data)
	.data <- sub("\\] ()", "\\] = function ", .data)
	return(.data)	
}
)

startDate = 1
endDate = 69
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

plot(Animated.Geo.Chart)


## Analyse change over time

## Filter out the first and last year
fl.cntryData <- subset(cntryData, year %in% c(startYear, endYear))
## Reshape the data into a wide format to calculate the differences
wfl.cntryData <- reshape(fl.cntryData, v.names=c("life.expectancy", "population"),
                   idvar=c("country","iso2c"), timevar="year", direction="wide")
wfl.cntryData$life.expectancy.change <- with(wfl.cntryData,
                                             life.expectancy.2009 -
                                             life.expectancy.1960)

myColourAxis2 <- paste("{values: [-5, 0, 5, 10, 15, 25, 30], ",
                       "colors: [\'red\', \'#D0D1E6\', \'#A6BDDB\', \'#74A9CF\', \'#3690C0\',",
                       "\'#0570B0\', \'green\']}") 

Chart.Diff <- gvisGeoChart(wfl.cntryData,
                                "iso2c", "life.expectancy.change",
                                options=list(
                                  width=556, height=347,
                                  colorAxis=myColourAxis2),
                                chartid="Life_Expectancy_Change")
plot(Chart.Diff)


### Summarise and plot data in a table

tblData <- wfl.cntryData[,c("iso2c","name",
                            "life.expectancy.2009",
                            "life.expectancy.change",
                            "population.2009")]

## Show population in millions
tblData$population.2009 <- round(tblData$population.2009/1e6, 1)

names(tblData) <- c("Code", "Country", "Life expt. at birth in  2009",
                    "Difference in life exp. since 1960",
                    "Population in 2009 (millions)")

tblChart <- gvisTable(tblData, options=list(width=540, height=380),
                      chartid="Summary_table") 
plot(tblChart)


## Analyse change in distribution over time

probs <- seq(0,1,0.2)

## Calculate the quantiles of life expectancy weighted by population
wtdDistr <- do.call("rbind",
                    as.list(
                            by(cntryData,
                               list(year=cntryData$year), 
                               function(x){
                                 if(nrow(na.omit(x))>10){
                                   data.frame(year=x$year[1],
                                              t(wtd.quantile(x$life.expectancy, x$population,
                                                              probs=probs, na.rm=TRUE)))
                                 }
                               }
                               )))

names(wtdDistr) <- c("year", paste("wtd.", 100*probs, "%", sep=""))

## Reverse columns 7 to 2, so that the legend will be in the same
## order as the lines in the plot
wtdDistr <- wtdDistr[,c(1,7:2)]

Chart.wtdDistr <- gvisLineChart(wtdDistr, options=list(
           title="Distribution of life expectancy weighted by population",
           width=556, height=347,
           hAxis="{title: 'Year'}",
           vAxis="{title: 'Life expectancy at birth in years'}"),                   
           chartid="life_expectancy_distribution"
           )

plot(Chart.wtdDistr)


## Look at conflict data. Sourced from
## http://www.pcr.uu.se/digitalAssets/64/64464_UCDP_PRIO_ArmedConflictDataset_v42011.xls
conflictData <- read.csv(
  "http://dl.dropbox.com/u/7586336/blogger/LifeExpectancy/WorldConflicts1946-2010.csv",
                         na.string="", check.names=FALSE)

Chart.Conflicts <- gvisAreaChart(conflictData, options=list(isStacked=TRUE,
                                                 legend="{position: 'right'}",
                                                 title="Armed conflicts by type, 1946 - 2010",
                                                 vAxis="{title: 'No. of conflicts'}",
                                                 width=556, height=347
                                                 ),
                                 chartid="Conflicts"
                                 )
plot(Chart.Conflicts)