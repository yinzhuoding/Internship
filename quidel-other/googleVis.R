
library(googleVis)
#suppressPackageStartupMessages(library(googleVis))
#Andrew
data(Andrew)
AndrewGeoMap = gvisGeoMap(Andrew, locationvar='LatLong', numvar='Speed_kt', hovervar='Category', 
                          options=list(width=800,height=400, region='US', dataMode='Markers'))
AndrewMap = gvisMap(Andrew, 'LatLong' , 'Tip', 
                    options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE, mapType='hybrid', useMapTypeControl=TRUE,
                                  width=800,height=400))
AndrewTable = gvisTable(Andrew,options=list(width=800))
AndrewVis = gvisMerge(AndrewGeoMap, AndrewMap)
plot(AndrewVis)
#Cario
data(Cairo)
plot(gvisCalendar(Cairo))
#Sample data
data(CityPopularity)
str(CityPopularity)

data(dino)
str(dino)

data(Exports)
str(Exports)

data(Fruits)

M = gvisMotionChart(Fruits, idvar="Fruit", timevar="Year")
plot(M)

#############################  gvisAnnotatedTimeLine/gvisAnnotationnChart  #############################
data(Stock)
str(Stock)
A1 = gvisAnnotatedTimeLine(Stock, datevar="Date",  numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation", 
                            options=list(displayAnnotations=TRUE, legendPosition='newRow', width="600px", height="350px"))
plot(A1)
B1 <- gvisAnnotationChart(Stock, datevar="Date", numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                          options=list(displayAnnotations=TRUE, legendPosition='newRow', width=600, height=350))
plot(B1)
## Two Y-axis
A2 <- gvisAnnotatedTimeLine(Stock, datevar="Date", numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations=TRUE,  width="600px", height="350px", scaleColumns='[0,1]', scaleType='allmaximized'))
plot(A2)
B2 <- gvisAnnotationChart(Stock, datevar="Date", numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations=TRUE,  width="600px", height="350px", scaleColumns='[0,1]', scaleType='allmaximized'))
plot(B2)
## Zoom into the time window, no Y-axis ticks
A3 <- gvisAnnotatedTimeLine(Stock, datevar="Date",  numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations = TRUE, width="600px", height="350px", zoomStartTime=as.Date("2008-01-04"), zoomEndTime=as.Date("2008-01-05")) )
plot(A3)
B3 <- gvisAnnotationChart(Stock, datevar="Date",  numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(displayAnnotations = TRUE, width="600px", height="350px", zoomStartTime=as.Date("2008-01-04"), zoomEndTime=as.Date("2008-01-05")) )
plot(B3)
## Colouring the area below the lines to create an area chart
A4 <- gvisAnnotatedTimeLine(Stock, datevar="Date", numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(width="600px", height="350px", fill=10, displayExactValues=TRUE, colors="['#0000ff','#00ff00']"))
plot(A4)
B4 <- gvisAnnotatedTimeLine(Stock, datevar="Date", numvar="Value", idvar="Device", titlevar="Title", annotationvar="Annotation",
                            options=list(width="600px", height="350px", fill=10, displayExactValues=TRUE, colors="['#0000ff','#00ff00']"))
plot(B4)

#######################  gvisAeraChart/gvisBarChart/gvisColumnChart  ######################
df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))

## Area chart
Area1 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Area1)
## Bar chart
Bar1 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Bar1)
## Column chart
Col1 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Col1)

## Stacked chart
Area2 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"), options=list(isStacked=TRUE))
plot(Area2)
## Stacked bar chart
Bar2 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"), options=list(isStacked=TRUE))
plot(Bar2)
## Stacked column chart
Col2 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"), options=list(isStacked=TRUE))
plot(Col2)

## Add a customised title
Area3 <- gvisAreaChart(df, xvar="country", yvar=c("val1", "val2"),
                       options=list(title="Hello World", titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}"))
plot(Area3)
## Add a customised title and change width of bars
Bar3 <- gvisBarChart(df, xvar="country", yvar=c("val1", "val2"),
                     options=list(title="Hello World",
                                  titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                                  bar="{groupWidth:'100%'}"))
plot(Bar3)
## Add a customised title and and change width of columns
Col3 <- gvisColumnChart(df, xvar="country", yvar=c("val1", "val2"),
                        options=list(title="Hello World",
                                     titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}",
                                     bar="{groupWidth:'100%'}"))
plot(Col3)
#############################  gvisBubbleChart  #############################
bubble1 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses")
plot(bubble1)

## Set color and size
bubble2 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                           colorvar="Location", sizevar="Profit",
                           options=list(hAxis='{minValue:75, maxValue:125}'))

plot(bubble2)

## Use year to color the bubbles
bubble3 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                           colorvar="Year", sizevar="Profit",
                           options=list(hAxis='{minValue:75, maxValue:125}'))
plot(bubble3)

## Gradient colour example
bubble4 <- gvisBubbleChart(Fruits, idvar="Fruit", xvar="Sales", yvar="Expenses",
                           sizevar="Profit",
                           options=list(hAxis='{minValue:75,  maxValue:125}', 
                                        colorAxis="{colors: ['lightblue', 'blue']}"))
plot(bubble4)

cl1 <- gvisCalendar(Cairo, datevar="Date", numvar="Temp")
plot(cl1)

data(OpenClose)
C1 <- gvisCandlestickChart(OpenClose, xvar="Weekday", low="Low", open="Open", close="Close", high="High",
                           options=list(legend='none'))
plot(C1)

## Add the mean
CityPopularity$Mean=mean(CityPopularity$Popularity)

Combo1 <- gvisComboChart(CityPopularity, xvar="City", yvar=c("Mean", "Popularity"),
                     options=list(seriesType="bars", title="City Popularity", series='{0: {type:"line"}}'))
plot(Combo1)

## Changing the width of columns
Combo2 <- gvisComboChart(CityPopularity, xvar="City", yvar=c("Mean", "Popularity"),
                     options=list(seriesType="bars", bar="{groupWidth:'100%'}", title="City Popularity", series='{0: {type:"line"}}'))
plot(Combo2)

Gauge1 <- gvisGauge(CityPopularity, options=list(min=0, max=800, greenFrom=500,
                                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                                 redFrom=0, redTo=300))

plot(Gauge1)

## Regions examples
## The regions style fills entire regions (typically countries) with
## colors corresponding to the values that you assign

G1a <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit') 

plot(G1a)

## Change projection
G1b <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit',
                    options=list(projection="kavrayskiy-vii")) 

plot(G1b)

G1 <- gvisGeoMap(Exports, locationvar='Country', numvar='Profit',
                 options=list(dataMode="regions")) 

plot(G1)

## Plot only Europe
G2 <- gvisGeoChart(Exports, "Country", "Profit",
                   options=list(region="150"))

plot(G2)

G2 <- gvisGeoMap(CityPopularity, locationvar='City', numvar='Popularity',
                 options=list(region='US', height=350, 
                              dataMode='markers',
                              colors='[0xFF8747, 0xFFB581, 0xc06000]'))  

plot(G2) 
## Example showing US data by state 
require(datasets)

states <- data.frame(state.name, state.x77)
G3 <- gvisGeoChart(states, "state.name", "Illiteracy",
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width=600, height=400))
plot(G3)

states <- data.frame(state.name, state.x77)

G3 <- gvisGeoMap(states, "state.name", "Illiteracy",
                 options=list(region="US", dataMode="regions",
                              width=600, height=400))
plot(G3) 

## Markers Example
## A marker style map renders bubble-shaped markers at specified
## locations with the color and size that you specify.

G4 <- gvisGeoChart(CityPopularity, locationvar='City', colorvar='Popularity',
                   options=list(region='US', height=350, 
                                displayMode='markers',
                                colorAxis="{values:[200,400,600,800],
                                colors:[\'red', \'pink\', \'orange',\'green']}")) 
plot(G4)


G4 <- gvisGeoMap(Andrew, locationvar="LatLong", numvar="Speed_kt", 
                 hovervar="Category", 
                 options=list(height=350, region="US", dataMode="markers"))

plot(G4) 
G5 <- gvisGeoChart(Andrew, "LatLong", colorvar='Speed_kt',
                   options=list(region="US"))
plot(G5)

G6 <- gvisGeoChart(Andrew, "LatLong", sizevar='Speed_kt',
                   colorvar="Pressure_mb", options=list(region="US"))
plot(G6)

## Create lat:long values and plot a map of Oceania
## Set background colour to light-blue

require(stats)
data(quakes)
head(quakes)
quakes$latlong<-paste(quakes$lat, quakes$long, sep=":")

G7 <- gvisGeoChart(quakes, "latlong", "depth", "mag",
                   options=list(displayMode="Markers", region="009",
                                colorAxis="{colors:['red', 'grey']}",
                                backgroundColor="lightblue"))

plot(G7)

df=data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))
Intensity1 <- gvisIntensityMap(df, locationvar="country", numvar=c("val1", "val2"))
plot(Intensity1)

Intensity2 <- gvisIntensityMap(df, options=list(colors="['#4682b4', '#0073CF']"))
plot(Intensity2)

df <- data.frame(country=c("US", "GB", "BR"), val1=c(1,3,4), val2=c(23,12,32))

## Line chart
Line1 <- gvisLineChart(df, xvar="country", yvar=c("val1", "val2"))
plot(Line1)

data(Andrew)

M1 <- gvisMap(Andrew, "LatLong" , "Tip",
              options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE,
                           mapType='hybrid', useMapTypeControl=TRUE,
                           width=800,height=400))

plot(M1) 
## Example with address, here UK post-code and some html code in tooltip

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("<a href='http://www.lloyds.com'>Lloyd's</a>", 
                       "<a href='http://www.guildhall.cityoflondon.gov.uk/'>Guildhall</a>"))

M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)

G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=250, height=100))
T <- gvisTable(Exports, options=list(width=250, height=300))

GT <- gvisMerge(G,T, horizontal=FALSE) 
plot(GT)
plot(G)
plot(T)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         