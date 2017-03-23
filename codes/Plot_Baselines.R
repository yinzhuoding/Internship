
require(DICE)

start.year = 2016
nwindow    = 3
nweeksFit  = 35

# Repository Directory
RepDir = "~/GitReps/FluFore/"
# load FluSight functions form repository
source(paste0(RepDir,"codes/FluSight.R"))

# Plotting Directory
PlotDir = "~/Dropbox/MyLEPR/FluChallenge/plots/"


baseline = GetHistoric(curYear=start.year,nwindow=nwindow,nweeksFit=nweeksFit)

mydata = get.DICE.data(year=2016)

# open PDF
pdf(file=paste0(PlotDir,"Baselines",start.year,"_nweeksFit",nweeksFit,"_nwindow",nwindow,".pdf"), width=12, height=9)
pch = 15
cex = 0.45
par(mfrow=c(3,4))

weekoff = 0.45
intoff = 0.045

nint = length(baseline$intensity)

mydata$model$raw[(nweeksFit+1):mydata$nweeks] = NA
mydata$fit$raw[(nweeksFit+1):mydata$nweeks,]  = NA

# define how bin probabilities are transformed to a transparency value (0 to 1)
# The probablities are often all < .1 per bin.  The purpose of the transformation is to cover more of the 0 to 1 range while applying a consistent transformation to all regions/distributions

# this version makes transparency proportional to CDC-challenge-score
trans <- function(input) {
  trans = log(input)
  trans[trans < -10] = -10
  trans = trans/10 + 1
  return(trans)
}
# trans <- function(input) {
#   trans = sqrt(input)
#   return(input)
# }


# First Plot National
MaxInt = max(which(baseline$PeakIntGaus[11,] > 0))
plot(mydata$model$raw, col="blue", type="l", lwd=2, ylab="%ILI", xlab="EW", xaxt='n', ylim=c(0,baseline$intensity[MaxInt]), xlim=c(1,mydata$nweeks), main="National")
axis(1,at=c(1:mydata$nweeks),labels=mydata$weeks)
lines(c(1,mydata$nweeks),c(mydata$model$onset,mydata$model$onset), lty=2, col=alpha("gray",0.5), lwd=2)


#points(rep(nweeksFit+1,nint), baseline$intensity, pch=pch, col=alpha("red",trans(baseline$Week1ln[11,])), cex=cex)
rect(rep(nweeksFit+1,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+1,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week1ln[11,])), border=NA)
rect(rep(nweeksFit+2,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+2,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week2ln[11,])), border=NA)
rect(rep(nweeksFit+3,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+3,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week3ln[11,])), border=NA)
rect(rep(nweeksFit+4,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+4,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week4ln[11,])), border=NA)


#points(baseline$weeks,rep(0.1,mydata$nweeks), pch=pch, col=alpha("black",trans(baseline$OnsetGaus[11,])), cex=cex)
rect(baseline$weeks-weekoff,rep(0.1,mydata$nweeks)-intoff, baseline$weeks+weekoff,rep(0.1, mydata$nweeks)+intoff, col=alpha("black",trans(baseline$OnsetGaus[11,])), border=NA)

maxweek = baseline$weeks[which.max(baseline$PeakWeekGaus[11,])]
maxint = baseline$intensity[which.max(baseline$PeakIntGaus[11,])]
#points(baseline$weeks,rep(maxint,mydata$nweeks), pch=pch, col=alpha("forestgreen",trans(baseline$PeakWeekGaus[11,])), cex=cex)
rect(baseline$weeks-weekoff,rep(maxint,mydata$nweeks)-intoff, baseline$weeks+weekoff,rep(maxint,mydata$nweeks)+intoff, col=alpha("forestgreen",trans(baseline$PeakWeekGaus[11,])), border=NA)
#points(rep(maxweek,nint), baseline$intensity, pch=pch, col=alpha("forestgreen",trans(baseline$PeakIntGaus[11,])), cex=cex)
rect(rep(maxweek,nint)-weekoff, baseline$intensity-intoff, rep(maxweek,nint)+weekoff, baseline$intensity+intoff, col=alpha("forestgreen",trans(baseline$PeakIntGaus[11,])), border=NA)

fit = lm(y~x,list(x=1:nwindow,y=mydata$model$raw[(nweeksFit-nwindow+1):nweeksFit]))
LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]
#lines((nweeksFit+1):(nweeksFit+4), c(LinPred1,LinPred2,LinPred3,LinPred4), col="red", lty=3, lwd=2)
points((nweeksFit+1):(nweeksFit+4), c(LinPred1,LinPred2,LinPred3,LinPred4), col="blue", pch=8, cex=0.25)

# Now plot regions
for (region in 1:10) {
  MaxInt = max(which(baseline$PeakIntGaus[region,] > 0))
  plot(mydata$fit$raw[,region], col="blue", type="l", lwd=2, ylab="%ILI", xlab="EW", xaxt='n', ylim=c(0,baseline$intensity[MaxInt]), xlim=c(1,mydata$nweeks), main=paste0("Region ",region))
  axis(1,at=c(1:mydata$nweeks),labels=mydata$weeks)
  lines(c(1,mydata$nweeks),c(mydata$fit$onset[region],mydata$fit$onset[region]), lty=2, col=alpha("gray",0.5), lwd=2)
  
  # plot 1-week ahead distribution
#  points(rep(nweeksFit+1,nint), baseline$intensity, pch=pch, col=alpha("red",trans(baseline$Week1ln[region,])), cex=cex)
  rect(rep(nweeksFit+1,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+1,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week1ln[region,])), border=NA)
  rect(rep(nweeksFit+2,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+2,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week2ln[region,])), border=NA)
  rect(rep(nweeksFit+3,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+3,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week3ln[region,])), border=NA)
  rect(rep(nweeksFit+4,nint)-weekoff, baseline$intensity-intoff, rep(nweeksFit+4,nint)+weekoff, baseline$intensity+intoff, col=alpha("red",trans(baseline$Week4ln[region,])), border=NA)
  
#  points(baseline$weeks,rep(0.1,mydata$nweeks), pch=pch, col=alpha("black",trans(baseline$OnsetGaus[region,])), cex=cex)
  rect(baseline$weeks-weekoff,rep(0.1,mydata$nweeks)-intoff, baseline$weeks+weekoff,rep(0.1, mydata$nweeks)+intoff, col=alpha("black",trans(baseline$OnsetGaus[region,])), border=NA)
  
  maxweek = baseline$weeks[which.max(baseline$PeakWeekGaus[region,])]
  maxint = baseline$intensity[which.max(baseline$PeakIntGaus[region,])]
  #points(baseline$weeks,rep(maxint,mydata$nweeks), pch=pch, col=alpha("forestgreen",trans(baseline$PeakWeekGaus[region,])), cex=cex)
  #points(rep(maxweek,nint), baseline$intensity, pch=pch, col=alpha("forestgreen",trans(baseline$PeakIntGaus[region,])), cex=cex)
  rect(baseline$weeks-weekoff,rep(maxint,mydata$nweeks)-intoff, baseline$weeks+weekoff,rep(maxint,mydata$nweeks)+intoff, col=alpha("forestgreen",trans(baseline$PeakWeekGaus[region,])), border=NA)
  rect(rep(maxweek,nint)-weekoff, baseline$intensity-intoff, rep(maxweek,nint)+weekoff, baseline$intensity+intoff, col=alpha("forestgreen",trans(baseline$PeakIntGaus[region,])), border=NA)
  
  fit = lm(y~x,list(x=1:nwindow,y=mydata$fit$raw[(nweeksFit-nwindow+1):nweeksFit,region]))
  LinPred1 = fit$coefficients[2]*(nwindow+1) + fit$coefficients[1]
  LinPred2 = fit$coefficients[2]*(nwindow+2) + fit$coefficients[1]
  LinPred3 = fit$coefficients[2]*(nwindow+3) + fit$coefficients[1]
  LinPred4 = fit$coefficients[2]*(nwindow+4) + fit$coefficients[1]
  #lines((nweeksFit+1):(nweeksFit+4), c(LinPred1,LinPred2,LinPred3,LinPred4), col="red", lty=3, lwd=2)
  points((nweeksFit+1):(nweeksFit+4), c(LinPred1,LinPred2,LinPred3,LinPred4), col="blue", pch=8, cex=0.25)
}

legend_image <- as.raster(matrix(alpha("red",(10:0)/10), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Bin Score')
text(x=1.25, y = (0:10)/10, labels = -10:0)
rasterImage(legend_image, 0, 0, 1,1)

dev.off()
