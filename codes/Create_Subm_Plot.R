rm(list=ls())
require(DICE)

start.year = 2016
forecast.week = 9
mydata = get.subset(start.year=start.year,end.year=start.year+1, fit_level=3)
nweeksFit = which(mydata$weeks==forecast.week)

RepDir = "~/GitReps/FluFore/"
#RepDir = "~/FluFore/"

# load Submission reading and plotting codes
source(paste0(RepDir,"codes/ScoreCsv.R"))

# load submission file
filename = list.files(path=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",forecast.week,"-.*\\.csv"))
est = read.csv(file=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/",filename))

# load dump file
load(file=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_dump_files/CDC-Flu-Challenge-",start.year,"-",start.year+1,"-week-",nweeksFit,"-AggregateProfile.RData"))

SubDat = ReadCDCSubmission(est=est)

PlotSubmission(SubDat=SubDat, dump=dump, OutDir=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_plots/"))
