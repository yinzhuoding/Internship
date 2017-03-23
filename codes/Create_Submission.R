require(DICE)

RepDir = "~/GitReps/FluFore/"
start.year = 2016
forecast.week = 9
nweeksFit = 35

# load the DICE-dump file
load(paste0(RepDir,"CDC2016-2017/weekly_dump_files/CDC-Flu-Challenge-2016-2017-week-",nweeksFit,"-AggregateProfile.RData"))

# load blank submission form
table = read.csv(paste0(RepDir,"CDC2016-2017/Long_Flu_Submission_Template_update.csv"))

# Specify location of selection .csv
SelectFile = paste0(RepDir,"CDC2016-2017/weekly_selections/FluChallenge_Selection_nweeks-",nweeksFit,".csv")

# source the FluSight functions
source(paste0(RepDir,"codes/FluSight.R"))

FillTable(dump=dump,table=table,EW=forecast.week,SelectFile=SelectFile, OutDir=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/"))




# Read and plot the submission file
filename = list.files(path=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/"),pattern=paste0("EW",forecast.week,"-.*\\.csv"))
est = read.csv(file=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_submission/",filename))

source(paste0(RepDir,"codes/ScoreCsv.R"))
SubDat = ReadCDCSubmission(est=est)

PlotSubmission(SubDat=SubDat, dump=dump, OutDir=paste0(RepDir,"CDC",start.year,"-",start.year+1,"/weekly_plots/"))


