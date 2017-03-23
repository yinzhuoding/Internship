#!/usr/bin/python
import os
import string
import math
import csv

# turn to true to trigger printing

debug=True #False

def round_sigfigs(num, sig_figs):
    if num != 0:
        return round(num, -int(math.floor(math.log10(abs(num))) - (sig_figs - 1)))
    else:
        return 0  # Can't take the log of 0

def scanCsv(csvFile, mode):
    #Count Row and column Number
    file2 = open(csvFile,mode)
    reader = csv.reader(file2)
    rowNum=0
    for row in reader:
      colNum = 0
      for col in row:
        colNum +=1
      rowNum+=1
    file2.close()
    return [rowNum, colNum]

def csvToLatex(fileName, rowNum,colNum,csvFile, titleText, captionText, mode):
    #Insert Chart
    titleLine="\\begin{table}[H]\n\caption{"+titleText+"}\n\
\centering % used for centering table\n\\smallskip\n"
    fileName.write(titleLine)
    fileName.write("\\begin{tabular}{")
    for i in range(0,colNum):
      fileName.write("c ")
    fileName.write("} % centered columns (" + str(colNum) + " columns)\n")
    fileName.write("\hline\hline %inserts double horizontal lines\n")

    #Write column Names

    file2 = open(csvFile,mode)
    reader = csv.reader(file2)
    header = reader.next()
    for i in range(0,colNum - 1):
      fileName.write("\\verb\"" + str(header[i]) + " \"& ")
    fileName.write("\\verb\"" + str(header[colNum -1]) + "\" " + "\\\\ [0.5ex] % inserts table \n")
    file2.close()
    
    
    #Print Table
    fileName.write("%heading\n\hline % inserts single horizontal line\n")
    file2 = open(csvFile,mode)
    reader = csv.reader(file2)
    rowNum=0
    theColNum = colNum
    for row in reader:
      #Save header row
      if rowNum > 0:
        colNum = 0
        for col in row:
          if colNum > 0:
            fileName.write(str(round_sigfigs(float(col), 3))) 
          else:
            if col == "R0min" :
              col = "R"+"\\textsubscript{0min}"
            elif col == "deltaR":
              col = "$\\Delta$"+"\\textsubscript{R}"
            elif col == "aparam" :
              col = "A" + "\\textsubscript{param}"
            elif col == "t0" :
              col = "t"+"\\textsubscript{0}"
            elif col == "alpha":
              col = "$\\alpha$"
            elif col == "delta":
              col = "$\\Delta$"
            elif col == "dur":
              col = "$\\Delta$"+"\\textsubscript{t}"
            elif col == "ts":
              col = "t" + "\\textsubscript{1}"

            fileName.write(col) 
          if colNum < theColNum - 1:
            fileName.write(" & ")
          else:
            fileName.write(" \\\\\n")

          colNum +=1
      rowNum+=1
    file2.close()

    fileName.write("\hline %inserts single line\n")
    fileName.write("\end{tabular}\\par\n")
    fileName.write("\label{table:nonlin} % is used to refer this table in the text\n")
    captionLine="\\bigskip\nCaption: "+captionText
    fileName.write(captionLine)
    fileName.write("\end{table}\n")


# Function for finding a string (str) in a text file, returns the complete line
def findStrLogFile(logFile, mode, str):
   #scan line by line
    file = open(logFile,mode)
    myline=""
    while 1:
        line = file.readline()
        if not line: break
        ifind = line.rfind(str)
        if (ifind != -1):
            myline=line
            break
            
    file.close()
    return myline

# Function for reading a text file all at once 
def scanFile(logFile, mode):
    # read the whole file at once
    file = open(logFile,mode)
    logText=file.read()

    file.close()
    return logText

# name of log file from the run    
logFile="../examples/log.txt"

# name of templates files for Background and Methods Sections
bkgrndFile="templates/background.txt"
methodFile="templates/method.txt"
resultFile="templates/result.txt"

textLog = scanFile(logFile,mode="r")

#figure what type of data was modeled: MPZ/CDC or GFT
dataType=[]
dataType=["MPZ","CDC","GFT"]
for x in dataType:
    icount=textLog.count(x)
    if (icount > 0):
         mydataType = x
         
#The data type is mydataType

#read the template file for Background and Methods from pre-prepared text files
textBkgrnd= scanFile(bkgrndFile,mode="r") + "\n\n"
textMethod= scanFile(methodFile,mode="r")
textResult= scanFile(resultFile,mode="r") + "\n\n"
# in debug mode print the log file

if debug:
    print textLog


if (mydataType == "MPZ"):
    textBaseNumber = findStrLogFile(logFile,mode="r",str="Modeling Base Number")
    textMPZ = findStrLogFile(logFile,mode="r",str="Modeling MPZ")
    zipname = textMPZ.lstrip("Modeling MPZ:")
    zipname = zipname.strip()
    myName  = zipname
else:
    textRegion = findStrLogFile(logFile,mode="r",str="Modeling Region")
    textRegion = textRegion.lstrip("Modeling ")
    iRegion    = textRegion.find(":")
    textRegion = textRegion[iRegion:]
    textRegion = textRegion.strip("\n")
    textRegion = textRegion[1:]
    myName     = textRegion.strip()
    myName     ="region number "+myName
        
textSubDir = findStrLogFile(logFile,mode="r",str="All Data and Plots Saved in Sub-Directory:")
textChainNum = findStrLogFile(logFile,mode="r",str="Running MCMC Chain Number")
textDate = findStrLogFile(logFile,mode="r",str="Job Started on:")
textUser = findStrLogFile(logFile,mode="r",str="Job Running by User:")
textAcceptance = findStrLogFile(logFile,mode="r",str="Acceptance rate for Chain:")
# Find the string with the population and Tg
textN =findStrLogFile(logFile,mode="r",str="Population of:")
textTg =findStrLogFile(logFile,mode="r",str="Using a Fixed Generation Time of:")

textN=textN.lstrip("Population of:")
N=textN.strip()

textTg=textTg.lstrip("Using a Fixed Generation Time of:")
Tg=textTg[0:3]

acceptance = textAcceptance.lstrip("Acceptance rate for Chain:")

#Find the FY modeled
textFY = findStrLogFile(logFile,mode="r",str="Modeling FY")
textFY = textFY.lstrip("Modeling FY")
textFY = textFY.strip("\n")
myFY   = textFY.strip()
    
subDir  = textSubDir.lstrip("All Data and Plots Saved in ")
subDir  = subDir.lstrip("Sub-Directory:")
subDir  = subDir.strip()
chainNum = textChainNum.lstrip("Running MCMC Chain Number")
chainNum = chainNum.lstrip()
chainNum = chainNum.strip("\n")


# now we can build file names for the Python script
# First the two csv files
textStats=findStrLogFile(logFile,mode="r",str="param-stats")
textQuant=findStrLogFile(logFile,mode="r",str="param-quantiles")
textTbl  =findStrLogFile(logFile,mode="r",str="param-table")
iStats=textStats.find(subDir)
iQuant=textQuant.find(subDir)
iTbl  = textTbl.find(subDir)
fileStats=textStats[iStats:]
fileQuant=textQuant[iQuant:]
fileTbl  =  textTbl[iTbl:]
fileStats=fileStats.strip("\n")
fileQuant=fileQuant.strip("\n")
fileTbl  =fileTbl.strip("\n")
fileStats=fileStats.strip()
fileQuant=fileQuant.strip()
fileTbl  = fileTbl.strip()

# Second the pdf or png files
textProfile=findStrLogFile(logFile,mode="r",str="MCMC Profiles written to")
iProfile   =textProfile.find(subDir)
textProfile=textProfile[iProfile:]
textProfile=textProfile.strip("\n")
fileProfile=textProfile.strip()


textCAR=findStrLogFile(logFile,mode="r",str="Cumulative Attack Rate")
iCAR   =textCAR.find(subDir)
textCAR=textCAR[iCAR:]
textCAR=textCAR.strip("\n")
fileCAR=textCAR.strip()

textChain=findStrLogFile(logFile,mode="r",str="MCMC Chain written to:")
iChain   =textChain.find(subDir)
textChain=textChain[iChain:]
textChain=textChain.strip("\n")
fileChain=textChain.strip()

#For MPZ's there is an additional plot - something similar need to be added to the civilian cases
if (mydataType == "MPZ"):
    textEpiSHSc=findStrLogFile(logFile,mode="r",str="Specific Humidity and School Closure")
    iEpiSHSc   =textEpiSHSc.find(subDir)
    textEpiSHSc=textEpiSHSc[iEpiSHSc:]
    textEpiSHSc=textEpiSHSc.strip("\n")
    fileEpiSHSc=textEpiSHSc.strip()
    fileEpiSHSc="../"+fileEpiSHSc
    
if debug:
    print "File Names:"
    if (mydataType == "MPZ"):
        print "Base profile, SH and School Schedule %s" %(fileEpiSHSc)
    print "MCMC Statistice %s" %(fileStats)
    print "MCMC Quantiles %s" %(fileQuant)
    print "MCMC Fit %s" %(fileProfile)
    print "MCMC C.A.R %s" %(fileCAR)
    print "MCMC Chain History %s" %(fileChain)


# start writing the tex file
fileTex = open("report.tex",'w')

fileTex.write("\documentclass{article}\n")

#preamble
fileTex.write("\usepackage{graphicx}\n\usepackage{times}\n\usepackage{parskip}\n\usepackage{graphicx}\n\usepackage{setspace}\n\usepackage{url}\n\usepackage{lscape}\n\usepackage{multirow}\n\usepackage{longtable}\n\usepackage[top=3cm, bottom=3cm, left=3cm, right=3cm]{geometry}\n\usepackage{fixltx2e}\n\usepackage{textgreek}\n\usepackage{float}\n\\floatstyle{plaintop}\n\\restylefloat{table}\n")

#Begin document
fileTex.write("\\begin{document}\n")
fileTex.write("\\begin{center}\n")
fileTex.write("{\\bf {P-MEDDS Summary Report}}\\\\\n")
textDate=textDate+"\n"
textUser=textUser+"\n"
fileTex.write(textDate)
fileTex.write(textUser)
fileTex.write("\\end{center}\n")
fileTex.write("{\\bf Background}\\\\\n")
fileTex.write(textBkgrnd)
fileTex.write("{\\bf {Methods}}\\\\\n")
fileTex.write(textMethod)
fileTex.write("{\\bf Results}\\\\\n")
fileTex.write(textResult)

#Figure 1
#This Figure exists currently only for the MPZ data
if (mydataType == "MPZ"):
    fileTex.write("\\begin{figure}[H]\n")
    fileTex.write("\centering\n")
    nextLine="\includegraphics[width=90mm]{"+fileEpiSHSc+"}\n"
    fileTex.write(nextLine)
    captionLine="\caption{Weekly ILI incidence profile (red line and left y-axis) and specific humidity (blue line and right y-axis)  for MPZ "+myName+" for the"+ myFY+" pandemic year. The horizontal cynan bars mark school closure weeks. \label{overflow}}\n"
    fileTex.write(captionLine)
    fileTex.write("\end{figure}\n\n")

#Figure 2 
fileTex.write("\\begin{figure}[H]\n")
fileTex.write("\centering\n")
nextLine="\includegraphics[width=90mm]{"+fileProfile+"}\n"
fileTex.write(nextLine)
captionLine="\caption{Results of MCMC fitting for "+mydataType+" "+myName+" for the "+myFY+" pandemic year. The red, blue and grey lines (and left y-axis) are the ILI data and fit results with blue marking our best estimate and grey 100 randomly chosen estimates from the MCMC chain.  The green lines and right y-axis denote R(t) as predicted by our model.  Dark green is our best estimate and light green are the 100 randomly chosen estimates. The acceptence rate for this MCMC chain was: "+acceptance+"\label{overflow}}\n"
fileTex.write(captionLine)
fileTex.write("\end{figure}\n\n")

#Figure 3
fileTex.write("\\begin{figure}[H]\n")
fileTex.write("\centering\n")
nextLine="\includegraphics[width=90mm]{"+fileCAR+"}\n"
fileTex.write(nextLine)
captionLine="\caption{Cumulative Attack Rate for "+mydataType+" "+myName+" for the "+myFY+" pandemic year. The red and grey line denot the data and the MCMC fit, respectively. \label{overflow}}\n"
fileTex.write(captionLine)
fileTex.write("\end{figure}\n\n")

#Figure 4
fileTex.write("\\begin{figure}[H]\n")
fileTex.write("\centering\n")
nextLine="\includegraphics[width=90mm]{"+fileChain+"}\n"
fileTex.write(nextLine)
captionLine="\caption{The History of the MCMC chain for "+mydataType+" "+myName+". We show only the parametrs that are optimized and the resulting AICc score. The yscale for the parameters is set to the allowed min/max values in the sampling procedure. \label{overflow}}\n"
fileTex.write(captionLine)
fileTex.write("\end{figure}\n\n")


# START WORKING ON TABLE 1
temp = scanCsv(fileStats,"rb")
rowNum = temp[0]
colNum = temp[1]

titleText="MCMC Statistics"
captionText="Mean, standard deviation (SD), naive standard error of the mean (ignoring autocorrelation of the chain) and time-series standard error based on the MCMC chain. All the parameters that the p-medds package can optimize are shown.  Any parameter set to zero is not included in this particular run. Any parameter with zero SD was included but not optimized"

csvToLatex(fileTex,rowNum, colNum,fileStats, titleText,captionText,"rb")
#Table 2
temp = scanCsv(fileQuant,"rb")
rowNum = temp[0]
colNum = temp[1]

titleText="MCMC Quantiles"
captionText="The vector of quantiles for the same parameters as in the previouse table.  Any parameter set to zero was not inlcuded in the run and parameters that were not optimized have no variation in value"

csvToLatex(fileTex,rowNum, colNum,fileQuant, titleText, captionText,"rb")
#Condensed version of Tables 1 and 2
temp = scanCsv(fileTbl,"rb")
captionText=" Mean, SD and quantiles for the parameters that were optimized in this run."
rowNum = temp[0]
colNum = temp[1]

titleText="MCMC Statistics"
csvToLatex(fileTex,rowNum, colNum,fileTbl, titleText, captionText,"rb")


#References

fileTex.write("{\\bf References}\n")
fileTex.write("\\bibliographystyle{plos2009}\n")
fileTex.write("{\\def\section*#1{}\n\\bibliography{/templates/riley-epi-refs-v2,/templates/ste_refs}\n}")

#\bibliographystyle{plos2009}
#{\def\section*#1{}
#\bibliography{/Users/pete/Dropbox/manuscripts/references/riley-epi-refs-v2,/Users/pete/Dropbox/manuscripts/references/ste_refs}
#}
fileTex.write("\n\end{document}\n")

fileTex.close()


