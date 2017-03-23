library(pmedds.core)
library(ggplot2)
library(reshape2)
library(gtable)
library(grid)
rm(list = ls())
load(file = "pmedds_plot_inputs.RData")
logfile = file(description="log.txt",open="wt")
subDir = file.path(getwd(), "output")

myName=mydata$dataName
job.year=job.year
imodel=ptab$model
weeks=mydata$weeks
epi=mydata$cases
dsdt=dsdt
device.name=ptab$device
log=logfile
subDir = subDir
nweeksFit = 50
nweeksData=52


plot.car.mcmc(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,nweeksFit=50,nweeksData=mydata$nweeksData,weeks=mydata$weeks,epi=mydata$cases,dsdt=dsdt[,iBest],rnd.dsdt=rnd.dsdt,device.name=ptab$device,log=logfile, subDir = outputDir)


plot.car.mcmc.ggplot2 <- function(myName="",job.year=2009,imodel=5,nweeksFit=NULL,nweeksData=52,weeks=NULL,epi=NULL,dsdt=NULL,rnd.dsdt=NULL,device.name="pdf",log=NULL, subDir = file(getwd(), "output")) {
  nweeks <- length(weeks)
  if (is.null(nweeksFit)) nweeksFit=nweeks
  
  title =title =paste(" Cumulative Attack Rate - ",myName,sep="")
  
  # check to see if "data" sub-directory exists, if not create it
  if (!dir.exists(subDir)) {
    dir.create(file.path(subDir))
    cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
    text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
    writeLines(text=text,con=log)
  }
  
  fbase <- paste(subDir,"/car-",myName,sep="")
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname <- paste(fbase,".pdf",sep="")
    pdf(file=fname,width=9,height=7)
    cat("\n PDF Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n")
    text <- paste(" PDF Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  }  else {
    dev.next()
    dev.new()		
  }
  
  #par(mfcol=c(1,1),mar=c(5,5,5,5))
  cumsum.epi <- cumsum(epi)
  # For the current season if the number of data points is less than the season duration
  if (nweeksData < nweeks) cumsum.epi[(nweeksData+1):nweeks] = NA
  
  rnd.reals = dim(rnd.dsdt)[2]
  cumsum <- matrix(data = 0.0, nrow = unlist(nweeks), ncol = unlist(rnd.reals))
  for (i in 1:rnd.reals) {
    cumsum[,i] <- cumsum(rnd.dsdt[,i])
  }
  ylim <- c(0,max(cumsum.epi,cumsum,na.rm=TRUE))
  xtitle = paste("Week Number FY ",job.year,"-",job.year+1,sep="")
  
  breaks = seq(1,nweeks,4)
  labels = weeks[breaks]
  p = ggplot(data = NULL) + 
    scale_x_continuous(name = xtitle, limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "C.A.R.", limits =ylim) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  #grey are the random selections from this best chain
  #blue is the best result from the best chain
  #red is the c.a.r. of the EPI profile we fitted
  rnd.reals = dim(rnd.dsdt)[2]
  step = max(1,rnd.reals/100)
  ix.set = seq(from = 1, to = rnd.reals, by = step)
  dat.rnd = cumsum[1:nweeksFit,ix.set]
  dat.rnd.pred = cumsum[nweeksFit:nweeks,ix.set]
  data.rnd = melt(dat.rnd)
  data.rnd.pred = melt(dat.rnd.pred)

  p = p + 
    geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "grey", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = cumsum(dsdt[1:nweeksFit])), col = "royalblue", size = 1) +
    geom_line(aes(x = 1:nweeksFit, y = cumsum.epi[1:nweeksFit]), col = "violetred", size = 1)
    
  
  if (nweeksFit < nweeks) {
    p = p +
      geom_line(aes(x = (data.rnd.pred[,1] + nweeksFit - 1), y = data.rnd.pred[,3], group = data.rnd.pred[,2]), col = "grey", size = 2, linetype = 2,alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(x = 1:nweeks, y = cumsum(dsdt[1:nweeks])), col = "royalblue", size = 1, linetype = 2) +
      geom_line(aes(x = nweeksFit:nweeks, y = cumsum.epi[nweeksFit:nweeks]), col = "violetred", size = 1, linetype = 2, na.rm = TRUE) +
      geom_vline(xintercept = nweeksFit, col = "thistle2", size = 3, alpha = 0.7) 
  }

  p = p + 
    annotate("text", label = c("   --- Data","   --- Best MCMC","   --- Random MCMC"), col = c("violetred","royalblue","grey"),
           x = c(-Inf,-Inf,-Inf), y = c(Inf,Inf,Inf), hjust = c(0,0,0), vjust = c(2.5,4,5.5),
           family = "serif", size = 4.5)
  
  print(p)
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
  if (device.name=="X11") return()
  
  # now make the same plots for a PNG file 
  
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname <- paste(fbase,".png",sep="")
    png(file=fname,width=620,height=420)
    cat("\n PNG Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n")
    text <- paste(" PNG Plot of MCMC Cumulative Attack Rate written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
    
  }  else {
    dev.next()
    dev.new()		
  }
  
  
  breaks = seq(1,nweeks,4)
  labels = weeks[breaks]
  p = ggplot(data = NULL) + 
    scale_x_continuous(name = xtitle, limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "C.A.R.", limits =ylim) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  #grey are the random selections from this best chain
  #blue is the best result from the best chain
  #red is the c.a.r. of the EPI profile we fitted
  rnd.reals = dim(rnd.dsdt)[2]
  step = max(1,rnd.reals/100)
  ix.set = seq(from = 1, to = rnd.reals, by = step)
  dat.rnd = cumsum[1:nweeksFit,ix.set]
  dat.rnd.pred = cumsum[nweeksFit:nweeks,ix.set]
  data.rnd = melt(dat.rnd)
  data.rnd.pred = melt(dat.rnd.pred)
  
  p = p + 
    geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "grey", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = cumsum(dsdt[1:nweeksFit])), col = "royalblue", size = 1) +
    geom_line(aes(x = 1:nweeksFit, y = cumsum.epi[1:nweeksFit]), col = "violetred", size = 1)
  
  
  if (nweeksFit < nweeks) {
    p = p +
      geom_line(aes(x = (data.rnd.pred[,1] + nweeksFit - 1), y = data.rnd.pred[,3], group = data.rnd.pred[,2]), col = "grey", size = 2, linetype = 2,alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(x = 1:nweeks, y = cumsum(dsdt[1:nweeks])), col = "royalblue", size = 1, linetype = 2) +
      geom_line(aes(x = nweeksFit:nweeks, y = cumsum.epi[nweeksFit:nweeks]), col = "violetred", size = 1, linetype = 2, na.rm = TRUE) +
      geom_vline(xintercept = nweeksFit, col = "thistle2", size = 3, alpha = 0.7) 
  }
  
  p = p + 
    annotate("text", label = c("   --- Data","   --- Best MCMC","   --- Random MCMC"), col = c("violetred","royalblue","grey"),
             x = c(-Inf,-Inf,-Inf), y = c(Inf,Inf,Inf), hjust = c(0,0,0), vjust = c(2.5,4,5.5),
             family = "serif", size = 4.5)
  
  print(p)
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
}
