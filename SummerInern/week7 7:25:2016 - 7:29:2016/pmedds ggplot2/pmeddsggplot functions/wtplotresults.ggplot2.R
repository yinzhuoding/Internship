library(pmedds.core)
library(ggplot2)
library(reshape2)
library(gtable)
library(grid)
library(gridExtra)

#### Example ####
rm(list = ls())
load(file = "pmedds_WT_plot_inputs.RData")
logfile = file(description="log-wt.txt",open="wt")
subDir = file.path(getwd(), "output")
success <- wt.plot.results.ggplot2(mycountry=mydata$mycountry,cases.order=cases.order,Rlq.order=Rlq.order,Rlm.order=Rlm.order,Rlmd.order=Rlmd.order,dates=dates.vec,device=device,log=logfile,subDir=subDir)
#################


wt.plot.results.ggplot2 <- function(mycountry=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,device="X11",log=NULL,subDir=file.path(getwd(), "output")) {
  # check to see if "data" sub-directory exists, if not create it
  if (!dir.exists(subDir)) {
    dir.create(file.path(subDir))
    cat(" Created ", subDir, "Directory for all the Data of the W-T Data \n")
    text <- paste(" Created ", subDir, " Directory for all the W-T Data","\n",sep="")
    writeLines(text=text,con=log)
  }	
  
  if (device == "PDF" | device == "pdf") {
    pdfname = paste(subDir,"/W_T_",mycountry,".pdf",sep="")
    pdf(file=pdfname,onefile=TRUE,paper="a4r",width=11.5,height=8)
    cat(" Plotting Results to: ",pdfname,"\n")
    text <- paste(" Plotting Results to: ",pdfname,"\n",sep="")
    writeLines(text=text,con=log)
  }
  
  if (mycountry == "China") {
    title=paste("Probable cases of SARS by date of report - ",mycountry,sep="")
  } else {
    title=paste("Probable cases of SARS by date of onset - ",mycountry,sep="")
  }
  
  breaks = cases.order[1,]
  labels = as.character(dates[cases.order[1,]])
  xmin = min(cases.order[1,]) - 1
  xmax = max(cases.order[1,]) + 1
  p1 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(xmin,xmax),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "Cases") +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10, angle = 90),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"))
  p1 = p1 + geom_linerange(aes(x = cases.order[1,],ymax = cases.order[2,]), ymin = 0, col = "deeppink")
  
  
  title="Estimated daily reproduction number (95% CI, mean-red and median-green)"
  p2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(xmin,xmax),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "Reproduction Number",limits = c(0,14)) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10, angle = 90),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"))
  
  ymax = Rlq.order[2,]
  for(i in 1:length(ymax)) {
    ymax[i] = min(14,ymax[i])
  }
  p2 = p2 + geom_linerange(aes(x = Rlq.order[1,],ymax = ymax), ymin = 0, col = "mediumpurple") +
    geom_point(aes(x = Rlm.order[1,], y = Rlm.order[2,]), col = "deeppink", alpha = 0.7, na.rm = TRUE) +
    geom_point(aes(x = Rlmd.order[1,], y = Rlmd.order[2,]), col = "forestgreen", alpha = 0.7,na.rm = TRUE) +
    geom_hline(yintercept = 1, col = "grey")
  
  grid.arrange(p1,p2,nrow = 2)
  
  if (device == "PDF" | device == "pdf") dev.off()
 
}