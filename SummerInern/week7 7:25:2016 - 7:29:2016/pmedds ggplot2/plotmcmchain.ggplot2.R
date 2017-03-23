library(pmedds.core)
library(ggplot2)
library(reshape2)
library(gtable)
library(grid)

#### Example ####
rm(list = ls())
load(file = "pmedds_plot_inputs.RData")
logfile = file(description="log.txt",open="wt")
subDir = file.path(getwd(), "output")

plot.mcmc.chain.ggplot2(tab=sol$tab,model=model,ireal=ix,myName=mydata$dataName,device.name=ptab$device,log=logfile)
#################

plot.mcmc.chain.ggplot2 <- function(tab=NULL,model=NULL,ireal=1, myName="",device.name="X11",log=log, subDir = file.path(getwd(), "output")) {
  
  nparam  <- length(model$vecnames)
  tab <- matrix(tab,nc=(nparam+1))
  colnames(tab) <- c(model$vecnames,"AICc")
  vecnames <- model$vecnames
  vecopt   <- model$vecopt
  zipname  <- myName
  iburn    <-  1 # dim(tab)[1]/2 plot the complete chain
  
  tab.plot <- tab[iburn:dim(tab)[1],c(vecopt,"AICc")]
  nopt <- length(vecopt)
  nc = 2
  if (nopt %% 2 == 0) {
    nr = nopt/2
  } else {
    nr = (nopt+1)/2
  }
  title ="MCMC Chains"
  
  # check to see if "data" sub-directory exists, if not create it
  if (!dir.exists(subDir)) {
    dir.create(file.path(subDir))
    cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
    text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
    writeLines(text=text,con=log)
  }
  
  fbase <- file.path(subDir,paste("chain-",myName,"-",ireal,sep=""))
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname <- paste(fbase,".pdf",sep="")
    pdf(file=fname,width=9,height=7)
    cat("\n PDF Plot of MCMC Chain written to: ",fname,"\n")
    text <- paste(" PDF Plot of MCMC Chain written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  }  else {
    dev.next()
    dev.new()		
  }
  
  title = paste("MCMC Chain Number: ",ireal,sep="")
  
  plotlist = list()
  for (i in 1:(nopt+1)) {
    ylab = colnames(tab.plot)[i]
    ymin = model$model$par.min[ylab]
    ymax = model$model$par.max[ylab]
    main = ""
    if (i == 1) main=title
    plotlist[[i]] = ggplot(data = NULL) + 
      scale_x_continuous(name = "") +
      scale_y_continuous(name = ylab, limits =c(ymin,ymax)) +
      ggtitle(main) +
      theme(text = element_text(color = "grey10"),
            axis.text.x = element_text(face = "italic", size = 10),
            axis.text.y = element_text(face = "italic", size = 10),
            axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
            axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
            plot.title = element_text(face = "bold", size = 15, family = "Times")) +
      geom_point(aes_string(x = 1:length(tab.plot[,i]), y = tab.plot[,i]))
  }
  layout = matrix(seq(1,nr*nc,1), nrow = nr, byrow = FALSE)
  multiplot(plotlist = plotlist, layout = layout)
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
  if (device.name == "X11") return()
  
  #repeat with png file
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname <- paste(fbase,".png",sep="")
    png(file=fname,width=620,height=600)
    cat("\n PNG Plot of MCMC Chain written to: ",fname,"\n")
    text <- paste(" PNG Plot of MCMC Chain written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  }  else {
    dev.next()
    dev.new()		
  }
  
  plotlist = list()
  for (i in 1:(nopt+1)) {
    ylab = colnames(tab.plot)[i]
    ymin = model$model$par.min[ylab]
    ymax = model$model$par.max[ylab]
    main = ""
    if (i == 1) main=title
    plotlist[[i]] = ggplot(data = NULL) + 
      scale_x_continuous(name = "") +
      scale_y_continuous(name = ylab, limits =c(ymin,ymax)) +
      ggtitle(main) +
      theme(text = element_text(color = "grey10"),
            axis.text.x = element_text(face = "italic", size = 10),
            axis.text.y = element_text(face = "italic", size = 10),
            axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
            axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
            plot.title = element_text(face = "bold", size = 15, family = "Times")) +
      geom_point(aes_string(x = 1:length(tab.plot[,i]), y = tab.plot[,i]))
  }
  layout = matrix(seq(1,nr*nc,1), nrow = nr, byrow = FALSE)
  multiplot(plotlist = plotlist, layout = layout)	
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()	
}
