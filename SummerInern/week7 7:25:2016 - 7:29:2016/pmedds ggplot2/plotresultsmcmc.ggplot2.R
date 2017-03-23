library(pmedds.core)
library(ggplot2)
library(reshape2)
library(gtable)
library(grid)
library(gridExtra)

#### Example ####
rm(list = ls())
load(file = "pmedds_plot_inputs.RData")
logfile = file(description="log.txt",open="wt")
subDir = file.path(getwd(), "output")

plot.results.mcmc.ggplot2(myName=mydata$dataName,job.year=job.year,imodel=ptab$model,
                          nweeksFit = 45,nweeksData = mydata$nweeksData,
                          weeks=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,
                          dsdt=dsdt,roft=rmat,rnd.dsdt=rnd.dsdt,rnd.rvec=rnd.rvec,
                          boxplot.data = boxplot.data,device.name=ptab$device,log=logfile)
#################

plot.results.mcmc.ggplot2 <- function(myName="",job.year=2009,imodel=5,weeks=NULL,nweeksFit=nweeksFit,nweeksData=52,epi=NULL,sh=NULL,school=NULL,dsdt=NULL,roft=NULL,rnd.dsdt=NULL,rnd.rvec=NULL,boxplot.data=NULL,device.name="X11",log=log, subDir = file.path(getwd(), "output")) {
  nweeks <- length(weeks)
  pC.vec=boxplot.data$pC.vec
  Roft.vec=boxplot.data$Roft.vec
  
  title =paste("Current Estimate for Profile: ",myName,sep="")
  school[school == 0] <- NA #for plotting we only show when the school is closed
  # check to see if "data" sub-directory exists, if not create it
  if (!dir.exists(subDir)) {
    dir.create(file.path(subDir))
    cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
    text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
    writeLines(text=text,con=log)
  }
  fbase <- paste(subDir,"/plot-",myName,sep="")
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname = paste(fbase,".pdf",sep="")
    pdf(file=fname,width=9,height=7)
    cat("\n PDF Plot of MCMC Profiles written to: ",fname,"\n")
    text <- paste("\n PDF Plot of MCMC Profiles written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  }  else {
    dev.next()
    dev.new()		
  }
  
  # In case the data length is shorter than the number of weeks we have - it means that it was padded with zero's and we now want to put back NA instead of these zeros
  if (nweeksData < nweeks) 
    epi[(nweeksData+1):nweeks] = NA
  
  ylim <-  range(epi,dsdt,rnd.dsdt,na.rm=TRUE) #c(0,max(epi,dsdt,rnd.dsdt,na.rm=TRUE))
  xlab.title = paste("Week Number FY ",job.year,"-",(job.year+1),sep="")
  
  ymax = max(epi[1:nweeks])
  breaks = seq(1,nweeks,4)
  labels = weeks[breaks]
  p1 = ggplot(data = NULL) + 
    scale_x_continuous(name = xlab.title, limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "Incidence", limits = c(0, ymax)) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  #In grey we plot 100 random estimates from the best chain
  #In Blue we plot the best estimate from the best chain
  #Red is the EPI profile we are fitting
  irnd <- dim(rnd.dsdt)[2]
  step = max(1,irnd/100)
  ix.set = seq(from = 1, to = irnd, by = step)
  dat.rnd = rnd.dsdt[1:nweeksFit,ix.set]
  dat.rnd.pred = rnd.dsdt[nweeksFit:nweeks,ix.set]
  data.rnd = melt(dat.rnd)
  data.rnd.pred = melt(dat.rnd.pred)
  
  p1 = p1 + 
    geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "grey", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = epi[1:nweeksFit]), col = "violetred", size = 1) +
    geom_line(aes(x = 1:nweeksFit, y = dsdt[1:nweeksFit]), col = "royalblue", size = 1) 
   
  if (nweeksFit < nweeks) {
    p1 = p1 +
      geom_line(aes(x = (data.rnd.pred[,1] + nweeksFit - 1), y = data.rnd.pred[,3], group = data.rnd.pred[,2]), col = "grey", size = 2, linetype = 2,alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(x = nweeksFit:nweeks, y = epi[nweeksFit:nweeks]), col = "violetred", size = 1, linetype = 2) +
      geom_line(aes(x = nweeksFit:nweeks, y = dsdt[nweeksFit:nweeks]), col = "royalblue", size = 1, linetype = 2)
  }
  
  # for models that take the school schedule into account we will plot it
  if (imodel == 1 | imodel == 3) {
    factor <- 0.5 * max(epi,dsdt,na.rm=TRUE)
    p1 = p1 + 
      geom_point(aes(x = 1:nweeks, y = school*factor), pch = 20, col = "turquoise",size = 4, na.rm = TRUE)
  }
  p1 = p1 +
    annotate("text", label = c("   --- Data","   --- Best MCMC","   --- Random MCMC"), col = c("violetred","royalblue","grey"),
             x = c(-Inf,-Inf,-Inf), y = c(Inf,Inf,Inf), hjust = c(0,0,0), vjust = c(2.5,4,5.5),
             family = "serif", size = 4.5)

  R.ylim=c(0,4)
  R.ylim=c(min(roft,rnd.rvec,na.rm=TRUE)*0.8,max(roft,rnd.rvec,na.rm=TRUE)*1.2)

  p2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "",limits = c(1,nweeks)) +
    scale_y_continuous(name = "R(t)", limits = R.ylim) +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", color = "lightgreen", size = 10),
          axis.title.y = element_text(face = "plain", color = "lightgreen",size = 12, family = "serif"),
          plot.margin = unit(c(1,1,1,1), "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  dat_rnd = rnd.rvec[1:nweeksFit,ix.set]
  dat_rnd_pred = rnd.rvec[nweeksFit:nweeks,ix.set]
  data_rnd = melt(dat_rnd)
  data_rnd_pred = melt(dat_rnd_pred)
  p2 = p2 + 
    geom_line(aes(x = data_rnd[,1], y = data_rnd[,3], group = data_rnd[,2]), col = "lightgreen", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = roft[1:nweeksFit]), col = "forestgreen", size = 1)

  if (nweeksFit < nweeks) {
    p2 = p2 +
      geom_line(aes(x = (data_rnd_pred[,1] + nweeksFit - 1), y = data_rnd_pred[,3], group = data_rnd_pred[,2]), col = "lightgreen", size = 2, linetype = 2,alpha = 0.2, na.rm = TRUE) +
      geom_line(aes(x = nweeksFit:nweeks, y = roft[nweeksFit:nweeks]), col = "forestgreen", size = 1, linetype = 2) +
      geom_vline(xintercept = nweeksFit, col = "thistle2", size = 3) 
  }
  p2 = p2 +
    annotate("text", label = c("--- Best MCMC   ","--- Random MCMC   "), col = c("forestgreen","lightgreen"),
             x = c(Inf,Inf), y = c(Inf,Inf), hjust = c(1,1), vjust = c(2.5,4),
             family = "serif", size = 4.5)
  
  g1 = ggplot_gtable(ggplot_build(p1))
  g2 = ggplot_gtable(ggplot_build(p2))
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  plot = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  ia = which(g2$layout$name == "axis-l")
  ga = g2$grobs[[ia]]
  ax = ga$children[[2]]
  ax$widths = rev(ax$widths)
  ax$grobs = rev(ax$grobs)
  
  plot = gtable_add_cols(plot, g2$widths[g2$layout[ia, ]$l], length(plot$widths) - 1)
  plot = gtable_add_grob(plot, ax, pp$t, length(plot$widths) - 1, pp$b)
  plot = gtable_add_grob(plot, g2$grobs[[7]], pp$t, length(plot$widths), pp$b)
  
  bp1 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(-1,1)) +
    scale_y_continuous(name = "", limits = c(0.00001,1), trans = 'log10') +
    geom_boxplot(aes(x = 0, y = pC.vec), width = 1.5) +
    ggtitle("pC") +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(2,1,1,0), "cm")) +
    coord_fixed(ratio = 2)
  
  bp2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(-1,1)) +
    scale_y_continuous(name = "", limits = c(0.5,5)) +
    geom_boxplot(aes(x = 0, y = Roft.vec), width = 1.5) +
    ggtitle("R0") +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,2,0), "cm")) +
    coord_fixed(ratio = 2)
  
  grid.arrange(plot,bp1,bp2,ncol = 5,
               layout_matrix = cbind(c(1,1),c(1,1),c(1,1),c(1,1),c(2,3)))
  
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
  if (device.name == "X11") return()
  
  # now make the same plots for a PNG file 
  
  if (device.name == "pdf" | device.name== "PDF" | device.name=="png" | device.name=="PNG") {
    fname <- paste(fbase,".png",sep="")
    png(file=fname,width=620,height=420)
    cat("\n PNG Plot of MCMC Profiles written to: ",fname,"\n")
    text <- paste(" PNG Plot of MCMC Profiles written to: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  }  else {
    dev.next()
    dev.new()		
  }
  p1 = ggplot(data = NULL) + 
    scale_x_continuous(name = xlab.title, limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "Incidence", limits = c(0, ymax)) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"),
          plot.margin = unit(c(1,1,1,1), "cm"))
  
  #In grey we plot 100 random estimates from the best chain
  #In Blue we plot the best estimate from the best chain
  #Red is the EPI profile we are fitting
  irnd <- dim(rnd.dsdt)[2]
  step = max(1,irnd/100)
  ix.set = seq(from = 1, to = irnd, by = step)
  dat.rnd = rnd.dsdt[1:nweeksFit,ix.set]
  dat.rnd.pred = rnd.dsdt[nweeksFit:nweeks,ix.set]
  data.rnd = melt(dat.rnd)
  data.rnd.pred = melt(dat.rnd.pred)
  
  p1 = p1 + 
    geom_line(aes(x = data.rnd[,1], y = data.rnd[,3], group = data.rnd[,2]), col = "grey", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = epi[1:nweeksFit]), col = "violetred", size = 1) +
    geom_line(aes(x = 1:nweeksFit, y = dsdt[1:nweeksFit]), col = "royalblue", size = 1) 
  
  if (nweeksFit < nweeks) {
    p1 = p1 +
      geom_line(aes(x = (data.rnd.pred[,1] + nweeksFit - 1), y = data.rnd.pred[,3], group = data.rnd.pred[,2]), col = "grey", size = 2, linetype = 2,alpha = 0.4, na.rm = TRUE) +
      geom_line(aes(x = nweeksFit:nweeks, y = epi[nweeksFit:nweeks]), col = "violetred", size = 1, linetype = 2) +
      geom_line(aes(x = nweeksFit:nweeks, y = dsdt[nweeksFit:nweeks]), col = "royalblue", size = 1, linetype = 2)
  }
  
  # for models that take the school schedule into account we will plot it
  if (imodel == 1 | imodel == 3) {
    factor <- 0.5 * max(epi,dsdt,na.rm=TRUE)
    p1 = p1 + 
      geom_point(aes(x = 1:nweeks, y = school*factor), pch = 20, col = "turquoise",size = 4, na.rm = TRUE)
  }
  p1 = p1 +
    annotate("text", label = c("   --- Data","   --- Best MCMC","   --- Random MCMC"), col = c("violetred","royalblue","grey"),
             x = c(-Inf,-Inf,-Inf), y = c(Inf,Inf,Inf), hjust = c(0,0,0), vjust = c(2.5,4,5.5),
             family = "serif", size = 4.5)
  
  R.ylim=c(0,4)
  R.ylim=c(min(roft,rnd.rvec,na.rm=TRUE)*0.8,max(roft,rnd.rvec,na.rm=TRUE)*1.2)
  
  p2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "",limits = c(1,nweeks)) +
    scale_y_continuous(name = "R(t)", limits = R.ylim) +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", color = "lightgreen", size = 10),
          axis.title.y = element_text(face = "plain", color = "lightgreen",size = 12, family = "serif"),
          plot.margin = unit(c(1,1,1,1), "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  dat_rnd = rnd.rvec[1:nweeksFit,ix.set]
  dat_rnd_pred = rnd.rvec[nweeksFit:nweeks,ix.set]
  data_rnd = melt(dat_rnd)
  data_rnd_pred = melt(dat_rnd_pred)
  p2 = p2 + 
    geom_line(aes(x = data_rnd[,1], y = data_rnd[,3], group = data_rnd[,2]), col = "lightgreen", size = 2, alpha = 0.4,na.rm = TRUE) +
    geom_line(aes(x = 1:nweeksFit, y = roft[1:nweeksFit]), col = "forestgreen", size = 1)
  
  if (nweeksFit < nweeks) {
    p2 = p2 +
      geom_line(aes(x = (data_rnd_pred[,1] + nweeksFit - 1), y = data_rnd_pred[,3], group = data_rnd_pred[,2]), col = "lightgreen", size = 2, linetype = 2,alpha = 0.2, na.rm = TRUE) +
      geom_line(aes(x = nweeksFit:nweeks, y = roft[nweeksFit:nweeks]), col = "forestgreen", size = 1, linetype = 2) +
      geom_vline(xintercept = nweeksFit, col = "thistle2", size = 3) 
  }
  p2 = p2 +
    annotate("text", label = c("--- Best MCMC   ","--- Random MCMC   "), col = c("forestgreen","lightgreen"),
             x = c(Inf,Inf), y = c(Inf,Inf), hjust = c(1,1), vjust = c(2.5,4),
             family = "serif", size = 4.5)
  
  g1 = ggplot_gtable(ggplot_build(p1))
  g2 = ggplot_gtable(ggplot_build(p2))
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  plot = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  ia = which(g2$layout$name == "axis-l")
  ga = g2$grobs[[ia]]
  ax = ga$children[[2]]
  ax$widths = rev(ax$widths)
  ax$grobs = rev(ax$grobs)
  
  plot = gtable_add_cols(plot, g2$widths[g2$layout[ia, ]$l], length(plot$widths) - 1)
  plot = gtable_add_grob(plot, ax, pp$t, length(plot$widths) - 1, pp$b)
  plot = gtable_add_grob(plot, g2$grobs[[7]], pp$t, length(plot$widths), pp$b)
  
  bp1 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(-1,1)) +
    scale_y_continuous(name = "", limits = c(0.00001,1), trans = 'log10') +
    geom_boxplot(aes(x = 0, y = pC.vec), width = 1.5) +
    ggtitle("pC") +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(2,1,1,0), "cm")) +
    coord_fixed(ratio = 2)
  
  bp2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "", limits = c(-1,1)) +
    scale_y_continuous(name = "", limits = c(0.5,5)) +
    geom_boxplot(aes(x = 0, y = Roft.vec), width = 1.5) +
    ggtitle("R0") +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,2,0), "cm")) +
    coord_fixed(ratio = 2)
  
  grid.arrange(plot,bp1,bp2,ncol = 5,
               layout_matrix = cbind(c(1,1),c(1,1),c(1,1),c(1,1),c(2,3)))
  
  if (device.name == "pdf" | device.name == "PDF" | device.name=="png" | device.name=="PNG") dev.off()
  
}