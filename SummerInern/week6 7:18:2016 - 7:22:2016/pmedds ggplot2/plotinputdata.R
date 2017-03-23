library(pmedds.core)
library(ggplot2)
library(gtable)
library(grid)
rm(list = ls())
mydata = get.data()
load(file = "pmedds_plot_inputs.RData")
logfile = file(description="log.txt",open="wt")

### Testing this function - call:
#plot.input.data.ggplot2(myName=mydata$dataName,FY=mydata$FY,nweeksFit=mydata$nweeksFit,nweeksData=mydata$nweeks,week=mydata$weeks,epi=mydata$cases,sh=mydata$sh,school=mydata$school,log=logfile,device.name = "pdf")


plot.input.data.ggplot2 <- function(myName=NULL,FY=NULL,nweeksFit=NULL,nweeksData=NULL,week=NULL,epi=NULL,sh=NULL,school=NULL,device.name="X11",log=NULL, subDir = file.path(getwd(), "output")) {
  #replace the zero's in school data with NA - better for plotting
  school[school == 0] <- NA
  school[!is.na(school)] <- 1
  nweeks <- length(week)
  if(is.null(nweeksFit)) nweeksFit = nweeks
  
  # check to see if "data" sub-directory exists, if not create it
  if (!dir.exists(subDir)) {
    dir.create(file.path(subDir))
    cat(" Created ", subDir, "Directory for all the Data of MCMC chain \n")
    text <- paste(" Created ", subDir, "Directory for all the Data of MCMC chain","\n",sep="")
    writeLines(text=text,con=log)
  }
  if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") {
    fname <- paste(subDir,"/",myName,"-epi-sh-school.pdf",sep="")
    pdf(file=fname,width=9,height=6)
    cat("\n\n For a PDF Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n\n")
    text <- paste(" For a PDF Plot of Incidence Profile, Specific Humidity and School Closure See: ",fname,"\n",sep="")
    writeLines(text=text,con=log)
  } else {
    dev.next()
    dev.new(width=9,height=6)	
  }
  if (nweeksData < nweeks) epi[(nweeksData+1):nweeks] = NA
  
  title = myName
  xlab = paste("Week FY ",FY,sep="")

  ymax = max(epi[1:nweeks])
  breaks = seq(1,nweeks,4)
  labels = week[breaks]
  p1 = ggplot(data = NULL) + 
    scale_x_continuous(name = xlab, limits = c(1,nweeks),
                       breaks = breaks, labels = labels) +
    scale_y_continuous(name = "Incidence", limits = c(0, ymax)) +
    ggtitle(title) +
    theme_bw() + 
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", color = "violetred", size = 10),
          axis.title.x = element_text(face = "plain", size = 12, family = "serif"),
          axis.title.y = element_text(face = "plain", color = "violetred",size = 12, family = "serif"),
          plot.title = element_text(face = "bold", size = 15, family = "Times"),
          plot.margin = unit(c(1,2,1,1), "cm"))
  
  p1 = p1 + 
    geom_line(aes(x = 1:nweeksFit, y = epi[1:nweeksFit]), col = "violetred", size = 1)
  if (nweeksFit < nweeks) {
    p1 = p1 +
      geom_rect(aes(xmin = nweeksFit, xmax = nweeks, ymin = 0, ymax = ymax), 
                fill = "thistle2", alpha = 0.7) +
      geom_line(aes(x = nweeksFit:nweeks, y = epi[(nweeksFit):nweeks]), col = "violetred", size = 1, linetype = 2)
  }
  
  if (all(is.na(school))) {
    p1 = p1 + 
      annotate("text", label = c("   --- ILI","   --- SH"), col = c("violetred","royalblue"),
               x = c(-Inf,-Inf), y = c(Inf,Inf), hjust = c(0,0), vjust = c(2.5,5),
               family = "serif", size = 4.5)
  } else {
    p1 = p1 + 
      annotate("text", label = c("   --- ILI","   --- SH","   --- School"), col = c("violetred","royalblue","turquoise"),
               x = c(-Inf,-Inf,-Inf), y = c(Inf,Inf,Inf), hjust = c(0,0,0), vjust = c(2.5,4,5.5),
               family = "serif", size = 4.5)
  }
  
  factor <- max(epi,na.rm=TRUE)/2
  if (all(is.na(school))) { 
    cat("\n\n\ No school closure information for this Location\n\n")
    text <-paste("No school closure information for this Location")
    if(!is.null(log)) {
      writeLines(text=text,con=log)
    }
  } else {
    p1 = p1 + 
      geom_point(aes(x = 1:length(school*factor), y = school*factor), pch = 20, col = "turquoise",size = 4, na.rm = TRUE)
  }


  p2 = ggplot(data = NULL) + 
    scale_x_continuous(name = "",limits = c(1,nweeks)) +
    scale_y_continuous(name = "Specific Humidity (kg/kg)",
                       limits = c(min(sh), max(sh))) +
    theme_bw() %+replace%
    theme(panel.background = element_rect(fill = NA)) +
    theme(text = element_text(color = "grey10"),
          axis.text.x = element_text(face = "italic", size = 10),
          axis.text.y = element_text(face = "italic", color = "royalblue", size = 10),
          axis.title.y = element_text(face = "plain", color = "royalblue",size = 12, family = "serif"),
          plot.margin = unit(c(1,2,1,1), "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  p2 = p2 + 
    geom_line(aes(x = 1:nweeksFit, y = sh[1:nweeksFit]), col = "royalblue", size = 1, na.rm = TRUE)
  if (nweeksFit < nweeks) {
    p2 = p2 + 
      geom_line(aes(x = nweeksFit:nweeks, y = sh[(nweeksFit):nweeks]), col = "royalblue", size = 1, linetype = 2)
  }
  
  ## Draw p1 and p2 at the same layer
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

  grid.draw(plot)
  
  if (device.name == "pdf" | device.name == "PDF" | device.name == "png" | device.name == "PNG") 
    dev.off()
  if (device.name == "X11") 
    return()
}