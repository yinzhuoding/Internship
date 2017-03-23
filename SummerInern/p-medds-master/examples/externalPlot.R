
plot.input.data.external <- function (myName, FY, nweeksFit, nweeksData, week, epi ,sh, school, device.name, log, subDir = file.path(getwd(), "output")) 
{
  # use this function to create your own plot of the input data
}

plot.mcmc.chain.external <- function(tab, model, ireal, myName, device.name, log, subDir = file.path(getwd(), "output"))
{
 # use this function to create your own plot of the chain data
}

plot.car.mcmc.external <- function(myName, job.year, imodel, nweeksFit, nweeksData, weeks, epi, dsdt, rnd.dsdt, device.name, log, subDir = file.path(getwd(), "output")) 
{
 # use this function to create your own plot of the car data
}

plot.results.mcmc.external <- function(myName, job.year, imodel, nweeksFit, nweeksData, weeks, epi, sh, school,dsdt, roft,rnd.dsdt, rnd.rvec, boxplot.data, device.name, log, subDir = file.path(getwd(), "output"))
{
  # use this function to create your own plot of the results data
}


wt.plot.results.external <- function(mycountry=NULL,cases.order=NULL,Rlq.order=NULL,Rlm.order=NULL,Rlmd.order=NULL,dates=NULL,device="X11",log=NULL,subDir=file.path(getwd(), "output")) {
  # use this function to create your own plot of the WT results data
}
