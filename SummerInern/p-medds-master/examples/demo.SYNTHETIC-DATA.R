##
## USING A SIMPLE COMPARTMENTAL SIR MODEL CREATE SYNTHETIC DATA AND THEN FIT IT
##

##
#clean all previous data and objects
rm(list=ls())

##
## turn all previous graphics windows off
graphics.off()

##
## Load the package
require(pmedds.core)

##
## Choose a population size (1e4 to 1e6 is reasonable)
##

N = 1e4

##
## Choose a value for the force of infection (recommended range 1.2-2.0)
##
R0 = 1.4

##
## Choose a value for the infectious period (in days, typically 2.6 for ili), this will not be fitted
##

Tg = 2.6

##
## Choose pC - the percent clinical (0.07-0.2 is reasonable)
##

pC = 0.1

##
## Choose a Background value (1-5 is reasonable)
##

Background = 2

##
## Choose to generate the Synthetic profile using either a stochastic or determenistic procedure. (stochastic=TRUE/FALSE, respectively)
## in case of stochastic=TRUE the code will fit the average stochastic profile, calculated using only realizations that took-off
## in case of stochastic = TRUE the number os realizations should be set to > 50. If it is FALSE this number is ignored and only a single
## determenistic profile is created
##

stochastic = FALSE #TRUE

## to run a model with a fixed value of R(t) -  set imodel = 4
## to run a model with a two value R(t) - the paper2 model set imodel = 5
## default is imodel = 4

imodel = 5


##
## In case of imodel = 5 the parameters delta, ts and dur need to be set
## delta is the change in the original value of R0 using the formula: R(t) = R0(1+delta)
## delta can be negative or positive and we recommend to choose a value between -0.2/+0.2 Default value is -0.1
## ts - the point in time that the change in R(t) begins in units of weeks. It must be greater than 10 because the epidemic starts at week 10. 
## default value is 12 weeks
## dur - the duration of time, in weeks, that the change in R(t) is effective.  We recommend 3-6 weeks. Default value is 5
## all of these three parameters are ignored if imodel is set to 4.
##

delta = -0.1
ts    = 12.
dur   = 5.

mydata = psi.sim.modelA(R0=R0,Tg=Tg,pC=pC,N=N,Background=Background,delta=delta,ts=ts,dur=dur,stochastic=stochastic,reals=100,imodel=imodel) 

##
## Fit the profile and plot the results 
##
runPMEDDS.model(mydata=mydata)




