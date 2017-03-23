## It is the simple SIS epidemic without births or deaths. 

## Parameters
# beta: Product of contact rates and transmission probability;
# gamma: Recovery Rate;
# S: Proportion of the population that are susceptible;
# I: Proportion of the population that are infectious.

library(deSolve)

SIS = function(time, state, pars) {
  with(as.list(c(state,pars)), {
    dS = gamma * I0 - beta * S0 * I0
    dI = beta * S0 * I0 - gamma * I0
    return(list(c(dS,dI)))
  })
}
yini = c(S0 = 1-1e-6,I0 = 1e-6)
pars = c(beta = 1.4247,gamma = 0.14286)                                                         
times = seq(0,70,by = 1)


out = ode(func = SIS, y = yini, parms = pars, times = times)
out = as.data.frame(out)
out$time = NULL

matplot(times,out,type = "l", xlab = "Time(days)", ylab = "Susceptibles and Infectious", main = "SIS Model",
        lwd = 2, lty = 1, col = 2:3)
legend(40,0.5,c("Susceptibles","Infecteds"),pch = 1, col = 2:3, bty = "n")