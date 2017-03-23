library(DICE)
library(simpleboot)

output = runDICE(dataType = 'cdc', year = 2015, mod_level = 2, fit_level = 3, nMCMC = 1000)
## National data
model_profile = output$model_profile
bootci = function(x) {
  boot.out = one.boot(x,mean,1000)
  boot.ci = boot.ci(boot.out,conf = 0.95,type = "perc")
  ub = boot.ci$percent[1,5]
  lb = boot.ci$percent[1,4]
  return(c(lb,ub))
}
nation.out = apply(model_profile, 2, bootci)
nation.lb = nation.out[1,]
nation.ub = nation.out[2,]
 ## Region data
profile = output$profile
mean = apply(profile,c(2,3),mean)
onset = output$rtn
region.out = apply(profile, c(2,3), bootci)
region.lb = region.out[1, , ]
region.ub = region.out[2, , ]


##### Quantile confidence interval
quanci = function(x) {
  ub = quantile(x, 0.975)
  lb = quantile(x, 0.025)
  return(c(lb,ub))
}
profile = output$profile
onset = output$rtn
quan.out = apply(profile,c(2,3),quanci)
region.lb = quan.out[1, , ]
region.ub  =quan.out[2, , ]
