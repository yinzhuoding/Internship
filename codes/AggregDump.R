# Function to load model/prior/chain selections for CDC challenge.  Then aggregates the 'dump' info to a single 'dump' structure.
#    - 'dump' is saved to an .RData file
#    - 'dump' is also processed to the CDC submission format

# Directories

rm(list = ls())

graphics.off()

Save2Rep = TRUE  # If TRUE dump and pdf are saved directly to the local repository (potentially overwriting the current file(s)).  If FALSE, files are written to the working directory.
pdf = TRUE #or FALSE, If FALSE will plot to screen

year.start = 2016
year.end = 2017

FY = paste(year.start, "-", year.end, sep = "")

# File locations
#RepDir      = "~/GitReps/FluFore/"
RepDir = "/Users/michal/work/DICE-util/for-2016-2017/FluFore/"
TemplateDir = paste0(RepDir,"CDC",FY,"/")
MainDataDir = paste0("~/work/dice-results/", FY, "-season/data/")
#MainDataDir = "~/Dropbox/Share/2016/"


## 
## Create or read in an aggregate dump

createDump = TRUE # if FALSE will need a filename for reading in an aggregate dump file , see below circa line 47


library(DICE)

mod_level = 2
fit_level = 3
dataType = "cdc"

mydata = get.subset(mod_level = mod_level, fit_level = fit_level, dataType = dataType, start.year = year.start, end.year = year.end)

nweeksData = mydata$nweeksData

nweeksFit = nweeksData

nweeks = mydata$nweeks

sh = mydata$fit$sh

school = mydata$fit$school

onset = mydata$fit$onset

nregion = mydata$fit$nregion

nregion1 = nregion + 1

if (!createDump) inputfile = paste0(getwd(), "/CDC-Flu-Challenge-", FY, "-nweeks-", nweeksFit, "-AggregateProfile.RData")

##
## Start creating the dump

if (createDump) {
	
# coupled = FALSE

# Load selections from .csv
select = read.csv(paste0(TemplateDir,"weekly_selections/FluChallenge_Selection_nweeks-", nweeksData, ".csv"))

select$coupled = as.logical(select$coupled)

for (ii in 1:10) {
	region = select$region[ii]

	# Create Directory and File names
	if (select$prior[ii] == 0) {
	  subDir2 <- paste0("prior", 0, "-nweeks-", nweeksFit, "/")
	} else if (select$prior[ii] == 1) {
	  subDir2 <- paste0("prior", 1, "-nweeks-", nweeksFit, "/")	
	} else if (select$prior[ii] == 3) {
	  subDir2 <- paste0("prior", 3, "-nweeks-", nweeksFit, "/")
	}
	else if (select$prior[ii] == 2) {
	  subDir2 <- paste0("prior", 2, "-sig", 10, "-nweeks-", nweeksFit, "/")
	}
	else if (select$prior[ii] == 4) { # A quck way to get the heated augmented to work..
	  subDir2 <- paste0("prior", 3, "-sig", 10, "-nweeks-", nweeksFit, "/")
	}
	
	coupled = select$coupled[ii]
	if (coupled) {
	  subDir1 = paste0(dataType,"-cpl-")
	} else {
	  subDir1 = paste0(dataType,"-uncpl-")
	}
		
	subDir = paste0(subDir1,subDir2)
	
	dataName = paste0(dataType, "-", mydata$model$name)
	if (!coupled) {
		filename = paste0("profiles-", dataName, "-uncpl-", FY, "-", select$model[ii], "-", nweeksFit, "-", select$chain[ii], ".RData")
		} else {
		filename = paste0("profiles-cdc-United.States-cpl-", FY, "-", select$model[ii], "-", nweeksFit, "-", select$chain[ii],".RData")
	}
	cat("Processing Region: ", region, "\n")
	cat('Using File: ',filename,'\n')

	# Load 'profiles' .RData file
	load(paste0(MainDataDir, subDir, filename))

	# if first region, initialize data structure
	if (region == select$region[1]) {
		out = dump
	}

	out$rtn_ili[, ii] = dump$rtn_ili[, ii]
	out$profile_ili[, , ii] = dump$profile_ili[, , ii]

} #End of loop over regions 

##
## Dictionary
## data: fit_ili, model_ili
## fit:   rtn_ili, profile_ili
## model direct: model_rtn_ili, model_mean,   model_profile_ili
## model indirect: fit_model, fit_model_mean, fit_model_profile

# Combine the region fits to produce a national profile and place in 'direct fit' dump-variables
# We will also place it in the 'indirect' dump variable 

if (any(is.na(select[select$region=="national",c("model","prior","chain")]))) {
  cat("Aggregating National Profile Using Regional Selections\n")
  out$model_rtn_ili = out$rtn_ili %*% dump$fit_coef
  out$fit_model = out$rtn_ili %*% dump$fit_coef
  for (ii in 1:dim(out$profile_ili)[1]) {
    out$model_profile_ili[ii, ] = out$profile_ili[ii, , ] %*% dump$fit_coef
    out$fit_model_profile[ii, ] = out$profile_ili[ii, , ] %*% dump$fit_coef
  }
  
  for (iweek in 1:nweeks) {
    out$model_mean[iweek] = mean(out$model_profile_ili[, iweek])
    out$fit_model_mean[iweek] = mean(out$fit_model_profile[, iweek])
  }
} else {
  ## Extract the national info from the appropriate profile*.RData file
  # Create Directory and File names

  if (select$prior[select$region=="national"] == 0) {
    subDir2 <- paste0("prior", 0, "-nweeks-", nweeksFit, "/")
  } else if (select$prior[select$region=="national"] == 1) {
    subDir2 <- paste0("prior", 1, "-nweeks-", nweeksFit, "/")	
  } else if (select$prior[select$region=="national"] == 3) {
    subDir2 <- paste0("prior", 3, "-nweeks-", nweeksFit, "/")
  }
  else if (select$prior[select$region=="national"] == 2) {
    subDir2 <- paste0("prior", 2, "-sig", 10, "-nweeks-", nweeksFit, "/")
  }
  else if (select$prior[select$region=="national"] == 4) { # A quck way to get the heated augmented to work..
    subDir2 <- paste0("prior", 3, "-sig", 10, "-nweeks-", nweeksFit, "/")
  }
  
  coupled = select$coupled[select$region=="national"]
  dataName = paste0(dataType, "-", mydata$model$name)
  if (!coupled) {
    subDir1 = paste0(dataType,"-uncpl-")
    filename = paste0("profiles-", dataName, "-uncpl-", FY, "-", select$model[select$region=="national"], "-", nweeksFit, "-", select$chain[select$region=="national"], ".RData")
  } else {
    subDir1 = paste0(dataType,"-cpl-")
    filename = paste0("profiles-cdc-United.States-cpl-", FY, "-", select$model[select$region=="national"], "-", nweeksFit, "-", select$chain[select$region=="national"],".RData")
  }
  subDir = paste0(subDir1,subDir2)
  cat("Processing coupled National profile \n")
  
  # Load 'profiles' .RData file
  load(paste0(MainDataDir, subDir, filename))
  
  if (coupled) {
    out$model_rtn_ili = dump$fit_model
    out$model_mean = dump$fit_model_mean
    out$model_profile_ili[, ] = dump$fit_model_profile
  } else {
    out$model_rtn_ili = dump$model_rtn_ili
    out$model_mean = dump$model_mean
    out$model_profile_ili[, ] = dump$model_profile_ili
  }
  # remove extraneous national profile structures
  out$fit_model = NULL
  out$fit_model_mean = NULL
  out$fit_model_profile = NULL

}

##
## 
# create unique name for flu challenge-submission .RData file and write it to disk
  if (Save2Rep) {
    outfile = paste0(RepDir,"CDC",FY,"/weekly_dump_files/CDC-Flu-Challenge-", FY, "-week-", nweeksFit, "-AggregateProfile.RData")
  } else {
    outfile = paste0(getwd(), "/CDC-Flu-Challenge-", FY, "-week-", nweeksFit, "-AggregateProfile.RData")
  }
	
	dump = out
	save(dump, file = outfile)
	cat("Aggregate dump file saved to: ",outfile)

} else {
	
	cat("\n\n Loading Aggregate Dump File: ",inputfile,'\n\n')
	
	load(file = inputfile )	
}


## Let's plot what we have 

model_profile_ili = dump$model_profile_ili
model_rtn_ili = dump$model_rtn_ili
fit_model_profile = dump$model_profile_ili
fit_model_mean = dump$model_mean
fit_model = dump$model_rtn_ili

profile_ili = dump$profile_ili
rtn_ili = dump$rtn_ili

##
## This is data
model_ili = dump$model_ili
fit_ili = dump$fit_ili

fit_onset = dump$fit_onset
model_onset = dump$model_onset
fit_coef = dump$fit_coef

## End data 


reg.fit.name = mydata$fit$name
reg.model.name = mydata$model$name

tps = mydata$weeks
weeks = mydata$weeks


### Begin Plotting Section

nRnd = dim(profile_ili)[1]

colnames(fit_ili) = mydata$fit$attr$NAME_3
colnames(rtn_ili) = mydata$fit$attr$NAME_3


if (pdf == TRUE) {
  if (Save2Rep) {
    filename = paste0(RepDir,"CDC",FY,"/weekly_plots/submission-plot-EW-", weeks[nweeksFit], ".pdf")
  } else {
    filename = paste0(getwd(),"/submission-plot-EW-", weeks[nweeksFit], ".pdf")
  }
	
	pdf(file = filename, width = 15, height = 11)
	cat("\nFor a Plot of Proposed Submission See: ",filename,'\n')
}



## Plot one region at a time



par(mfrow = c(3, 4), mar = c(4, 4, 1, 1))



for (i in 1:nregion) {
  
  
  
  profile.mean = rep(NA, nweeks)
  
  
  
  for (j in 1:nweeks) profile.mean[j] = mean(profile_ili[, j, i])
  
  
  
  ymax = max(fit_ili[1:nweeks, i], rtn_ili[1:nweeks, i], profile_ili[, 1:nweeks, i], unlist(fit_onset[i]), na.rm = TRUE)
  
  
  
  plot(1:nweeks, fit_ili[1:nweeks, i], type = "l", col = "red", xlim = c(1, nweeks), ylim = c(0, ymax), xaxt = "n", xlab = "EW #", ylab = "% ILI", lwd = 1, lty = 1)
  
  
  
  for (irnd in seq(from = 1, to = nRnd, by = 10)) {
    
    lines(1:nweeksFit, profile_ili[irnd, 1:nweeksFit, i], col = "grey", lwd = 1, lty = 1)
    
    lines(nweeksFit:nweeks, profile_ili[irnd, nweeksFit:nweeks, i], col = "grey", lwd = 1, lty = 2)
    
  }
  
  
  
  #lines(nweeksFit:nweeks, rtn_ili[nweeksFit:nweeks, i], type = "l", col = "blue", lwd = 1, lty = 2)
  
  
  
  #lines(1:nweeksFit, rtn_ili[1:nweeksFit, i], type = "l", col = "blue", lwd = 1, lty = 1)
  
  
  
  lines(1:nweeksData, profile.mean[1:nweeksData], type = "l", col = "purple", lwd = 1, lty = 1)
  
  
  
  lines(nweeksData:nweeks, profile.mean[(nweeksData):nweeks], type = "l", col = "purple", lwd = 1, lty = 2)
  
  
  
  lines(1:nweeksFit, fit_ili[1:nweeksFit, i], type = "l", col = "red", lwd = 2)
  
  
  if (length(fit_onset[i]) > 0) 
    
    abline(h = fit_onset[i], col = "black", lty = 2, lwd = 1)
  
  
  
  rect(xleft = nweeksFit, ybottom = 0, xright = (nweeksFit + 4), ytop = (ymax * 1.2), col = rgb(0.1, 0.1, 0.1, alpha = 0.1), border = NA)
  
  
  
  reg.name = reg.fit.name[i]
  
  rel.pop = fit_coef[i]
  
  rel.pop = round(rel.pop, digits = 3)
  
  legend("topleft", c(reg.name, rel.pop), text.col = c("black", "black"), bty = "n")
  
  
  
  axis(1, at = 1:nweeks, label = weeks)
  
  
  
  school = mydata$fit$school[, i]
  
  school[school == 0] = NA
  
  par(new = TRUE)
  
  plot(school, ylim = c(0, 6), xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "p", pch = 22, lwd=2)
  
  
  
  sh = mydata$fit$sh[, i]
  
  par(new = TRUE)
  
  plot(sh, xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "l", lwd = 1)
  
  
  
  pk_wk_best = weeks[which.max(rtn_ili[, i])]
  
  pk_val_best = round(max(rtn_ili[, i]), digits = 1)
  
  pk_best = paste0("P-WK ", pk_wk_best, "/P-Val ", pk_val_best, "%")
  
  
  
  profile_pk_wk = rep(0, nRnd)
  
  for (j in 1:nRnd) profile_pk_wk[j] = which.max(profile_ili[j, , i])
  
  groups = cut(profile_pk_wk, breaks = 1:nweeks, right = FALSE)
  
  freq = as.data.frame(table(groups))
  
  ind = which.max(freq$Freq)
  
  pk_wk_prfl = weeks[ind]
  
  pk_val_prfl = max(profile.mean)
  
  pk_val_prfl = round(pk_val_prfl,digits=1)
  
  pk_mp = paste0("P-WK ", pk_wk_prfl, "/P-Val ", pk_val_prfl, "%")
  
  #legend("topright", c(pk_best, pk_mp), text.col = c("blue", "purple"), bty = "n")
  
  legend("topright", pk_mp, text.col = "purple", bty = "n")
  
  
  
}



# Plot the indirect national

ymax = max(fit_model, fit_model_mean, fit_model_profile, model_ili, na.rm = TRUE)


plot(1:nweeks, model_ili, type = "l", col = "red", xlim = c(1, nweeks), ylim = c(0, ymax), xaxt = "n", xlab = "EW #", ylab = "% ILI", lwd = 2, lty = 1)


for (irnd in seq(from = 1, to = nRnd, by = 10)) {
  
  lines(1:nweeksFit, fit_model_profile[irnd, 1:nweeksFit], col = "grey", lwd = 1, lty = 1)
  
  lines(nweeksFit:nweeks, fit_model_profile[irnd, nweeksFit:nweeks], col = "grey", lwd = 1, lty = 2)
  
}



#lines(1:nweeksFit, fit_model[1:nweeksFit], col = "blue", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 1)

#lines(nweeksFit:nweeks, fit_model[nweeksFit:nweeks], col = "blue", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 2)



lines(1:nweeksFit, fit_model_mean[1:nweeksFit], col = "purple", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 1)

lines(nweeksFit:nweeks, fit_model_mean[nweeksFit:nweeks], col = "purple", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 2)



lines(1:nweeks, model_ili, type = "l", col = "red", xaxt = "n", xlab = "", ylab = "", lwd = 2, lty = 1)

#points(1:nweeksFit, model_ili[1:nweeksFit], type = "p", col = "red", pch = 20, xaxt = "n", xlab = "", ylab = "")



if (length(model_onset) > 0) 
  
  abline(h = model_onset, col = "black", lty = 2, lwd = 1)



rect(xleft = nweeksFit, ybottom = 0, xright = (nweeksFit + 4), ytop = (ymax * 1.2), col = rgb(0.1, 0.1, 0.1, alpha = 0.1), border = NA)

reg.name = paste(mydata$model$name, c("-Data", "-Mean", "-Random"), sep = "")



legend("topleft", c(mydata$FY, reg.name), text.col = c("red", "purple", "grey"), bty = "n")



axis(1, at = 1:nweeks, label = weeks)



school = mydata$model$school

school[school == 0] = NA

par(new = TRUE)

plot(school, ylim = c(0, 6), xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "p", pch = 22, lwd = 2)





sh = mydata$model$sh

par(new = TRUE)

plot(sh, xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "l", lwd = 1)



pk_wk_best = weeks[which.max(fit_model)]

pk_val_best = round(max(fit_model), digits = 1)

pk_best = paste0("P-WK ", pk_wk_best, "/P-Val ", pk_val_best, "%")



profile_pk_wk = rep(0, nRnd)

for (j in 1:nRnd) profile_pk_wk[j] = which.max(fit_model_profile[j, ])

groups = cut(profile_pk_wk, breaks = 1:nweeks, right = FALSE)

freq = as.data.frame(table(groups))

ind = which.max(freq$Freq)

pk_wk_prfl = weeks[ind]

pk_val_prfl = max(fit_model_mean)

pk_val_prfl = round(pk_val_prfl,digits=1)

pk_mp = paste0("P-WK ", pk_wk_prfl, "/P-Val ", pk_val_prfl, "%")

#legend("topright", c(pk_best, pk_mp), text.col = c("blue", "purple"), bty = "n")

legend("topright", pk_mp, text.col = "purple", bty = "n")





if (pdf == TRUE) 
  
  dev.off()



## End plotting section
##------------------------------------


# ## Plot one region at a time
# 
# par(mfrow = c(3, 4), mar = c(4, 4, 1, 1))
# 
# for (i in 1:nregion) {
# 
# 	profile.mean = rep(NA, nweeks)
# 
# 	for (j in 1:nweeks) profile.mean[j] = mean(profile_ili[, j, i])
# 
# 	ymax = max(fit_ili[1:nweeks, i], rtn_ili[1:nweeks, i], profile_ili[, 1:nweeks, i], unlist(fit_onset[i]), na.rm = TRUE)
# 
# 	plot(1:nweeks, fit_ili[1:nweeks, i], type = "l", col = "red", xlim = c(1, nweeks), ylim = c(0, ymax), xaxt = "n", xlab = "EW #", ylab = "% ILI", lwd = 1, lty = 1)
# 
# 	for (irnd in seq(from = 1, to = nRnd, by = 10)) {
# 		lines(1:nweeksFit, profile_ili[irnd, 1:nweeksFit, i], col = "grey", lwd = 1, lty = 1)
# 		lines(nweeksFit:nweeks, profile_ili[irnd, nweeksFit:nweeks, i], col = "grey", lwd = 1, lty = 2)
# 	}
# 
# 	lines(nweeksFit:nweeks, rtn_ili[nweeksFit:nweeks, i], type = "l", col = "blue", lwd = 1, lty = 2)
# 
# 	lines(1:nweeksFit, rtn_ili[1:nweeksFit, i], type = "l", col = "blue", lwd = 1, lty = 1)
# 
# 	lines(1:nweeksData, profile.mean[1:nweeksData], type = "l", col = "purple", lwd = 1, lty = 1)
# 
# 	lines(nweeksData:nweeks, profile.mean[(nweeksData):nweeks], type = "l", col = "purple", lwd = 1, lty = 2)
# 
# 	lines(1:nweeksFit, fit_ili[1:nweeksFit, i], type = "l", col = "red", lwd = 2)
# 	
# 	if (length(fit_onset[i]) > 0) 
# 		abline(h = fit_onset[i], col = "black", lty = 2, lwd = 1)
# 
# 	rect(xleft = nweeksFit, ybottom = 0, xright = (nweeksFit + 4), ytop = (ymax * 1.2), col = rgb(0.1, 0.1, 0.1, alpha = 0.1), border = NA)
# 
# 	reg.name = reg.fit.name[i]
# 	rel.pop = fit_coef[i]
# 	rel.pop = round(rel.pop, digits = 3)
# 	legend("topleft", c(reg.name, rel.pop), text.col = c("black", "black"), bty = "n")
# 
# 	axis(1, at = 1:nweeks, label = weeks)
# 
# 	school = mydata$fit$school[, i]
# 	school[school == 0] = NA
# 	par(new = TRUE)
# 	plot(school, ylim = c(0, 6), xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "p", pch = 22, lwd=2)
# 
# 	sh = mydata$fit$sh[, i]
# 	par(new = TRUE)
# 	plot(sh, xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "l", lwd = 1)
# 
# 	pk_wk_best = weeks[which.max(rtn_ili[, i])]
# 	pk_val_best = round(max(rtn_ili[, i]), digits = 1)
# 	pk_best = paste0("P-WK ", pk_wk_best, "/P-Val ", pk_val_best, "%")
# 
# 	profile_pk_wk = rep(0, nRnd)
# 	for (j in 1:nRnd) profile_pk_wk[j] = which.max(profile_ili[j, , i])
# 	groups = cut(profile_pk_wk, breaks = 1:nweeks, right = FALSE)
# 	freq = as.data.frame(table(groups))
# 	ind = which.max(freq$Freq)
# 	pk_wk_prfl = weeks[ind]
# 	pk_val_prfl = max(profile.mean)
# 	pk_val_prfl = round(pk_val_prfl,digits=1)
# 	pk_mp = paste0("P-WK ", pk_wk_prfl, "/P-Val ", pk_val_prfl, "%")
# 	legend("topright", c(pk_best, pk_mp), text.col = c("blue", "purple"), bty = "n")
# 
# }
# 
# # Plot the indirect national
# 
# 
# ymax = max(fit_model, fit_model_mean, fit_model_profile, model_ili, na.rm = TRUE)
# 
# plot(1:nweeks, model_ili, type = "l", col = "red", xlim = c(1, nweeks), ylim = c(0, ymax), xaxt = "n", xlab = "EW #", ylab = "% ILI", lwd = 2, lty = 1)
# 
# 
# for (irnd in seq(from = 1, to = nRnd, by = 10)) {
# 	lines(1:nweeksFit, fit_model_profile[irnd, 1:nweeksFit], col = "grey", lwd = 1, lty = 1)
# 	lines(nweeksFit:nweeks, fit_model_profile[irnd, nweeksFit:nweeks], col = "grey", lwd = 1, lty = 2)
# }
# 
# lines(1:nweeksFit, fit_model[1:nweeksFit], col = "blue", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 1)
# lines(nweeksFit:nweeks, fit_model[nweeksFit:nweeks], col = "blue", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 2)
# 
# lines(1:nweeksFit, fit_model_mean[1:nweeksFit], col = "purple", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 1)
# lines(nweeksFit:nweeks, fit_model_mean[nweeksFit:nweeks], col = "purple", xaxt = "n", xlab = "", ylab = "", lwd = 1, lty = 2)
# 
# lines(1:nweeks, model_ili, type = "l", col = "red", xaxt = "n", xlab = "", ylab = "", lwd = 2, lty = 1)
# #points(1:nweeksFit, model_ili[1:nweeksFit], type = "p", col = "red", pch = 20, xaxt = "n", xlab = "", ylab = "")
# 
# if (length(model_onset) > 0) 
# 	abline(h = model_onset, col = "black", lty = 2, lwd = 1)
# 
# rect(xleft = nweeksFit, ybottom = 0, xright = (nweeksFit + 4), ytop = (ymax * 1.2), col = rgb(0.1, 0.1, 0.1, alpha = 0.1), border = NA)
# reg.name = paste(mydata$model$name, c("-Data", "-Best", "-Mean", "-Random"), sep = "")
# 
# legend("topleft", c(mydata$FY, reg.name), text.col = c("red", "blue", "purple", "grey"), bty = "n")
# 
# axis(1, at = 1:nweeks, label = weeks)
# 
# school = mydata$model$school
# school[school == 0] = NA
# par(new = TRUE)
# plot(school, ylim = c(0, 6), xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "p", pch = 22, lwd = 2)
# 
# 
# sh = mydata$model$sh
# par(new = TRUE)
# plot(sh, xaxt = "n", yaxt = "n", xlab = "n", ylab = "n", col = "cyan", type = "l", lwd = 1)
# 
# pk_wk_best = weeks[which.max(fit_model)]
# pk_val_best = round(max(fit_model), digits = 1)
# pk_best = paste0("P-WK ", pk_wk_best, "/P-Val ", pk_val_best, "%")
# 
# profile_pk_wk = rep(0, nRnd)
# for (j in 1:nRnd) profile_pk_wk[j] = which.max(fit_model_profile[j, ])
# groups = cut(profile_pk_wk, breaks = 1:nweeks, right = FALSE)
# freq = as.data.frame(table(groups))
# ind = which.max(freq$Freq)
# pk_wk_prfl = weeks[ind]
# pk_val_prfl = max(fit_model_mean)
# pk_val_prfl = round(pk_val_prfl,digits=1)
# pk_mp = paste0("P-WK ", pk_wk_prfl, "/P-Val ", pk_val_prfl, "%")
# legend("topright", c(pk_best, pk_mp), text.col = c("blue", "purple"), bty = "n")
# 
# 
# if (pdf == TRUE) 
# 	dev.off()
# 
# ## End plotting section
# 
# ## Now for the actual csv submission preparation part
# 
# #source(paste0(RepDir,"codes/FluSight.R"))
# 
# #table = read.csv(paste0(RepDir,"CDC",FY,"/Long_Flu_Submission_Template_update.csv"))
# 
# #err <- FillTable(dump = dump, table = table, EW = weeks[nweeksData])
# 







