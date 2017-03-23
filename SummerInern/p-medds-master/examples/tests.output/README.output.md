Examples for p-medds Output Files 
===================================

In general many files appear as both pdf and png files.

———————————————————
OUTPUT for an MPZ RUN 
———————————————————

	Output for an MPZ run for MPZ 23708 FY 2009-2010

1. Epi profile along with school schedule and specific humidity as a
   function of epi week for the 2009-2010 pandemic season. 
   MPZ_2009_23708-epi-sh-school.pdf 
   MPZ_2009_23708-epi-sh-school.png

2. Epi profile (red) along with our current best estimate (blue) and
   100 randomly selected results (grey) as a function of epi week.
   Our estimate for R(t) is shown in green (right y-axis) with dark-green
   being our current best estimate. If more than one chain was calculated
   we show our best results.

	plot-MPZ_2009_23708.pdf
	plot-MPZ_2009_23708.png

3. The cumulative attack rate (CAR) as a fucntion of epi week  for the
   2009-2010 pandemic year MPZ 23708. If more than one MCMC chain was
   calculated we plot plot our best results. Red is data and grey prediction.
   car-MPZ_2009_23708.pdf
   car-MPZ_2009_23708.png

4. The history of the MCMC chain for all the parameters that were
   optimized. We save a file for each MCMC chain.
   chain-MPZ_2009_23708-1.pdf
   chain-MPZ_2009_23708-1.png

5. Mean, standard deviation and quantiles (5%, 25%, 50%, 75% and 95%)
   for all the parameters that were optimized. In the case of model number 5 
	we also use R0min and delta to calculate RA and RB defined as: RA=R0min and RB=R0min*(1+delta)
	The maximum of the two we define as R0=max(RA,RB).
   This file is a summary of the files described in 6 and 7. 
   
   Each MCMC chain has a file.

	param-table-MPZ_2009_23708-1.csv

6. Mean, standard deviation and more stats for all the parameters. In the case of model number 5 
	we also use R0min and delta to calculate RA and RB defined as: RA=R0min and RB=R0min*(1+delta)
	The maximum of the two we define as R0=max(RA,RB).
    The SD is zero for any parameter that was not optimized. Each MCMC chain has a file.
   
   Each MCMC chain has a file.
   
	param-stats-MPZ_2009_23708-1.csv

7. Quantiles, 5%, 25%, 50%, 75% and 95%, for all the parameters. In the case of model number 5 
	we also use R0min and delta to calculate RA and RB defined as: RA=R0min and RB=R0min*(1+delta)
	The maximum of the two we define as R0=max(RA,RB).

   Each MCMC chain has a file.

	param-quantiles-MPZ_2009_23708-1.csv


8. An RData file that hold ALL the information related to an MCMC
   chain. The file has all the data information, model parameters,
   run parameters and the results.  For each chain we write a file.
   You can read the file into R using load("filename.RData") and see
   what the resulting list holds. This can be used to remake any plots
   or re-calculate any statistics
   
   mcmc-MPZ_2009_23708-1.RData

———————————————————-------
OUTPUT for a region or nation CDC or GFT/GFT+ RUN 
——————————————————---------

	The output is similar to what was described above but we will list it
	here

1. Epi profile (red) along with our current best estimate (blue) and
   100 randomly selected results (grey) as a function of epi week, 2009-2010.
   Our estimate for R(t) is shown in green (right y-axis) with dark-green
   being our current best estimate. If more than one chain was calculated
   we show our best results.  The file name is determined by the type of
   run (CDC or GFT) and it is a run of a single region or for the
   entire US.

	For region 1 CDC and GFT we have:

	plot-CDC_2009_reg1.pdf
	plot-CDC_2009_reg1.png

	plot-GFT_2009_reg1.pdf
	plot-GFT_2009_reg1.png
	
	plot-GFTPlus_2009_reg1.pdf
	plot-GFTPlus_2009_reg1.png	

	For national run CDC and GFT we have:

	plot-CDC_2009_national.pdf
	plot-CDC_2009_national.png

	plot-GFT_2009_national.pdf
	plot-GFT_2009_national.png
	
	plot-GFTPlus_2009_national.pdf
	plot-GFTPlus_2009_national.png	

2. The cumulative attack rate (CAR) as a fucntion of epi week  for the
   2009-2010 pandemic year, region 1 CDC/GFT/GFT+ and national CDC/GFT/GFT+

	car-CDC_2009_reg1.pdf
	car-CDC_2009_reg1.png

	car-GFT_2009_reg1.pdf
	car-GFT_2009_reg1.png
	
	car-GFTPlus_2009_reg1.pdf
	car-GFTPlus_2009_reg1.png	

	car-CDC_2009_national.pdf 
	car-CDC_2009_national.png

	car-GFT_2009_national.pdf 
	car-GFT_2009_national.png

	car-GFTPlus_2009_national.pdf 
	car-GFTPlus_2009_national.png	

3. The history of the MCMC chain for all the parameters that were
   optimized. We save a file for each chain. 
   We have files for region 1 CDC/GFT/GFT+ and national CDC/GFT/GFT+. 
 	Each MCMC chain has a file.

	chain-CDC_2009_reg1-1.pdf
	chain-CDC_2009_reg1-1.png

	chain-GFT_2009_reg1-1.pdf
	chain-GFT_2009_reg1-1.png
	chain-GFTPlus_2009_reg1-1.png	

	chain-CDC_2009_national-1.pdf
	chain-CDC_2009_national-1.png

	chain-GFT_2009_national-1.pdf
	chain-GFT_2009_national-1.png
	chain-GFTPlus_2009_national-1.png	

4. Mean, standard deviation and quantiles (5%, 25%, 50%, 75% and 95%)
   for all the parameters that were optimized. This file is a summary
   of the files described in 5 and 6. 
   In the case of model number 5 we also use R0min and delta to calculate 
   RA and RB defined as: RA=R0min and RB=R0min*(1+delta). 
   The maximum of the two we define as R0=max(RA,RB).
   We have region 1 files for CDC/GFT/GFT+ and national files for CDC/GFT/GFT+. 
   
	Each MCMC chain has a file.
	
	param-table-CDC_2009_reg1-1.csv
	param-table-GFT_2009_reg1-1.csv
	param-table-GFTPlus_2009_reg1-1.csv	

	param-table-CDC_2009_national-1.csv
	param-table-GFT_2009_national-1.csv
	param-table-GFTPlus_2009_national-1.csv	


5. Mean, standard deviation and more stats for all the parameters.
   The SD is zero for any parameter that was not optimized. 
   In the case of model number 5 we also use R0min and delta to calculate 
   RA and RB defined as: RA=R0min and RB=R0min*(1+delta). 
   The maximum of the two we define as R0=max(RA,RB).   
   We have region 1 files for CDC/GFT/GFT+ and national files for CDC/GFT/GFT+. 
   
   Each MCMC chain has a file.
   
	param-stats-CDC_2009_reg1-1.csv
	param-stats-GFT_2009_reg1-1.csv
	param-stats-GFTPlus_2009_reg1-1.csv

	param-stats-CDC_2009_national-1.csv
	param-stats-GFT_2009_national-1.csv
	param-stats-GFTPlus_2009_national-1.csv
	
6. Quantiles, 5%, 25%, 50%, 75% and 95%, for all the parameters.
   In the case of model number 5 we also use R0min and delta to calculate 
   RA and RB defined as: RA=R0min and RB=R0min*(1+delta). 
   The maximum of the two we define as R0=max(RA,RB).
   
  	Each MCMC chain has a file.
  
	param-quantiles-CDC_2009_reg1-1.csv
	param-quantiles-GFT_2009_reg1-1.csv
	param-quantiles-GFTPlus_2009_reg1-1.csv
	
	param-quantiles-CDC_2009_national-1.csv
	param-quantiles-GFT_2009_national-1.csv
	param-quantiles-GFTPlus_2009_national-1.csv

7. An RData file that holds ALL the information related to an MCMC
   chain/run. The file has all the input data, model parameters,
   run parameters and the results.  For each chain we write a file.
   You can read the file into R using load("filename.RData") and see
   what the resulting list holds. This can be used to remake any plots
   or re-calculate any statistics. We have files for region 1 CDC and
   GFT and national CDC and GFT.

	mcmc-CDC_2009_reg1-1.RData
	mcmc-GFT_2009_reg1-1.RData
	mcmc-GFTPlus_2009_reg1-1.RData	

	mcmc-CDC_2009_national-1.RData
	mcmc-GFT_2009_national-1.RData
	mcmc-GFTPlus_2009_national-1.RData	
	
———————————————————-------
OUTPUT for a W-T run on the 2003 SARS data
——————————————————---------
	The P-MEDDS data base hs the 2003 data for the following countries:
	Canada, China, HK, Singapore and Vietnam.

	The output files are for each of these countries.

1. Upper panel: the number of probable SARS cases by onset or report
   day. Lower panel: The estimatedd daily reprodection number, R(t).
   Red-mean, green - median and blue 95% quantile.

	W_T_Canada.pdf
	W_T_China.pdf
	W_T_Hong-Kong.pdf
	W_T_Singapore.pdf
	W_T_Vietnam.pdf


2. The statistics for R(t): date, day, number of cases, mean value of
   R(t), median value of R(t) and the 95%CI for R(t). One file per
   country. This file can be used to re-create the figures in 1. 
   
   Rstats-Canada.csv
   Rstats-China.csv
   Rstats-Hong-Kong.csv
   Rstats-Singapore.csv
   Rstats-Vietnam.csv

3. A file with 10,000 realizations for R(t) used to calcualte the
   statistics above. Columns are the realizations, rows are day or
   report or onset.

	Rl.Canada.csv
	Rl.Hong-Kong.csv
	Rl.Singapore.csv
	Rl.Vietnam.csv

	Rl.China.csv - we had to remove this file from the github since it
	is more that the github limit of 100 MB

4. For each country we have a binary RData file holding all the
   details of the W-T run.  This object can be loaded and used to
   calculate any desired statistics for R(t).
   
   Canada.RData
   China.RData
   Hong-Kong.RData
   Singapore.RData
   Vietnam.RData
