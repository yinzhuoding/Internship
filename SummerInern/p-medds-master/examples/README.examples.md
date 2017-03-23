p-medds
=======

Prognostic Prototype Tool for DTRA LEPR01 Project

———————————————————
RUNNING THE PACKAGE 
———————————————————

The P-MEDDS package is capable of modeling Military, CDC and GFT/GFT+ ILI
data and the 2003 SARS data (using the Wallinga-Teunis procedure)
The three scripts in this directory will familiarize you with the
package's capabilities and requirements.

## demo.MCMC.R
This script demonstrates the basic functionality and outputs of P-MEDDS.
The user specifies a flu season and data type: Military, CDC, or GFT/GFT+.
Parameters are automatically set for a short MCMC optimization.

There are three ways you can run this script:

1. Copy and paste it into the R console window

2. In the R console window type: 
> source(“demo.MCMC.R”)

3. Run it from the command line using: 
%Rscript demo.MCMC.R 


## example.driver.R

This script can be used to model any of the ILI data in the P-MEDDS
data base: Military, CDC, GFT GFT+. The script  explains every parameter
that the package requires and shows the default values of each one.

There are three ways you can run this script:

1. Copy and paste it into the R console window

2. In the R console window type: 
   > source(“example.driver.R”)
   
3. Run it from the command line using: 
   %Rscript example.driver.R 

You can set the plotting device to either "X11" "pdf" or "png"
Even if you do not set to "X11" the script will use a system call to
open all the "pdf" or "png" files at the end of the run.
In general it is best to set the device to "pdf"/"png" - so plots are saved.  Setting to "X11"
is recommended only when using options 1 or 2 with very short runs.

As the script executes it will explain what output has been
generated, see more below. This information is also written to the
'log.txt' file. 

After you have successfully run this script with it's current settings
you can start changing the parameters in it.


##example.interactive.R

This is an interactive version of the example.driver.R: it will prompt
you to select each of the parameters for a P-MEDDS run. It will
explain your options and use their defaults if you provide an
incorrect value.  This script can be run interactively only from the
command line using:

%Rscript example.interactive.R

As the script executes it will explain what output has been
generated, see more below. This information is alos written to the
'log.txt' file.  When the run is completed the script
will use a system call to open all the "pdf" files.


##example.wt.R

This script demonstartes how the P-MEDDS package can be used to model
the 2003 SARS data using the Wallinga-Teunis procedure.

There are again three ways to run this script:

1. Copy and paste it into the R console window

2. In the R console window type: 
   > source(“example.wt.R”)
   
3. Run it from the command line using: 
   %Rscript example.wt.R

The script is currently set to run the procedure on all the data we
have, one country after the other. But it also shows how to run only
for one country.  The script has estimates for the run time for each
country and it explains what the output is.  The script will write
information to the screen and to the 'log-wt.txt' file.

When running from the command line (option 3) set the device to "pdf".
When running options 1 or 2 you can set it to either "X11" or "pdf".
"X11" is not recommended if the run is long. In general it is best to
set the device to "pdf" - so plots are saved.  When the run is
completed the script uses a system call to open the pdf file.

______________________________________
P-MEDDS OUTPUT
______________________________________
As your job is excuting it produces plots, tables and binary files.
The names of the files are unique and are built using the parameters of
the run.  The script provides information on each file that is written
and this can also be found in the log file of the run ('log.txt" or "log-wt.txt".

Example output files for each data type supported by P-MEDDS,
along with a detailed REDME file, can be found in the "tests.output"
sub-directory.

