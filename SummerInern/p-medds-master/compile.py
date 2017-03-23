#!/usr/bin/python
import os
import string
os.system("/bin/rm -rf pmedds.core_1.0.tar.gz pmedds.core/src/*o pmedds.core/src/*so")
#os.chdir("pmedds.core/vignettes")
#os.system("echo `pwd`")
##os.system("make")
##os.system("make install")
##os.chdir("../..")
os.system("R CMD build pmedds.core")
##
## For a global installation 
os.system("R CMD INSTALL pmedds.core")

## For a local installation - comment the above line and uncomment the three lines starting with 'os.system'
## Create a directory in the home directory, called for example R_libs:
#os.system("mkdir $HOME/R_libs")
## set a variable to point R at that directory: 
#os.system("export R_LIBS='$HOME/R_libs'")
## and then install the package ih this directory:
#os.system("R CMD INSTALL -l $R_LIBS pmedds.core")


#Running ILI example
#os.system("Rscript example.driver.R")
# Running W-T example
#os.system("Rscript example.wt.R")

