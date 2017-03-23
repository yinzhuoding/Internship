
The /test subdirectory provides the following PMEDDS test scripts:


TestInstall.R
————————————-
This script runs several PMEDDS functions and compares their output to reference files.  This should ensure that the local installation is working as intended.  Expected execution time: approx 1 minute.

Executing this script:
1.  Command line: cd to the p-medds/test directory, then enter `Rscript TestInstall.R’
OR
2.  In an R console window: setwd() to the p-medds/test directory, then enter `source(“TestInstall.R”)’

NOTE: Setting the `test’ flag to FALSE within the script will generate new reference files. This is intended for developer use only.


TestChanges.R
—————————————
When changes are made to the source code, this script systematically looks for errors by running many combinations of data, model, and other functions.  This test is not necessary for users.  Developers should execute this script following changes and before pushing a commit to GitHub.  Expected execution time: approx 20 minutes.  