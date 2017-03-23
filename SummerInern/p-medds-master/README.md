p-medds
=======

Prototype-Military Epidemic Disease Dynamics System

———————————————————
REQUEIREMENTS
———————————————————

P-MEDDS requires a basic installation of  R (64-bit), and a GNU Fortran/C compiler 

—————————————————————————
COMPILING AND INSTALLING THE PACKAGE
—————————————————————————

To COMPILE and INSTALL the package run the Python script ‘compile.py’ which is provided in this same directory:

%./compile.py

The script is set for a global installation of the package but it also
explains how to do a local installation.

You can execute this script yourself using these two commands:
%R CMD build pmedds.core
%R CMD INSTALL pmedds.core

This will do a golbal installation.  For a local installation see the
comments in compile.py

———————————————————
RUNNING THE PACKAGE
———————————————————

To see a set of examples for running the package please go to the
'examples' sub-directory and read the README.examples.md file

———————————————————
PACKAGE OUTPUT
———————————————————

For an explanation on the output see the README.output.md file
in the 'examples/tests.output' sub-directory.  
For example output tables/plots and RData files see the 
'examples/tests.output' sub-directory

———————————————————
POST-PROCESSING 
———————————————————
The P-MEDDS package provides a python script which can post-process
your run and generate a Latex file.  Go to the genReports
sub-directory and run:

%./makeFullReport.py
