rm(list=ls())

require(utils)
require(tools)

Sweave("test2.Rnw")
texi2pdf("test2.tex")

