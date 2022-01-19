### Title:    Scratch Space for Cat MI Lecture
### Author:   Kyle M. Lang
### Created:  2018-OCT-15
### Modified: 2018-OCT-15

library(devtools)
install_github("kylelang/SURF/source/SURF", ref = "develop")

##### Set up the problem #####
parms <- list()
parms$nObs <- 1000
parms$pm <- 0.3
parms$auxVar <- "z"
parms$incompVars <- c("w", "x", "y")
parms$meanVec <- c(1 : 4)
parms$corVal <- 0.25
parms$marType <- c("lower", "center", "upper")

## Simulate some data:
simData <- simCovData(nObs  = 1000,
                      sigma = 0.25,
                      nVars = 4,
                      means = 1 : 4)
missData <- imposeMissData(data    = simData,
                           targets = list(mar = paste0("x", 1 : 3)),
                           preds   = "x4",
                           pm      = list(mar = 0.3),
                           snr     = list(mar = 5.0)
                           )

missData

x <- 0.25

ifelse(x < 0.05, "<", ">")
