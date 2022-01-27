### Title:   Saturated Correlates Example for FIML Lecture
### Author:  Kyle M. Lang
### Created: 2022-01-26
### Modifed: 2022-01-26

rm(list = ls(all = TRUE))

set.seed(235711)

library(pROC)
library(lavaan)
library(dplyr)

dataDir  <- "../data/"
fileName <- "diabetes.rds"

source("supportFunctions.R")
source("sim_missing/code/simMissingness.R")

###-Prep Data----------------------------------------------------------------###

dat1 <- readRDS(paste0(dataDir, fileName))

dat2 <- imposeMissData(data    = dat1,
                       targets = "bp",
                       preds   = c("glu", "tc"),
                       types   = "high",
                       pm      = 0.25,
                       stdData = TRUE)


###-Fit Models---------------------------------------------------------------###

mod1 <- "bp ~ age"
mod2 <- paste(mod1, "glu + tc", sep = " + ")
mod3 <- paste(mod1, "glu ~~ tc + age + bp\ntc ~~ age + bp", sep = "\n")

fit1 <- sem(mod1, data = dat1, missing = "fiml")
fit2 <- sem(mod2, data = dat1, missing = "fiml")
fit3 <- sem(mod3, data = dat1, missing = "fiml")

summary(fit1)
summary(fit2)
summary(fit3)

inspect(fit1, "est")
inspect(fit3, "est")
