### Title:   Saturated Correlates Example for FIML Lecture
### Author:  Kyle M. Lang
### Created: 2022-01-26
### Modifed: 2022-01-26

rm(list = ls(all = TRUE))

set.seed(235711)

library(pROC)
library(lavaan)
library(dplyr)
library(magrittr)
library(parallel)
library(mvtnorm)

dataDir  <- "../data/"
fileName <- "diabetes.rds"

source("supportFunctions.R")
source("sim_missing/code/simMissingness.R")

dat0 <- readRDS(paste0(dataDir, fileName))

dat0 %<>% select(-sex)
dat1 <- rmvnorm(500, colMeans(dat0), cov(dat0))


###-Resampling Study---------------------------------------------------------###

nReps <- 500

mod1 <- "bp ~ tc"
mod2 <- "bp ~ tc + bmi"
mod3 <- "
bp ~ tc
bmi ~~ bp + tc
"

parms <- list()
parms$data <- dat1 
parms$n <- 200
parms$target <- c("tc", "bmi")
parms$preds <- "bp"
parms$type <- c("high", "low")
parms$pm <- 0.25
parms$models <- list(mod1, mod2, mod3)

doRep <- function(rp, parms) {
    paste0("Doing replication", rp, ".\n") %>% cat()
    
    ## Resample the data:
    dat1 <- with(parms, data[sample(1:nrow(data), n), ])

    ## Impose missing data:
    suppressMessages(
        dat2 <- with(parms,
                     imposeMissData(data    = dat1,
                                    targets = target,
                                    preds   = preds,
                                    types   = type,
                                    pm      = pm,
                                    stdData = TRUE)
                     )
    )
    
    ## Fit models:
    fit1 <- lapply(parms$models, sem, data = dat1, meanstructure = TRUE, fixed.x = FALSE)
    fit2 <- lapply(parms$models, sem, data = dat2, missing = "fiml", fixed.x = FALSE)
    fit3 <- lapply(parms$models, sem, data = dat2, meanstructure = TRUE, fixed.x = FALSE)
    
    ## Extract and return coefficients:
    list(obs = lapply(fit1, coef),
         mis = lapply(fit2, coef),
         del = lapply(fit3, coef)
         )
}

## Run the resampling study:
set.seed(235711)
out <- mclapply(1:nReps, doRep, parms = parms, mc.cores = 3)

## Extract the estimates:
tmp <- lapply(out, "[[", x = "obs")
obs <- list()
for(i in 1:3)
    obs[[i]] <- lapply(tmp, "[[", x = i) %>% do.call(rbind, .)

tmp <- lapply(out, "[[", x = "mis")
mis <- list()
for(i in 1:3)
    mis[[i]] <- lapply(tmp, "[[", x = i) %>% do.call(rbind, .)

tmp <- lapply(out, "[[", x = "del")
del <- list()
for(i in 1:3)
    del[[i]] <- lapply(tmp, "[[", x = i) %>% do.call(rbind, .)

labs <- c("bp~1", "bp~tc")

mObs <- lapply(obs, colMeans) %>% lapply("[", x = labs)
mMis <- lapply(mis, colMeans) %>% lapply("[", x = labs)
mDel <- lapply(del, colMeans) %>% lapply("[", x = labs)

mObs[[1]]
mMis[[1]]
mDel[[1]]

unlist(mMis) - unlist(mDel)

mMis[[3]]

mObs[[2]]
mMis[[2]]
mDel[[2]]

(mMis[[3]] - mObs[[1]]) / mObs[[1]]

mObs[[3]]
mMis[[3]]
mDel[[3]]


?lavaan
