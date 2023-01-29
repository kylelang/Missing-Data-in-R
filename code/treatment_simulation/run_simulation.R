### Title:    Monte Carlo Simulation to Compare Missing Data Treatments
### Author:   Kyle M. lang
### Created:  2023-01-28
### Modified: 2023-01-29

rm(list = ls(all = TRUE))

library(parallel)
source("code/treatment_simulation/init.R")

reps <- 1:250

## Define the simulation parameters:
parms <- list(
    targets = c("glu", "bp"),
    preds   = list(glu = c("bmi", "age"),
                   bp  = c("bmi", "tc")
                   ),
    pm      = c(glu = 0.2, bp = 0.3),
    where   = c(glu = "high", bp = "low"),
    auc     = c(glu = 0.8, bp = 0.85),
    nImp    = 25,
    nIter   = 10,
    seed    = 235711,
    nStreams = 500,
    meanVars = c("glu", "bp"),
    covVars = c("age", "bmi", "bp", "tc", "ltg", "glu")
)

## Read in the synthesized 'diabetes' data:
dat0 <- readRDS("data/diabetes_norm.rds")

## Save an example dataset for the slides:
imposeMissing(dat0, parms) %>% saveRDS("data/diabetes_incomplete.rds")

## Create a cluster object:
clus <- makeCluster(4)

## Initialize the environment on the worker nodes:
clusterCall(cl = clus, fun = source, file = "code/treatment_simulation/init.R")

## Run the computations in parallel on the 'clus' object:
out <- parLapply(cl    = clus,
                 X     = reps,
                 fun   = doRep,
                 data  = dat0,
                 parms = parms)

## Kill the cluster:
stopCluster(clus)

saveRDS(out, "data/treatment_sim_out.rds")
