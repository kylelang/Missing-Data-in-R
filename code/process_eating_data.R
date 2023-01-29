### Title:    Process Enders (2010) Eating Attitudes Data
### Author:   Kyle M. Lang
### Created:  2022-07-01
### Modified: 2022-07-03

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)
library(lavaan)
library(semTools)
library(mice)
library(ggmice)
library(miceadds)

set.seed(235711)

dataDir <- "../../data/"

## Read in the data:
eat <- read.table(paste0(dataDir, "eatingattitudes.dat"))

## Assign columns names:
colnames(eat) <- c("id",
                   "eat1",
                   "eat2",
                   "eat10",
                   "eat11",
                   "eat12",
                   "eat14",
                   "eat24",
                   "eat3",
                   "eat18",
                   "eat21",
                   "bmi",
                   "wsb",
                   "anx")

## Code missing values:
eat[eat == -99] <- NA

## Save the raw data as RDS:
saveRDS(eat, paste0(dataDir, "eating_attitudes.rds"))


###-Missing Data Imputation--------------------------------------------------###

## Generate a huge pool of multiply imputed data:
miceOut <- mice(eat, m = 500, maxit = 25, method = "pmm", seed = 235711)

## Check convergence:
targets <- which(miceOut$method != "") %>% names()

for(v in targets) {
    plot_trace(miceOut, v) %>% print()
    readline("Hit any key to proceed...")
}

Rhat.mice(miceOut)

## Save the mids object:
saveRDS(miceOut, paste0(dataDir, "eating_attitudes_mids.rds"))

## Save a single imputed dataset:
saveRDS(complete(miceOut, 1), paste0(dataDir, "eating_attitudes_completed.rds"))
