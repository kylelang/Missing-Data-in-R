### Title:    Process Diabetes Data
### Author:   Kyle M. lang
### Created:  2023-01-27
### Modified: 2023-01-27

rm(list = ls(all = TRUE))

library(dplyr)
library(magrittr)
library(mvtnorm)

set.seed(235711)

dataDir <- "data/"

## Read in the raw 'diabetes'some data:
dat0 <- readRDS(paste0(dataDir, "diabetes.rds"))

## Set aside the 'sex' factor:
sex <- dat0$sex
dat0 %<>% select(-sex)

## Synthesize normal data based on moments of continuous variables:
dat0 <- rmvnorm(nrow(dat0), colMeans(dat0), cov(dat0)) %>% data.frame(sex)

saveRDS(dat0, paste0(dataDir, "diabetes_norm.rds"))
