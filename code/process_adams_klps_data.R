### Title:    Missing Data in R: Process Adams KLPS Data
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2022-01-28

rm(list = ls(all = TRUE))

## Parameters for the data synthesis:
n   <- 250
snr <- 5

library(dplyr)
library(mice)
library(mvtnorm)
library(pROC)
library(naniar)

source("supportFunctions.R")
source("sim_missing/code/simMissingness.R")

set.seed(235711)

dataDir  <- "../data/"
fileName <- "adams_klps_data.txt"


###-Data Ingest--------------------------------------------------------------###

## Read the data:
tmp <- readLines(paste0(dataDir, fileName)) %>%
    strsplit("\t") %>%
    lapply("[", x = 1:33)

## Populate a numeric data frame with the raw data:
dat1 <- do.call(rbind, tmp[-1]) %>%
    apply(2, as.numeric) %>%
    data.frame()

## Apply the variable names:
colnames(dat1) <- tolower(tmp[[1]])

## Save the data (unmodified) as RDS:
saveRDS(dat1, paste0(dataDir, "adams_klps_data-raw.rds"))


###-Data Synthesis-----------------------------------------------------------###

## Fill the missing values:
dat2 <- mice(data = dat1, m = 1, method = "pmm") %>% complete(1)

## Estimate original coveriance matrix:
sigma0 <- cov(dat2)

## Resample the original cases to produce a larger sample:
dat2           <- dat2[sample(1:nrow(dat2), n, TRUE), ]
rownames(dat2) <- 1:n

## Add Gaussian noise:
dat2.1 <- dat2 + rmvnorm(n, sigma = (1 / 2) * sigma0)

## Save the synthesized data as RDS:
saveRDS(dat2.1, paste0(dataDir, "adams_klps_data-synthetic.rds"))


###-Data Augmentation--------------------------------------------------------###

## Create a more complex version of the dataset to demonstrate MICE:
dat2.2 <- dat2.1 %>% select(-polv, -matches("^policy\\d"))

## Add back the un-noised version for the policy items:
dat2.2 <- data.frame(dat2.2,
                     dat2 %>% select(matches("^policy\\d"))
                     )

## Covert "polv" into a three-level factor:
dat2.2$polv <- factor(
    cut(dat2$polv, 3, labels = c("conservative", "moderate", "liberal"))
)

## Create a trivial "sex" factor:
dat2.2$sex <- factor(sample(c("male", "female"), n, TRUE))

## Save the synthesized data as RDS:
saveRDS(dat2.2, paste0(dataDir, "adams_klps_data-augmented.rds"))


###-Missing Data Imposition 1------------------------------------------------###

targets <- grep("^wpriv\\d|^riae\\d|^policy\\d", colnames(dat2.1), value = TRUE)
preds   <- grep("^polv|^nori\\d", colnames(dat2.1), value = TRUE)

dat3.1 <- imposeMissData(data    = dat2.1,
                         targets = targets,
                         preds   = preds,
                         pm      = 0.25,
                         types   = "random",
                         stdData = TRUE)

## Check the results:
head(dat3.1)

colMeans(is.na(dat3.1))

vis_miss(dat3.1)

cc <- md.pairs(dat3.1)$rr / nrow(dat3.1)
range(cc[cc < 1])

## Save the incomplete, synthesized data as RDS:
saveRDS(dat3.1, paste0(dataDir, "adams_klps_data-incomplete.rds"))


###-Missing Data Imposition 2------------------------------------------------###

targets <- c(targets, "polv", "sex")
preds   <- setdiff(preds, "polv")

dat3.2 <- imposeMissData(data    = dat2.2,
                         targets = targets,
                         preds   = preds,
                         pm      = 0.25,
                         types   = "random",
                         stdData = TRUE)


## Check the results:
head(dat3.2)

colMeans(is.na(dat3.2))

vis_miss(dat3.2)

cc <- md.pairs(dat3.2)$rr / nrow(dat3.2)
range(cc[cc < 1])

## Save the incomplete, synthesized data as RDS:
saveRDS(dat3.2, paste0(dataDir, "adams_klps_data-example.rds"))


###-Missing Data Imputation--------------------------------------------------###

## Use mice::mice() to multiply impute the missing values:
miceOut <-
    mice(data = dat3.1, m = 25, maxit = 50, method = "norm", seed = 235711)

## Check convergence:
targets <- which(miceOut$method != "") %>% names()
for(v in targets) {
    plot(miceOut, v, layout = c(1, 2)) %>% print()
    readline("Hit a key...")
}

## Check imputations:
for(v in targets) {
    f <- as.formula(paste0("~", v))
    densityplot(miceOut, f, layout = c(1, 1)) %>% print()
    readline("Hit a key...")
}

## Save the mids object as RDS:
saveRDS(miceOut, paste0(dataDir, "adams_klps_data-mids.rds"))
