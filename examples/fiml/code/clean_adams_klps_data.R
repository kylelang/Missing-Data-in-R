### Title:    Missing Data in R: Process Adams KLPS Data
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2022-01-26

rm(list = ls(all = TRUE))

library(dplyr)
library(mice)
library(mvtnorm)

## Parameters for the data synthesis:
n   <- 500
snr <- 5

set.seed(235711)

dataDir  <- "../data/"
fileName <- "adamsklps_sem_data.txt"


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
dat2 <- dat2[sample(1:nrow(dat2), n, TRUE), ]

## Add Gaussian noise:
dat2 <- dat2 + rmvnorm(n, sigma = (1 / 2) * sigma0)

## Save the synthesized data as RDS:
saveRDS(dat2, paste0(dataDir, "adams_klps_data-synthetic.rds"))


###-Missing Data Imposition--------------------------------------------------###

targets <- grep("^wpriv\\d|^riae\\d|^policy\\d", colnames(dat2), value = TRUE)
preds   <- grep("^polv|^nori\\d", colnames(dat2), value = TRUE)



dat2 <- dat2 + rnorm(
mod1 <- "
fac1 =~
RIAE2 +
RIAE3 +
RIAE7 +
RIAE8 +
RIAE9 +
RIAE11 +
RIAE12 +
NORI2 +
NORI7 +
NORI9
"

mod2 <- "
fac2 =~
RIAE1 +
RIAE4 +
RIAE5 +
RIAE6 +
RIAE10 +
NORI1 +
NORI4 +
NORI10
"

mod3 <- "
fac3 =~
POLICY1 +
POLICY3 +
POLICY4 +
POLICY5 +
POLICY6
"

sysRacPar <- createParcels(model = mod1,
                           data = dat1,
                           nameStem = "sysRacP")

indRacPar <- createParcels(model = mod2,
                           data = dat1,
                           nameStem = "indRacP")

policyPar <- createParcels(model = mod3,
                           data = dat1,
                           nameStem = "policyP")

dat2 <- data.frame(sysRacPar,
                   indRacPar,
                   policyPar,
                   dat1[ , c("POLV", "POLICY2")]
                   )

colnames(dat2) <- c(paste0("sysRac", c(1 : 3)),
                    paste0("indRac", c(1 : 3)),
                    paste0("policy", c(1 : 3)),
                    "polAffil",
                    "revDisc")

parms <- list()
parms$pm <- 0.15
parms$auxVar <- "revDisc"
parms$incompVars <- colnames(dat2)[-grep(parms$auxVar, colnames(dat2))]
parms$marType <- sample(c("lower", "tails", "upper"),
                        length(parms$incompVars),
                        replace = TRUE)

## Impose missing
missData <- imposeMissing(dat2, parms)

missData$sysRacT <- rowMeans(missData[ , grep("sysRac", colnames(missData))])
missData$indRacT <- rowMeans(missData[ , grep("indRac", colnames(missData))])
missData$policyT <- rowMeans(missData[ , grep("policy", colnames(missData))])

head(missData)

## Save the data:
saveRDS(dat1, paste0(dataDir, "adamsKlpsData.rds"))
saveRDS(missData, paste0(dataDir, "adamsKlpsMissData.rds"))
