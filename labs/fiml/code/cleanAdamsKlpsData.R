### Title:    Missing Data Course: Example Data Prep
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2015-OCT-26

rm(list = ls(all = TRUE))

set.seed(235711)

dataDir <- "../data/"
source("lecture9SupportFunctions.R")

## Read the data:
tmp <- readLines(paste0(dataDir, "adamsklps_sem_data.txt"), -1)
tmp2 <- strsplit(tmp, "\t")
dat1 <- do.call("rbind", lapply(tmp2[-1], function(x) {x[1 : 33]}))
dat1 <- apply(dat1, 2, as.numeric)
colnames(dat1) <- tmp2[[1]][1 : 33]

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
