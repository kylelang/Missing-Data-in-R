### Title:    Missing Data Analysis: Example Data Prep
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2018-OCT-20

rm(list = ls(all = TRUE))

set.seed(235711)

dataDir <- "../../../data/"

library(mvtnorm)
library(mice)
library(SURF)

makeScores <- function(vNames, nOut, stem = "v", bRange = c(-10, 10)) {
    out <- as.matrix(dat1[ , vNames]) %*%
        matrix(runif(length(vNames) * nOut, bRange[1], bRange[2]), ncol = nOut)
    colnames(out) <- paste0(stem, 1 : ncol(out))
    out
}

## Read the data:
dat1 <- readRDS(paste0(dataDir, "adamsKlpsData.rds"))

## Fill missing:
miceOut <- mice(dat1, m = 1)
dat1    <- complete(miceOut, 1)

## Define subscale variable names:
srV <- c(
    paste0("RIAE", c(2, 3, 7, 8, 9, 11, 12)),
    paste0("NORI", c(2, 7, 9))
)

irV <- c(
    paste0("RIAE", c(1, 4, 5, 6, 10)),
    paste0("NORI", c(1, 4, 10))
)

wpV <- grep("WPRIV", colnames(dat1), value = TRUE)

plV <- paste0("POLICY", c(1, 3, 4))

## Compute random-linear-combination subscale scores:
srScores <- makeScores(vNames = srV, nOut = 3, stem = "sysRac")
irScores <- makeScores(vNames = irV, nOut = 3, stem = "indRac")
wpScores <- makeScores(vNames = wpV, nOut = 3, stem = "wPriv")

## Create ordered factors from 'policy' items:
#polVars           <- data.frame(lapply(dat1[ , plV], as.factor))
polVars           <- dat1[ , plV]
colnames(polVars) <- paste0("policy", 1 : ncol(polVars))

## Create a binary political affiliation indicator:
liberal <- as.numeric(
    dat1[ , "POLV"] > median(dat1[ , "POLV"])
)
liberal <- factor(liberal,
                  levels = c(0, 1),
                  labels = c("cons", "lib")
                  )

## Combine variables into new dataset:
dat2 <- data.frame(id = 1 : nrow(dat1),
                   srScores,
                   irScores,
                   wpScores,
                   polVars,
                   liberal)

## Impose missing data:
tmp1 <- imposeMissData(data    = dat2,
                       targets = list(mar = paste0("sysRac", 1 : 3)),
                       preds   = paste0("wPriv", 1 : 3),
                       snr     = 5,
                       pm      = 0.2)$data[ , paste0("sysRac", 1 : 3)]

tmp2 <- imposeMissData(data    = dat2,
                       targets = list(mar = paste0("indRac", 1 : 3)),
                       preds   = paste0("policy", 1 : 3),
                       snr     = 4,
                       pm      = 0.3)$data[ , paste0("indRac", 1 : 3)]

tmp3 <- imposeMissData(data    = dat2,
                       targets = list(mcar = paste0("policy", 1 : 3)),
                       preds   = paste0("wPriv", 1 : 3),
                       pm      = 0.25)$data[ , paste0("policy", 1 : 3)]

liberal <- imposeMissData(data    = dat2,
                          targets = list(mar = "liberal"),
                          preds   = paste0("sysRac", 1 : 3),
                          snr     = 6,
                          pm      = 0.25)$data$liberal

## Compile the incomplete example data:
dat4 <- data.frame(dat2[ , c("id", paste0("wPriv", 1 : 3))],
                   tmp1, tmp2, tmp3, liberal)

## Check stuff:
sapply(dat4, class)
colMeans(is.na(dat4))

## Write the processed data to disk:
saveRDS(dat4, paste0(dataDir, "adamsKlpsSynthData.rds"))
