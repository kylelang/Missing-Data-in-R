### Title:    MI Example Scratch Space
### Author:   Kyle M. Lang
### Created:  2018-09-12
### Modified: 2018-10-20

rm(list = ls(all = TRUE))

library(mice)
library(SURF)

library(psych)

data(msq)

colMeans(is.na(msq))


dataDir  <- "../../../data/"
fileName <- "cleanAdamsKlpsData.rds"
saveDate <- format(Sys.time(), format = "%Y%m%d")

dat1 <- readRDS(paste0(dataDir, fileName))
dat2 <- readRDS(paste0(dataDir, "adamsKlpsMissData.rds"))

setdiff(colnames(dat2), colnames(dat1))

marV <- grep("RIAE|NORI", colnames(dat1), value = TRUE)
mcarV <- grep("POLICY", colnames(dat1), value = TRUE)
preds <- grep("WPRIV"

              
              
missData <- imposeMissData(data = dat1,
                           targets = c(mar = 

head(dat1)

## Load the data and re-assign it:
load(paste0(dataDir, fileName))

## Subset by country:
filter <- WV6_Data_R$V2 %in% c(156, 276, 356, 643, 840)
dat1   <- WV6_Data_R[filter, ]

## Convert various missing data codes to 'NA':
dat1[dat1 < 0] <- NA

## Compute within-country PM:
pm <- lapply(dat1, FUN = function(x, country)
    tapply(x, country, FUN = function(x) mean(is.na(x))),
    country = dat1$V2)
pm <- do.call(rbind, pm)

## Remove variables for which PM >= 50% within any country:
good <- names(which(apply(pm < 0.5, 1, all)))
dat1 <- dat1[ , good]

## Remove indices and weights:
dat1 <- dat1[ , grep("^V", colnames(dat1))]

## Remove otherwise problematic varaibles:
dat1 <- dat1[ , -grep("V2[5-9]$|V3[0-5]$|V2A$|V3$|125|144|211|228|241|247|254|265|257|258|260|262",
                      colnames(dat1)
                      )
             ]

## Identify "good" nominal variables:
catNums <- c(24:35, 60:66, 81:83, 147:151, 176:180, 187, 229:230, 234:236, 240,
             243:246, 250, 252, 255, 265)
catVars <- intersect(paste0("V", catNums), colnames(dat1))

## Cast nominal variables as factors:
dat1[ , catVars] <- data.frame(lapply(dat1[ , catVars], factor))

## Remove "point-process" variables:
filter <- sapply(dat1,
                 FUN = function(x)
                     is.factor(x) | length(unique(na.omit(x))) > 2
                 )
dat1 <- dat1[ , filter]

## Check the PM:
pm <- colMeans(is.na(dat1))
hist(pm)

colnames(dat1)

sapply(dat1, class)

## Impute missing data:
miceOut <- mice(data            = dat1,
                m               = 1,
                maxit           = 20,
                predictorMatrix = quickpred(dat1),
                seed            = 235711)

saveRDS(miceOut, file = paste0(dataDir, "wvs_mice_out-", saveDate, ".rds"))

## Extract and save the imputations:
imps      <- miceOut$imp
fullSlots <- names(which(sapply(imps, function(x) length(x[ , 1]) > 0)))
imps2     <- imps[fullSlots]

saveRDS(imps2, file = paste0(dataDir, "wvs_imputations.rds"))

## Save the column names of the processed data:
saveRDS(colnames(dat1), file = paste0(dataDir, "wvs_column_names.rds"))


