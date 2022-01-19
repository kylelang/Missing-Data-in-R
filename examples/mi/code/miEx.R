### Title:    Missing Data Course: Lecture 8 Examples
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2015-OCT-21

rm(list = ls(all = TRUE)) # Clear workspace

## Intall packages as necessary:
install.packages(c("Amelia", "mice", "mitools"),
                 repos = "http://cloud.r-project.org")

## Load the packages we'll use:
library(mice)
library(mitools)
library(Amelia)
library(SURF)
library(semTools)

dataDir <- "../data/"
plotDir <- "../output/plots/"
outDir  <- "../output/"

## Read in the incomplete data:
missData <- readRDS(paste0(dataDir, "adamsKlpsSynthData.rds"))

## Take a peek:
summary(missData)


##--Missing Data Descriptives-------------------------------------------------##

## First look at the percent missing:
pm <- colMeans(is.na(missData))
pm

## Compute the covaraince coverage using the md.pairs() is part of the mice package
cover <- md.pairs(missData)$rr / nrow(missData)
cover
hist(cover)
range(cover)

## Compute the unique response patterns:
pats <- md.pattern(missData)
pats


##--Missing Data Imputation---------------------------------------------------##

### First, we'll look at mice ###

### We need a vector of elementary imputation methods.

## Start by getting each variables class:
classVec <- sapply(missData, class)

## Build up the method vector by matching elementary imputation method to
## variable type
methVec                        <- rep("", ncol(missData))
methVec[classVec == "numeric"] <- "norm"
methVec[classVec == "factor"]  <- "logreg"
methVec[pm == 0]               <- ""

## Check the number of levels for each variable:
countLevels <- function(x) length(unique(na.omit(x)))
levelVec    <- sapply(missData, countLevels)

## Make sure everything is lining up correctly:
checkFrame <- data.frame(names  = colnames(missData),
                         method = methVec,
                         levels = levelVec,
                         class  = classVec,
                         pm     = round(colMeans(is.na(missData)), 3)
                         )
checkFrame

## Run the imputation algorithm:
miceOut <- mice(data   = missData,
                m      = 50,
                method = methVec,
                maxit  = 20,
                seed   = 235711)

### Now we need to run our imputation diagnostics to check that the imputation
### process went well

## First we construct the traceplots of the imputation model parameters:
pdf(paste0(plotDir, "miceTrace-i20.pdf"), onefile = TRUE)

plot(miceOut)

dev.off()

## Populate the imputed data sets:
impList <- list()
for(m in 1 : miceOut$m) impList[[m]] <- complete(miceOut, m)

## Next we need to check that the imputed values themselves are sensible:

pdf(paste0(plotDir, "miceDensity-i20.pdf"), onefile = TRUE)

targets <- colnames(missData)[pm > 0]
for(v in targets)
    print(densityplot(miceOut, as.formula(paste0("~", v))))

dev.off()

## Density plots using 'SURF::plotImps':
types <- sapply(classVec,
                function(x) ifelse(x == "numeric", "con", "cat")
                )
plotImps(impList     = impList,
         rMat        = miceOut$where,
         typeVec     = types,
         interactive = TRUE)


## Scatterplots
pdf(paste0(plotDir, "miceScatter-i10.pdf"), onefile = TRUE)

for(v in targets) print(stripplot(miceOut, as.formula(paste0(v, "~.imp"))))

dev.off()

##----------------------------------------------------------------------------##

## Change the method used to impute 'policy' items:
methVec2 <- methVec
methVec2[grep("policy", colnames(missData))] <- "pmm"

## Make sure everything is lining up correctly:
checkFrame <- data.frame(names  = colnames(missData),
                         method = methVec2,
                         levels = levelVec,
                         class  = classVec,
                         pm     = round(colMeans(is.na(missData)), 3)
                         )
checkFrame


## Run the imputation algorithm:
miceOut2 <- mice(data   = missData,
                 m      = 50,
                 method = methVec2,
                 maxit  = 20,
                 seed   = 235711)

impList2 <- list()
for(m in 1 : miceOut2$m) impList2[[m]] <- complete(miceOut2, m)

### Check convergence:
pdf(paste0(plotDir, "miceTrace2-i20.pdf"), onefile = TRUE)

plot(miceOut2)

dev.off()

## Next we need to check that the imputed values themselves are sensible:

pdf(paste0(plotDir, "miceDensity2-i20.pdf"), onefile = TRUE)

targets <- colnames(missData)[pm > 0]
for(v in targets) print(densityplot(miceOut2, as.formula(paste0("~", v))))

dev.off()

## Density plots using 'SURF::plotImps':
types <- sapply(levelVec, function(x) ifelse(x > 7, "con", "cat"))
plotImps(impList     = impList2,
         rMat        = miceOut$where,
         typeVec     = types,
         interactive = TRUE)


## Scatterplots
pdf(paste0(plotDir, "miceScatter2-i20.pdf"), onefile = TRUE)

for(v in targets) print(stripplot(miceOut2, as.formula(paste0(v, "~.imp"))))

dev.off()

##----------------------------------------------------------------------------##

## Use the 'predictorMatrix' argument to impute using only a subset of the
## possible predictors in each EIM:
predMat <-
    quickpred(missData,
              mincor  = 0.3,
              exclude = "id",
              include = "liberal")
predMat

## Run the imputation algorithm:
miceOut3 <- mice(data            = missData,
                 m               = 50,
                 method          = methVec2,
                 predictorMatrix = predMat,
                 ridge           = 1e-5,
                 maxit           = 20,
                 seed            = 235711)

impList3 <- list()
for(m in 1 : miceOut3$m) impList3[[m]] <- complete(miceOut3, m)

### Check convergence:
pdf(paste0(plotDir, "miceTrace3-i20.pdf"), onefile = TRUE)

plot(miceOut3)

dev.off()

## Check the imputed values' sanity:

pdf(paste0(plotDir, "miceDensity3-i20.pdf"), onefile = TRUE)

targets <- colnames(missData)[pm > 0]
for(v in targets) print(densityplot(miceOut3, as.formula(paste0("~", v))))

dev.off()

## Density plots using 'SURF::plotImps':
types <- sapply(levelVec, function(x) ifelse(x > 7, "con", "cat"))
plotImps(impList     = impList3,
         rMat        = miceOut$where,
         typeVec     = types,
         interactive = TRUE)


## Scatterplots
pdf(paste0(plotDir, "miceScatter3-i20.pdf"), onefile = TRUE)

for(v in targets) print(stripplot(miceOut3, as.formula(paste0(v, "~.imp"))))

dev.off()

##----------------------------------------------------------------------------##

## Fit a model directly to the imputed data in the mids object:
fit3 <- with(miceOut3, lm(policy1 ~ sysRac1 + sysRac2 + sysRac3))

## Pool the results:
pool3 <- pool(fit3)
summary(pool3)
pool3

## Extract the FMIs:
pool3$pooled$fmi

## Process the imputed items:
impList3.2 <-
    lapply(impList3,
           FUN = function(x) {
               data.frame(
                   sysRac = rowMeans(x[ , grep("sysRac", colnames(x))]),
                   indRac = rowMeans(x[ , grep("indRac", colnames(x))]),
                   wPriv  = rowMeans(x[ , grep("wPriv",  colnames(x))]),
                   policy = rowMeans(x[ , grep("policy", colnames(x))]),
                   liberal = x$liberal
               )
           })

impList3.2[[1]]

## Fit a simple regression model:
fit3.2 <- lapply(X   = impList3.2,
                 FUN = function(x)
                     lm(policy ~ sysRac + wPriv + liberal, data = x)
                 )

pool3.2 <- MIcombine(fit3.2)
summary(pool3.2)

summary(fit3.2[[1]])
ls(fit3.2[[1]])

## Extract the FMI:
fmi3.2 <- pool3.2$missinfo
fmi3.2

## Extract coefficients:
cf3.2 <- coef(pool3.2)
cf3.2

## Extract SEs:
se3.2 <- sqrt(diag(vcov(pool3.2)))
se3.2

## Calcuate t-statistics:
t3.2 <- cf3.2 / se3.2
t3.2

## Get p-values:
p3.2 <- 2 * pt(abs(t3.2), df = pool3.2$df, lower = FALSE)
p3.2

## Put the results in a nice table:
outTab           <- round(cbind(cf3.2, se3.2, t3.2, p3.2, fmi3.2), 3)
colnames(outTab) <- c("Estimate", "SE", "t-Stat", "p-Value", "FMI")
rownames(outTab) <- c("Intercept", "Sys. Rac.", "W. Priv.", "Liberal")
outTab

## Write out nice table to disk:
write.csv(outTab, paste0(outDir, "resTab3.2.csv"), row.names = TRUE)

##----------------------------------------------------------------------------##


### Now we'll try amelia() ###

## Amelia wants all of our data to be numeric:
missData$liberal <- as.numeric(missData$liberal == "lib")
missData         <- sapply(missData, as.numeric)

## Define nominal and ordinal variables:
nomVars <- "liberal"
ordVars <- paste0("policy", c(1 : 3))

## Run the imputation:
ameliaOut <- amelia(x       = missData,
                    m       = 50,
                    noms    = nomVars,
                    ords    = ordVars,
                    p2s     = 1,
                    autopri = 0.1,
                    seed    = 235711)

## We can easily get overlaid density plots:
plot(ameliaOut)


## Amelia will give us some helpful summary information:
summary(ameliaOut)

## Process the imputed items:
impList4 <-
    lapply(ameliaOut$imputations,
           FUN = function(x) {
               x <- as.data.frame(x)
               data.frame(
                   sysRac = rowMeans(x[ , grep("sysRac", colnames(x))]),
                   indRac = rowMeans(x[ , grep("indRac", colnames(x))]),
                   wPriv  = rowMeans(x[ , grep("wPriv",  colnames(x))]),
                   policy = rowMeans(x[ , grep("policy", colnames(x))]),
                   liberal = x$liberal
               )
           })

## Fit a simple regression model:
fit4 <- lapply(X   = impList4,
               FUN = function(x)
                   lm(policy ~ sysRac + wPriv + liberal, data = x)
               )

## Pool the results:
pool4 <- MIcombine(fit4)

## Extract the FMI:
fmi4 <- pool4$missinfo
fmi4

## Extract coefficients:
cf4 <- coef(pool4)
cf4

## Extract SEs:
se4 <- sqrt(diag(vcov(pool4)))
se4

## Calcuate t-statistics:
t4 <- cf4 / se4
t4

## Get p-values:
p4 <- 2 * pt(abs(t4), df = pool4$df, lower = FALSE)
p4

## Put the results in a nice table:
outTab4           <- round(cbind(cf4, se4, t4, p4, fmi4), 3)
colnames(outTab4) <- c("Estimate", "SE", "t-Stat", "p-Value", "FMI")
rownames(outTab4) <- c("Intercept", "Sys. Rac.", "W. Priv.", "Liberal")
outTab4


##--SEM with MI---------------------------------------------------------------##

impList5 <- lapply(impList3, function(x) {
    x$liberal <- as.numeric(x$liberal == "lib")
    scale(x)
})

## Simple structural model:
mod2 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
"

out5 <- cfa.mi(model = mod2, data = impList5, std.lv = TRUE)
warnings()

?runMI
