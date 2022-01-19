### Title:    Missing Data Course: Lecture 8 Examples
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2015-OCT-20

rm(list = ls(all = TRUE)) # Clear workspace

## Intall packages as necessary:
install.packages(
    c("mi",
      "norm",
      "Amelia",
      "mice",
      "mitools",
      "semTools",
      "lavaan"),
    repos = "http://rweb.quant.ku.edu/cran")

## Load the packages we'll use:
library(mice)
library(mitools)
library(norm)
library(Amelia)
library(mi)
library(semTools)

source("plotImps.R")

dataDir <- "../data/"
plotDir <- "../output/plots/"

## Read in the incomplete data:
missData <- readRDS(paste0(dataDir, "adamsKlpsMissData.rds"))

## Take a peek:
summary(missData)



##################################
##### MISSING DATA SCREENING #####
##################################



## First look at the percent missing:
colMeans(is.na(missData))

## Look at the covaraince coverage
## md.pairs() is part of the mice package
mice::md.pairs(missData)$rr

## Check the multivariate coverage (influx and outflux):
mice::flux(missData)

### Coverage looks pretty good, not too much missing data
### we should go ahead and try to impute the nonresponse.



##################################
##### MISSING DATA TREATMENT #####
##################################



###-------------------------###
### First, we'll try mice() ###
###-------------------------###



### STEP 1: IMPUTATION PHASE ###


### We need a vector of elementary imputation methods.

## Start by getting each variables class:
classVec <- do.call("rbind", lapply(missData, class))[ , 1]

## Build up the method vector by matching elementary
## imputation method to variable type
methVec <- rep(NA, ncol(missData))
methVec[classVec == "numeric"] <- "norm"
methVec[classVec == "ordered"] <- "polr"
methVec[classVec == "factor"] <- "logreg"

## OPTIONAL: Check the number of levels for each variable:
countLevels <- function(x) { length(unique(x[!is.na(x)])) }
levelVec <- apply(missData, 2, countLevels)

## Make sure everything is lining up correctly:
checkMat <- cbind(colnames(missData),
                  methVec,
                  levelVec,
                  classVec,
                  round(colMeans(is.na(missData)), 3)
                  )
colnames(checkMat) <- c("variable", "method", "levels", "class", "pm")
checkMat

## OPTIONAL: Specify a predictor matrix to only use
##           a subset of the variables as auxiliaries
predMat <- quickpred(missData, mincor = 0.1)

## Run the imputation algorithm:
miceOut <- mice(missData,
                m = 50,
                method = methVec,
                predictorMatrix = predMat,
                maxit = 10,
                ridge = 1e-5,
                printFlag = TRUE,
                seed = 235711)

### Now we need to run our imputation diagnostics to
### check that the imputation process went well

## First we construct the traceplots of
## the imputation model parameters:
pdf(paste0(plotDir, "miceTraceplots.pdf"),
    onefile = TRUE)
plot(miceOut)
dev.off()

## Populate the imputed data sets:
impList <- list()
for(m in 1 : miceOut$m) impList[[m]] <- mice::complete(miceOut, m)

## Next we need to check that the imputed values
## themselves are sensible:

## Scatterplots first:
pdf(paste0(plotDir, "miceScatterplots.pdf"),
    onefile = TRUE)
plotImps(impList)
dev.off()

## Now, overlaid densities:
pdf(paste0(plotDir, "miceDensityPlots.pdf"),
    onefile = TRUE)
plotImps(impList, type = "density")
dev.off()

impList2 <- lapply(impList,
                   FUN = function(x) {
                       numData <- data.frame(
                           lapply(x, as.numeric)
                       )
                       sysRac <-
                           rowMeans(numData[ , grep("sysRac",
                                                    colnames(numData)
                                                    )
                                            ])
                       policy <-
                           rowMeans(numData[ , grep("policy",
                                                    colnames(numData)
                                                    )
                                            ])
                       data.frame(sysRac,
                                  policy,
                                  x[ , c("polAffil", "revDisc")]
                                  )
                   })


### STEP 2: ANALYSIS PHASE ###


## Fit a simple regression model:
fitList1 <- lapply(impList2,
                   FUN = function(x) {
                       lm(policy ~ sysRac + polAffil + revDisc, data = x)
                   })


### STEP 3: POOLING PHASE ###


pooledOut <- MIcombine(fitList1)

## Look at the FMI:
pooledOut$missinfo

## Calcuate t-statistics:
pooledWald <- coef(pooledOut) / sqrt(diag(pooledOut$var))

## Get p-values:
pValues <- round(
    pt(abs(pooledWald), df = pooledOut$df, lower = FALSE) * 2,
    3)

## Put the results in a nice table:
outTab <- round(cbind(pooledOut$coef,
                      sqrt(diag(pooledOut$var)),
                      pooledWald,
                      pValues), 3)
colnames(outTab) <- c("Estimate", "SE", "Wald", "P-Value")

outTab


###------------------------###
### Now we'll try amelia() ###
###------------------------###


missData2 <- data.frame(
    lapply(missData, as.numeric)
)


### STEP 1: IMPUTATION PHASE ###


nomVars <- "polAffil"
ordVars <- paste0("policy", c(1 : 5))

ameliaOut <- amelia(missData2,
                    m = 50,
                    noms = nomVars,
                    ords = ordVars,
                    p2s = 1,
                    autopri = 0.1,
                    seed = 235711)

## We can easily get overlaid density plots:
plot(ameliaOut)

## We can also write these plots to PDF in a nicer format:
pdf(paste0(plotDir, "ameliaDensityPlots1.pdf"),
    onefile = TRUE)

plotVars <- colnames(missData)[colSums(is.na(missData)) > 0]
for(i in plotVars) plot(ameliaOut, i)

dev.off()

## Alternatively, we can make our own plots:
pdf(paste0(plotDir, "ameliaScatterplots.pdf"),
    onefile = TRUE)
plotImps(ameliaOut$imputations)
dev.off()

pdf(paste0(plotDir, "ameliaDensityPlots2.pdf"),
    onefile = TRUE)
plotImps(ameliaOut$imputations, type = "density")
dev.off()

## Amelia will give us the FMI for each variable:
summary(ameliaOut)

impList3 <- lapply(ameliaOut$imputations,
                   FUN = function(x) {
                       numData <- data.frame(
                           lapply(x, as.numeric)
                       )
                       sysRac <-
                           rowMeans(numData[ , grep("sysRac",
                                                    colnames(numData)
                                                    )
                                            ])
                       policy <-
                           rowMeans(numData[ , grep("policy",
                                                    colnames(numData)
                                                    )
                                            ])
                       data.frame(sysRac,
                                  policy,
                                  x[ , c("polAffil", "revDisc")]
                                  )
                   })


### STEP 2: ANALYSIS PHASE ###


## Fit a simple regression model:
fitList2 <- lapply(impList3,
                   FUN = function(x) {
                       lm(policy ~ sysRac + polAffil + revDisc, data = x)
                   })


### STEP 3: POOLING PHASE ###


pooledOut2 <- MIcombine(fitList2)

## Look at the FMI:
pooledOut2$missinfo

## Calcuate t-statistics:
pooledWald2 <- coef(pooledOut2) / sqrt(diag(vcov(pooledOut2)))

## Get p-values:
pValues2 <- round(
    pt(abs(pooledWald2), df = pooledOut2$df, lower = FALSE) * 2,
    3)

## Put the results in a nice table:
outTab2 <- round(cbind(coef(pooledOut2),
                       sqrt(diag(vcov(pooledOut2))),
                       pooledWald2,
                       pValues2), 3)
colnames(outTab2) <- c("Estimate", "SE", "Wald", "P-Value")

outTab2


###-------------------------###
### Finally, let's try mi() ###
###-------------------------###


## Need a special type of data object:
missData3 <- missing_data.frame(missData)

## Check the meta data:
show(missData3)

### Everything was guessed correctly because
### we correctly typed the data to begin with.

## We want to use parallel processing
## we need to set the global option for the number of parallel cores
## used by all R processes to get mi() to use more than 2 cores.
options("mc.cores" = 4)


### STEP 1: IMPUTATION PHASE ###


miOut <- mi(missData3,
            parallel = TRUE,
            n.chains = 4,
            n.iter = 50,
            seed = 235711)

Rhats(miOut)
max(Rhats(miOut)) # Not looking good

## Let's continue running the model
## mi() allows you to pick up with model estimation
## where a previous run left off.
miOut2 <- mi(miOut,
             parallel = TRUE,
             n.iter = 50,
             seed = 131719)

Rhats(miOut2)
max(Rhats(miOut2)) # Not quite.

## Let's keep going:
miOut3 <- mi(miOut2,
             parallel = TRUE,
             n.iter = 50,
             seed = 232931)

Rhats(miOut3)
max(Rhats(miOut3)) # There we go :)

round(mipply(miOut3, mean, to.matrix = TRUE), 3)

## That's troubling.
## Let's run the model a bit longer:
miOut4 <- mi(miOut3,
             parallel = TRUE,
             n.iter = 200,
             seed = 374143)

Rhats(miOut4)
max(Rhats(miOut4))

## Something wrong with sysRac items:
round(mipply(miOut4, mean, to.matrix = TRUE), 3)

## We can get a lot of fine-grained plots:
plot(miOut4, m = 2)

### If we ignore the problematic convergence
### (which we should not!!!), then we can go on
### to fit our analysis models.

impList4 <- mi::complete(miOut4, 50)

impList5 <- lapply(impList4,
                   FUN = function(x) {
                       numData <- data.frame(
                           lapply(x, as.numeric)
                       )
                       sysRac <-
                           rowMeans(numData[ , grep("sysRac",
                                                    colnames(numData)
                                                    )
                                            ])
                       policy <-
                           rowMeans(numData[ , grep("policy",
                                                    colnames(numData)
                                                    )
                                            ])
                       data.frame(sysRac,
                                  policy,
                                  x[ , c("polAffil", "revDisc")]
                                  )
                   })



### STEP 2: ANALYSIS PHASE ###



## Fit a simple regression model:
fitList3 <- lapply(impList5,
                   FUN = function(x) {
                       lm(policy ~ sysRac + polAffil + revDisc, data = x)
                   })


### STEP 3: POOLING PHASE ###


pooledOut3 <- MIcombine(fitList3)

## Look at the FMI:
pooledOut3$missinfo

## Calcuate t-statistics:
pooledWald3 <- coef(pooledOut3) / sqrt(diag(vcov(pooledOut3)))

## Get p-values:
pValues3 <- round(
    pt(abs(pooledWald3), df = pooledOut3$df, lower = FALSE) * 2,
    3)

## Put the results in a nice table:
outTab3 <- round(cbind(coef(pooledOut3),
                       sqrt(diag(vcov(pooledOut3))),
                       pooledWald3,
                       pValues3), 3)
colnames(outTab3) <- c("Estimate", "SE", "Wald", "P-Value")

outTab3



###################
##### SUMMARY #####
###################



## Compare FMI across methods:
fmiMat <- round(cbind(pooledOut$missinfo,
                      pooledOut2$missinfo,
                      pooledOut3$missinfo), 3)
colnames(fmiMat) <- c("mice", "amelia", "mi")
fmiMat
