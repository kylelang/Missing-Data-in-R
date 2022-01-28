### Title:    Missing Data in R: MI Demonstration
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2022-01-28

rm(list = ls(all = TRUE)) # Clear workspace

## Intall packages as necessary:
install.packages("miceadds", #c("Amelia", "mice", "mitools"),
                 repos = "http://cloud.r-project.org")

## Load the packages we'll use:
library(naniar)
library(mice)
library(miceadds)
library(mitools)
library(Amelia)
library(semTools)
library(dplyr)
library(magrittr)

dataDir <- "../../../data/"
plotDir <- "../plots/"
outDir  <- "../output/"

## Read in the incomplete data:
missData <- readRDS(paste0(dataDir, "adams_klps_data-example.rds"))

## Subset the data to make examples run faster: 
#drops <- c(paste0("riae", c(2, 3, 7:9, 11, 12)),
#           paste0("nori", c(2, 7, 9)),
#           paste0("wpriv", c(1:3, 5:9))
#           )

#missData %<>% select(-drops)


##--Missing Data Descriptives-------------------------------------------------##

## Take a peek:
summary(missData)

## Visualize the missing data:
vis_miss(missData)

## First look at the percent missing:
(pm <- is.na(missData) %>% colMeans())

## Compute the covaraince coverage using the md.pairs() is part of the mice package
cover <- md.pairs(missData)$rr / nrow(missData)
hist(cover)
cover[cover < 1] %>% range()

## How many unique response patterns:
md.pattern(missData) %>% nrow() - 1


###-Missing Data Imputation--------------------------------------------------###

### We need a vector of elementary imputation methods. We'll build the method
### vector by matching elementary imputation method to variable type.

## Start by getting each variables class:
classVec <- sapply(missData, class)

## Check the number of levels for each variable:
countLevels <- function(x) na.omit(x) %>% unique() %>% length()
levelVec    <- sapply(missData, countLevels)

## Bayesian linear regression will be the defaul method: 
methVec <- rep("norm", ncol(missData))

## Change the method for cateogrical variables
methVec[classVec == "factor" & levelVec == 2] <- "logreg"
methVec[classVec == "factor" & levelVec > 2]  <- "polyreg"

## Don't try to impute completely observed variables:
methVec[pm == 0] <- ""

## Make sure everything is lining up correctly:
data.frame(names  = colnames(missData),
           method = methVec,
           levels = levelVec,
           class  = classVec,
           pm     = round(pm, 3)
           )

## Run the imputation algorithm:
miceOut <- mice(data   = missData,
                m      = 20,
                method = methVec,
                maxit  = 5,
                seed   = 235711)

### Now we need to run our imputation diagnostics to check that the imputation
### process went well

## Use miceadds::Rhat.mice() to compute PSR factors for the imputed item means
## and variances:
Rhat.mice(miceOut)

## Construct the traceplots of the imputed item means and standard deviations:
pdf(paste0(plotDir, "mice_trace-i10.pdf"), onefile = TRUE)
plot(miceOut)
dev.off()

### The R-Hats and traceplots suggest potential nonstationarity. We should run
### more iterations.

## Use mice::mice.mids() to add more iterations to the Markov Chain we started
## with our previous mice() run:
miceOut <- mice.mids(miceOut, maxit = 10)

## Re-check convergence
Rhat.mice(miceOut)

## Construct the traceplots of the imputed item means and standard deviations:
pdf(paste0(plotDir, "mice_trace-i15.pdf"), onefile = TRUE)
plot(miceOut)
dev.off()

## Use mice::mice.mids() to add more iterations to our Markov Chain:
miceOut <- mice.mids(miceOut, maxit = 20)

## Re-check convergence
Rhat.mice(miceOut)

## Construct the traceplots of the imputed item means and standard deviations:
pdf(paste0(plotDir, "mice_trace-i35.pdf"), onefile = TRUE)
plot(miceOut)
dev.off()

### Let's say that the imputation model has converged. Next, we need to check
### that the imputed values themselves are sensible:

## Create density plots of imputed and observed values:
pdf(paste0(plotDir, "mice_density-i35.pdf"), onefile = TRUE)

targets <- colnames(missData)[pm > 0]
for(v in targets) {
    f <- paste0("~", v) %>% as.formula()
    densityplot(miceOut, f) %>% print()
}

dev.off()

## Stripplots of imputed and observed values:
pdf(paste0(plotDir, "mice_strip-i35.pdf"), onefile = TRUE)

for(v in targets) {
    stripplot(miceOut,
              paste0(v, "~.imp") %>% as.formula()
              ) %>%
        print()
}

dev.off()


##----------------------------------------------------------------------------##

## Change the method used to impute 'policy' items:
methVec[grep("policy", colnames(missData))] <- "pmm"

## Make sure everything is lining up correctly:
data.frame(names  = colnames(missData),
           method = methVec,
           levels = levelVec,
           class  = classVec,
           pm     = round(pm, 3)
           )

## Use mice::quickpred() to construct a predictor matrix to select a subset of
## the possible predictors for use in each EIM:
predMat <- quickpred(missData,
                     mincor  = 0.3,
                     exclude = "sex",
                     include = c("nori1", "riae1")
                     )
predMat

## Run the imputation algorithm:
miceOut2 <- mice(data            = missData,
                 m               = 20,
                 method          = methVec,
                 predictorMatrix = predMat,
                 maxit           = 30,
                 seed            = 235711)

## Check convergence:
Rhat.mice(miceOut2)

pdf(paste0(plotDir, "mice_trace2-i30.pdf"), onefile = TRUE)
plot(miceOut2)
dev.off()

pdf(paste0(plotDir, "mice_density2-i30.pdf"), onefile = TRUE)

for(v in targets) {
    f <- paste0("~", v) %>% as.formula()
    densityplot(miceOut2, f) %>% print()
}

dev.off()

pdf(paste0(plotDir, "mice_strip2-i30.pdf"), onefile = TRUE)

for(v in targets) {
    stripplot(miceOut2,
              paste0(v, "~.imp") %>% as.formula()
              ) %>%
        print()
}

dev.off()


###-Analyzing MI Data--------------------------------------------------------###

## Use the with.mids() function to fiit a model directly to the imputed data in
## the mids object:
fit1 <- with(miceOut2, lm(policy1 ~ nori1 + nori4 + nori10 + polv + sex))

## Pool the results:
(pool1 <- pool(fit1))
summary(pool1)

## Extract the FMIs:
pool1$pooled$fmi

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
