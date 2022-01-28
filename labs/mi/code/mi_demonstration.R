### Title:    Missing Data in R: MI Demonstration
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2022-01-28

rm(list = ls(all = TRUE)) # Clear workspace

## Intall packages as necessary:
install.packages("psych", #c("Amelia", "mice", "mitools"),
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
library(psych)

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

## Use miceadds::micombine.cor() to run correlation tests on the imputed data:
varPositions <- grep("riae\\d", colnames(missData))
micombine.cor(miceOut2, varPositions)

## Use mice::with.mids() to fit a linear regression directly to the imputed data
## in the mids object:
fit1 <- with(miceOut2, lm(policy1 ~ nori1 + nori4 + nori10 + polv + sex))

## Pool the results:
(pool1 <- pool(fit1))
summary(pool1)

## Extract the FMIs:
pool1$pooled$fmi

## Pool the R^2 and adjusted R^2:
pool.r.squared(fit1)
pool.r.squared(fit1, adjusted = TRUE)

## Pool the F-statistic two ways:
D1(fit1)
D3(fit1)

## Use miceadds::mi.anova() to run a factorial ANOVA on the imputed data:
mi.anova(miceOut2, "policy1 ~ polv * sex")


###-Processing the Imputed Data----------------------------------------------###

## Use mice::complete() to generate a list of imputed datasets:
impList <- complete(miceOut2, "all")

## Use psych::scoreVeryFast() to create some scale scores from the imputed data:
keys <- list(indRac = grep("riae\\d|nori\\d", colnames(missData), value = TRUE),
             policy = grep("policy\\d", colnames(missData), value = TRUE)
             )

impList <- lapply(impList,
                  function(x, keys) {
                      scales <- scoreVeryFast(keys, x)
                      data.frame(x, scales)
                  },
                  keys = keys)

summary(impList[[1]])

## Fit a linear regression model using the processed data:
fit1 <- lapply(impList, function(x) lm(policy ~ indRac + polv, data = x))
       
pool1 <- MIcombine(fit1)
summary(pool1)

summary(fit3.2[[1]])
ls(fit3.2[[1]])

## Extract the FMI:
(fmi <- pool1$missinfo)

## Extract coefficients:
(cf <- coef(pool1))

## Extract SEs:
(se <- vcov(pool1) %>% diag() %>% sqrt())

## Calcuate t-statistics:
(t <- cf / se)

## Get p-values:
(p <- 2 * pt(abs(t), df = pool1$df, lower = FALSE))

## Put the results in a nice table:
outTab           <- round(cbind(cf, se, t, p, fmi), 3)
colnames(outTab) <- c("Estimate", "SE", "t-Stat", "p-Value", "FMI")
rownames(outTab) <- c("Intercept", "Ind. Rac.", "Moderate", "Liberal")
outTab


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
