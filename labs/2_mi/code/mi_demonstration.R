### Title:    Missing Data in R: MI Demonstration
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2022-01-28

rm(list = ls(all = TRUE)) # Clear workspace

## Load the packages we'll use:
library(naniar)
library(mice)
library(miceadds)
library(mitools)
library(semTools)
library(dplyr)
library(psych)

dataDir <- "../../../data/"
plotDir <- "../plots/"

## Read in the incomplete data:
missData <- readRDS(paste0(dataDir, "adams_klps_data-example.rds"))


###-Missing Data Descriptives------------------------------------------------###

## Take a peek:
summary(missData)

## Visualize the missing data:
vis_miss(missData)

## Look at the percent missing:
(pm <- is.na(missData) %>% colMeans())

## Compute the covaraince coverage using mice::md.pairs():
cover <- md.pairs(missData)$rr / nrow(missData)
hist(cover)
cover[cover < 1] %>% range()

## How many unique response patterns?
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


###-Tweak the Imputation Model-----------------------------------------------###

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

## Fit a restricted model:
fit2 <- with(miceOut2, lm(policy1 ~ nori1 + nori4 + nori10 + sex))

## Summarize the results:
pool(fit2) %>% summary()

## Compare models via a likelihood ratio test:
anova(fit1, fit2)
D1(fit1, fit2)
D3(fit1, fit2)

## Use miceadds::mi.anova() to run a factorial ANOVA on the imputed data:
mi.anova(miceOut2, "policy1 ~ polv * sex")


###-Working Directly with the Imputed Datasets-------------------------------###

## Use mice::complete() to generate a list of imputed datasets:
impList <- complete(miceOut2, "all")

## Use psych::scoreVeryFast() to create some scale scores from the imputed data:
keys <- list(indRac = grep("riae\\d", colnames(missData), value = TRUE),
             policy = grep("policy\\d", colnames(missData), value = TRUE)
             )

## "policy2" needs to be reverse coded:
keys$policy <- gsub("policy2", "-policy2", keys$policy)

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


###-SEM with MI--------------------------------------------------------------###

## Simple CFA model:
mod <- "
fIndRac =~ riae1 + riae4 + riae5 + riae6 + riae10
fPolicy =~ policy1 + policy2 + policy3 + policy4 + policy5 + policy6
"

## Use semTools::cfa.mi() to fit the above lavaan model to the imputed data and
## pool the results:
fit <- cfa.mi(model = mod, data = impList, std.lv = TRUE)

## Summarize the results:
summary(fit, fit.measures = TRUE)


###-END----------------------------------------------------------------------###
