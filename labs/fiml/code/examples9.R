### Title:    Missing Data Course: Lecture 9 Examples
### Author:   Kyle M. Lang
### Created:  2015-OCT-26
### Modified: 2015-OCT-27

install.packages(c("lavaan", "semTools", "mice"),
                 repos = "http://rweb.quant.ku.edu/cran")

rm(list = ls(all = TRUE))

dataDir <- "../data/"

set.seed(235711)

library(mice)
library(lavaan)
library(semTools)
source("lecture9SupportFunctions.R")

missData <- readRDS(paste0(dataDir, "adamsKlpsMissData.rds"))

## Peak at the data:
summary(missData)

## First look at the percent missing:
colMeans(is.na(missData))

## Look at the covaraince coverage
## md.pairs() is part of the mice package
mice::md.pairs(missData)$rr / nrow(missData)

### Coverage looks pretty good, not too much missing data
### we should go ahead and try to impute the nonresponse.


##### LATENT VARIABLE MODELS ######


## Simple CFA:
mod1 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
indRac =~ indRac1 + indRac2 + indRac3
policy =~ policy1 + policy2 + policy3
"

out1 <- cfa(mod1,
            data = missData,
            std.lv = TRUE,
            missing = "fiml",
            information = "observed")

summary(out1)
fitMeasures(out1)

## CFA w/ auxiliary variable:
mod2 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
indRac =~ indRac1 + indRac2 + indRac3
policy =~ policy1 + policy2 + policy3

# Correlate the auxiliary variable
# with all residual variances:
revDisc ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

out2 <- cfa(mod2,
            data = missData,
            std.lv = TRUE,
            missing = "fiml",
            information = "observed")

summary(out2)

## Special null model to accomodate the auxiliary variables:
nullMod2 <- "
revDisc ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

nullOut2 <- cfa(nullMod2,
                data = missData,
                missing = "fiml",
                information = "observed")

summary(nullOut2)

fitIndices2 <- fitMeasures(out2)
afi2 <- getAFI(altFitObj = out2, nullFitObj = nullOut2)

fitIndices2
afi2

## Structural model w/ auxiliary variable:
mod3 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
indRac =~ indRac1 + indRac2 + indRac3
policy =~ policy1 + policy2 + policy3

policy + sysRac + indRac ~ polAffil

revDisc ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

out3 <- sem(mod3,
            data = missData,
            std.lv = TRUE,
            missing = "fiml",
            information = "observed")

summary(out3)

## We need a slightly modified null model:
nullMod3 <- "
polAffil ~~ polAffil
revDisc ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

nullOut3 <- cfa(nullMod3,
                data = missData,
                missing = "fiml",
                information = "observed")

summary(nullOut3)

fitIndices3 <- fitMeasures(out3)
afi3 <- getAFI(altFitObj = out3, nullFitObj = nullOut3)

fitIndices3
afi3


## We can 'automatically' include auxiliary variables
## with the auxiliary() function in semTools.

## First, get the fitted model object (w/o auxiliaries):
out1.2 <- cfa(mod1,
              data = missData,
              std.lv = TRUE,
              meanstructure = TRUE)

## Now we let semTools::auxiliary() modify that fitted lavaan
## object to add the auxiliaries and adjust the null model:
out1.2.2 <- cfa.auxiliary(out1.2,
                          aux = "revDisc",
                          data = missData,
                          missing = "fiml")

summary(out1.2.2)

## The results are identical to what
## we got via the manual approach:
inspect(out1.2.2, "coef")$lambda -
    inspect(out2, "coef")$lambda

inspect(out1.2.2, "coef")$psi -
    inspect(out2, "coef")$psi

inspect(out1.2.2, "coef")$theta -
    inspect(out2, "coef")$theta

fitIndices1.2.2 <- inspect(out1.2.2, "fit")

## Fit indices are also equal:
fitIndices1.2.2[c("chisq", "df", "rmsea")] -
    fitIndices2[c("chisq", "df", "rmsea")]

fitIndices1.2.2[c("cfi", "tli")] - unlist(afi2)


##### MANIFEST VARIABLE REGRESSION #####


## Linear regression model w/o auxiliaries:
mod4 <- "
policyT ~ indRacT + sysRacT + polAffil
"

out4 <- sem(mod4,
            data = missData,
            missing = "fiml",
            information = "observed",
            fixed.x = FALSE)

summary(out4)

## Including the auxliary variable:
mod5 <- "
policyT ~ indRacT + sysRacT + polAffil

# Correlate the auxiliary variable with
# all predictors and the DV's residual:
revDisc ~~
indRacT +
sysRacT +
polAffil +
policyT

indRacT ~~
sysRacT +
polAffil

sysRacT ~~
polAffil
"

out5 <- sem(mod5,
            data = missData,
            missing = "fiml",
            information = "observed",
            fixed.x = FALSE)

## We can't correlate the auxiliaries with fixed predictors,
## since such predictors don't have any variance, so we need
## to treat all predictors as random variables and model their
## covariances (to keep the degrees of freedom consistent with
## the usual fixed X OLS).

summary(out5)

## We can also use semTools::auxiliary():
out5.2 <- sem(mod4,
              data = missData,
              fixed.x = FALSE,
              meanstructure = TRUE)

out5.2.2 <- sem.auxiliary(out5.2,
                          aux = "revDisc",
                          data = missData,
                          missing = "fiml",
                          information = "observed",
                          fixed.x = FALSE)

summary(out5.2.2)

## Again, we get the same results:
inspect(out5.2.2, "coef")$beta -
    inspect(out5, "coef")$beta


##### INCLUDING MULTIPLE AUXILIARIES ######


## CFA w/ auxiliary variable:
mod6 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
indRac =~ indRac1 + indRac2 + indRac3
policy =~ policy1 + policy2 + policy3

# Simply correlate additional auxiliaries in the same
# pattern as the single auxiliary.
# Make sure to correlate the auxiliary variables themselves.
revDisc ~~
polAffil +
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3

polAffil ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

out6 <- cfa(mod6,
            data = missData,
            std.lv = TRUE,
            missing = "fiml",
            information = "observed")

summary(out6)

## Special null model to accomodate the auxiliary variables:
nullMod6 <- "
revDisc ~~
polAffil +
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3

polAffil ~~
sysRac1 +
sysRac2 +
sysRac3 +
indRac1 +
indRac2 +
indRac3 +
policy1 +
policy2 +
policy3
"

nullOut6 <- cfa(nullMod6,
                data = missData,
                missing = "fiml",
                information = "observed")

summary(nullOut6)

fitIndices6 <- fitMeasures(out6)
afi6 <- getAFI(altFitObj = out6, nullFitObj = nullOut6)

fitIndices6
afi6

## Using semTools::auxiliary() to get the same result:
out1.3 <- cfa(mod1,
              data = missData,
              std.lv = TRUE,
              meanstructure = TRUE)

## Now we let semTools::auxiliary() modify that fitted lavaan
## object to add the auxiliaries and adjust the null model:
out1.3.2 <- cfa.auxiliary(out1.3,
                          aux = c("revDisc", "polAffil"),
                          data = missData,
                          missing = "fiml")

summary(out1.3.2)

## The results are identical to what
## we got via the manual approach:
inspect(out1.3.2, "coef")$lambda -
    inspect(out6, "coef")$lambda

inspect(out1.3.2, "coef")$psi -
    inspect(out6, "coef")$psi

inspect(out1.3.2, "coef")$theta -
    inspect(out6, "coef")$theta

fitIndices1.3.2 <- inspect(out1.3.2, "fit")

## Fit indices are also equal:
fitIndices1.3.2[c("chisq", "df", "rmsea")] -
    fitIndices6[c("chisq", "df", "rmsea")]

fitIndices1.3.2[c("cfi", "tli")] - unlist(afi6)
