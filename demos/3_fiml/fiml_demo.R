### Title:    Missing Data in R: FIML Demonstration
### Author:   Kyle M. Lang
### Created:  2015-10-26
### Modified: 2023-01-31

rm(list = ls(all = TRUE))

dataDir <- "data/"

library(lavaan)
library(semTools)

## Load the data:
compData <- readRDS(paste0(dataDir, "adams_klps_data-synthetic.rds"))
missData <- readRDS(paste0(dataDir, "adams_klps_data-incomplete.rds"))


###-Confirmatory Factor Analysis---------------------------------------------###

## Simple CFA:
cfaMod1 <- "
indRac =~ riae1 + riae4 + riae5 + riae6 + riae10
policy =~ policy1 + policy2 + policy3 + policy4 + policy5 + policy6
"

## Estimate the model:
cfaFit1 <- cfa(model   = cfaMod1,
               data    = missData,
               std.lv  = TRUE,
               missing = "fiml")

summary(cfaFit1)
fitMeasures(cfaFit1)


##----------------------------------------------------------------------------##

## CFA w/ auxiliary variables:
cfaMod2 <- "
indRac =~ riae1 + riae4 + riae5 + riae6 + riae10
policy =~ policy1 + policy2 + policy3 + policy4 + policy5 + policy6

# Correlate the auxiliary variables with each other and all residuals:
nori1 ~~
nori4 +
riae1 +
riae4 +
riae5 +
riae6 +
riae10 +
policy1 +
policy2 +
policy3 +
policy4 +
policy5 +
policy6

nori4 ~~
riae1 +
riae4 +
riae5 +
riae6 +
riae10 +
policy1 +
policy2 +
policy3 +
policy4 +
policy5 +
policy6
"

## Estimate the model:
cfaFit2 <- cfa(model   = cfaMod2,
               data    = missData,
               std.lv  = TRUE,
               missing = "fiml")

summary(cfaFit2)
fitMeasures(cfaFit2)


##----------------------------------------------------------------------------##

## Compare fit of the auxiliary and the no-auxiliary models:
fitMeasures(cfaFit2) - fitMeasures(cfaFit1)

## Try the same comparison without any missing data:
compFit1 <- cfa(model = cfaMod1, data = compData, std.lv = TRUE)
compFit2 <- cfa(model = cfaMod2, data = compData, std.lv = TRUE)

fitMeasures(compFit2) - fitMeasures(compFit1)


###-Structural Equation Model------------------------------------------------###

## Simple SEM:
semMod1 <- "
indRac =~ riae1 + riae4 + riae5 + riae6 + riae10
policy =~ policy1 + policy2 + policy3 + policy4 + policy5 + policy6

policy + indRac ~ polv
"

## Fit the model:
semFit1 <- sem(model   = semMod1,
               data    = missData,
               std.lv  = TRUE,
               missing = "fiml")

summary(semFit1, fit.measures = TRUE, fmi = TRUE)


##----------------------------------------------------------------------------##

## SEM w/ auxiliary variables:
semMod2 <- "
indRac =~ riae1 + riae4 + riae5 + riae6 + riae10
policy =~ policy1 + policy2 + policy3 + policy4 + policy5 + policy6

policy + indRac ~ polv

# Also correlate the auxiliary variables with the IV:
nori1 ~~
nori4 +
riae1 +
riae4 +
riae5 +
riae6 +
riae10 +
policy1 +
policy2 +
policy3 +
policy4 +
policy5 +
policy6 +
polv

nori4 ~~
riae1 +
riae4 +
riae5 +
riae6 +
riae10 +
policy1 +
policy2 +
policy3 +
policy4 +
policy5 +
policy6 +
polv
"

## Fit the model:
semFit2 <- sem(model   = semMod2,
               data    = missData,
               std.lv  = TRUE,
               missing = "fiml")

summary(semFit2, fit.measures = TRUE, fmi = TRUE)


###-Path Analysis------------------------------------------------------------###

## Simple mediation model
paMod1 <- "
policy1 ~ b*riae1 + c*polv
riae1 ~ a*polv

ie := a * b
te := a + b + c
"

paFit1 <- sem(model = paMod1, data = missData, missing = "fiml")
summary(paFit1)


##----------------------------------------------------------------------------##

## Mediation model w/ auxiliaries:
paMod2 <- "
policy1 ~ b*riae1 + c*polv
riae1 ~ a*polv

# Correlate the auxiliary variables with each other, the IVs, and the DVs:
nori1 ~~
nori4 +
policy1 +
riae1 +
polv

nori4 ~~
policy1 +
riae1 +
polv

ie := a * b
te := a + b + c
"

paFit2 <- sem(model = paMod2, data = missData, missing = "fiml")

summary(paFit2, fit.measures = TRUE, fmi = TRUE)


###-Making Life Easier with semTools-----------------------------------------###

## We can automatically include auxiliaries with semTools::cfa.auxiliary()
## and semTools::sem.auxiliary():
cfaFit3 <- cfa.auxiliary(model  = cfaMod1,
                         data   = missData,
                         aux    = c("nori1", "nori4"),
                         std.lv = TRUE)

summary(cfaFit3, fit.measures = TRUE, fmi = TRUE)

semFit3 <- sem.auxiliary(model  = semMod1,
                         data   = missData,
                         aux    = c("nori1", "nori4"),
                         std.lv = TRUE)

summary(semFit3, fit.measures = TRUE, fmi = TRUE)

paFit3 <- sem.auxiliary(model  = paMod1,
                        data   = missData,
                        aux    = c("nori1", "nori4"),
                        std.lv = TRUE)

summary(paFit3, fit.measures = TRUE, fmi = TRUE)


##----------------------------------------------------------------------------##

## The results should be identical to what we got via the manual approach:
inspect(cfaFit3, "coef")$lambda - inspect(cfaFit2, "coef")$lambda
inspect(cfaFit3, "theta") - inspect(cfaFit2, "theta")
inspect(cfaFit3, "coef")$psi - inspect(cfaFit2, "coef")$psi


###-END----------------------------------------------------------------------###
