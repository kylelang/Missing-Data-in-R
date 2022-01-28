### Title:    Missing Data Analysis: FIML Examples
### Author:   Kyle M. Lang
### Created:  2015-OCT-26
### Modified: 2018-OCT-21

install.packages(c("lavaan", "semTools"), repos = "http://cloud.r-project.org")

rm(list = ls(all = TRUE))

dataDir <- "../data/"

library(lavaan)
library(semTools)

## Load the data:
missData <- readRDS(paste0(dataDir, "adamsKlpsSynthData.rds"))

## Scale the data:
missData$liberal <- as.numeric(missData$liberal == "lib")
missData         <- as.data.frame(scale(missData))

## Simple CFA:
mod1 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
policy =~ policy1 + policy2 + policy3
"

out1 <- cfa(model   = mod1,
            data    = missData,
            std.lv  = TRUE,
            missing = "fiml")

summary(out1)
fitMeasures(out1)


## CFA w/ auxiliary variable:
mod1.2 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
policy =~ policy1 + policy2 + policy3

# Correlate the auxiliary variable with all residual variances:
wPriv1 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3
"

out1.2 <- cfa(model = mod1.2, data = missData, std.lv = TRUE, missing = "fiml")

summary(out1.2)
fitMeasures(out1.2)

fitMeasures(out1.2) - fitMeasures(out1)

## Special null model to accomodate the auxiliary variables:
nullMod1.2 <- "
wPriv1 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3
"

nullOut1.2 <- cfa(model = nullMod1.2, data = missData, missing = "fiml")
summary(nullOut1.2)

fitMeasures(nullOut1.2)["df"] - fitMeasures(out1)["baseline.df"]

## Define a function to use the special null model to compute CFI & TLI:
getAfi <- function(altObj, nullObj) {
    ## Extract the fit indices:
    nullFit <- inspect(nullObj, "fit")
    altFit  <- inspect(altObj, "fit")
    
    ## Compute non-centrality parameters:
    nullNCP <- nullFit["chisq"] - nullFit["df"]
    altNCP  <- altFit["chisq"] - altFit["df"]
    
    ## Compute TLI ratio terms:
    nullRatio <- nullFit["chisq"] / nullFit["df"]
    altRatio <- altFit["chisq"] / altFit["df"]
    
    ## Compute corrected CFI and TLI values:
    cfi <- min(1, ((nullNCP - altNCP) / nullNCP))
    tli <- (nullRatio - altRatio) / (nullRatio - 1.0)

    c(cfi = as.numeric(cfi), tli = as.numeric(tli))
}

getAfi(altObj = out1.2, nullObj = nullOut1.2)
fitMeasures(out1.2)[c("cfi", "tli")]

## Simple structural model:
mod2 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
policy =~ policy1 + policy2 + policy3

policy + sysRac ~ liberal
"

out2 <- sem(model = mod2, data = missData, std.lv = TRUE, missing = "fiml")
out2 <- sem(model   = mod2,
            data    = missData,
            std.lv  = TRUE,
            missing = "fiml",
            fixed.x = FALSE)
summary(out2)

## Structural model w/ auxiliary variable:
mod2.2 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
policy =~ policy1 + policy2 + policy3

policy + sysRac ~ liberal

wPriv1 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3 +
liberal
"

out2.2 <- sem(model   = mod2.2,
              data    = missData,
              std.lv  = TRUE,
              missing = "fiml",
              fixed.x = FALSE)

summary(out2.2)
fitMeasures(out2.2)

## We need a slightly modified null model:
nullMod2.2 <- "
liberal ~~ liberal # We must estimate a variance for the random predictor
wPriv1 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3 +
liberal
"

nullOut2.2 <- cfa(model = nullMod2.2, data = missData, missing = "fiml")
summary(nullOut2.2)

fitMeasures(nullOut2.2)["df"]
fitMeasures(out2)["baseline.df"]

fitMeasures(out2.2)[c("cfi", "tli")]
getAfi(altObj = out2.2, nullObj = nullOut2.2)


## We can 'automatically' include auxiliary variables with the cfa.auxiliary()
## and sem.auxiliary() functions in semTools.
out1.3 <-
    cfa.auxiliary(model = mod1, data = missData, aux = "wPriv1", std.lv = TRUE)
summary(out1.3)

out2.3 <-
    sem.auxiliary(model = mod2, data = missData, aux = "wPriv1", std.lv = TRUE)
summary(out2.3)

## The results should be identical to what we got via the manual approach:
inspect(out2.3, "coef")$lambda - inspect(out2.2, "coef")$lambda
inspect(out2.3, "theta") - inspect(out2.2, "theta")
inspect(out2.3, "coef")$psi - inspect(out2.2, "coef")$psi

## Fit indices are also equal:
fitMeasures(out2.3)[c("chisq", "df", "rmsea", "aic", "bic")] -
    fitMeasures(out2.2)[c("chisq", "df", "rmsea", "aic", "bic")]

fitMeasures(out2.3)[c("cfi", "tli")] -
    getAfi(altObj = out2.2, nullObj = nullOut2.2)


##--MANIFEST VARIABLE REGRESSION----------------------------------------------##


## Linear regression model w/o auxiliaries:
mod3 <- "policy1 ~ sysRac1 + liberal"

out3 <- sem(model = mod3, data = missData, missing = "fiml", fixed.x = FALSE)
summary(out3)

## Including the auxliary variable:
mod3.2 <- "
policy1 ~ sysRac1 + liberal

# Correlate the auxiliary variable with all predictors and the DV's residual:
wPriv1 ~~
policy1 +
sysRac1 +
liberal

sysRac1 ~~ liberal
"

out3.2 <-
    sem(model = mod3.2, data = missData, missing = "fiml", fixed.x = FALSE)
summary(out3.2)

## We can also use semTools::sem.auxiliary():
out3.3 <- sem.auxiliary(model = mod3, data = missData, aux = "wPriv1")
summary(out3.3)

## Again, we get the same results:
inspect(out3.2, "coef")$beta - inspect(out3.3, "coef")$beta
inspect(out3.2, "coef")$alpha - inspect(out3.3, "coef")$alpha
inspect(out3.2, "coef")$psi - inspect(out3.3, "coef")$psi


##--MULTIPLE AUXILIARIES------------------------------------------------------##


## CFA w/ auxiliary variable:
mod4 <- "
sysRac =~ sysRac1 + sysRac2 + sysRac3
policy =~ policy1 + policy2 + policy3

# Correlate additional auxiliaries in the same pattern as the single auxiliary.
# Make sure to correlate the auxiliary variables themselves.

wPriv1 ~~
wPriv2  +
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3

wPriv2 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3
"

out4 <- cfa(model = mod4, data = missData, std.lv = TRUE, missing = "fiml")
summary(out4)

## Special null model to accomodate the auxiliary variables:
nullMod4 <- "
wPriv1 ~~
wPriv2  +
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3

wPriv2 ~~
sysRac1 +
sysRac2 +
sysRac3 +
policy1 +
policy2 +
policy3
"

nullOut4 <- cfa(model = nullMod4, data = missData, missing = "fiml")
summary(nullOut4)

afi4 <- getAfi(altObj = out4, nullObj = nullOut4)
afi4

## Using semTools::cfa.auxiliary() to get the same result:
out4.2 <- cfa.auxiliary(model  = mod1,
                        data   = missData,
                        aux    = c("wPriv1", "wPriv2"),
                        std.lv = TRUE)
summary(out4.2)

## Compare approaches:
inspect(out4, "coef")$lambda - inspect(out4.2, "coef")$lambda
inspect(out4, "theta") - inspect(out4.2, "theta")
inspect(out4, "coef")$psi - inspect(out4.2, "coef")$psi

## Fit indices are also equal:
fitMeasures(out4)[c("chisq", "df", "rmsea", "aic", "bic")] -
    fitMeasures(out4.2)[c("chisq", "df", "rmsea", "aic", "bic")]

fitMeasures(out4.2)[c("cfi", "tli")] - afi4

predict(out4.2)


##--COMPUTE FMI---------------------------------------------------------------##

n <- nrow(missData)

## Extract the model-implied covariance matrix and mean vector:
cov0  <- fitted.values(out4)$cov * (n / (n - 1))
mean0 <- fitted.values(out4)$mean

## Get the "observed" SE:
seO <- inspect(out4, "se")

## Re-estimate the model using the "complete data" sufficient stats:
out5 <- cfa(model         = mod4,
            sample.cov    = cov0,
            sample.mean   = mean0,
            sample.nobs   = nrow(missData),
            std.lv        = TRUE,
            information   = "observed",
            meanstructure = TRUE)

## Get the "complete data" SE:
seC <- inspect(out5, "se")

## Compute the FMI for all parameters:
fmi <- list()
for(i in 1 : length(se1)) fmi[[i]] <- 1 - seC[[i]] / seO[[i]]

fmi
