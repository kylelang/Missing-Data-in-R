### Title:    Missing Data in R: MI Demonstration
### Author:   Kyle M. Lang
### Created:  2015-10-04
### Modified: 2023-01-31

rm(list = ls(all = TRUE)) # Clear workspace

## Load the packages we'll use:
library(naniar)
library(mice)
library(miceadds)
library(mitools)
library(dplyr)
library(psych)
library(ggplot2)
library(ggmice)

dataDir <- "data/"
plotDir <- "demos/2_mi/plots/"

## Read in the incomplete data:
missData <- readRDS(paste0(dataDir, "adams_klps_data-example.rds"))


###-Missing Data Descriptives------------------------------------------------###

## Take a peek:
summary(missData)

## Visualize the missing data:
vis_miss(missData)

## Look at the percent missing:
(pm <- is.na(missData) %>% colMeans())

## Compute the covariance coverage using mice::md.pairs():
cover <- md.pairs(missData)$rr / nrow(missData)
hist(cover)
cover[cover < 1] %>% range()

## How many unique response patterns?
md.pattern(missData) %>% nrow() - 1


###-Missing Data Imputation--------------------------------------------------###

## Impute the missing values using a default application of mice():
miceOut <- mice(missData)

## Change a few settings:
miceOut <- mice(missData, m = 10, maxit = 20, seed = 235711)

## Estimate a model from the imputed data:
fits <- with(miceOut, lm(policy1 ~ sex))
res <- pool(fits)

res
summary(res)

## Create a list of multiply impute datasets:
impList <- complete(miceOut, "all")

###--------------------------------------------------------------------------###

## Check the elementary imputation methods:
(meth <- miceOut$method)

## Use Bayesian linear regression to impute the RIAE items:
meth[grep("riae", names(meth))] <- "norm"
meth

## Re-run the imputation:
miceOut <- mice(missData, m = 10, maxit = 20, method = meth, seed = 235711)

###--------------------------------------------------------------------------###

## Use a dummy run of mice() to initialize the meta data:
init <- mice(missData, maxit = 0)

ls(init)

(meth <- init$method)

## Impute all continuous variables using CART:
meth <- gsub("pmm", "cart", meth)

## Impute 'sex' with somethin' fancy:
meth["sex"] <- "lasso.select.logreg"

## Check the default predictor matrix:
(pred <- init$predictorMatrix)

## Do not use 'policy' items to impute 'riae' items:
pred[grep("riae", rownames(pred)), grep("policy", colnames(pred))] <- 0
pred

## Run the imputation algorithm:
miceOut <- mice(data = missData,
                m = 10,
                maxit = 10,
                method = meth,
                predictorMatrix = pred,
                seed = 235711)


###-Diagnostics-------------------------------------------------------------###

## Use miceadds::Rhat.mice() to compute PSR factors for the imputed item means
## and variances:
Rhat.mice(miceOut)

## Construct the traceplots of the imputed item means and standard deviations:
plot(miceOut)

## Do the same, but write the plots to a PDF:
pdf(paste0(plotDir, "mice_trace-i20.pdf"), onefile = TRUE)
plot(miceOut)
dev.off()

### If the R-Hats or the traceplots suggest nonstationarity. We should run more
### iterations.

## Use mice::mice.mids() to add more iterations to the Markov Chain we started
## with our previous mice() run:
miceOut <- mice.mids(miceOut, maxit = 10)

## Re-check convergence
Rhat.mice(miceOut)

plot(miceOut)

## Construct density plots of imputed vs. observed:
targets <- names(pm)[pm > 0]
for(v in targets) 
  p[[v]] <- ggmice(miceOut, aes(.data[[v]], group = .imp)) + geom_density()

p # Print the plots

## Create strip plots of imputed vs. observed values:
targets <- names(pm)[pm > 0]
for(v in targets) 
  p[[v]] <- ggmice(miceOut, aes(.imp, .data[[v]])) + geom_jitter(width = 0.25)

p # Print the plots


###-Analyzing MI Data--------------------------------------------------------###

## Use miceadds::micombine.cor() to run correlation tests on the imputed data:
varPositions <- grep("riae\\d", colnames(missData))
micombine.cor(miceOut, varPositions)

## Use mice::with.mids() to fit a linear regression directly to the imputed data
## in the mids object:
fit1 <- with(miceOut, lm(policy1 ~ nori1 + nori4 + nori10 + polv + sex))

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
fit2 <- with(miceOut, lm(policy1 ~ nori1 + nori4 + nori10 + sex))

## Summarize the results:
pool(fit2) %>% summary()

## Compare models via a likelihood ratio test:
anova(fit1, fit2)
D1(fit1, fit2)
D3(fit1, fit2)

## Use miceadds::mi.anova() to run a factorial ANOVA on the imputed data:
mi.anova(miceOut, "policy1 ~ polv * sex")


###-Working Directly with the Imputed Datasets-------------------------------###

## Use mice::complete() to generate a list of imputed datasets:
impList <- complete(miceOut, "all")

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


###-END----------------------------------------------------------------------###
