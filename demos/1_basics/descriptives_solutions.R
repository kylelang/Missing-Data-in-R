### Title:    Missing Data in R: Missing Data Descriptives Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2025-01-27


###-Preliminaries------------------------------------------------------------###

## Load the packages we'll need below:
library(mice)
library(ggplot2)
library(naniar)
library(dplyr)

## 1.1) Use the readRDS() function to load the "adams_klps_data-example.rds" and
##    "bfiOE.rds" datasets.

dataDir <- "data/"

bfi   <- readRDS(paste0(dataDir, "bfiOE.rds"))
adams <- readRDS(paste0(dataDir, "adams_klps_data-example.rds"))


###-Missing Data Descriptives------------------------------------------------###

## 1.2a) Compute the proportion of missing values for each variable.

pm <- colMeans(is.na(bfi))

## 1.2b) What is the percentage of missing data for "O1"?

100 * pm["O1"]

## 1.2c) Compute the number of observed values for each variable:

nObs <- colSums(!is.na(bfi))

## 1.2d) What is the number of observed values for "E1"?

nObs["E1"]

##----------------------------------------------------------------------------##

## 1.3a) Compute the missing data patterns for these data.

pats <- md.pattern(bfi, plot = FALSE)
pats

## 1.3b) How many distinct missing data patterns exist in these data?

nrow(pats) - 1

## 1.3c) How many missing data patterns have only one missing value?

### Find the entries in the last column equal to 1:
flag <- pats[-nrow(pats), ncol(pats)] == 1

sum(flag)

## 1.3d) How many observations are affected by patterns that involve only one
##     missing value?

### Convert the rownames into a numeric vector giving the number of cases
### affected by each pattern:
counts <- rownames(pats)[-nrow(pats)] %>% as.numeric()

### Count the number of cases affected by the patterns flagged in (4d):
counts[flag] %>% sum()

##----------------------------------------------------------------------------##

## 1.4a) Compute the covariance coverage matrix.

cc <- md.pairs(bfi)$rr / nrow(bfi)

## 1.4b) What is the range of covariance coverage values?

### Extract unique coverage values:
cc2 <- cc[lower.tri(cc)]
range(cc2)

## 1.4c) What is the covariance coverage between "E2" and "O4"?

cc["E2", "O4"]

## 1.4d) How many covariance coverages are less that 0.75?

sum(cc2 < 0.75)


###-Missing Data Visualizations----------------------------------------------###

## 1.5) Use naniar::vis_miss() to visualize the distribtuion of the missing data.

vis_miss(adams)

##----------------------------------------------------------------------------##

## 1.6) Use naniar::gg_miss_var() to visualize the percents missing for each
##      variable in the data.

gg_miss_var(adams, show_pct = TRUE)

##----------------------------------------------------------------------------##

## 1.7a) Use naniar::geom_miss_point() to visualize the relative distribution of
##       missing values between "raie1" and "policy1"

head(adams)

(p1 <- ggplot(adams, aes(riae1, policy1)) + geom_miss_point())

## 1.7b) Facet the plot you made in (1.7a) by "sex".

p1 + facet_wrap(vars(sex))

##----------------------------------------------------------------------------##

## 1.8) Use naniar::gg_miss_upset() to visualize the coverages for all "policy"
##      items in the data.

adams %>%
    select(matches("policy\\d")) %>%
    gg_miss_upset(nsets = ncol(.), nintersects = NA)


###-END----------------------------------------------------------------------###
