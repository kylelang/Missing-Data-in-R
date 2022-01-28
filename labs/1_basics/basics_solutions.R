### Title:    Missing Data in R: Missing Data Basics Suggested Solutions
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2022-01-28


###-Overview-----------------------------------------------------------------###

## You will practice exploring a missing data problem in R.

## You will need the "bfiOE.rds" and "adams_klps_data-example.rds" datasets.
## These datasets are saved in the "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) If you have not already done so, use the install.packages() function to
##    install the "mice", "ggplot2", and "naniar" packages.

install.packages(c("mice", "ggplot2", "naniar"),
                 repos = "http://cloud.r-project.org")

## 2) Use the library() function to load the "mice", "ggplot2", and "naniar"
##    packages.

library(mice)
library(ggplot2)
library(naniar)
library(dplyr) # We'll also use this to streamline some answers

## 3) Use the readRDS() function to load the "adams_klps_data-example.rds" and
##    "bfiOE.rds" datasets.

dataDir <- "../../data/"

bfi   <- readRDS(paste0(dataDir, "bfiOE.rds"))
adams <- readRDS(paste0(dataDir, "adams_klps_data-example.rds"))


##--Missing Data Descriptives-------------------------------------------------##

### Use the "bfiOE" data to answer the following questions:

## 1a) Compute the proportion of missing values for each variable.

pm <- colMeans(is.na(bfi))

## 1b) What is the percentage of missing data for "O1"?

100 * pm["O1"]

## 2a) Compute the number of observed values for each variable:

nObs <- colSums(!is.na(bfi))

## 2b) What is the number of observed values for "E1"?

nObs["E1"]

## 3a) Compute the covariance coverage matrix.

cc <- md.pairs(bfi)$rr / nrow(bfi)

## 3b) What is the range of covariance coverage values?

### Extract unique coverage values:
cc2 <- cc[lower.tri(cc)]
range(cc2)

## 3c) What is the covariance coverage between "E2" and "O4"?

cc["E2", "O4"]

## 3d) How many covariance coverages are less that 0.75?

sum(cc2 < 0.75)

## 4a) Compute the missing data patterns for these data.

pats <- md.pattern(bfi, plot = FALSE)
pats

## 4b) How many distinct missing data patterns exist in these data?

nrow(pats) - 1

## 4c) How many missing data patterns have only one missing value?

### Find the entries in the last column equal to 1:
flag <- pats[-nrow(pats), ncol(pats)] == 1

sum(flag)

## 4d) How many observations are affected by patterns that involve only one
##     missing value?

### Convert the rownames into a numeric vector giving the number of cases
### affected by each pattern:
counts <- rownames(pats)[-nrow(pats)] %>% as.numeric()

### Count the number of cases affected by the patterns flagged in (4d):
counts[flag] %>% sum()


###-Missing Data Visualizations----------------------------------------------###

### Use the "adams_klps_data-example" data to answer the following questions:

## 1) Use naniar::vis_miss() to visualize the spatial distribtuion of the
##    missing data.

vis_miss(adams)

## 2) Use naniar::gg_miss_var() to visualize the percents missing for each
##    variable in the data.

gg_miss_var(adams, show_pct = TRUE)

## 3a) Use naniar::geom_miss_point() to visualize the relative distribution of
##     missing values between "raie1" and "wpriv1"

(p1 <- ggplot(adams, aes(riae1, wpriv1)) + geom_miss_point())

## 3b) Facet the plot you made in (3a) by "sex".

p1 + facet_wrap(vars(sex))

## 4) Use naniar::gg_miss_upset() to visualize the coverages for all "policy"
##    items in the data.

adams %>%
    select(matches("policy\\d")) %>%
    gg_miss_upset(nsets = ncol(.), nintersects = NA)


###-END----------------------------------------------------------------------###
