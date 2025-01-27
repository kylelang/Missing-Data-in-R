### Title:    Missing Data in R: Missing Data Descriptives
### Author:   Kyle M. Lang
### Created:  2018-09-10
### Modified: 2025-01-27

## Clear the workspace:
rm(list = ls(all = TRUE))

## Load packages:
library(mice)    # For missing data descriptives
library(naniar)  # For visualizations
library(dplyr)   # For data processing
library(ggplot2) # For plotting
library(ggmice)  # more plotting

## Define the data directory:
dataDir <- "data/"

################################################################################
## Practice Problem 1.1
## 
## Use the readRDS() function to load the "adams_klps_data-example.rds" and
## "bfiOE.rds" datasets.
##
################################################################################


###-Descriptives-------------------------------------------------------------###

bfi <- readRDS(paste0(dataDir, "bfiANC.rds"))

## Compute summary stats for each variable:
summary(bfi)


## Use the summary() function to summarize the 'nhanes' dataset from the 
## Compute the missingness and response matrices:
mMat <- is.na(bfi)
rMat <- !is.na(bfi)

## Compute variable-wise counts/percents missing/observed:
(cm <- colSums(mMat))
(pm <- colMeans(mMat))

(co <- colSums(rMat))
(po <- colMeans(rMat))

nrow(bfi) - cm
1 - pm

nrow(bfi) - co
1 - po

## Summarize PM:
mean(pm)
median(pm)
sd(pm)
range(pm)

pm2 <- pm[pm > 0]

mean(pm2)
median(pm2)
sd(pm2)
range(pm2)

## Find variables with PM greater than 10%:
pm[pm > 0.1]

### NOTE: Use the "bfiOE" data to answer Practice Problems 1.2 - 1.4

################################################################################
## Practice Problem 1.2
##
## a) Compute the proportion of missing values for each variable.
## b) What is the percentage of missing data for "O1"?
## c) Compute the number of observed values for each variable:
## d) What is the number of observed values for "E1"?
##
################################################################################

## Find missing data patterns:
missPat <- md.pattern(bfi)
missPat

## Create a nicer visualization:
plot_pattern(bfi, rotate = TRUE)

## Extract the variablewise missing counts:
missPat[nrow(missPat), ]
missPat[nrow(missPat), -ncol(missPat)]

## Extract the patternwise missing counts:
missPat[ , ncol(missPat)]
missPat[-nrow(missPat), ncol(missPat)]

## Extract the counts of pattern membership:
rownames(missPat) %>% as.numeric()
rownames(missPat) %>% head(-1)  %>% as.numeric()

## Close the graphics device:
dev.off()

################################################################################
## Practice Problem 1.3
##
## a) Compute the missing data patterns for these data.
## b) How many distinct missing data patterns exist in these data?
## c) How many missing data patterns have only one missing value?
## d) How many observations are affected by patterns that involve only one
##    missing value?
##
################################################################################

## Compute covariance coverage:
(cc <- md.pairs(bfi)$rr / nrow(bfi))

## Summarize coverages:
range(cc)
cc[cc < 1] %>% range()

## Check that coverage exceeds some threshold:
eps <- 0.8
all(cc > eps)

## Find problematic pairs:
pat <- cc <= eps
apply(pat, 1, function(x) names(x)[x])

## Select only the unique elements of the coverage matrix:
cc[lower.tri(cc)]

## Also include the diagonal elements:
cc[lower.tri(cc, diag = TRUE)]

## What proportion of coverages exceed 0.8?
(cc[lower.tri(cc)] > 0.8) %>% mean()

## What's on the diagonal of the coverage matrix?
diag(cc) - po

################################################################################
## Practice Problem 1.4
##
## a) Compute the covariance coverage matrix.
## b) What is the range of covariance coverage values?
## c) What is the covariance coverage between "E2" and "O4"?
## d) How many covariance coverages are less that 0.75?
##
################################################################################


###-Visualizations-----------------------------------------------------------###

## Visualize the spatial distribution of missing data using naniar::vis_mis():
vis_miss(bfi)

### NOTE: Use the "adams_klps_data-example" data to answer Practice Problems
###       1.5 - 1.8

################################################################################
## Practice Problem 1.5
##
## Use naniar::vis_miss() to visualize the distribtuion of the missing data.
##
################################################################################

## Visualize the variablewise nonresponse rates using naniar::gg_miss_var():
gg_miss_var(bfi)
gg_miss_var(bfi, show_pct = TRUE) + ggtitle("Percent Missing for Each Variable")
gg_miss_var(bfi, facet = education)
gg_miss_var(bfi, facet = education, show_pct = TRUE)

################################################################################
## Practice Problem 1.6
##
## Use naniar::gg_miss_var() to visualize the percents missing for each variable
## in the data.
##
################################################################################

## Visualize the casewise nonresponse rates using naniar::gg_miss_case():
gg_miss_case(bfi)
gg_miss_case(bfi, show_pct = TRUE)
gg_miss_case(bfi, order_cases = FALSE)

## Visualize the relative distributions of missing values using
## naniar::geom_miss_point():
p1 <- ggplot(bfi, aes(A1, N1)) + geom_miss_point()
p1 + facet_wrap(vars(gender))

################################################################################
## Practice Problem 1.7
##
## a) Use naniar::geom_miss_point() to visualize the relative distribution of
##    missing values between "raie1" and "policy1"
## b) Facet the plot you made in (3a) by "sex".
##
################################################################################

## Visualize coverages using naniar::gg_miss_upset():
gg_miss_upset(bfi)

bfi %>%
    select(education, matches("^A\\d")) %>%
    gg_miss_upset(nsets = ncol(.), nintersects = NA)

################################################################################
## Practice Problem 1.8
##
## Use naniar::gg_miss_upset() to visualize the coverages for all "policy" items
## in the data.
##
################################################################################

## Visualize response patterns using mice::md.pattern():
bfi %>%
    select(education, matches("^A\\d")) %>%
    plot_pattern(rotate = TRUE)


###-END----------------------------------------------------------------------###
