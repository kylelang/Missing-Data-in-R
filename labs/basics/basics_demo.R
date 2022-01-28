### Title:    Missing Data in R: Missing Data Basics Demonstration Script
### Author:   Kyle M. Lang
### Created:  2018-09-10
### Modified: 2022-01-28

## Clear the workspace:
rm(list = ls(all = TRUE))

## Set your working directory to the "labs/basics/" directory, if necessary:
setwd("")

## Load packages:
library(mice)    # For missing data descriptives
library(naniar)  # For visualizations
library(dplyr)   # For data processing
library(ggplot2) # For plotting

## Define the data directory:
dataDir <- "../../data/"


###-Descriptives-------------------------------------------------------------###

bfi <- readRDS(paste0(dataDir, "bfiANC.rds"))

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

## Find missing data patterns:
missPat <- md.pattern(bfi)
missPat

## Extract the variablewise missing counts:
missPat[nrow(missPat), ]
missPat[nrow(missPat), -ncol(missPat)]

## Extract the patternwise missing counts:
missPat[ , ncol(missPat)]
missPat[-nrow(missPat), ncol(missPat)]

## Extract the counts of pattern membership:
rownames(missPat) %>% as.numeric()
rownames(missPat)[-nrow(missPat)] %>% as.numeric()

## Close the graphics device:
dev.off()

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
(cc[lower.tri(cc, diag = TRUE)] > 0.8) %>% mean()

## What's on the diagonial of the coverage matrix?
diag(cc) - po


###-Visualizations-----------------------------------------------------------###

## Visualize the spatial distribution of missing data using naniar::vis_mis():
vis_miss(bfi)

## Visualize the variablewise nonresponse rates using naniar::gg_miss_var():
gg_miss_var(bfi)
gg_miss_var(bfi, show_pct = TRUE) + ggtitle("Percent Missing for Each Variable")
gg_miss_var(bfi, facet = education)
gg_miss_var(bfi, facet = education, show_pct = TRUE)

## Visualize the casewise nonresponse rates using naniar::gg_miss_case():
gg_miss_case(bfi)
gg_miss_case(bfi, show_pct = TRUE)
gg_miss_case(bfi, order_cases = FALSE)

## Visualize the relative distributions of missing values using
## naniar::geom_miss_point():
p1 <- ggplot(bfi, aes(A1, N1)) + geom_miss_point()
p1 + facet_wrap(vars(gender))

## Visualize coverages using naniar::gg_miss_upset():
gg_miss_upset(bfi)

bfi %>%
    select(education, matches("^A\\d")) %>%
    gg_miss_upset(nsets = ncol(.), nintersects = NA)

## Visualize response patterns using mice::md.pattern():
bfi %>%
    select(education, matches("^A\\d")) %>%
    md.pattern(rotate.names = TRUE)

