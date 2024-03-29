### Title:    Missing Data in R: Missing Data Basics Practice Script
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2022-01-30


###-Overview-----------------------------------------------------------------###

## You will practice exploring a missing data problem in R.

## You will need the "bfiOE.rds" and "adams_klps_data-example.rds" datasets.
## These datasets are saved in the "data" directory for this course.


###-Preliminaries------------------------------------------------------------###

## 1) Use the library() function to load the "mice", "ggplot2", and "naniar"
##    packages.

## 2) Use the readRDS() function to load the "adams_klps_data-example.rds" and
##    "bfiOE.rds" datasets.


###-Missing Data Descriptives------------------------------------------------###

### Use the "bfiOE" data to answer the following questions:

## 1a) Compute the proportion of missing values for each variable.
## 1b) What is the percentage of missing data for "O1"?

## 2a) Compute the number of observed values for each variable:
## 2b) What is the number of observed values for "E1"?

## 3a) Compute the covariance coverage matrix.
## 3b) What is the range of covariance coverage values?
## 3c) What is the covariance coverage between "E2" and "O4"?
## 3d) How many covariance coverages are less that 0.75?

## 4a) Compute the missing data patterns for these data.
## 4b) How many distinct missing data patterns exist in these data?
## 4c) How many missing data patterns have only one missing value?
## 4d) How many observations are affected by patterns that involve only one
##     missing value?


###-Missing Data Visualizations----------------------------------------------###

### Use the "adams_klps_data-example" data to answer the following questions:

## 1) Use naniar::vis_miss() to visualize the spatial distribtuion of the
##    missing data.

## 2) Use naniar::gg_miss_var() to visualize the percents missing for each
##    variable in the data.

## 3a) Use naniar::geom_miss_point() to visualize the relative distribution of
##     missing values between "raie1" and "policy1"
## 3b) Facet the plot you made in (3a) by "sex".

## 4) Use naniar::gg_miss_upset() to visualize the coverages for all "policy"
##    items in the data.


###-END----------------------------------------------------------------------###
