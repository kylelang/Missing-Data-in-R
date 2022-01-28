### Title:    Stats & Methods Lab 1 Practice Script
### Author:   Kyle M. Lang
### Created:  2018-04-10
### Modified: 2020-09-09


###          ###
### Overview ###
###          ###

## You will practice inferential testing, some basic EDA techniques, missing
## data descriptives, and outlier analysis.

## You will need four datasets to complete the following tasks: "bfi_clean.rds",
## "tests.rds", "bfiOE.rds", and "airQual.rds". These datasets are saved in the
## "data" directory for this set of lab materials.


###                   ###
### Tasks / Questions ###
###                   ###


##--Preliminaries-------------------------------------------------------------##

## 1) If you have not already done so, use the "install.packages" function to
##    install the "mice" package.

## 2) Use the "library" function to load the "mice" and "MASS" packages.

## 3) Use the "paste0" function and the "readRDS" function to load the four
##    datasets into memory.


##--Testing-------------------------------------------------------------------##

### Use the "bfi_clean" data to complete the following analyses/tasks:

## 1a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Do not assume equal variances.
## 1b) What is the value of the estimated mean difference in "agree"?
## 1c) What is the value of the estimated t-statistic?
## 1d) Is the estimated mean difference significant at the alpha = 0.05 level?

## 2a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Assume equal variances.
## 2b) What is the value of the estimated mean "agree" for males?
## 2c) What is the value of the estimated t-statistic?
## 2d) What is the 95% CI for the estimated mean difference?
## 2e) Is the t-statistic you found here different from the one you computed in
##     Q1? If so, why?

## 3a) Test for a significant Pearson correlation between "agree" and "neuro"
##    (i.e., neuroticism).
## 3b) What is the value of the correlation coefficient?
## 3c) Is this correlation significantly different from zero at the alpha = 0.05
##     level?

## 4a) Test the hypothesis that the correlation between "consc"
##     (i.e., conscientiousness) and "neuro" is less than zero.
## 4b) What is the value of the estimated correlation coefficient?
## 4c) What is the 95% CI for the estimated correlation coefficient?
## 4d) Is this correlation significantly less than zero at the alpha = 0.05
##     level?
## 4e) How can you use the 95% CI to answer Q4d?


##--EDA-----------------------------------------------------------------------##

### Use the "tests" data to answer the following questions:

## 1) What are the dimensions of these data?

## 2) What is the mean "SATQ" score?

## 3) What is the variance of the "SATQ" scores?

## 4) What is the median "SATV" score?

## 5) Create a histogram of the "ACT" variable.

## 6) Create a kernel density plot of the "ACT" variable.

## 7) Overlay a normal density on top of the "ACT" histogram.

## 8) Create a grouped boxplot that plots "ACT" by "education".

## 9) Create a frequency table of "education".

## 10) Create a contingency table that cross-classifies "gender" and
##     "education".

## 11) Suppose a certain university admits any student with an ACT score of, at
##     least, 25. How many of the women in the "tests" data would be admitted?


##--Missing Data Descriptives-------------------------------------------------##

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


##--Outliers------------------------------------------------------------------##

### NOTE: You can use the functions provided in the demonstrations script to
###       complete the following tasks.

### Use the "airQual" data to complete the following:

## 1a) Use Tukey's boxplot method to find possible and probable outliers in the
##     "Ozone", "Solar.R", "Wind", and "Temp" variables.
## 1b) Did you find any possible outliers?
## 1c) Did you find any probable outliers?
## 1d) Which, if any, observations were possible outliers on "Ozone"?
## 1e) Which, if any, observations were probable outliers on "Wind"?

## 2a) Use the MAD method (with a cutoff of 3) to find potential outliers in
##     the "Ozone", "Solar.R", "Wind", and "Temp" variables.
## 2b) Did you find any potential outliers?
## 2c) Which, if any, observations are potential outliers on "Wind"?

### For Question 3, you will use different parameterizations of robust
### Mahalanobis distance (with MCD estimation) to check for multivariate
### outliers on the "Ozone", "Solar.R", "Wind", and "Temp" variables.

### NOTE: When running the mcdMahaonobis() function, set the seed to "235711".

## 3a) Which, if any, observations are flagged as multivariate outliers when
##     using 75% of the sample for the MCD estimation and using a probability of
##     0.99 for the cutoff value?
## 3b) Which, if any, observations are flagged as multivariate outliers when
##     using 75% of the sample for the MCD estimation and using a probability of
##     0.999 for the cutoff value?
## 3c) Which, if any, observations are flagged as multivariate outliers when
##     using 50% of the sample for the MCD estimation and using a probability of
##     0.99 for the cutoff value
## 3d) Which, if any, observations are flagged as multivariate outliers when
##     using 50% of the sample for the MCD estimation and using a probability of
##     0.999 for the cutoff value?
## 3e) Based on the above, what consequences do you observe when changing the
##     fraction of the sample used for MCD estimation and when changing the
##     cutoff probability?
