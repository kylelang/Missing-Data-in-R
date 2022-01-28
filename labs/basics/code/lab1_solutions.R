### Title:    Stats & Methods Lab 1 Suggested Solutions
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

rm(list = ls(all = TRUE))

##--Preliminaries-------------------------------------------------------------##

## 1) If you have not already done so, use the "install.packages" function to
##    install the "mice" package.

install.packages("mice", repos = "http://cloud.r-project.org")

## 2) Use the "library" function to load the "mice" and "MASS" packages.

library(mice)
library(MASS)

## 3) Use the "paste0" function and the "readRDS" function to load the four
##    datasets into memory.

dataDir <- "../data/"

bfi1    <- readRDS(paste0(dataDir, "bfi_clean.rds"))
bfi2    <- readRDS(paste0(dataDir, "bfiOE.rds"))
tests   <- readRDS(paste0(dataDir, "tests.rds"))
airQual <- readRDS(paste0(dataDir, "airQual.rds"))


##--Testing-------------------------------------------------------------------##

### Use the "bfi_clean" data to complete the following analyses/tasks:

## 1a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Do not assume equal variances.

out1.1 <- t.test(agree ~ gender, data = bfi1)
out1.1

## 1b) What is the value of the estimated mean difference in "agree"?

diff(out1.1$estimate)

### OR ###

diff(rev(out1.1$estimate))

## 1c) What is the value of the estimated t-statistic?

out1.1$statistic

## 1d) Is the estimated mean difference significant at the alpha = 0.05 level?

ifelse(out1.1$p.value < 0.05, "YES", "NO")

## 2a) Conduct a t-test to check for gender differences in mean levels of
##     "agree" (i.e., agreeableness). Assume equal variances.

out1.2 <- t.test(agree ~ gender, data = bfi1, var.equal = TRUE)
out1.2

## 2b) What is the value of the estimated mean "agree" for males?

out1.2$estimate[1]

## 2c) What is the value of the estimated t-statistic?

out1.2$statistic

## 2d) What is the 95% CI for the estimated mean difference?

out1.2$conf.int

## 2e) Is the t-statistic you found here different from the one you computed in
##     Q1? If so, why?

### YES. Although the estimated mean difference hasn't changed, the SE in Q1 was
### corrected to control for the unequal group variances, so the estimated
### t-statistic in Q1 was smaller.

## 3a) Test for a significant Pearson correlation between "agree" and "neuro"
##    (i.e., neuroticism).

out1.3 <- with(bfi1, cor.test(agree, neuro))
out1.3

## 3b) What is the value of the correlation coefficient?

out1.3$estimate

## 3c) Is this correlation significantly different from zero at the alpha = 0.05
##     level?

ifelse(out1.3$p.value < 0.05, "YES", "NO")

## 4a) Test the hypothesis that the correlation between "consc"
##     (i.e., conscientiousness) and "neuro" is less than zero.

out1.4 <- with(bfi1, cor.test(consc, neuro, alternative = "less"))
out1.4

## 4b) What is the value of the estimated correlation coefficient?

out1.4$estimate

## 4c) What is the 95% CI for the estimated correlation coefficient?

out1.4$conf.int

## 4d) Is this correlation significantly less than zero at the alpha = 0.05
##     level?

ifelse(out1.4$p.value < 0.05, "YES", "NO")

## 4e) How can you use the 95% CI to answer Q4d?

### Check if the interval surrounds zero.


##--EDA-----------------------------------------------------------------------##

### Use the "tests" data to answer the following questions:

## 1) What are the dimensions of these data?

dim(tests)

## 2) What is the mean "SATQ" score?

mean(tests$SATQ)

## 3) What is the variance of the "SATQ" scores?

var(tests$SATQ)

## 4) What is the median "SATV" score?

median(tests$SATQ)

## 5) Create a histogram of the "ACT" variable.

hist(tests$ACT)

## 6) Create a kernel density plot of the "ACT" variable.

plot(density(tests$ACT))

## 7) Overlay a normal density on top of the "ACT" histogram.

m <- mean(tests$ACT)
s <- sd(tests$ACT)
x <- with(tests, seq(min(ACT), max(ACT), length.out = 1000))
d <- dnorm(x = x, mean = m, sd = s)

hist(tests$ACT, probability = TRUE, ylim = range(d))
lines(x = x, y = d)

## 8) Create a grouped boxplot that plots "ACT" by "education".

with(tests, boxplot(ACT ~ education))

## 9) Create a frequency table of "education".

table(tests$education)

## 10) Create a contingency table that cross-classifies "gender" and
##     "education".

with(tests, table(gender, education))

## 11) Suppose a certain university admits any student with an ACT score of, at
##     least, 25. How many of the women in the "tests" data would be admitted?

tab <- with(tests, table(gender, admit = ACT >= 25))
tab
tab["female", "TRUE"]

### OR ###

sum(with(tests, gender == "female" & ACT >= 25))


##--Missing Data Descriptives-------------------------------------------------##

### Use the "bfiOE" data to answer the following questions:

## 1a) Compute the proportion of missing values for each variable.

pm <- colMeans(is.na(bfi2))
pm

## 1b) What is the percentage of missing data for "O1"?

100 * pm["O1"]

## 2a) Compute the number of observed values for each variable:

nObs <- colSums(!is.na(bfi2))
nObs

## 2b) What is the number of observed values for "E1"?

nObs["E1"]

## 3a) Compute the covariance coverage matrix.

cc <- md.pairs(bfi2)$rr / nrow(bfi2)
cc

## 3b) What is the range of covariance coverage values?

### Extract unique coverage values:
cc2 <- cc[lower.tri(cc)]
range(cc2)

## 3c) What is the covariance coverage between "E2" and "O4"?

cc["E2", "O4"]

## 3d) How many covariance coverages are less that 0.75?

sum(cc2 < 0.75)

## 4a) Compute the missing data patterns for these data.

pats <- md.pattern(bfi2, plot = FALSE)
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
counts <- as.numeric(rownames(pats))[-nrow(pats)]

### Count the number of cases affected by the patterns flagged in (4d):
sum(counts[flag])


##--Outliers------------------------------------------------------------------##

### NOTE: You can use the functions provided in the demonstrations script to
###       complete the following tasks.

### Initialize the functions we'll need to answer the questions below:

## Function to implement the boxplot method:
bpOutliers <- function(x) {
    ## Compute inner and outer fences:
    iFen <- boxplot.stats(x, coef = 1.5)$stats[c(1, 5)]
    oFen <- boxplot.stats(x, coef = 3.0)$stats[c(1, 5)]
    
    ## Return the row indices of flagged 'possible' and 'probable' outliers:
    list(possible = which(x < iFen[1] | x > iFen[2]),
         probable = which(x < oFen[1] | x > oFen[2])
         )
}

## Function to implement the MAD method:
madOutliers <- function(x, cut = 2.5, na.rm = TRUE) {
    ## Compute the median and MAD of x:
    mX   <- median(x, na.rm = na.rm)
    madX <- mad(x, na.rm = na.rm)
    
    ## Return row indices of observations for which |T_MAD| > cut:
    which(abs(x - mX) / madX > cut)
}

## Function to implement a robust version of Mahalanobis distance using MCD
## estimation:
mcdMahalanobis <- function(data, prob, ratio = 0.75, seed = NULL) {
    ## Set a seed, if one is provided:
    if(!is.null(seed)) set.seed(seed)
    
    ## Compute the MCD estimates of the mean and covariance matrix:
    stats <- cov.mcd(data, quantile.used = floor(ratio * nrow(data)))
    
    ## Compute robust squared Mahalanobis distances
    md <- mahalanobis(x = data, center = stats$center, cov = stats$cov)
    
    ## Find the cutoff value:
    crit <- qchisq(prob, df = ncol(data))
    
    ## Return row indices of flagged observations:
    which(md > crit)
}

### Use the "airQual" data to complete the following:

## 1a) Use Tukey's boxplot method to find possible and probable outliers in the
##     "Ozone", "Solar.R", "Wind", and "Temp" variables.

### Define a vector of variable names:
vars <- c("Ozone", "Solar.R", "Wind", "Temp")

### Check for outliers in each of the variables named in 'vars':
bpOuts <- lapply(airQual[vars], bpOutliers)
bpOuts

## 1b) Did you find any possible outliers?

### Extract possible outliers:
posOl <- lapply(bpOuts, "[[", x = "possible")

### Check for the presence of possible outliers:
check <- any(sapply(posOl, length) > 0)
ifelse(check, "YES", "NO")

## 1c) Did you find any probable outliers?

### Extract probable outliers:
probOl <- lapply(bpOuts, "[[", x = "probable")

### Check for the presence of probable outliers:
check <- any(sapply(probOl, length) > 0)
ifelse(check, "YES", "NO")

## 1d) Which, if any, observations were possible outliers on "Ozone"?

bpOuts$Ozone$possible

## 1e) Which, if any, observations were probable outliers on "Wind"?

bpOuts$Wind$probable

## 2a) Use the MAD method (with a cutoff of 3) to find potential outliers in
##     the "Ozone", "Solar.R", "Wind", and "Temp" variables.

madOuts <- lapply(airQual[vars], madOutliers, cut = 3.0)
madOuts

## 2b) Did you find any potential outliers?

check <- length(unlist(madOuts)) > 0
ifelse(check, "YES", "NO")

## 2c) Which, if any, observations are potential outliers on "Wind"?

madOuts$Wind

### For Question 3, you will use different parameterizations of robust
### Mahalanobis distance (with MCD estimation) to check for multivariate
### outliers on the "Ozone", "Solar.R", "Wind", and "Temp" variables.

### NOTE: When running the mcdMahaonobis() function, set the seed to "235711".

## 3a) Which, if any, observations are flagged as multivariate outliers when
##     using 75% of the sample for the MCD estimation and using a probability of
##     0.99 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.99, ratio = 0.75, seed = 235711)

## 3b) Which, if any, observations are flagged as multivariate outliers when
##     using 75% of the sample for the MCD estimation and using a probability of
##     0.999 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.999, ratio = 0.75, seed = 235711)

## 3c) Which, if any, observations are flagged as multivariate outliers when
##     using 50% of the sample for the MCD estimation and using a probability of
##     0.99 for the cutoff value

mcdMahalanobis(airQual[vars], prob = 0.99, ratio = 0.5, seed = 235711)

## 3d) Which, if any, observations are flagged as multivariate outliers when
##     using 50% of the sample for the MCD estimation and using a probability of
##     0.999 for the cutoff value?

mcdMahalanobis(airQual[vars], prob = 0.999, ratio = 0.5, seed = 235711)

## 3e) Based on the above, what consequences do you observe when changing the
##     fraction of the sample used for MCD estimation and when changing the
##     cutoff probability?

### Increasing the cutoff probability or the sample ratio decreases the number
### of identified outliers. In other words, lower cutoff probabilities and
### sample ratios result in more liberal tests.
