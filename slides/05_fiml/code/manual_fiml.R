### Title:    Manual ML/FIML Examples
### Author:   Kyle M. Lang
### Created:  2018-OCT-11
### Modified: 2018-OCT-19

rm(list = ls(all = TRUE))

source("../../../code/supportFunctions.R")
library(mvtnorm)
library(optimx)
library(SURF)
library(mgcv)
library(lavaan)

dataDir  <- "../../../data/"
diabetes <- readRDS(paste0(dataDir, "diabetes.rds"))


##--ML------------------------------------------------------------------------##

## Complete data loglikelihood function:
ll <- function(par, data) {
    ## Extract the parameter matrices:
    p  <- ncol(data)
    mu <- par[1 : p]
    
    ## Populate sigma from its cholesky factor:
    sigma <-vecChol(par[-c(1 : p)], revert = TRUE)
    
    ## Compute the row-wise contributions to the LL:
    ll0 <- 
        dmvnorm(data, mean = mu, sigma = sigma, log = TRUE)
    
    sum(ll0)# return the overall LL value
}

## Find the number of variables given a vector of unique variance/covariances:
nV <- function(x) (-1 + sqrt(1 + 8 * length(x))) / 2

## Convert from covariance matrix to vectorized Cholesky factor and back:
vecChol <- function(x, revert = FALSE) {
    if(revert) {
        tmp                  <- matrix(0, nV(x), nV(x))
        tmp[!lower.tri(tmp)] <- x
        crossprod(tmp)
    }
    else
        chol(x)[!lower.tri(x)]
}

dat1 <- as.matrix(diabetes[ , c("bmi", "ldl", "glu")])

m0   <- rep(0, 3)
s0   <- vecChol(diag(3))
par0 <- c(m0, s0)

## Use optimx() to numerically optimize the LL function:
mle <- optimx(par     = par0,
              fn      = ll,
              data    = dat1,
              method  = "BFGS",
              control = list(maximize = TRUE, maxit = 1000)
              )

## Get the optimize mean vector and covariance matrix:
muHat    <- mle[1 : 3]
sigmaHat <- vecChol(as.numeric(mle[4 : 9]), revert = TRUE)

## Look at the results:
muHat
sigmaHat


##--FIML----------------------------------------------------------------------##

## Compute the within-pattern contributions to the LL:
ll0 <- function(i, mu, sigma, pats, ind, data) {
    ## Find the current pattern:
    p1 <- pats[i, ]
    
    if(sum(p1) > 1) # More than one observed variable?
        dmvnorm(x     = data[ind == i, p1],
                mean  = mu[p1],
                sigma = sigma[p1, p1],
                log   = TRUE)
    else
        dnorm(x    = data[ind == i, p1],
              mean = mu[p1],
              sd   = sqrt(sigma[p1, p1]),
              log  = TRUE)
}


llm <- function(par, data, pats, ind) {
    ## Extract the parameter matrices:
    p  <- ncol(data)
    mu <- par[1 : p]
    
    ## Populate sigma from its cholesky factor:
    sigma <-vecChol(par[-c(1 : p)], revert = TRUE)
    
    ## Compute the pattern-wise contributions to the LL:
    ll1 <- sapply(X     = 1 : nrow(pats),
                  FUN   = ll0,
                  mu    = mu,
                  sigma = sigma,
                  pats  = pats,
                  ind   = ind,
                  data  = data)

    sum(unlist(ll1))
}
    
## Prepare the data
dat1 <- as.matrix(diabetes[ , c("bmi", "ldl", "glu")])
dat2 <- imposeMissData(dat     = dat1,
                       targets = list(mar = c("ldl", "glu")),
                       preds   = "bmi",
                       pm      = 0.3,
                       snr     = 2,
                       pattern = "low")$data

## Summarize response patterns:
rMat <- !is.na(dat2)
pats <- uniquecombs(rMat)
ind  <- attr(pats, "index")

## Choose some starting values:
m0   <- colMeans(dat2, na.rm = TRUE)
s0   <- vecChol(cov(dat2, use = "pairwise"))
par0 <- c(m0, s0)

## Use optimx() to numerically optimize the LL function:
mle <- optimx(par     = par0,
              fn      = llm,
              data    = dat2,
              pats    = pats,
              ind     = ind,
              method  = "BFGS",
              control = list(maximize = TRUE, maxit = 1000)
              )

## Get the optimize mean vector and covariance matrix:
muHat    <- mle[1 : 3]
sigmaHat <- vecChol(as.numeric(mle[4 : 9]), revert = TRUE)

## Confirm the manual approach by using lavaan() to get the FIML estimates:
mod1 <- "
bmi ~~ ldl + glu
ldl ~~ glu
"

## Fit the model with lavaan():
out1 <- cfa(mod1, data = dat2, missing = "fiml")
summary(out1)

muHat2    <- inspect(out1, "est")$nu
sigmaHat2 <- inspect(out1, "theta")

muHat - muHat2
(sigmaHat - sigmaHat2)
