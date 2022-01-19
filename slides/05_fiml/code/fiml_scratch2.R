### Title:    FIML Slides Scratch Space
### Author:   Kyle M. Lang
### Created:  2018-OCT-11
### Modified: 2018-OCT-11

source("../../../code/supportFunctions.R")
library(mvtnorm)
library(optimx)
library(SURF)

data(iris)

dataDir <- "../../../data/"

diabetes <- readRDS(paste0(dataDir, "diabetes.rds"))

## Fit a model:
out1 <- lm(ldl ~ bp + glu + bmi, data = diabetes)

## Extract the predicted values and estimated residual 
## standard error:
yHat <- predict(out1)
s    <- summary(out1)$sigma

## Compute the row-wise probabilities:
pY <- dnorm(diabetes$ldl, mean = yHat, sd = s)

## Compute the loglikelihood, and compare to R's version:
sum(log(pY)); logLik(out1)[1]

tmp <- logLik(out1)

tmp[1]
tmp

## Complete data loglikelihood function:
ll <- function(par, data) {
    ## Extract the parameter matrices:
    p  <- ncol(data)
    mu <- par[1 : p]
    
    ## Populate sigma from its cholesky factor:
    sigma <-vecChol(par[-c(1 : p)], revert = TRUE)
    
    ## Compute the row-wise contributions to the LL:
    ll0 <- rep(NA, nrow(data))
    for(n in 1 : nrow(data))
        ll0[n] <- 
            dmvnorm(data[n, ], mean = mu, sigma = sigma, log = TRUE)
    
    sum(ll0)# return the overall LL value
}

?chol

parms <- list()
parms$nObs    <- 1000
parms$meanVec <- c(1.0, 2.0, 3.0) # x, y, z
parms$corVec  <- c(0.2, 0.5, 0.4) # xy, xz, yz

## Create some toy data:
simData <- simulateSimpleData(parms)

## Vector of starting values for the parameters:
mu0    <- colMeans(iris[ , -5])
sigma0 <- cov(iris[ , -5])

matrix(sigma0, 4)

par0 <- c(mu0, chol(sigma0)[!lower.tri(sigma0)])

?dmvnorm

getLL(par0, iris[ , -5])

tmp <- matrix(runif(9), 3, 3)
tmp <- tmp %*% t(tmp)

r <- chol(tmp)

crossprod(r) == tmp

tmp[lower.tri(tmp, diag = TRUE)]



?lower.tri

x <- tmp
x <- y

x <- 1 : 10
y <- (x * (x + 1)) / 2

cbind(x, y)

p <- function(x) (-1 + sqrt(1 + 8 * length(x))) / 2

vecChol <- function(x, revert = FALSE) {
    if(revert) {
        tmp                  <- matrix(0, p(x), p(x))
        tmp[!lower.tri(tmp)] <- x
        crossprod(tmp)
    }
    else
        chol(x)[!lower.tri(x)]
}

r <- vecChol(tmp)
tmp2 <- vecChol(r, TRUE)

tmp == tmp2


## Use optimx() to numerically optimize the LL function:
llOpt <- optimx(par     = par0,
                fn      = ll,
                data    = as.matrix(iris[ , -5]),
                method  = "BFGS",
                upper   = 100.0,
                control = list(maximize = TRUE)
                )

## Get the optimize mean vector and covariance matrix:
muHat    <- llOpt[1 : 4]
sigmaHat <- vecChol(as.numeric(llOpt[5 : 14]), revert = TRUE)

## Look at the results:
muHat
sigmaHat

muHat - colMeans(iris[ , -5])
sigmaHat - cov(iris[ , -5])
