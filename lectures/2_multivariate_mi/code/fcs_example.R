### Title:    FCS Example
### Author:   Kyle M. Lang
### Created:  2018-OCT-15
### Modified: 2018-OCT-15

install.packages("norm", repos = "http://cloud.r-project.org")

library(devtools)
install_github("kylelang/SURF/source/SURF", ref = "develop")

library(SURF)
library(mice)
library(LaplacesDemon)
library(mitools)
library(xtable)
library(mgcv)
library(MCMCpack)
library(norm)

##### Set up the problem #####
parms <- list()
parms$nObs <- 1000
parms$pm <- 0.3
parms$auxVar <- "z"
parms$incompVars <- c("w", "x", "y")
parms$meanVec <- c(1 : 4)
parms$corVal <- 0.25
parms$marType <- c("lower", "center", "upper")

## Simulate some data:
simData <- simCovData(nObs  = 1000,
                      sigma = 0.25,
                      nVars = 4,
                      means = 1 : 4)
missData <- imposeMissData(data    = simData,
                           targets = list(mar = paste0("x", 1 : 3)),
                           preds   = "x4",
                           pm      = 0.3,
                           snr     = 5.0,
                           pattern = c(x1 = "low", x2 = "center", x3 = "high")
                           )$data

v <- "x1"
data <- impData
rVec <- rMat[ , v]

eif <- function(data, rVec, v) {
    ## Get the expected betas:
    fit  <- lm(paste(v, "~ ."), data = data[rVec, ])
    beta <- coef(fit)
    
    ## Sample sigma:
    sigmaScale <- (1 / fit$df) * crossprod(resid(fit))
    sigmaSam   <- rinvchisq(1, df = fit$df, scale = sigmaScale)
    
    ## Sample beta:
    betaVar <- sigmaSam * solve(crossprod(qr.X(fit$qr)))
    betaSam <- rmvnorm(1, mean = beta, sigma = betaVar)
    
    ## Return a randomly sampled imputation:
    predict(fit, newdata = data[!rVec, ]) + rnorm(sum(!rVec), 0, sqrt(sigmaSam))
}

targets <- paste0("x", 1 : 3)
nSams <- 500
nBurn <- 400
rMat <- !is.na(missData$data)
impData <- missData$data

## Iterate through the FCS algorithm:
impList <- list()
for(s in 1 : nSams) {
    for(v in targets) {
        rVec              <- rMat[ , v]
        impData[!rVec, v] <- eif(data = impData, rVec = rVec, v = v)
    }# END for(v in incompVars)
    
    ## If the chains are burnt-in, save imputed datasets:
    if(s > nBurn) impList[[s - nBurn]] <- impData
}# END for(s in nSams)

fits1 <- lapply(impList, function(x) lm(x1 ~ x2 + x3, data = x))
pool1 <- MIcombine(fits1)

## Do the same analysis with mice():
miceOut <- mice(data      = missData$data,
                m         = 100,
                method    = "norm",
                printFlag = FALSE)
fits2 <- with(miceOut, lm(x1 ~ x2 + x3))
pool2 <- pool(fits2)

summary(pool1)
summary(pool2)

ls(pool1)

cf1  <- coef(pool1)
se1  <- sqrt(diag(vcov(pool1)))
t1   <- cf1 / se1
df1  <- pool1$df
p1   <- 2 * pt(t1, df1, lower.tail = FALSE)
fmi1 <- pool1$missinfo

res1 <- cbind(cf1, se1, t1, p1, fmi1)
colnames(res1) <- c("Est", "SE", "t", "p", "FMI")
res1

cf2  <- pool2$pooled$estimate
se2  <- sqrt(pool2$pooled$t)
t2   <- cf2 / se2
df2  <- pool2$pooled$df
p2   <- 2 * pt(t2, df2, lower.tail = FALSE)
fmi2 <- pool2$pooled$fmi

res2 <- cbind(cf2, se2, t2, p2, fmi2)
colnames(res2) <- c("Est", "SE", "t", "p", "FMI")
res2


?print.xtable


x <- matrix(rnorm(4))
b <- matrix(rnorm(12), ncol = 3)

x
b

t(b) %*% x

cx <- matrix(rnorm(16), 4, 4)

t(b) %*% cx %*% b

X <- matrix(runif(40), 10, 4)
Y <- matrix(runif(30), 10, 3)

b <- solve(t(X) %*% X) %*% t(X) %*% Y

b

##### Do some prep work #####
nReps <- 200
nImps <- 100

## Partition the rows with no missing out of the dataset:
fullRows <- missData[rowSums(is.na(missData)) == 0, ]
missRows <- missData[rowSums(is.na(missData)) > 0, ]

## Compute the observed response pattern for each row,
## and find the set of unique response patterns:
rMat       <- !is.na(missRows)
respPats   <- apply(rMat, 1, toString)
uniquePats <- unique(respPats)

pats <- uniquecombs(rMat)
ind  <- attr(pats, "index")


getImps <- function(data, mu, sigma, p1) {
    ## Pull out predictors:
    X <- as.matrix(data[ , p1])
    
    ## Store meta-data:
    n <- nrow(X)
    
    ## Partition the parameter matrices:
    mY  <- matrix(mu[!p1])
    mX  <- matrix(mu[p1])
    sY  <- sigma[!p1, !p1]
    sX  <- sigma[p1, p1]
    cXY <- sigma[p1, !p1]
    
    ## Compute the imputation model parameters:
    beta  <- solve(sX) %*% cXY
    alpha <- mY - t(beta) %*% mX
    sE    <- sY - t(beta) %*% sX %*% beta
    
    ## Generate and return the imputations:
    matrix(1, n) %*% t(alpha) + X %*% beta + rmvnorm(n, sigma = sE)
}


iStep <- function(data, pats, ind, p0, pars) {
    ## Loop over non-trivial response patterns:
    for(i in c(1 : nrow(pats))[-p0]) {
        ## Define the current response pattern:
        p1 <- pats[i, ]
        
        ## Subset the data:
        dat1 <- data[ind == i, ]
        
        ## Replace missing data with imputations:
        data[ind == i, !p1] <- 
            getImps(data = dat1, mu = pars$mu, sigma = pars$sigma, p1 = p1)
    }
    
    ## Return the imputed data:
    data
}


pStep <- function(data) {
    ## Update the complete-data sufficient statistics:
    n <- nrow(data)
    m <- colMeans(data)
    s <- (n - 1) * cov(data)
    
    ## Sample sigma and mu:
    sigma <- riwish((n - 1), s)
    mu    <- rmvnorm(1, m, (sigma / n))
    
    ## Return the updated parameters:
    list(mu = mu, sigma = sigma)
}


jmImp <- function(data, nImps, nIter) {
    ## Summarize response patterns:
    rMat <- !is.na(data)
    pats <- uniquecombs(rMat)
    ind  <- attr(pats, "index")
    p0   <- which(apply(pats, 1, all))
    
    ## Get starting values for the parameters:
    pars <- list(mu    = colMeans(data, na.rm = TRUE),
                 sigma = cov(data, use = "pairwise")
                 )
    
    ## Iterate over I- and P-Steps to generate imputations:
    imps <- list()
    for(m in 1 : nImps) {
        for(rp in 1 : nIter) {
            data <- iStep(data = data,
                          pats = pats,
                          ind  = ind,
                          p0   = p0,
                          pars = pars)
            pars <- pStep(data)
            
            if(rp == nIter) imps[[m]] <- data
        }
    }
    ## Return imputed datasets:
    imps
}

impList3 <- jmImp(data = missData, nImps = 100, nIter = 50)


fits3 <- lapply(impList3, function(x) lm(x1 ~ x2 + x3, data = x))
pool3 <- MIcombine(fits3)
summary(pool3)


missData <- as.matrix(missData)

meta   <- prelim.norm(missData)
theta0 <- em.norm(meta)

rngseed(235711)   

impList4 <- list()
for(m in 1 : nImps) {
    theta1 <- da.norm(s     = meta,
                      start = theta0,
                      steps = nReps)
    impList4[[m]] <- imp.norm(s     = meta,
                              theta = theta1,
                              x     = missData)
}

fits4 <- lapply(impList4, function(x) lm(x1 ~ x2 + x3, data = as.data.frame(x)))
pool4 <- MIcombine(fits4)

summary(pool4)
summary(pool3)
