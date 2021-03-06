### Title:    Imputation Testing
### Author:   Kyle M. Lang
### Created:  2018-JUL-30
### Modified: 2018-JUL-30

rm(list = ls(all = TRUE))

library(SURF)
library(mice)
library(rlecuyer)
library(parallel)

impute <- function(method, data) {
    miceOut <- mice(data      = data,
                    m         = 1,
                    maxit     = 10,
                    method    = method,
                    printFlag = FALSE)
    
    complete(miceOut, 1)
}

getStats <- function(data) {
    ## Fit a simple regression model:
    lmOut <- lm(y ~ x1, data = data)
    
    ## Extract, organize, and return output:
    c(
        cf = coef(lmOut),
        r2 = summary(lmOut)$r.squared,
        t  = coef(lmOut) / sqrt(diag(vcov(lmOut))),
        m  = colMeans(data)[1 : 2],
        v  = sapply(data, var)[1 : 2],
        c  = cov(data$y, data$x1),
        r  = cor(data$y, data$x1)
    )
}

doRep <- function(rp, seed) {
    
    ## Setup the RNG:
    .lec.SetPackageSeed(rep(seed, 6))
    suppressWarnings(.lec.CreateStream(1 : 1000))
    oldRng <- .lec.CurrentStream(rp)
    
    ## Simulate data:
    testData <- simRegData(nObs   = nObs,
                           nPreds = nPreds,
                           r2     = 0.5,
                           sigma  = 0.2,
                           means  = 2.0,
                           beta   = matrix(c(0.25, rep(0.75, nPreds)))
                           )

    ## Impose missing:
    missData <- imposeMissData(data    = testData,
                               targets = list(mcar = c("y", "x1", "x2"),
                                              mar  = NA,
                                              mnar = NA
                                              ),
                                        #preds   = c("x3", "x4"),
                               pm      = list(mcar = 0.3),
                                        #snr     = list(mar = 5),
                                        #pattern = "low"
                               )
     
    ## Impute missing data:
    impOut <-
        lapply(c("mean", "norm.predict", "norm.nob"), impute, data = missData$data)
    names(impOut) <- c("mean", "det", "sto")
    
    ## Compute stats:
    out     <- lapply(impOut, getStats)
    out$tru <- getStats(testData)
    
    ## Clean up the RNG state:
    .lec.CurrentStreamEnd(oldRng)
    
    ## Return results:
    out
}

### Run The Simulation ###

nPreds <- 4
nObs   <- 1000
nReps  <- 500

simOut <- mclapply(X        = 1 : nReps,
                   FUN      = doRep,
                   seed     = 235711,
                   mc.cores = 3)

tru  <- colMeans(do.call(rbind, lapply(simOut, "[[", x = "tru")))
det  <- colMeans(do.call(rbind, lapply(simOut, "[[", x = "det")))
sto  <- colMeans(do.call(rbind, lapply(simOut, "[[", x = "sto")))
mean <- colMeans(do.call(rbind, lapply(simOut, "[[", x = "mean")))

100 * (det - tru) / tru
100 * (sto - tru) / tru
100 * (mean - tru) / tru

x <- testData$x1
y <- testData$y

cov(x, y)
cor(x, y)

cov(x, y) / (sd(x) * sd(y))
