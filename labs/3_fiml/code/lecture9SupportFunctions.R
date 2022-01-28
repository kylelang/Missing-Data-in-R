### Title:    Missing Data Course: Lecture 9 Support Functions
### Author:   Kyle M. Lang
### Created:  2015-SEP-23
### Modified: 2015-SEP-24


simulateSimpleData <- function(parms)
{
    library(mvtnorm)

    nObs <- parms$nObs
    meanVec <- parms$meanVec
    corVal <- parms$corVal
    nVars <- length(meanVec)

    sigma <- matrix(corVal, nVars, nVars)
    diag(sigma) <- 1.0

    simData <- as.data.frame( rmvnorm(nObs, meanVec, sigma) )
    colnames(simData) <- letters[(26 - nVars + 1) : 26]
    simData
}


simulateFactorData <- function(parms)
{
    nObs <- parms$nObs
    xyCor <- parms$xyCor
    meanVec <- parms$meanVec
    lam1 <- parms$lamVec[1]
    lam2 <- parms$lamVec[2]

    psi <- matrix(c(1.0, xyCor, xyCor, 1.0), nrow = 2)
    factorScores <- rmvnorm(nObs, meanVec, psi)

    lambda <- matrix(c(rep(lam1, 3),
                       rep(0.0, 6),
                       rep(lam2, 3)),
                     ncol = 2)

    theta <- diag(1 - lambda[lambda != 0.0]^2)

    simData <- factorScores %*% t(lambda) +
        rmvnorm(nObs, rep(0, nrow(lambda)), theta)

    colnames(simData) <-
        paste0(rep(c("x", "y"), each = 3), c(1 : 3))

    as.data.frame(simData)
}


imposeMissing <- function(inData, parms)
{
    incompVars <- parms$incompVars
    auxVar <- parms$auxVar
    pm <- parms$pm
    marType <- vector("character", length(incompVars))
    marType <- parms$marType

    pVec <- pnorm(inData[ , auxVar],
                  mean(inData[ , auxVar]),
                  sd(inData[ , auxVar])
                  )

    for(v in 1 : length(incompVars)) {
        if(marType[v] == "tails") {
            rVec <- pVec < (pm / 2) | pVec > (1 - (pm/2))
        } else if(marType[v] == "center") {
            rVec <- pVec > (0.5 - (pm / 2)) & pVec < (0.5 + (pm / 2))
        } else if(marType[v] == "lower") {
            rVec <- pVec < pm
        } else if(marType[v] == "upper") {
            rVec <- pVec > (1 - pm)
        } else {
            stop("Please provide a valid 'marType'")
        }

        inData[rVec, incompVars[v]] <- NA
    }
    inData
}


createParcels <- function(model, data, nameStem = "parcel") {
    library(lavaan)
    out1 <- cfa(model,
                data = as.data.frame(data),
                std.lv = TRUE,
                missing = "fiml",
                information = "observed")

    lam1 <- inspect(out1, "coef")$lambda
    lamSort <- rownames(lam1)[order(lam1)]

    orderMat <- matrix(NA, ceiling(length(lam1) / 3), 3)
    s1 <- rep(c(0, 4), nrow(orderMat) / 2)
    s2 <- rep(c(1, -1), nrow(orderMat) / 2)
    j <- 1
    for(i in 1 : length(lamSort)) {
        tmp <- i %% 3
        k <- ifelse(tmp == 0, 3, tmp)
        orderMat[j, (s1[j] + s2[j]*k)] <- lamSort[i]
        if(tmp == 0) j <- j + 1
    }

    parFrame <- apply(orderMat, 2,
                      FUN = function(x, data) {
                          if(sum(!is.na(x)) > 1)
                              out <- rowMeans(data[ , x[!is.na(x)]])
                          else
                              out <- data[ , x[!is.na(x)]]
                          out
                      },
                      data = dat1)
    colnames(parFrame) <- paste0(nameStem, c(1 : 3))
    parFrame
}


getAFI <- function(altFitObj, nullFitObj, scaled = FALSE)
{
  nullFit <- inspect(nullFitObj, "fit")
  altFit <- inspect(altFitObj, "fit")

  if(scaled) {
    nullNCP <- nullFit["chisq.scaled"] - nullFit["df.scaled"]
    altNCP <- altFit["chisq.scaled"] - altFit["df.scaled"]

    nullRatio <- nullFit["chisq.scaled"] / nullFit["df.scaled"]
    altRatio <- altFit["chisq.scaled"] / altFit["df.scaled"]
  } else {
    nullNCP <- nullFit["chisq"] - nullFit["df"]
    altNCP <- altFit["chisq"] - altFit["df"]

    nullRatio <- nullFit["chisq"] / nullFit["df"]
    altRatio <- altFit["chisq"] / altFit["df"]
  }

  cfi <- min( 1, ( (nullNCP - altNCP) / nullNCP ) )
  tli <- (nullRatio - altRatio) / (nullRatio - 1.0)

  list(cfi = cfi, tli = tli)
}

