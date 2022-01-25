### Title:    Support Functions for Teaching
### Author:   Kyle M. Lang
### Created:  2017-08-24
### Modified: 2021-01-22

###--------------------------------------------------------------------------###

## Create an basic ggplot object using my preferred styling:
gg0 <- function(x, y, points = TRUE) {
    p0 <- ggplot(mapping = aes(x = x, y = y)) +
        theme_classic() +
        theme(text = element_text(family = "Courier", size = 16))

    if(points) p0 + geom_point()
    else       p0
}

###--------------------------------------------------------------------------###

rangeNorm <- function(x, newMin = 0.0, newMax = 1.0) {

    r0 <- diff(range(x))
    r1 <- newMax - newMin

    (x * r1 - min(x) * r1) / r0 + newMin
}

###--------------------------------------------------------------------------###

## Force line-wrapping for wide output when 'width' option is ignored:
wrap <- function(x, w = 60) {
    out <- capture.output(x)

    for(x in out) {
        if(nchar(x) < w)
            cat(x, sep = "", "\n")
        else {
            y <- strsplit(x, "\\s")[[1]]

            while(TRUE) {
                z <- sapply(y, nchar, USE.NAMES = FALSE)

                check <- cumsum(z + 1)
                cut   <- max(which(check < w))

                cat(y[1 : cut], sep = " ", "\n")

                if(max(check) <= w) break
                else                y <- y[-c(1 : cut)]
            }
        }
    }
}

###--------------------------------------------------------------------------###

### The following 'paragraphs'-related functions were adapted from code by Brett
### Presnell
### http://r.789695.n4.nabble.com/Sweave-and-Slides-Beamer-td3451049.html

## Break up long output into "paragraphs":
paragraphs <- function(x) {
    text <- capture.output(x)

    if(text[length(text)] == "") text <- text[-length(text)]

    blanks <- which(text == "")
    if(blanks[1] != 1) blanks <- c(-1, blanks)

    starts <- blanks + 1
    ends   <- c(blanks[-1] - 1, length(text))

    res <- list()
    for(i in 1 : length(starts))
        res <- c(res, list(text[starts[i] : ends[i]]))

    class(res) <- c("paragraphs", res$class)
    res
}

as.paragraphs <- paragraphs

## Make sure subsets of 'paragraphs' objects are also 'paragraphs' objects:
assign("[.paragraphs",
       function(x, ..., drop = TRUE) {
           cl       <- oldClass(x)
           class(x) <- NULL

           val        <- NextMethod("[")
           class(val) <- cl

           val
       })

###--------------------------------------------------------------------------###

print.paragraphs <- function(x, ...) {
    for (i in 1 : (length(x))) {
        cat(x[[i]], sep="\n")
        cat("\n")
    }
}

###--------------------------------------------------------------------------###

## Print only a subset of a summary object:
partSummary <- function(x, which = Inf, stars = FALSE) {
    tmp <- paragraphs(print(summary(x), signif.stars = stars))

    check <- length(which) == 1 && is.infinite(which)
    if(check) tmp
    else      tmp[which]
}

###--------------------------------------------------------------------------###

## A summary method for cell-means coded lm models.
## This function will correct the R^2 and F-stats from the usual summary.lm().
summary.cellMeans <- function(obj) {
    ## Get broken summaries:
    s0  <- summary.lm(obj)
    av0 <- anova(obj)

    ## Extract model info:
    y  <- obj$model[ , 1]
    df <- obj$rank - 1

    ## Compute correct measures of variability:
    ss <- crossprod(obj$fitted.values - mean(y))
    ms <- ss / df

    ## Compute correct stats:
    r2  <- as.numeric(ss / crossprod(y - mean(y)))
    r2a <- as.numeric(1 - (1 - r2) * ((length(y) - 1) / obj$df.residual))
    f   <- as.numeric(ms / av0["Residuals", "Mean Sq"])

    ## Replace broken stats:
    s0$r.squared           <- r2
    s0$adj.r.squared       <- r2a
    s0$fstatistic[c(1, 2)] <- c(f, df)

    s0 # Return corrected summary
}

###--------------------------------------------------------------------------###

## Do K-Fold Cross-Validation with lm():
cv.lm <- function(data, models, names = NULL, K = 10, seed = NULL) {

    if(!is.null(seed)) set.seed(seed)

    ## Create a partition vector:
    part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]

    ## Find the DV:
    dv <- ifelse(is.character(models[[1]]),
                 trimws(strsplit(models[[1]], "~")[[1]][1]),
                 all.vars(models[[1]], max.names = 1)
                 )

    ## Apply over candidate models:
    cve <- sapply(X = models, FUN = function(model, data, dv, K, part) {
        ## Loop over K repititions:
        mse <- c()
        for(k in 1 : K) {
            ## Partition data:
            train <- data[part != k, ]
            valid <- data[part == k, ]

            ## Fit model, generate predictions, and save the MSE:
            fit    <- lm(model, data = train)
            pred   <- predict(fit, newdata = valid)
            mse[k] <- MSE(y_pred = pred, y_true = valid[ , dv])
        }
        ## Return the CVE:
        sum((table(part) / length(part)) * mse)
    },
    data = data,
    K    = K,
    dv   = dv,
    part = part)

    ## Name output:
    if(!is.null(names))          names(cve) <- names
    else if(is.null(names(cve))) names(cve) <- paste("Model", 1 : length(cve))

    cve
}

###--------------------------------------------------------------------------###

## Create polynomial formula objects:
polyList <- function(y, x, n) {
    out <- list(paste(y, x, sep = " ~ "))
    for(i in 2 : n) {
        xn       <- paste0("I(", x, "^", i, ")")
        out[[i]] <- paste(out[[i - 1]], xn, sep = " + ")
    }
    out
}

###--------------------------------------------------------------------------###

simulateSimpleData <- function(parms) {
    nObs    <- parms$nObs
    xyCor   <- parms$corVec[1]
    xzCor   <- parms$corVec[2]
    yzCor   <- parms$corVec[3]
    meanVec <- parms$meanVec

    sigma <- matrix(c(1.0, xyCor, xzCor,
                      xyCor, 1.0, yzCor,
                      xzCor, yzCor, 1.0),
                    nrow = 3)

    simData <- as.data.frame(rmvnorm(nObs, meanVec, sigma))
    colnames(simData) <- c("x", "y", "z")
    simData
}

###--------------------------------------------------------------------------###

                                        #imposeMissing <- function(compData, parms)
                                        #{
                                        #    incompVars <- parms$incompVars
                                        #    auxVar <- parms$auxVar
                                        #    pm <- parms$pm
                                        #    marType <- parms$marType
                                        #
                                        #    pVec <- pnorm(compData[ , auxVar],
                                        #                  mean(compData[ , auxVar]),
                                        #                  sd(compData[ , auxVar])
                                        #                  )
                                        #
                                        #    if(marType == "tails") {
                                        #        rVec <- pVec < (pm / 2) | pVec > (1 - (pm/2))
                                        #    } else if(marType == "center") {
                                        #        rVec <- pVec > (0.5 - (pm / 2)) & pVec < (0.5 + (pm / 2))
                                        #    } else if(marType == "lower") {
                                        #        rVec <- pVec < pm
                                        #    } else if(marType == "upper") {
                                        #        rVec <- pVec > (1 - pm)
                                        #    } else {
                                        #        stop("Please provide a valid 'marType'")
                                        #    }
                                        #
                                        #    missData <- simData
                                        #    missData[rVec, incompVars] <- NA
                                        #    missData
                                        #}

###--------------------------------------------------------------------------###

## Generate uniform random attrition after T1 with a 'pComp' probability of
## finishing the study
attrit <- function(x, pComp = 0.5) {
    finish <- sample(c(TRUE, FALSE), 1, prob = c(pComp, 1 - pComp))

    if(!finish) {
        cut <- sample(2:length(x), 1)
        x[cut:length(x)] <- NA
    }
    x
}

###--------------------------------------------------------------------------###

## Fill missing cases with their last observed value:
locf <- function(x) {
    m <- is.na(x)
    if(any(m)) x[m] <- tail(x[!m], 1)
    x
}

###--------------------------------------------------------------------------###

## Estimate the pooled correlation matrix from a mids object
pooledCorMat <- function(x, vars) {
    ## Compute the correlation matrices for each imputed dataset:
    tmp <- lapply(complete(x, "all"),
                  function(data, vars) cor(data[ , vars]),
                  vars = vars)
    
    ## Average the correlation matrices:
    unlist(tmp) %>% 
        matrix(ncol = length(vars)^2, byrow = TRUE) %>% 
        colMeans() %>% 
        matrix(ncol = length(vars), dimnames = list(vars, vars))
}

###--------------------------------------------------------------------------###

simCovData <- function(nObs,
                       nVars,
                       mu = rep(0, nVars),
                       sigma = diag(nVars),
                       names = NULL)
{
    ## Populate a covariance matrix, if necessary:
    if(length(sigma) == 1) {
        sigma       <- matrix(sigma, nVars, nVars)
        diag(sigma) <- 1
    }

    ## Generate the data:
    dat <- as.data.frame(rmvnorm(nObs, mu, sigma))

    if(is.null(names))
        colnames(dat) <- paste0("x", 1:nVars)
    else
        colnames(dat) <- names
    
    dat
}

###--------------------------------------------------------------------------###

imposeMissData <- function(data, targets, preds, pm, types) {
    parms <- data.frame(y = targets, pm = pm, type = types)
    
    M <- list()
    for(i in 1 : nrow(parms))
        M[[i]] <- simLogisticMissingness0(data  = data,
                                          preds = preds,
                                          pm    = parms$pm[i],
                                          type  = parms$type[i])$r
    
    names(M) <- targets

    for(v in targets) data[M[[v]], v] <- NA

    data
}
