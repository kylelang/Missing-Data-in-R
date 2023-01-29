### Title:    Support Functions for Examples
### Author:   Kyle M. Lang
### Created:  2017-08-24
### Modified: 2022-07-01

###--------------------------------------------------------------------------###

## Suppress any printed output:
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  x 
}

###--------------------------------------------------------------------------###

## Create an basic ggplot object using my preferred styling:
gg0 <- function(x, y = NULL, points = TRUE) {
    if(is.null(y))
        p0 <- ggplot(mapping = aes(x = x))
    else
        p0 <- ggplot(mapping = aes(x = x, y = y))

    p0 <- p0 +
        theme_classic() +
        theme(text = element_text(family = "Courier", size = 16),
              plot.title = element_text(family = "Courier",
                                        size = 16,
                                        face = "bold",
                                        hjust = 0.5)
              )

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
paragraphs <- as.paragraphs <- function(x, lines = NULL) {
    text <- capture.output(x)

    if(!is.null(lines)) text <- text[lines]
    
    if(tail(text, 1) == "") text <- text[-length(text)]

    blanks <- which(text == "")
    if(length(blanks) > 0) {
        if(blanks[1] != 1) blanks <- c(-1, blanks)

        starts <- blanks + 1
        ends   <- c(blanks[-1] - 1, length(text))
        
        res <- list()
        for(i in 1 : length(starts))
            res <- c(res, list(text[starts[i] : ends[i]]))
    }
    else
        res <- list(text)
    
    class(res) <- c("paragraphs", class(res))
    res
}

###--------------------------------------------------------------------------###

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
partSummary <- function(x,
                        which = Inf,
                        lines = NULL,
                        stars = FALSE,
                        drops = NULL,
                        ...)
{
    s0 <- summary(x, ...) %>% quiet()
    if(!is.null(drops)) s0 <- dplyr::select(s0, -all_of(drops))

    out <- paragraphs(print(s0, signif.stars = stars))
    
    check <- length(which) == 1 && is.infinite(which)
    if(!check) out <- out[which]

    if(!is.null(lines)) out <- paragraphs(out, lines)

    out
}

###--------------------------------------------------------------------------###

## Extract the DV name from an lm.fit object
## NOTE:
##  This function only works when lm is run using the fomula interface.
dvName <- function(x) all.vars(x$terms)[1]

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

### NOTE: The following, RHat-related funcitons were (slightly) adapted from the
###       miceadds package (v.3.13-12)

###--------------------------------------------------------------------------###

################################################################################
## auxiliary functions for Rhat statistic
################################################################################

## Code from rube package
## Source: http://www.stat.cmu.edu/~hseltman/rube/rube0.2-16/R/Rhat.R
## Inference from Iterative Simulation Using Multiple Sequences
## Author(s): Andrew Gelman and Donald B. Rubin
## Source: Statistical Science, Vol. 7, No. 4 (Nov., 1992), pp. 457-472
## Stable URL: http://www.jstor.org/stable/2246093
## Matches gelman.diag() from package "coda", but not WinBUGS() "summary" component.
## Better than gelman.diag() because multivariate stat is not bothered to be calculated

.rhat0 <- function(mat)
{
    m <- ncol(mat)
    n <- nrow(mat)
    b <- apply(mat,2,mean)
    B <- sum((b-mean(mat))^2)*n/(m-1)
    w <- apply(mat,2, stats::var)
    W <- mean(w)
    s2hat <- (n-1)/n*W + B/n
    Vhat <- s2hat + B/m/n
    covWB <- n /m * (stats::cov(w,b^2)-2*mean(b)*stats::cov(w,b))
    varV <- (n-1)^2 / n^2 * stats::var(w)/m +
                (m+1)^2 / m^2 / n^2 * 2*B^2/(m-1) +
                2 * (m-1)*(n-1)/m/n^2 * covWB
    df <- 2 * Vhat^2 / varV
    R <- sqrt((df+3) * Vhat / (df+1) / W)
    return(R)
}

###--------------------------------------------------------------------------###

.rhat <- function(arr)
{
    dm <- dim(arr)
    if (length(dm)==2) return(.rhat0(arr))
    if (dm[2]==1) return(NULL)
    if (dm[3]==1) return(.rhat0(arr[,,1]))
    return(apply(arr,3,.rhat0))
}

###--------------------------------------------------------------------------###

## Define an S3 generic to dispatch rhat.mids():
rhat <- function(x, ...) UseMethod("rhat")

###--------------------------------------------------------------------------###

## Define the rhat method for mids objects:
rhat.mids <- function(object, all = FALSE, ...)
{
    chainMean <- object$chainMean
    chainVar  <- object$chainVar

    dcM <- dim(chainMean)
    dfr <- data.frame(matrix( 0, nrow=dcM[1], ncol=4 ))

    for (vv in 1:dcM[1]) {
        dfr[vv, 3] <- .rhat(chainMean[vv, , ])
        dfr[vv, 4] <- .rhat(chainVar[vv, , ])
        dfr[vv, 1] <- rownames(chainMean[, , 1])[vv]
        dfr[vv, 2] <- 100 * mean(is.na(object$data[ , dfr[vv, 1]]))
    }
    
    colnames(dfr) <- c("Variable", "MissProp", "RHat_Mean", "RHat_Variance")

    if(all) return(dfr)

    dfr[!is.na(dfr$RHat_Mean), ]
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

imposeMissData <- function(data, targets, preds, pm, types = "random", ...) {
    if(is.matrix(data))
        data <- as.data.frame(data)
    
    if(length(types) == 1 && types == "random")
        types <- sample(c("high", "low", "center", "tails"),
                        length(targets),
                        TRUE)
    
    parms <- data.frame(y = targets, pm = pm, type = types)
    
    M <- list()
    for(i in 1 : nrow(parms))
        M[[i]] <- simLogisticMissingness0(data  = data,
                                          preds = preds,
                                          pm    = parms$pm[i],
                                          type  = parms$type[i],
                                          ...)$r
    
    names(M) <- targets

    for(v in targets) data[M[[v]], v] <- NA

    data
}

###--------------------------------------------------------------------------###

## Extract the FMI for a parameter (what) from a lavaan object (x):
getFmi <- function(x, what) {
    ## Create the summary:
    tmp <- summary(x, fmi = TRUE) %>% quiet()
    
    if(class(x) == "lavaan") tmp <- tmp$pe
    
    ## Create labels á la coef():
    labs <- with(tmp, c(lhs, op, rhs)) %>% matrix(ncol = 3) %>% apply(1, paste0, collapse = "")
    
    ## Extract the appropriate FMI:
    tmp$fmi[labs %in% what]
}

###--------------------------------------------------------------------------###
