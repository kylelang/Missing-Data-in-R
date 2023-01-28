### Title:    Subroutines for Monte Carlo Simulation
### Author:   Kyle M. lang
### Created:  2023-01-28
### Modified: 2023-01-28


## Simluate nonresponse:
imposeMissing <- function(data, parms)
{
    for(v in parms$targets) {
        m <- with(parms,
                  simLinearMissingness(data  = data,
                                       pm    = pm[v],
                                       preds = preds[[v]],
                                       type  = where[v],
                                       auc   = auc[v])$r
                  )
        data[m, v] <- NA
    }
    data
}

###--------------------------------------------------------------------------###

## Impute the missing data:
impute <- function(method, data, parms)
{
    miceOut <- with(parms,
                    mice(data,
                         m = ifelse(method == "norm", nImp, 1),
                         maxit = ifelse(method == "mean", 1, nIter),
                         method = method,
                         seed = seed,
                         printFlag = FALSE)
                    )

    if(method == "norm") miceOut
    else                 complete(miceOut, 1)
}

###--------------------------------------------------------------------------###

## Estimate statistics from simulated/imputed data:
getStats <- function(x, parms)
{
    meanVars <- parms$meanVars
    covVars  <- parms$covVars

    if(is.data.frame(x)) {
        means <- x %>%
            dplyr::select(all_of(meanVars)) %>%
            colMeans(na.rm = TRUE)

        vcov <- x %>%
            dplyr::select(all_of(covVars)) %>%
            cov(use = "complete")

        fit <- x %>%
            data.frame(z = rnorm(nrow(x))) %$%
            lm(bp ~ bmi + glu + age + z) %>%
            summary()

        cf  <- fit$coefficients[ , 1]
        sig <- fit$coefficients[ , 4] < 0.05
        r2  <- fit$r.squared
        fmi <- NA
    }
    else if(is.mids(x)) {
        means <- complete(x, "stacked") %>%
            dplyr::select(all_of(meanVars)) %>%
            colMeans()

        vcov <- micombine.cov(x, variables = covVars) %>% attr("cov_matrix")

        fit <- with(x,
        {
            z <- rnorm(nrow(x$data));
            lm(bp ~ bmi + glu + age + z)
        }
        )
        r2 <- pool.r.squared(fit)

        fit <- pool(fit)
        fmi <- fit$pooled$fmi

        fit <- summary(fit)
        cf  <- fit$estimate
        sig <- fit$p.value < 0.05

        names(fmi) <- names(cf) <- names(sig) <- fit$term
    }
    else
        stop("I don't know what to do with an input of class '", class(x), "'.")

    list(mean = means, cov = vcov, coef = cf, sig = sig, r2 = r2, fmi = fmi)
}

###--------------------------------------------------------------------------###

## Do the missing data treatment and analysis for a single method:
runMethod <- function(method, data, parms)
    impute(method, data, parms) %>% getStats(parms)

###--------------------------------------------------------------------------###

## Conduct one replication of the simulation:
doRep <- function(rp, data, parms)
{
    ## Setup the PRNG:
    .lec.SetPackageSeed(rep(parms$seed, 6))

    if(!rp %in% .lec.GetStreams())
        .lec.CreateStream(1:parms$nStreams)

    .lec.CurrentStream(rp)

    ## Impose missing data:
    dat1 <- imposeMissing(data, parms)

    ## Analyze the incomplete data using each impuation method:
    stats <- lapply(c(mean = "mean",
                      dri  = "norm.predict",
                      sri  = "norm.nob",
                      mi   = "norm"),
                    runMethod,
                    data = dat1,
                    parms = parms)

    ## Analyze the incomplete data using complete case analysis:
    stats$cc <- getStats(dat1, parms)

    ## Return the estimated statistics:
    stats
}
