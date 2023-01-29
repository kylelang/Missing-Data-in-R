### Title:    Analyze Monte Carlo Simulation Results
### Author:   Kyle M. lang
### Created:  2023-01-28
### Modified: 2023-01-28

rm(list = ls(all = TRUE))

library(dplyr)
source("code/treatment_simulation/init.R")

# Define the simulation parameters:
parms <- list(
    meanVars = c("glu", "bp"),
    covVars  = c("age", "bmi", "bp", "tc", "ltg", "glu")
)

out  <- readRDS("data/treatment_sim_out.rds")
dat0 <- readRDS("data/diabetes_norm.rds")

true <- getStats(dat0, parms)

flat <- lapply(out, unlist) %>% do.call(rbind, .) %>% data.frame()

head(flat)

###-Process Means------------------------------------------------------------###

sim <- flat %>%
    select(matches("\\w\\.mean\\.\\w")) %>%
    colMeans()

labs <- sim %>%
    names() %>%
    strsplit("\\.") %>%
    sapply("[", x = c(1, 3)) %>%
    apply(1, unique)

meanRes <- sim %>%
    matrix(nrow = 2, dimnames = rev(labs)) %>%
    data.frame(full = true$mean)

###-Process Power------------------------------------------------------------###

sim <- flat %>%
    select(matches("\\w\\.sig\\.+\\w")) %>%
    colMeans()

labs <- sim %>%
    names() %>%
    strsplit("\\.+") %>%
    sapply("[", x = c(1, 3)) %>%
    apply(1, unique, simplify = FALSE)

sigRes <- sim %>%
    matrix(nrow = length(labs[[2]]), dimnames = rev(labs)) %>%
    data.frame()

###-Process Regression Weights-----------------------------------------------###

sim <- flat %>%
    select(matches("\\w\\.coef\\.+\\w")) %>%
    colMeans()

labs <- sim %>%
    names() %>%
    strsplit("\\.+") %>%
    sapply("[", x = c(1, 3)) %>%
    apply(1, unique, simplify = FALSE)

fitTrue <- lm(bp ~ bmi + glu + age, data = dat0)
coefRes <- sim %>%
    matrix(nrow = length(labs[[2]]), dimnames = rev(labs)) %>%
    head(-1) %>%
    data.frame(full = coef(fitTrue))

###-Process Regression R^2---------------------------------------------------###

sim <- flat %>%
    select(matches("\\w\\.r2$|mi\\.r21$")) %>%
    sapply(poolR2)

labs <- sim %>%
    names() %>%
    strsplit("\\.") %>%
    sapply("[", x = 1)

r2Res <- c(sim, full = summary(fitTrue)$r.squared)

###-Process Covariance Matrices----------------------------------------------###

sim <- flat %>%
    select(matches("\\w\\.cov")) %>%
    colMeans()

covRes <-
    lapply(labs,
           function(x, data, vars) {
               data[grep(paste0("^", x, "\\."), names(data))] %>%
                   matrix(ncol = length(vars), dimnames = list(vars, vars))
           },
           data = sim,
           vars = parms$covVars
           )

names(covRes) <- labs
covRes$full <- true$cov

###-Pack Up and Write Out----------------------------------------------------###

list(mean = meanRes,
     power = sigRes,
     coef = coefRes,
     r2 = r2Res,
     cov = covRes) %>%
    saveRDS("data/treatment_simulation_results.rds")
