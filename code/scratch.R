
library(mice)
library(mvtnorm)
library(lme4)
library(ggplot2)
library(dplyr)
library(magrittr)

data(boys)

colMeans(is.na(boys))

R <- !is.na(boys)

iFlux <- oFlux <- 0
j <- 5

for(k in 1 : ncol(R))
    iFlux <- iFlux + sum((1 - R[ , j]) * R[ , k])

for(k in 1 : ncol(R))
    oFlux <- oFlux + sum(R[ , j] * (1 - R[ , k]))

iFlux / sum(R)
oFlux / sum(!R)

tmp <- apply(R, 2, function(x, y) x * y, y = !R[ , j])
sum(tmp) / sum(R)

tmp <- apply(!R[ , -j], 2, function(x, y) x * y, y = R[ , j])
sum(tmp) / sum(!R)

flux(boys)


n1 <- 5
n2 <- 50

s     <- 1.0
gamma <- c(1.5, 1.25)
tau   <- matrix(c(1.0, -0.3, -0.3, 1.0), 2, 2)

U    <- rmvnorm(n2, gamma, tau)
time <- matrix(1:n1 - 1)
tmp  <- U[ , 1] + U[ , 2] %*% t(time)

dat1 <- dat2 <- dat3 <-
    data.frame(y = as.numeric(tmp) + rnorm(n1 * n2, 0, s),
               t = rep(0:(n1 - 1), each = n2),
               id = rep(1:n2, n1)
               ) %>%
    arrange(id)

fit <- lmer(y ~ t + (1 + t | id), data = dat1)

summary(fit)

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

dat2$y <- dat1 %$% tapply(y, id, attrit, pComp = 0.25) %>% unlist()

## Fill missing cases with their last observed value:
locf <- function(x) {
    m <- is.na(x)
    if(any(m)) x[m] <- tail(x[!m], 1)
    x
}

dat3$y <- dat2 %$% tapply(y, id, locf) %>% unlist()
dat3$m <- is.na(dat2$y)

dat2 %$% tapply(m
p0 <- ggplot(data = dat1, mapping = aes(y = y, x = t)) +
    ylab("Y") +
    xlab("Time")

p0 + geom_point() + geom_line(aes(group = id))
p0

p0 + geom_point(data = dat2) +
    geom_line(data = dat2, mapping = aes(y = y, x = t, group = id))
p0

p0 <- ggplot(data = dat3, mapping = aes(y = y, x = t, color = m)) +
    geom_point() +
    geom_line(aes(group = id)) +
    scale_color_manual(values = c("black", "red"))
p0

dat3

p0 + geom_smooth(aes(y = y, x = t), method = "lm", se = FALSE)

dat1$y - dat2$y

dat1$id

dat2 %$% tapply(y, t, function(x) mean(is.na(x)))
