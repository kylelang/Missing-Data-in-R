### Title:    Prep Data for Practical 2b
### Author:   Kyle M. Lang
### Created:  2022-02-11
### Modified: 2022-02-11

rm(list = ls(all = TRUE))

dataDir <- "../data/"
n       <- 1000

library(mice)
library(mvtnorm)
library(dplyr)
library(magrittr)
library(ggplot2)

set.seed(235711)

## Generate complete data:
sigma       <- matrix(0.8, 3, 3)
diag(sigma) <- 1.0

dat1 <- rmvnorm(n, c(2.5,  5.0, 7.5), sigma) %>% data.frame()
colnames(dat1) <- c("x", "y", "z")

colMeans(dat1)
cor(dat1)

## Impose MAR missing on X (predicted by Z):
pats <- wts <- matrix(c(0, 1, 1), 1, 3)
wts[1, 2] <- 0

pats
wts

dat2 <- 
  ampute(dat1, patterns = pats, weights = wts, prop = 0.1, bycase = FALSE)$amp

## Impose MCAR missing on Y:
mY            <- as.logical(rbinom(n, 1, 0.3))
dat2[mY, "y"] <- NA

## Check results:
dat2 %>% is.na() %>% colMeans()

dat2 %$% t.test(x ~ is.na(y))
dat2 %$% t.test(z ~ is.na(y))

dat2 %$% t.test(y ~ is.na(x))
dat2 %$% t.test(z ~ is.na(x))

## Visualize response models:
mX  <- dat2 %$% is.na(x) %>% as.numeric()
tmp <- data.frame(dat2, mX, mY = as.numeric(mY))

ggplot(tmp, aes(z, mX)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Probability of Missing on X") +
  xlab("Z") +  
  theme_classic()

ggplot(tmp, aes(z, mY)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Probability of Missing on Y") +
  xlab("Z") +  
  theme_classic()

ggplot(tmp, aes(x, mY)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Probability of Missing on Y") +
  xlab("X") +  
  theme_classic()

## Save data to disk:
saveRDS(dat1, paste0(dataDir, "complete_data.rds"))
saveRDS(dat2, paste0(dataDir, "incomplete_data.rds"))
