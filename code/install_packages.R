### Title:    Missing Data in R: Package Installation Script
### Author:   Kyle M. Lang
### Created:  2022-01-29
### Modified: 2022-01-30

install.packages(c("mice",  
                   "naniar",
                   "dplyr",
                   "ggplot2",
                   "miceadds",
                   "mitools",
                   "semTools",
                   "psych",
                   "lavaan",
                   "pROC",
                   "mvtnorm"),
                 repos = "http://cloud.r-project.org",
                 dependencies = TRUE)
