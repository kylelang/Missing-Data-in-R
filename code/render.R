### Title:    Render Exercises
### Author:   Kyle M. Lang
### Created:  2021-10-14
### Modified: 2021-10-20

## Extract command line arguments:
args <- commandArgs(trailingOnly = TRUE)

## Define an appropriate file name:
outFile <- paste0(args[1],
                  ifelse(as.logical(args[2]), "_solutions", ""),
                  ".html")

## Render the document:
rmarkdown::render(input = paste0(args[1], ".Rmd"),
                  params = list(answers = as.logical(args[2])),
                  output_file = outFile,
                  envir = new.env()
                  )
