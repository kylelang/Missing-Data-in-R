# -*- Makefile -*-

## Find all RMD files:
RMDS := $(wildcard lab*.Rmd)

## Strip the file extension off the located RMD files:
NAMES := $(RMDS:%.Rmd=%)

all: ${NAMES}

## Rule to compile both the RMD files with and without solutions: 
%: %.Rmd
	Rscript --vanilla ../../../code/render.R $@ TRUE
	Rscript --vanilla ../../../code/render.R $@ FALSE
