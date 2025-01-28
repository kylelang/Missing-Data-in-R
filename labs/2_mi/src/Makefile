# -*- Makefile -*-

CODE_DIR = ../../../code/
HTML_DIR = ../

## Find all RMD files beginning with "exercise":
RMDS := $(wildcard *.Rmd)

## Strip the file extension off the located RMD files:
NAMES := $(RMDS:%.Rmd=%)

all: ${NAMES}

## Rule to compile both the RMD files with and without solutions: 
%: %.Rmd
	Rscript --vanilla ${CODE_DIR}render.R $@ ${HTML_DIR} TRUE
	Rscript --vanilla ${CODE_DIR}render.R $@ ${HTML_DIR} FALSE
