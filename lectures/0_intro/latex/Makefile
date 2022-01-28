# -*- Makefile -*-

### Assumptions:
### 1. If you run `make all`, you want to compile every RNW file in the current
###    directory
### 2. After compilation, you want to move every PDF file to the current
###    directory's parent directory and remove all of the detritus produced in
###    the compilation process

## Find all RNW files beginning:
RNWS := $(wildcard *.Rnw)

## Strip the file extension off the located RNW files:
NAMES := $(RNWS:%.Rnw=%)

all: ${NAMES}

%: %.Rnw
	Rscript -e "library(knitr); knit(input = '$?', output = '$@.tex')"
	latexmk -pdf $@.tex

clean:
	mv *.pdf ../
	latexmk -c
	rm -f *.tex *.nav *.snm *.vrb *.fls *.bbl
