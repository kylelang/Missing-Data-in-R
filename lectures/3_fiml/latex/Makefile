# -*- Makefile -*-

TARGETS = fiml.pdf ml_example.pdf
DEST = ../

all: $(TARGETS)
	
%.pdf: %.tex
	latexmk -pdf -f $^

%.tex: %.Rnw
	Rscript -e "knitr::knit('$^')"

.PHONY: nuke clean tidy

nuke: clean
	rm -f cache/* figure/* $(TARGETS)

tidy: clean
	mv $(TARGETS) $(DEST)

clean:
	rm -f *.tex *.aux *.log *.out *.nav *.snm *.toc *.vrb *.bbl *.blg *.fdb_latexmk *.fls
