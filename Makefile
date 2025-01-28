# -*- Makefile -*-

all: index.html

index.html: index.Rmd
	Rscript --vanilla -e 'rmarkdown::render("$^")'
