all: targets

FIGURES := lts01.png lts01quotient.png
TARGETS := thesis.pdf

.PHONY: targets
targets: $(TARGETS)

$(TARGETS): Makefile

lts01.png: lts01.dot
	dot -Tpng $< -o $@

lts01quotient.png: lts01quotient.dot
	dot -Tpng $< -o $@

thesis.pdf: thesis.tex thesis.bib $(FIGURES)
	pdflatex \\nonstopmode\\input thesis.tex
	bibtex --min-crossref=100 thesis
	pdflatex \\nonstopmode\\input thesis.tex
	pdflatex \\nonstopmode\\input thesis.tex
