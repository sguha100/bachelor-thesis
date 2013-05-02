all: targets

FIGURES := lts01.png lts01quotient.png breaking2.png \
breaking2-zones01.png breaking2-zones02.png breaking2-zones03.png
TARGETS := thesis.pdf

.PHONY: targets
targets: $(TARGETS)

$(TARGETS): Makefile

lts01.png: lts01.dot
	dot -Tpng $< -o $@

lts01quotient.png: lts01quotient.dot
	dot -Tpng $< -o $@

breaking2.png: breaking2.dot
	dot -Tpng $< -o $@

breaking2-zones01.png: breaking2-zones01.dot
	dot -Tpng $< -o $@

breaking2-zones02.png: breaking2-zones02.dot
	dot -Tpng $< -o $@

breaking2-zones03.png: breaking2-zones03.dot
	dot -Tpng $< -o $@

thesis.pdf: thesis.tex thesis.bib $(FIGURES)
	pdflatex \\nonstopmode\\input thesis.tex
	bibtex --min-crossref=100 thesis
	pdflatex \\nonstopmode\\input thesis.tex
	pdflatex \\nonstopmode\\input thesis.tex
