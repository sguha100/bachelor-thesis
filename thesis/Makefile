all: targets

FIGURES := lts01.png lts01quotient.png breaking2.pdf_tex \
breaking2-zones01.pdf_tex breaking2-zones02.pdf_tex breaking2-zones03.pdf_tex
TARGETS := thesis.pdf

.PHONY: targets
targets: $(TARGETS)

$(TARGETS): Makefile

lts01.png: lts01.dot
	dot -Tpng $< -o $@

lts01quotient.png: lts01quotient.dot
	dot -Tpng $< -o $@

breaking2.svg: breaking2.dot
	dot -Tsvg $< -o $@

breaking2.pdf_tex: breaking2.svg
	inkscape -D -z --file=$< --export-pdf=breaking2.pdf --export-latex

breaking2-zones01.svg: breaking2-zones01.dot
	dot -Tsvg $< -o $@

breaking2-zones01.pdf_tex: breaking2-zones01.svg
	inkscape -D -z --file=$< --export-pdf=breaking2-zones01.pdf --export-latex

breaking2-zones02.svg: breaking2-zones02.dot
	dot -Tsvg $< -o $@

breaking2-zones02.pdf_tex: breaking2-zones02.svg
	inkscape -D -z --file=$< --export-pdf=breaking2-zones02.pdf --export-latex

breaking2-zones03.svg: breaking2-zones03.dot
	dot -Tsvg $< -o $@

breaking2-zones03.pdf_tex: breaking2-zones03.svg
	inkscape -D -z --file=$< --export-pdf=breaking2-zones03.pdf --export-latex

thesis.pdf: thesis.tex thesis.bib $(FIGURES)
	pdflatex \\nonstopmode\\input thesis.tex
	bibtex --min-crossref=100 thesis
	pdflatex \\nonstopmode\\input thesis.tex
	pdflatex \\nonstopmode\\input thesis.tex
