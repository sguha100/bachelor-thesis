sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

FIGURES := $(d)/lts01.pdf_tex \
$(d)/lts01quotient.pdf_tex \
$(d)/breaking2.pdf_tex \
$(d)/breaking2-zones01.pdf_tex \
$(d)/breaking2-zones02.pdf_tex \
$(d)/breaking2-zones03.pdf_tex \
$(d)/breaking2-zones-abstracted.pdf_tex \
$(d)/pair01first.pdf_tex \
$(d)/pair01second.pdf_tex \
$(d)/pair02first.pdf_tex \
$(d)/pair02second.pdf_tex \
$(d)/pair03first.pdf_tex \
$(d)/pair03second.pdf_tex \
$(d)/regiongraph01.pdf_tex \
$(d)/regiongraph02.pdf_tex
TGT_$(d) := $(d)/thesis.pdf

.PHONY: targets
targets: $(TGT_$(d))

$(TGT_$(d)): d := $(d)

$(TGT_$(d)): $(d)/Rules.mk

$(d)/%.svg : $(d)/%.dot
	dot -Tsvg $< -o $@

$(d)/%.png : $(d)/%.dot
	dot -Tpng $< -o $@

$(d)/%.pdf_tex: $(d)/%.svg
	inkscape -D -z --file=$< --export-pdf=$(subst pdf_tex,pdf,$@) \
	--export-latex

$(d)/UDBM_utilities.ml.tex: utilities/UDBM_utilities.ml.tex
	cp $< $@

$(d)/thesis.pdf: $(d)/thesis.tex $(d)/thesis.bib $(FIGURES) \
$(d)/chapter01.tex $(d)/chapter02.tex $(d)/chapter03.tex \
$(d)/chapter04.tex $(d)/chapter05.tex \
$(d)/UDBM_utilities.ml.tex
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex
	env TEXMFOUTPUT=$(d) BIBINPUTS=$(d) bibtex --min-crossref=100 $(d)/thesis
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex
	-env TEXINPUTS=./$(d): pdflatex -output-directory $(d) \\nonstopmode\\input $(d)/thesis.tex

# The argument passed with the -R switch does not require
# qualification with $(d) because it refers solely to the file name
# that appears before each code chunk.
d := $(dirstack_$(sp))
sp := $(basename $(sp))
