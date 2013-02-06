all: targets

dir	:= fernandez-ocaml-noweb
include		$(dir)/Rules.mk
dir	:= grammar-noweb
include		$(dir)/Rules.mk

.PHONY: targets
targets: calc.native next_step.native

calc.native: \
zone-valuation-graph/calc.ml \
grammar-noweb/grammar_types.mli \
grammar-noweb/grammar_types.ml \
grammar-noweb/lexer.mll \
grammar-noweb/parser.mly \
fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli \
Rules.mk \
_tags

next_step.native: \
zone-valuation-graph/calc.ml \
grammar-noweb/grammar_types.mli \
grammar-noweb/grammar_types.ml \
grammar-noweb/lexer.mll \
grammar-noweb/parser.mly \
fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli \
Rules.mk \
_tags

%.native:
	ocamlbuild zone-valuation-graph/calc.native next_step/next_step.native

calc.top: zone-valuation-graph/calc.mltop
	ocamlbuild zone-valuation-graph/calc.top
