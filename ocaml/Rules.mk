all: targets

dir	:= fernandez-ocaml-noweb
include		$(dir)/Rules.mk
dir	:= grammar-noweb
include		$(dir)/Rules.mk
dir	:= next_step
include		$(dir)/Rules.mk
dir	:= utilities
include		$(dir)/Rules.mk

.PHONY: targets
targets: next_step.native

grammar_types := grammar-noweb/grammar_types.mli \
grammar-noweb/grammar_types.ml

clock_utilities := utilities/clock_utilities.ml \
utilities/clock_utilities.mli

fernandez := fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli

OCAMLFLAGS := 

calc.native: \
zone-valuation-graph/calc.ml \
$(grammar_types) \
grammar-noweb/lexer.mll \
grammar-noweb/parser.mly \
grammar-noweb/parse_timed_automaton.ml \
fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli \
Rules.mk \
_tags

next_step.native: \
next_step/next_step.ml \
$(clock_utilities) \
$(grammar_types) \
grammar-noweb/lexer.mll \
grammar-noweb/parser.mly \
utilities/parse_timed_automaton.ml \
$(fernandez) \
Rules.mk \
_tags

%.native:
	ocamlbuild $(OCAMLFLAGS) zone-valuation-graph/calc.native next_step/next_step.native

calc.top: zone-valuation-graph/calc.mltop
	ocamlbuild zone-valuation-graph/calc.top
