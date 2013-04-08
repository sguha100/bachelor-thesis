# First, and therefore default, target.
all: targets

# We will need to change this when we make the OCaml-C interface.
OCAMLBUILD := ocamlbuild

# The next three are to compact some of the dependencies in the top
# level.
grammar_types := grammar-noweb/grammar_types.mli \
grammar-noweb/grammar_types.ml

fernandez := fernandez-ocaml-noweb/Fernandez_modules.ml

dir := c
include $(dir)/Rules.mk
dir := fernandez-ocaml-noweb
include $(dir)/Rules.mk
dir := grammar-noweb
include $(dir)/Rules.mk
dir := next_step
include $(dir)/Rules.mk
dir := utilities
include $(dir)/Rules.mk

# Top level dependencies.

.PHONY: targets
targets: next_step.native calc.native test.native

libzone_stubs.a: \
c/zone_stubs.mli \
c/zone.mllib \
c/zone_stubs.c
	$(OCAMLBUILD) libzone_stubs.a

calc.native: \
zone-valuation-graph/calc.ml \
zone-valuation-graph/ZVG_modules.ml \
zone-valuation-graph/ZVG_tree.ml \
$(grammar_types) \
grammar-noweb/timed_automaton_lexer.mll \
grammar-noweb/timed_automaton_parser.mly \
utilities/parse_timed_automaton.ml \
fernandez-ocaml-noweb/Fernandez_modules.ml \
Rules.mk \
_tags \
myocamlbuild.ml

next_step.native: \
next_step/next_step.ml \
$(grammar_types) \
grammar-noweb/timed_automaton_lexer.mll \
grammar-noweb/timed_automaton_parser.mly \
utilities/parse_timed_automaton.ml \
$(fernandez) \
c/zone_stubs.mli \
c/zone_stubs.c \
zone-valuation-graph/graph_functions.ml \
utilities/UDBM_utilities.ml \
Rules.mk \
_tags\
myocamlbuild.ml

test.native: \
$(grammar_types) \
utilities/Clock_constraint_utilities.ml \
zone-valuation-graph/graph_functions.ml \
utilities/UDBM_utilities.ml \
test/test.ml \
Rules.mk \
_tags\
myocamlbuild.ml

%.native:
	$(OCAMLBUILD) $(OCAMLCFLAGS) $(OCAMLLFLAGS) \
	next_step/next_step.native \
	zone-valuation-graph/calc.native \
	test/test.native

calc.top: \
zone-valuation-graph/calc.ml \
$(grammar_types) \
grammar-noweb/timed_automaton_lexer.mll \
grammar-noweb/timed_automaton_parser.mly \
utilities/parse_timed_automaton.ml \
fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli \
Rules.mk \
_tags \
myocamlbuild.ml
	ocamlbuild calc.top

Fernandez_modules.cmo: fernandez-ocaml-noweb/Fernandez_modules.ml
	ocamlbuild Fernandez_modules.cmo

