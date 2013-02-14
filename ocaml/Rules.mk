# First, and therefore default, target.
all: targets

# We will need to change this when we make the OCaml-C interface.
OCAMLFLAGS := 

# We will need to change this when we make the OCaml-C interface.
CC := gcc
CFLAGS := -Wall
CFLAGS += -I/home/mihir/uppaal/include
CFLAGS += -L/home/mihir/uppaal/lib
LIBS := -ludbm

# The next three are to compact some of the dependencies in the top
# level.
grammar_types := grammar-noweb/grammar_types.mli \
grammar-noweb/grammar_types.ml

clock_utilities := utilities/clock_utilities.ml \
utilities/clock_utilities.mli

fernandez := fernandez-ocaml-noweb/fernandez.ml \
fernandez-ocaml-noweb/fernandez.mli

# The next four are to include the Rules.mk from each of the subdirectories.
SUBDIRS := c fernandez-ocaml-noweb grammar-noweb next_step utilities \
zone-valuation-graph

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	include $@/Rules.mk

# Top level dependencies.

.PHONY: targets
targets: next_step.native c/interface

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
