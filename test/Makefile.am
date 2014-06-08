bin_PROGRAMS += %D%/test

%C%_test_SOURCES = \
grammar-noweb/Grammar_types.ml.nw \
%D%/Test_base.ml \
utilities/UDBM_utilities.ml.nw \
c/Zone_stubs.mli.nw \
utilities/Clock_constraint_utilities.ml.nw \
utilities/NRQueue.ml.nw \
utilities/PCQueue_test.ml \
fernandez-ocaml-noweb/Fernandez_modules.ml.nw \
c/Zone_stubs_test.ml \
zone-valuation-graph/Graph_functions2.ml.nw \
grammar-noweb/Timed_automaton_parser.mly.nw \
grammar-noweb/Timed_automaton_lexer.mll.nw \
utilities/Parse_timed_automaton.ml.nw \
zone-valuation-graph/ZVG_modules.ml.nw \
c/zone_stubs.c.nw \
%D%/test.ml

%D%/test$(EXEEXT): \
grammar-noweb/Grammar_types.cmo \
utilities/UDBM_utilities.cmo \
utilities/NRQueue.cmo \
utilities/Clock_constraint_utilities.cmo \
test/Test_base.cmo \
zone-valuation-graph/Graph_functions2.cmo \
c/Zone_stubs_test.cmo \
utilities/PCQueue_test.cmo \
grammar-noweb/Timed_automaton_lexer.cmo \
grammar-noweb/Timed_automaton_parser.cmo \
utilities/Parse_timed_automaton.cmo \
fernandez-ocaml-noweb/Fernandez_modules.cmo \
zone-valuation-graph/ZVG_modules.cmo \
%D%/test.cmo \
c/zone_stubs.o
	$(OCAMLLINK)

%D%/test.cmo: \
utilities/PCQueue_test.cmi

%D%/Test_base.cmo %D%/Test_base.cmi: \
grammar-noweb/Grammar_types.cmi

