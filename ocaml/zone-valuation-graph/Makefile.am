bin_PROGRAMS += %D%/calc %D%/compare_automata

%C%_calc_SOURCES = \
grammar-noweb/Timed_automaton_lexer.mll.nw \
grammar-noweb/Grammar_types.ml.nw \
grammar-noweb/Timed_automaton_parser.mly.nw \
utilities/Parse_timed_automaton.ml.nw \
c/zone_stubs.c.nw \
c/Zone_stubs.mli.nw \
c/Zone_stubs_test.ml \
utilities/UDBM_utilities.ml.nw \
utilities/NRQueue.ml.nw \
utilities/Clock_constraint_utilities.ml.nw \
test/Test_base.ml \
%D%/Graph_functions2.ml.nw \
fernandez-ocaml-noweb/Fernandez_modules.ml.nw \
%D%/ZVG_modules.ml.nw \
%D%/calc.ml

%D%/calc$(EXEEXT): \
grammar-noweb/Timed_automaton_lexer.cmo \
grammar-noweb/Grammar_types.cmo \
grammar-noweb/Timed_automaton_parser.cmo \
utilities/Parse_timed_automaton.cmo \
c/zone_stubs.o \
utilities/UDBM_utilities.cmo \
utilities/NRQueue.cmo \
utilities/Clock_constraint_utilities.cmo \
test/Test_base.cmo \
%D%/Graph_functions2.cmo \
fernandez-ocaml-noweb/Fernandez_modules.cmo \
%D%/ZVG_modules.cmo \
%D%/calc.cmo
	$(OCAMLLINK)

%D%/calc.cmo: \
utilities/Parse_timed_automaton.cmi \
utilities/Clock_constraint_utilities.cmi \
%D%/Graph_functions2.cmi \
grammar-noweb/Grammar_types.cmi \
utilities/UDBM_utilities.cmi \
%D%/ZVG_modules.cmi

%D%/Graph_functions2.cmi %D%/Graph_functions2.cmo: \
utilities/NRQueue.cmi \
c/Zone_stubs_test.cmi \
utilities/Clock_constraint_utilities.cmi \
c/Zone_stubs_test.cmi

%D%/ZVG_modules.cmo %D%/ZVG_modules.cmi: \
fernandez-ocaml-noweb/Fernandez_modules.cmi \
grammar-noweb/Grammar_types.cmi \
utilities/UDBM_utilities.cmi \
utilities/Clock_constraint_utilities.cmi \
%D%/Graph_functions2.cmi \
utilities/Parse_timed_automaton.cmi

%C%_compare_automata_SOURCES = %D%/compare_automata.ml

%D%/compare_automata$(EXEEXT): \
grammar-noweb/Timed_automaton_lexer.cmo \
grammar-noweb/Grammar_types.cmo \
grammar-noweb/Timed_automaton_parser.cmo \
utilities/Parse_timed_automaton.cmo \
utilities/UDBM_utilities.cmo \
utilities/NRQueue.cmo \
utilities/Clock_constraint_utilities.cmo \
test/Test_base.cmo \
%D%/Graph_functions2.cmo \
fernandez-ocaml-noweb/Fernandez_modules.cmo \
%D%/ZVG_modules.cmo \
relations/STAB.cmo \
relations/TADB.cmo \
relations/TAOB.cmo \
utilities/Table_using_list.cmo \
relations/Relations.cmo \
%D%/compare_automata.cmo \
c/zone_stubs.o
	$(OCAMLLINK)

%D%/compare_automata.cmo: \
relations/Relations.cmi

