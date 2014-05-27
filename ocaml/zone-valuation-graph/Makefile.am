%C%_calc_SOURCES = %D%/calc.ml

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

%D%/Graph_functions2.cmi: \
utilities/NRQueue.cmi \
c/Zone_stubs_test.cmi

%D%/ZVG_modules.cmi: \
fernandez-ocaml-noweb/Fernandez_modules.cmi

%C%_compare_automata_SOURCES = %D%/compare_automata.ml

%D%/compare_automata$(EXEEXT): %D%/compare_automata.cmo
	$(OCAMLLINK)
