%C%_calc_SOURCES = %D%/calc.ml

%D%/calc$(EXEEXT): \
%D%/calc.cmo
	$(OCAMLLINK)

%D%/calc.cmo: \
utilities/Parse_timed_automaton.cmo \
utilities/Clock_constraint_utilities.cmo \
%D%/Graph_functions2.cmo \
grammar-noweb/Grammar_types.cmo \
utilities/UDBM_utilities.cmo \
%D%/ZVG_modules.cmo

%C%_compare_automata_SOURCES = %D%/compare_automata.ml

%D%/compare_automata$(EXEEXT): %D%/compare_automata.cmo
	$(OCAMLLINK)
