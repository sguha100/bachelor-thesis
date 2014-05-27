%C%_test_SOURCES = %D%/test.ml

%D%/test$(EXEEXT): \
grammar-noweb/Grammar_types.cmo \
utilities/UDBM_utilities.cmo \
utilities/NRQueue.cmo \
utilities/Clock_constraint_utilities.cmo \
test/Test_base.cmo \
zone-valuation-graph/Graph_functions2.cmo \
clutter/Unit_constraint_intersection.cmo \
clutter/Test_base_clutter.cmo \
clutter/Unit_constraint_intersection_test.cmo \
clutter/UDBM_utilities_clutter.cmo \
clutter/Clock_constraint_clutter.cmo \
clutter/Clock_constraint_clutter_test.cmo \
clutter/ZVG_tree.cmo \
clutter/Graph_functions3.cmo \
clutter/Graph_functions3_test.cmo \
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
clutter/Graph_functions3.cmi \
clutter/Graph_functions3_test.cmi \
clutter/Unit_constraint_intersection_test.cmi \
clutter/Clock_constraint_clutter_test.cmi \
utilities/PCQueue_test.cmi
