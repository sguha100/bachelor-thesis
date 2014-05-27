%C%_test_SOURCES = %D%/test.ml

%D%/test$(EXEEXT): %D%/test.cmo
	$(OCAMLLINK)

%D%/test.cmo: \
clutter/Graph_functions3.cmi \
clutter/Graph_functions3_test.cmi \
clutter/Unit_constraint_intersection_test.cmi \
clutter/Clock_constraint_clutter_test.cmi \
utilities/PCQueue_test.cmi
