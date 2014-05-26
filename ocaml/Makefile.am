ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = zone-valuation-graph/calc test/test zone-valuation-graph/compare_automata

OCAMLPACKAGES = -package ocamlgraph
OCAMLLIBS     = -linkpkg

OCAMLCOMP = $(OCAMLFIND) ocamlc $(OCAMLPACKAGES) -c -o $@ $^
OCAMLLINK = $(OCAMLFIND) ocamlc $(OCAMLPACKAGES) $(OCAMLLIBS) -o $@ $^

CXXLINK = $(CXX) $(CXXFLAGS) -o $@ $^
CXXCOMP = $(CXX) $(CXXFLAGS) -c -o $@ $^

%.cmo: %.ml
	$(OCAMLCOMP)

%.ml: %.ml.nw
	notangle -R$(shell basename $@) $^ >$@

include $(srcdir)/zone-valuation-graph/Makefile.am
include $(srcdir)/test/Makefile.am

relts$(EXEEXT): LTS_modules.cmo relts.cmo
	$(OCAMLLINK)

LTS_modules_test$(EXEEXT): LTS_modules.cmo LTS_modules_test.cmo
	$(OCAMLLINK)

random_lts$(EXEEXT): random_lts.o
	$(CXXLINK)

relts.cmo: LTS_modules.cmo

.cpp.o:
	$(CXXCOMP)

EXTRA_DIST = LTS_modules.ml relts.ml random_lts.cpp LTS_modules_test.ml $(TEST_DOT_FILES)

CLEANFILES = *.cmi *.cmo *.o $(bin_SCRIPTS) *~
