ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = zone-valuation-graph/calc test/test zone-valuation-graph/compare_automata

OCAMLPACKAGES = -package ocamlgraph
OCAMLLIBS     = -linkpkg
# OCAMLCOMP = $(OCAMLFIND) ocamlc -custom -I c $(OCAMLPACKAGES) -c -o $@ $^
OCAMLCOMP = $(OCAMLFIND) ocamlc -custom -I c -I grammar-noweb $(OCAMLPACKAGES) -c $<
OCAMLLINK = $(OCAMLFIND) ocamlc $(OCAMLPACKAGES) $(OCAMLLIBS) -o $@ $^
OCAMLCXXCOMP = \
$(OCAMLFIND) ocamlc -cc $(CXX) $(foreach flag,$(CPPFLAGS) $(CXXFLAGS),-ccopt \
$(flag)) -o $@ $^
NOTANGLERULE = notangle -R$(shell basename $@) $^ >$@
CXXLINK = $(CXX) $(CXXFLAGS) -o $@ $^
CXXCOMP = $(CXX) $(CXXFLAGS) -c -o $@ $^

%.cmo %.cmi: %.ml
	$(OCAMLCOMP)

%.cmi: %.mli
	$(OCAMLCOMP)

%.ml: %.ml.nw
	$(NOTANGLERULE)

%.mli: %.mli.nw
	$(NOTANGLERULE)

%.c: %.c.nw
	$(NOTANGLERULE)

%.o: %.c
	$(OCAMLCXXCOMP)

include $(srcdir)/zone-valuation-graph/Makefile.am
include $(srcdir)/test/Makefile.am
include $(srcdir)/utilities/Makefile.am
include $(srcdir)/grammar-noweb/Makefile.am

relts$(EXEEXT): LTS_modules.cmo relts.cmo
	$(OCAMLLINK)

LTS_modules_test$(EXEEXT): LTS_modules.cmo LTS_modules_test.cmo
	$(OCAMLLINK)

random_lts$(EXEEXT): random_lts.o
	$(CXXLINK)

relts.cmo: LTS_modules.cmo

EXTRA_DIST = LTS_modules.ml relts.ml random_lts.cpp LTS_modules_test.ml $(TEST_DOT_FILES)

CLEANFILES = *.cmi *.cmo *.o $(bin_SCRIPTS) *~
