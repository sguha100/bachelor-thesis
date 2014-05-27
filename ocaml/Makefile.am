## Process this file with automake to produce Makefile.in
ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = zone-valuation-graph/calc test/test zone-valuation-graph/compare_automata

OCAMLPACKAGES = -package ocamlgraph
OCAMLLIBS     = -linkpkg
OCAMLCOMP = \
$(OCAMLFIND) ocamlc -custom -I c -I grammar-noweb -I test -I utilities \
-I zone-valuation-graph -I fernandez-ocaml-noweb $(OCAMLPACKAGES) -c $<
OCAMLLINK = \
$(OCAMLFIND) ocamlc -custom \
$(foreach flag, $(LDFLAGS),-ccopt $(LDFLAGS)) \
$(OCAMLPACKAGES) $(OCAMLLIBS) \
-o $@ $^ \
$(foreach lib, $(LIBS),-cclib $(lib))
OCAMLYACCRULE = ocamlyacc $<
OCAMLLEXRULE = ocamllex $<
# OCAMLCXXCOMP = \
# $(OCAMLFIND) ocamlc -cc $(CXX) $(foreach flag,$(CPPFLAGS) $(CXXFLAGS),-ccopt \
# $(flag)) -o $@ $^
OCAMLCXXCOMP = \
$(CXX) -c -I$(shell ocamlc -where) $(CPPFLAGS) $(CXXFLAGS) -o $@ $<
NOTANGLERULE = notangle -R$(shell basename $@) $^ >$@
CXXLINK = $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $^
CXXCOMP = $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLCOMP)

%.cmo %.cmi: %.ml
	$(OCAMLCOMP)

%.ml: %.ml.nw
	$(NOTANGLERULE)

%.ml %.mli: %.mly
	$(OCAMLYACCRULE)

%.ml: %.mll
	$(OCAMLLEXRULE)

%.mli: %.mli.nw
	$(NOTANGLERULE)

%.mly: %.mly.nw
	$(NOTANGLERULE)

%.mll: %.mll.nw
	$(NOTANGLERULE)

%.c: %.c.nw
	$(NOTANGLERULE)

%.o: %.c
	$(OCAMLCXXCOMP)

include $(srcdir)/zone-valuation-graph/Makefile.am
include $(srcdir)/test/Makefile.am
include $(srcdir)/utilities/Makefile.am
include $(srcdir)/grammar-noweb/Makefile.am

EXTRA_DIST = LTS_modules.ml relts.ml random_lts.cpp LTS_modules_test.ml $(TEST_DOT_FILES)

CLEANFILES = *.cmi *.cmo *.o $(bin_SCRIPTS) *~
