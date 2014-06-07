## Process this file with automake to produce Makefile.in
ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS =

OCAMLPACKAGES = -package ocamlgraph
OCAMLLIBS     = -linkpkg
OCAMLINCLUDES = \
-I c \
-I grammar-noweb \
-I test \
-I utilities \
-I zone-valuation-graph \
-I fernandez-ocaml-noweb \
-I clutter \
-I relations

OCAMLCOMP = \
$(OCAMLFIND) ocamlc -custom $(OCAMLINCLUDES) $(OCAMLPACKAGES) -c $<

# the C libraries in $(LIBS) are placed at the end of the link command
# because placing them elsewhere simply did not work. the hint was
# taken from the curses example in section 19.6 of the ocaml 4.01 manual.
OCAMLLINK = \
$(OCAMLFIND) ocamlc -custom \
$(foreach flag, $(LDFLAGS),-ccopt $(LDFLAGS)) \
$(OCAMLPACKAGES) $(OCAMLLIBS) \
-o $@ $^ \
$(foreach lib, $(LIBS),-cclib $(lib))

OCAMLYACCRULE = ocamlyacc $<
OCAMLLEXRULE = ocamllex $<

# we use the C++ compiler directly, instead of going through ocamlc,
# for the simple reason that ocamlc is incapable of placing object
# files in subdirectories. this is ONLY for compilation of .c files
# into .o files, it does not seem to apply to compilation of .ml files
# into .cmo and .cmi files, or to compilation of .mli files into .cmi
# files.
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

include $(top_srcdir)/%D%/zone-valuation-graph/Makefile.am
include $(top_srcdir)/%D%/test/Makefile.am
include $(top_srcdir)/%D%/utilities/Makefile.am
include $(top_srcdir)/%D%/grammar-noweb/Makefile.am
include $(top_srcdir)/%D%/relations/Makefile.am
include $(top_srcdir)/%D%/c/Makefile.am

EXTRA_DIST =

CLEANFILES = *.cmi *.cmo *.o $(bin_SCRIPTS) *~
