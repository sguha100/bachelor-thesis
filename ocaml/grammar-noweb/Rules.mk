sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/calc.ml $(d)/lexer.mll $(d)/timed_automaton_parser.mly \
$(d)/grammar_types.ml $(d)/grammar_types.mli $(d)/calc.native

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/calc.ml:$(d)/calc.ml.nw
	notangle -Rcalc.ml $(d)/calc.ml.nw >$(d)/calc.ml
$(d)/lexer.mll:$(d)/lexer.mll.nw
	notangle -Rlexer.mll $(d)/lexer.mll.nw >$(d)/lexer.mll
$(d)/timed_automaton_parser.mly:$(d)/timed_automaton_parser.mly.nw
	notangle -Rtimed_automaton_parser.mly $(d)/timed_automaton_parser.mly.nw >$(d)/timed_automaton_parser.mly
$(d)/grammar_types.ml:$(d)/grammar_types.nw
	notangle -Rgrammar_types.ml $(d)/grammar_types.nw \
>$(d)/grammar_types.ml
$(d)/grammar_types.mli: $(d)/grammar_types.nw
	notangle -Rgrammar_types.mli $(d)/grammar_types.nw \
	>$(d)/grammar_types.mli
$(d)/calc.native:$(d)/calc.ml $(d)/lexer.mll $(d)/timed_automaton_parser.mly \
	$(d)/grammar_types.ml $(d)/grammar_types.mli
	ocamlbuild $(d)/calc.native

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
