sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/calc.ml $(d)/lexer.mll $(d)/parser.mly \
$(d)/grammar_types.ml $(d)/grammar_types.mli $(d)/calc.native

# all: calc.native
$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(d)/calc.ml:$(d)/calc.ml.nw
	notangle -Rcalc.ml $(d)/calc.ml.nw >$(d)/calc.ml
$(d)/lexer.mll:$(d)/lexer.mll.nw
	notangle -Rlexer.mll $(d)/lexer.mll.nw >$(d)/lexer.mll
$(d)/parser.mly:$(d)/parser.mly.nw
	notangle -Rparser.mly $(d)/parser.mly.nw >$(d)/parser.mly
$(d)/grammar_types.ml:$(d)/grammar_types.ml.nw
	notangle -Rgrammar_types.ml $(d)/grammar_types.ml.nw \
>$(d)/grammar_types.ml
$(d)/grammar_types.mli: $(d)/grammar_types.mli.nw
	notangle -Rgrammar_types.mli $(d)/grammar_types.mli.nw \
	>$(d)/grammar_types.mli
$(d)/calc.native:$(d)/calc.ml $(d)/lexer.mll $(d)/parser.mly \
	$(d)/grammar_types.ml $(d)/grammar_types.mli
	ocamlbuild $(d)/calc.native

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
