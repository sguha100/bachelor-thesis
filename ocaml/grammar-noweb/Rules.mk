sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/Timed_automaton_lexer.mll \
$(d)/Timed_automaton_lexer.mll.tex \
$(d)/Timed_automaton_parser.mly \
$(d)/Timed_automaton_parser.mly.tex \
$(d)/Grammar_types.ml \
$(d)/Grammar_types.ml.tex

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/Timed_automaton_lexer.mll:$(d)/Timed_automaton_lexer.mll.nw
	notangle -RTimed_automaton_lexer.mll $(d)/Timed_automaton_lexer.mll.nw >$(d)/Timed_automaton_lexer.mll

$(d)/Timed_automaton_parser.mly:$(d)/Timed_automaton_parser.mly.nw
	notangle -RTimed_automaton_parser.mly $(d)/Timed_automaton_parser.mly.nw >$(d)/Timed_automaton_parser.mly

$(d)/Grammar_types.ml:$(d)/Grammar_types.ml.nw
	notangle -RGrammar_types.ml $(d)/Grammar_types.ml.nw \
>$(d)/Grammar_types.ml

$(d)/%.tex: $(d)/%.nw
	noweave -n -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -latex $< > $@

$(d)/%.tex: $(d)/%.nw
	noweave -n -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -latex $< > $@

$(d)/%.tex: $(d)/%.nw
	noweave -n -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -latex $< > $@

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
