sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/Relations.ml $(d)/STAB.ml $(d)/TADB.ml $(d)/TAOB.ml

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/%.ml: $(d)/%.ml.nw
	notangle -R$(subst $(d)/,,$@) $< > $@

$(d)/%.ml.tex: $(d)/%.ml.nw
	noweave -filter 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"' -index -latex $< > $@

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
