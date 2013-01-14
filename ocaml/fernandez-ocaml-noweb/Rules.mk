sp := $(sp).x
dirstack_$(sp) := $(d)
d := $(dir)

TGT_$(d) := fernandez_app.native $(d)/fernandez_app.ml \
 $(d)/fernandez.ml $(d)/fernandez.mli $(d)/fernandez.html

# all:fernandez.ml fernandez.mli fernandez_app.native fernandez.html

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

fernandez_app.native: $(d)/fernandez_app.ml $(d)/fernandez.ml \
$(d)/fernandez.mli
	ocamlbuild $(d)/fernandez_app.native

$(d)/fernandez_app.ml: $(d)/fernandez.nw
	notangle -Rfernandez_app.ml $(d)/fernandez.nw \
	>$(d)/fernandez_app.ml

$(d)/fernandez.ml:$(d)/fernandez.nw;echo $(d)
	notangle -Rfernandez.ml $(d)/fernandez.nw >$(d)/fernandez.ml

$(d)/fernandez.mli:$(d)/fernandez.nw
	notangle -Rfernandez.mli $(d)/fernandez.nw > $(d)/fernandez.mli

$(d)/fernandez.html:$(d)/fernandez.nw
	noweave -filter l2h -index -html $(d)/fernandez.nw | htmltoc \
	>$(d)/fernandez.html

d := $(dirstack_$(sp))
sp := $(basename $(sp))
