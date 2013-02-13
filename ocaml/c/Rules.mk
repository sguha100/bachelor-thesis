sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)

TGT_$(d) := $(d)/interface

$(TGT_$(d)): d := $(d)
#This is a target-specific variable, meant to
#prevent the current value of d from getting lost when a target from
#here is actually rebuilt.

$(TGT_$(d)): $(d)/Rules.mk
#We do want the related targets to be re-built when the Makefile
#fragment is updated.

$(d)/interface: $(d)/interface.c
	$(CC) -o $(d)/interface $(d)/interface.c

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
