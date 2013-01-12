all:fernandez.ml fernandez.mli fernandez_app.native fernandez.html

fernandez_app.native: fernandez_app.ml
	ocamlbuild fernandez_app.native

fernandez_app.ml: fernandez.nw
	notangle -Rfernandez_app.ml fernandez.nw > fernandez_app.ml

fernandez.ml:fernandez.nw
	notangle -Rfernandez.ml fernandez.nw > fernandez.ml

fernandez.mli:fernandez.nw
	notangle -Rfernandez.mli fernandez.nw > fernandez.mli

fernandez.html:fernandez.nw
	noweave -filter l2h -index -html fernandez.nw | htmltoc > fernandez.html
