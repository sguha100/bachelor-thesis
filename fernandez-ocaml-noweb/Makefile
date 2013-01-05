all:fernandez.ml fernandez.mli fernandez_app.native fernandez.html

fernandez_app.native: fernandez_app.ml
	ocamlbuild fernandez_app.native

fernandez_app.ml: fernandez.ml.nw
	notangle -Rfernandez_app.ml fernandez.ml.nw > fernandez_app.ml

fernandez.ml:fernandez.ml.nw
	notangle -Rfernandez.ml fernandez.ml.nw > fernandez.ml

fernandez.mli:fernandez.ml.nw
	notangle -Rfernandez.mli fernandez.ml.nw > fernandez.mli

fernandez.html:fernandez.ml.nw
	noweave -filter l2h -index -html fernandez.ml.nw | htmltoc > fernandez.html
