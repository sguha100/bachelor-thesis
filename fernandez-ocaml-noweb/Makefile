all:fernandez.ml fernandez.html

fernandez.ml:fernandez.ml.nw
	notangle -Rfernandez.ml fernandez.ml.nw > fernandez.ml

fernandez.html:fernandez.ml.nw
	noweave -filter l2h -index -html fernandez.ml.nw | htmltoc > fernandez.html
