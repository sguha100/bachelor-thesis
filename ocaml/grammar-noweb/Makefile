all: calc.native
calc.ml:calc.ml.nw
	notangle -Rcalc.ml calc.ml.nw > calc.ml
lexer.mll:lexer.mll.nw
	notangle -Rlexer.mll lexer.mll.nw > lexer.mll
parser.mly:parser.mly.nw
	notangle -Rparser.mly parser.mly.nw > parser.mly
types.ml:types.ml.nw
	notangle -Rtypes.ml types.ml.nw > types.ml
types.mli:types.mli.nw
	notangle -Rtypes.mli types.mli.nw > types.mli
calc.native:calc.ml lexer.mll parser.mly types.ml types.mli
	ocamlbuild calc.native
