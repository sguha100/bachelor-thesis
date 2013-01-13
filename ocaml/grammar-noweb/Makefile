all: calc.native
calc.ml:calc.ml.nw
	notangle -Rcalc.ml calc.ml.nw > calc.ml
lexer.mll:lexer.mll.nw
	notangle -Rlexer.mll lexer.mll.nw > lexer.mll
parser.mly:parser.mly.nw
	notangle -Rparser.mly parser.mly.nw > parser.mly
grammar_types.ml:grammar_types.ml.nw
	notangle -Rgrammar_types.ml grammar_types.ml.nw > grammar_types.ml
grammar_types.mli:grammar_types.mli.nw
	notangle -Rgrammar_types.mli grammar_types.mli.nw > grammar_types.mli
calc.native:calc.ml lexer.mll parser.mly grammar_types.ml grammar_types.mli
	ocamlbuild calc.native
