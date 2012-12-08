all: calc

lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml

parser.ml parser.mli: parser.mly types.cmi
	ocamlyacc parser.mly

parser.cmo: parser.ml parser.cmi types.cmi
	ocamlc -c parser.ml

types.cmo: types.ml types.cmi
	ocamlc -c types.ml

types.cmi: types.mli
	ocamlc -c types.mli

calc.cmo: calc.ml parser.cmi
	ocamlc -c calc.ml

calc: lexer.cmo parser.cmo calc.cmo parser.cmo types.cmo
	ocamlc -o calc lexer.cmo parser.cmo calc.cmo parser.cmo types.cmo

parser.cmi: types.cmi
	ocamlc -c parser.mli

types.cmi:
calc.cmo: parser.cmi lexer.cmo
calc.cmx: parser.cmx lexer.cmx
lexer.cmo: parser.cmi
lexer.cmx: parser.cmx
parser.cmo: types.cmi parser.cmi
parser.cmx: types.cmx parser.cmi
types.cmo: types.cmi
types.cmx: types.cmi
