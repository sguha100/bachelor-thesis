all: zone-valuation-graph/calc.native

zone-valuation-graph/calc.native: zone-valuation-graph/calc.ml
	ocamlbuild zone-valuation-graph/calc.native

zone-valuation-graph/calc.top: zone-valuation-graph/calc.mltop
	ocamlbuild zone-valuation-graph/calc.top
