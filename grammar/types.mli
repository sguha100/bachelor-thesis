type clock = {clockname:string}
type comparator = Lt | Le | Eq | Ge | Gt
type proposition = True | False | Comparison of string*comparator*int | And of proposition list
type transition = {condition: proposition; clockresets: clock array; nextstate: int}
type state = {stateindex:int; invariant:proposition; departures: transition array}
type timedautomaton = {numstates: int; numtrans: int; numclocks: int; clocks: clock array; states: state array}
