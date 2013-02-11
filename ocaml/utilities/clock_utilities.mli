open Grammar_types

val zero_valuation: timed_automaton -> float Valuation.t

val add_delay: float Valuation.t -> float -> float Valuation.t

val reset_clocks: float Valuation.t -> string list -> float Valuation.t

