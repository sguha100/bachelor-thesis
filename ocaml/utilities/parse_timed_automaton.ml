open Grammar_types

let init_zone = Zone_stubs.zone_init

exception Insane_automaton

let rec is_sane_proposition ta proposition = match proposition with
    True -> true
  | False -> true
  | Comparison (cn, _, _) -> (Array.fold_left
				(function truth ->
				  (function clock_name ->
				  truth || clock_name == cn))
				true
				ta.clock_names)
  | And propl -> (List.fold_left
		    (function truth -> (
		      function proposition ->
			truth && (is_sane_proposition ta proposition)))
		    true
		    propl
  )
let is_sane_timed_automaton ta = 
  ta.numlocations == Array.length ta.locations
  &&
    ta.numtrans ==
    (Array.fold_left
       (function partial_sum ->
	 (function location -> partial_sum + (Array.length
						location.departures)))
       0
       ta.locations
    )
  &&
    ta.numclocks == Array.length ta.clock_names
  &&
    ta.numinit >= 0
  &&
    ta.numinit < ta.numlocations
  &&
    (Array.fold_left
       (function truth ->
	 (function location ->
	   truth
	   &&
	     location.location_index >= 0
	   &&
	     location.location_index < ta.numlocations
	   &&
	     is_sane_proposition ta location.invariant
	   &&
	     (Array.fold_left
		(function truth ->
		  (function transition ->
		    truth
		    &&
		      is_sane_proposition ta transition.condition
		    &&
		      transition.next_location >= 0
		    &&
		      transition.next_location < ta.numlocations
		  )
		)
		true
		location.departures)
	 )
       )
       true
       ta.locations)
    
let parse_timed_automaton channel =
  let
      lexbuf = Lexing.from_channel channel
  in
  let
      ta = Parser.main Lexer.token lexbuf
  in
  if
    is_sane_timed_automaton ta
  then
    ta
  else
    raise Insane_automaton

