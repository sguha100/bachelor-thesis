open Grammar_types

exception Insane_automaton

let is_sane_proposition proposition = true

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
    ta.numclocks == Array.length ta.clocks
  &&
    ta.numinit >= 0
  &&
    ta.numinit < ta.numlocations
  &&
    (Array.fold_left
       (function truth ->
	 (function location ->
	   location.locationindex >= 0
	   &&
	     location.locationindex < ta.numlocations
	   &&
	     is_sane_proposition location.invariant
	   &&
	     (Array.fold_left
		(function truth ->
		  (function transition ->
		    truth
		    &&
		      is_sane_proposition transition.condition
		    &&
		      true (*Fix the clocks, now!*)
		    &&
		      transition.nextlocation >= 0
		    &&
		      transition.nextlocation < ta.numlocations
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

