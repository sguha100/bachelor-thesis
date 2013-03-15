open Grammar_types
open Zone_stubs

exception Insane_automaton

let is_sane_unit_clock_constraint ta unit_clock_constraint =
  let f (cn, n) = (n >= 0)
    &&
      (List.exists
	 ((=) cn)
	 (Array.to_list ta.clock_names)
      )
  in
  match unit_clock_constraint with
    True -> true
  | False -> false
  | Lt (cn, n) -> f (cn, n)
  | Le (cn, n) -> f (cn, n)
  | Eq (cn, n) -> f (cn, n)
  | Ge (cn, n) -> f (cn, n)
  | Gt (cn, n) -> f (cn, n)

let rec is_sane_clock_constraint ta clock_constraint = 
  List.fold_left
    (function partial_sanity -> 
      function unit_clock_constraint ->
	partial_sanity
	&&
	  (is_sane_unit_clock_constraint
	     ta
	     unit_clock_constraint))
    true
    clock_constraint

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
	     is_sane_clock_constraint ta location.invariant
	   &&
	     (Array.fold_left
		(function truth ->
		  (function transition ->
		    truth
		    &&
		      is_sane_clock_constraint ta transition.condition
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
      ta = Timed_automaton_parser.main Lexer.token lexbuf
  in
  if
    is_sane_timed_automaton ta
  then
    ta
  else
    raise Insane_automaton

let clock_name_to_index cn clock_names =
  let len = Array.length clock_names in
  let res = ref (0 - 1) in
  for i = 0 to len - 1 do
    res := if (clock_names.(i) == cn) then i else !res;
  done;
  !res

let rec proposition_to_constraint_list numclocks clock_names proposition =
  match proposition with
    True -> []
  | False -> [(dbm_constraint2
		 0
		 0
		 0
		 true
  )] (*This weird expression signifies a constraint
       requiring a zero value to be less than zero.*)
  | Lt (cn, n) -> [dbm_constraint2
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  0
				  n
				  true
			      ]
  | Le (cn, n) -> [dbm_constraint2
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  0
				  n
				  false
			      ]
  | Eq (cn, n) -> [dbm_constraint2
				  0
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  (0-n)
				  false;
			       dbm_constraint2
				 (clock_name_to_index
				    cn
				    clock_names
				 )
				 0
				 n
				 false
			      ]
  | Ge (cn, n) -> [dbm_constraint2
				  0
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  (0-n)
				  false
			      ]
  | Gt (cn, n) -> [dbm_constraint2
				  0
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  (0-n)
				  true
			      ]
