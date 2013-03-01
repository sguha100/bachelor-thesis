open Grammar_types
open Zone_stubs

exception Insane_automaton

let rec is_sane_proposition ta proposition = match proposition with
    True -> true
  | False -> true
  | DBM _ -> true (*not sure about this though.*)
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
  | Comparison (cn, Lt, n) -> [dbm_constraint2
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  0
				  n
				  true
			      ]
  | Comparison (cn, Le, n) -> [dbm_constraint2
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  0
				  n
				  false
			      ]
  | Comparison (cn, Eq, n) -> [dbm_constraint2
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
  | Comparison (cn, Ge, n) -> [dbm_constraint2
				  0
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  (0-n)
				  false
			      ]
  | Comparison (cn, Gt, n) -> [dbm_constraint2
				  0
				  (clock_name_to_index
				     cn
				     clock_names
				  )
				  (0-n)
				  true
			      ]
  | And proposition_list -> (List.fold_left
			       (function partial_proposition_list -> function proposition ->
				 partial_proposition_list @ (proposition_to_constraint_list numclocks clock_names proposition)
			       )
			       []
			       proposition_list
  )
  | DBM x -> [] (*This is clearly nonsense, the data structures need
		  to be drastically revised.*)
