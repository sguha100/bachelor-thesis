open Zone_stubs
open Grammar_types

let clock_name_to_index cn clock_names =
  let len = Array.length clock_names in
  let res = ref (0 - 1) in
  for i = 0 to len - 1 do
    res := if (clock_names.(i) == cn) then i else !res;
  done;
  !res

let rec unit_clock_constraint_to_udbm_constraint_list clock_names unit_clock_constraint =
  match unit_clock_constraint with
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

let clock_constraint_to_udbm_constraint_list
    clock_names
    clock_constraint =
  List.fold_left
    (function partial_udbm_constraint_list ->
      function unit_clock_constraint ->
        (unit_clock_constraint_to_udbm_constraint_list
           clock_names
           unit_clock_constraint)
          @
          partial_udbm_constraint_list
    )
    []
    clock_constraint

let clock_constraint_to_raw_t clock_names clock_constraint =
  List.fold_left
    (function partial_raw_t ->
      function constraint_t ->
        dbm_constrainC
          partial_raw_t
          (1 + Array.length clock_names)
          constraint_t
    )
    (dbm_init (1 + Array.length clock_names))
    (clock_constraint_to_udbm_constraint_list
       clock_names
       clock_constraint
    )

