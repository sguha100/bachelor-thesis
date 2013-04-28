open Zone_stubs
open Grammar_types

let clock_name_to_index cn clock_names =
  let rec f i cn list =
    match list with
      [] -> -1
    | hd::tl ->
      if
        hd = cn
      then
        i
      else
        f (i+1) cn tl
  in
  f 0 cn (Array.to_list clock_names)

let raw_t_clock_reset_operation operation clock_names clock_resets raw_t =
  List.fold_left
    operation
    raw_t
    (Array.to_list clock_resets)

let raw_t_after_clock_resets clock_names =
  let
      dim = 1 + (Array.length clock_names)
  in
  raw_t_clock_reset_operation
    (function raw_t -> function cn ->
      dbm_updateValue
        raw_t
        dim
        (1 + (clock_name_to_index cn clock_names))
        0
    )
    clock_names

let raw_t_without_reset_clocks clock_names =
  let
      dim = 1 + (Array.length clock_names)
  in
  raw_t_clock_reset_operation
    (function raw_t -> function cn ->
      dbm_freeClock
        raw_t
        dim
        (1 + (clock_name_to_index cn clock_names))
    )
    clock_names

let rec unit_clock_constraint_to_udbm_constraint_list_option
    clock_names
    unit_clock_constraint =
  let
      index cn =
    1 + 
      (clock_name_to_index
         cn
         clock_names)
  in
  match unit_clock_constraint with
    True -> Some []
  | False -> None (*This weird expression signifies a constraint
       requiring a zero value to be less than zero.*)
    (*Update: this weird expression is illegal, that is, a constraint
      in which i = j cannot be applied to the DBM, and also, no
      constraint can be applied to a DBM which makes the DBM empty. I
      think we'll work around by preventing this from ever being sent
      to the DBM functions which are uptight about this.*)
  | Lt (cn, n) -> Some [dbm_constraint2
		      (index cn)
		      0
		      n
		      true
		  ]
  | Le (cn, n) -> Some [dbm_constraint2
		      (index cn)
		      0
		      n
		      false
		  ]
  | Eq (cn, n) -> Some [dbm_constraint2
		      0
		      (index cn)
		      (0-n)
		      false;
		   dbm_constraint2
		     (index cn)
		     0
		     n
		     false
		  ]
  | Ge (cn, n) -> Some [dbm_constraint2
		      0
		      (index cn)
		      (0-n)
		      false
		  ]
  | Gt (cn, n) -> Some [dbm_constraint2
		      0
		      (index cn)
		      (0-n)
		      true
		  ]

(* let clock_constraint_to_udbm_constraint_list_option *)
(*     clock_names *)
(*     clock_constraint = *)
(*   List.fold_left *)
(*     (function None -> (function _ -> None) *)
(*     | Some partial_udbm_constraint_list -> *)
(*       (function unit_clock_constraint -> *)
(*         match *)
(*           (unit_clock_constraint_to_udbm_constraint_list_option *)
(*              clock_names *)
(*              unit_clock_constraint) *)
(*         with *)
(*           None -> None *)
(*         | Some new_constraints -> *)
(*           Some (new_constraints *)
(*                 @ *)
(*                   partial_udbm_constraint_list) *)
(*       ) *)
(*     ) *)
(*     [] *)
(*     clock_constraint *)

let rec clock_constraint_to_raw_t_option clock_names clock_constraint =
  let
      dim = (1 + Array.length clock_names)
  in
  List.fold_left
    (function
    | None -> (function unit_clock_constraint -> None)
    | (Some partial_raw_t) ->
      (function unit_clock_constraint -> 
        (match
          unit_clock_constraint_to_udbm_constraint_list_option
            clock_names
            unit_clock_constraint
        with
          None -> None
        | Some constraint_t_list ->
          let
              dst =
            List.fold_left (*We just KNOW this folding will work.*)
              (function partial_raw_t ->
                function constraint_t ->
                  dbm_constrainC
                    partial_raw_t
                    dim
                    constraint_t
              )
              (dbm_init dim)
              constraint_t_list
          in
          if
            (dbm_haveIntersection
               dst
               partial_raw_t
               dim
            )
          then
            Some (dbm_intersection partial_raw_t dst dim)
          else
            None
        )
      )
    )
    (* (function partial_raw_t_option -> *)
    (*   function unit_clock_constraint -> *)
    (*     None *)
    (* ) *)
    (Some (dbm_init dim))
    clock_constraint

let clock_constraint_haveIntersection clock_names c1 c2 =
    match
      (clock_constraint_to_raw_t_option
         clock_names
         c1)
    with
      None -> false
    | Some dst ->
      (match
          (clock_constraint_to_raw_t_option
             clock_names
             c2)
       with
         None -> false
       | Some src -> (dbm_haveIntersection dst src (1 + Array.length clock_names))
      )

let constraint_list_to_string clock_names constraint_list =
  let
      clock_names = Array.of_list ("0"::(Array.to_list clock_names))
  in
  "[" ^
    (String.concat
       "; "
       (List.map
          (function (i, j, strictness, bound) ->
            if
              (bound > 0)
            then
              (clock_names.(i)) ^
                (if (j <> 0) then " - " ^ (clock_names.(j)) else "" ) ^
                (if strictness then " < " else " <= ") ^ 
                (string_of_int (bound))
            else
              if
                (bound < 0)
              then
                (clock_names.(j)) ^
                  (if (i <> 0) then " - " ^ (clock_names.(i)) else "" ) ^
                  (if strictness then " > " else " >= ") ^ 
                  (string_of_int (-bound))
              else
                (clock_names.(j)) ^ (if strictness then " > " else " >= ") ^
                  (clock_names.(i))
          )
        constraint_list
       )
    ) ^ "]"  

let raw_t_to_string clock_names raw_t =
  let
      dim = Array.length clock_names + 1
  in
  constraint_list_to_string clock_names (dbm_toConstraintList raw_t dim)

let constraint_list_to_raw_t_option dim constraint_list =
  List.fold_left
    (function
    | None -> (function _ -> None)
    | Some dbm ->
      (function (i, j, strictness, bound) ->
        let
            dbm =
          dbm_constrainC
            dbm
            dim
            (dbm_constraint2
	       i
	       j
	       bound
	       strictness
            )
        in
        if
          dbm_isEmpty dbm dim
        then
          None
        else
          Some dbm
      )
    )
    (Some (dbm_init dim))
    constraint_list

let split_raw_t_on_constraint dim dbm (i, j, strictness, bound) =
  List.concat
    (List.map
       (function constraint_t ->
         let
             dbm = 
           dbm_constrainC
             dbm
             dim
             constraint_t
         in
         if
           dbm_isEmpty dbm dim
         then
           []
         else
           [dbm]
        )
       [dbm_constraint2 i j bound strictness;
        dbm_constraint2 j i (0-bound) (not strictness)]
    )

let split_raw_t_list_on_constraint
    dim
    dbm_list
    (i, j, strictness, bound) =
  List.concat
    (List.map
       (function dbm -> split_raw_t_on_constraint dim dbm (i, j, strictness, bound))
       dbm_list
    )

let split_raw_t_list_on_constraint_list
    dim
    dbm_list
    constraint_list =
  List.fold_left
    (split_raw_t_list_on_constraint
       dim
    )
    dbm_list
    constraint_list

let split_raw_t_list_on_raw_t
    dim
    dbm_list
    dbm =
  split_raw_t_list_on_constraint_list
    dim
    dbm_list
    (dbm_toConstraintList dbm dim)
    
let split_raw_t_list_on_clock_constraint
    clock_names
    dbm_list
    clock_constraint =
  let
      dim = 1 + Array.length clock_names
  in
  match
    (clock_constraint_to_raw_t_option clock_names clock_constraint)
  with
  | None -> dbm_list
  | Some dbm -> split_raw_t_list_on_raw_t dim dbm_list dbm
