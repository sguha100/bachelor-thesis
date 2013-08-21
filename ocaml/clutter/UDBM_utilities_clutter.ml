open Grammar_types
open Zone_stubs
open UDBM_utilities

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

let unit_clock_constraint_to_constraint_list_option
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
  | False -> None 
  | Lt (cn, n) -> Some [(index cn, 0, true, n)]
  | Le (cn, n) -> Some [(index cn, 0, false, n)]
  | Eq (cn, n) -> Some [(0, index cn, false, 0-n);
                        (index cn, 0, false, n)
                       ]
  | Ge (cn, n) -> Some [(0, index cn, false, 0-n)]
  | Gt (cn, n) -> Some [(0, index cn, true, 0-n)]

let rec clock_constraint_to_dbm_option clock_name_array clock_constraint =
  let
      dim = (1 + Array.length clock_name_array)
  in
  List.fold_left
    (function
    | None -> (function unit_clock_constraint -> None)
    | (Some partial_dbm) ->
      (function unit_clock_constraint -> 
        (match
          unit_clock_constraint_to_constraint_list_option
            clock_name_array
            unit_clock_constraint
        with
          None -> None
        | Some constraint_list ->
          let
              dst =
            List.fold_left (*We just KNOW this folding will work.*)
              (function partial_dbm ->
                function (i, j, strictness, bound) ->
                  dbm_constrain_using_tuple
                    partial_dbm
                    (i, j, strictness, bound)
              )
              (dbm_init dim)
              constraint_list
          in
          if
            (dbm_haveIntersection
               dst
               partial_dbm
            )
          then
            Some (dbm_intersection partial_dbm dst)
          else
            None
        )
      )
    )
    (Some (dbm_init dim))
    clock_constraint

let clock_constraint_haveIntersection clock_names c1 c2 =
    match
      (clock_constraint_to_dbm_option
         clock_names
         c1)
    with
      None -> false
    | Some dst ->
      (match
          (clock_constraint_to_dbm_option
             clock_names
             c2)
       with
         None -> false
       | Some src -> (dbm_haveIntersection dst src)
      )
