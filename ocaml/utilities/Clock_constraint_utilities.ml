open Grammar_types
open UDBM_utilities

let string_of_clock_constraint clock_constraint =
  (String.concat
     " AND "
     (List.map
        (function
        | True -> "TRUE"
        | False -> "FALSE"
        | Lt (cn, n) -> cn ^ " < " ^ (string_of_int n)
        | Le (cn, n) -> cn ^ " <= " ^ (string_of_int n)
        | Eq (cn, n) -> cn ^ " = " ^ (string_of_int n)
        | Ge (cn, n) -> cn ^ " >= " ^ (string_of_int n)
        | Gt (cn, n) -> cn ^ " > " ^ (string_of_int n)
        )
        clock_constraint
     )
  )
    
let clock_constraint_without_reset_clocks clock_constraint clock_resets =
  List.fold_left
    (function clock_constraint -> function clock_reset ->
      List.filter
        (function
        | True
        | False -> true
        | Lt (cn, _)
        | Le (cn, _)
        | Eq (cn, _)
        | Ge (cn, _)
        | Gt (cn, _) -> cn <> clock_reset
        )
        clock_constraint
    )
    clock_constraint
    (Array.to_list clock_resets)

let clock_constraint_after_clock_resets clock_constraint clock_resets =
  (List.map
     (function clock_reset -> Eq (clock_reset, 0))
     (Array.to_list clock_resets)
  )
  @
    (clock_constraint_without_reset_clocks clock_constraint clock_resets)
