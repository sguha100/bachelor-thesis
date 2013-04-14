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

let unit_clock_constraint_max unit_clock_constraint cn =
  match
    unit_clock_constraint
  with
    True
  | False -> Lstrict 0
  | Lt (cn1, n1)
  | Le (cn1, n1)
  | Eq (cn1, n1)
  | Ge (cn1, n1)
  | Gt (cn1, n1) -> if (cn1 = cn) then Lslack n1 else Lstrict 0

let combine_max lbound1 lbound2 =
  match
    (lbound1, lbound2)
  with
    (Lstrict n1, Lstrict n2) -> if (n1 < n2)
      then (Lstrict n2)
      else (Lstrict n1)
  | (Lstrict n1, Lslack n2) -> if (n1 < n2)
    then (Lslack n2)
    else (Lstrict n1)
  | (Lslack n1, Lstrict n2) -> if (n2 < n1)
    then (Lslack n1)
    else (Lstrict n2)
  | (Lslack n1, Lslack n2) -> if (n2 < n2)
    then (Lslack n2)
    else (Lslack n1)

let clock_constraint_max clock_constraint cn =
  List.fold_left
    combine_max
    (Lstrict 0)
    clock_constraint

let split_on_unit_clock_constraint unit_clock_constraint =
  match
    unit_clock_constraint
  with
    True
  | False -> [True]
  | Lt (cn1, n1)
  | Ge (cn1, n1) ->
    if (n1 <= 0) then [True] else [Lt (cn1, n1); Ge (cn1, n1)]
  | Le (cn1, n1) 
  | Gt (cn1, n1) ->
    if (n1 < 0) then [True] else [Le (cn1, n1); Gt (cn1, n1)]
  | Eq (cn1, n1) ->
    if
      (n1 < 0)
    then
      [True]
    else
      if
        (n1 = 0)
      then
        [Le (cn1, n1); Gt (cn1, n1)]
      else
        [Lt (cn1, n1); Eq (cn1, n1); Gt (cn1, n1)]

let minimise_clock_constraint clock_names clock_constraint =
  let
      clock_name_list = Array.to_list clock_names
  in
  let
      phase1 clock_constraint =
    if
      List.exists
        ((=) False)
        clock_constraint
    then
      [False]
    else
      clock_constraint
  in
  let
      phase2 clock_constraint =
    List.filter
      ((<>) True)
      clock_constraint
  in
  let
      phase3 clock_constraint =
      List.concat
        (List.map
           (function unit_clock_constraint ->
             match
               unit_clock_constraint
             with
             | Eq (cn, c) -> [Le (cn, c); Ge (cn, c)]
             | _ -> [unit_clock_constraint]
           )
           clock_constraint
        )
  in
  let
      phase4 clock_constraint =
    List.fold_left
      (function clock_constraint ->
        function cn ->
          if
            List.exists
              (function Lt (cn1, _) -> (cn1 = cn) | _ -> false)
              clock_constraint
          then
            Lt (cn, (List.fold_left
                       (function current_min ->
                         function Lt (cn1, n1) ->
                           if
                             cn1 = cn
                           then
                             min
                               current_min
                               n1
                           else
                             current_min
                         | _ -> current_min
                       )
                       max_int
                       clock_constraint
            ))
            ::
              (List.filter
                 (function Lt (cn1, _) -> (cn1 <> cn) | _ -> true)
                 clock_constraint
              )
          else
            clock_constraint
      )
      clock_constraint
      clock_name_list
  in
  let
      phase5 clock_constraint =
    List.fold_left
      (function clock_constraint ->
        function cn ->
          if
            List.exists
              (function Le (cn1, _) -> (cn1 = cn) | _ -> false)
              clock_constraint
          then
            Le (cn, (List.fold_left
                       (function current_min ->
                         function Le (cn1, n1) ->
                           if
                             cn1 = cn
                           then
                             min
                               current_min
                               n1
                           else
                             current_min
                         | _ -> current_min
                       )
                       max_int
                       clock_constraint
            ))
            ::
              (List.filter
                 (function Le (cn1, _) -> (cn1 <> cn) | _ -> true)
                 clock_constraint
              )
          else
            clock_constraint
      )
      clock_constraint
      clock_name_list
  in
  let
      phase6 clock_constraint =
    List.fold_left
      (function clock_constraint ->
        function cn ->
          if
            List.exists
              (function Ge (cn1, _) -> (cn1 = cn) | _ -> false)
              clock_constraint
          then
            Ge (cn, (List.fold_left
                       (function current_max ->
                         function Ge (cn1, n1) ->
                           if
                             cn1 = cn
                           then
                             max
                               current_max
                               n1
                           else
                             current_max
                         | _ -> current_max
                       )
                       min_int
                       clock_constraint
            ))
            ::
              (List.filter
                 (function Ge (cn1, _) -> (cn1 <> cn) | _ -> true)
                 clock_constraint
              )
          else
            clock_constraint
      )
      clock_constraint
      clock_name_list
  in
  let
      phase7 clock_constraint =
    List.fold_left
      (function clock_constraint ->
        function cn ->
          if
            List.exists
              (function Gt (cn1, _) -> (cn1 = cn) | _ -> false)
              clock_constraint
          then
            Gt (cn, (List.fold_left
                       (function current_max ->
                         function Gt (cn1, n1) ->
                           if
                             cn1 = cn
                           then
                             max
                               current_max
                               n1
                           else
                             current_max
                         | _ -> current_max
                       )
                       min_int
                       clock_constraint
            ))
            ::
              (List.filter
                 (function Gt (cn1, _) -> (cn1 <> cn) | _ -> true)
                 clock_constraint
              )
          else
            clock_constraint
      )
      clock_constraint
      clock_name_list
  in
  phase7 (phase6 (phase5 (phase4 (phase3 (phase2 (phase1 clock_constraint))))))

let clock_constraint_intersection clock_names c1 c2 =
  minimise_clock_constraint clock_names (c1 @ c2)

let split_zone_on_clock_constraint zone clock_constraint clock_names=
  List.fold_left (*This is where we split by each of the constituents
                   of the constraint, one by one.*)
    (function zone_list ->
      function unit_clock_constraint -> (*return a zone list
                                          containing these zones split
                                          by this unit clock
                                          constraint.*)
        List.fold_left
          (function partial_zone_list -> 
            function zone -> (*return the partial zone list augmented
                               with the zones we get from the
                               splitting of this zone.*)
              (List.filter
                 (function zone ->
                   match
                     (clock_constraint_to_raw_t_option clock_names zone.zone_constraint1)
                   with
                     None -> false
                   | _ -> true
                 )
                 (List.map
                    (function unit_clock_constraint -> (*Yep, variable overuse.*)
                      {zone_location1 = zone.zone_location1;
                       zone_constraint1 =
                          (unit_clock_constraint ::
                             zone.zone_constraint1)
                      }
                    )
                    (split_on_unit_clock_constraint unit_clock_constraint)
                 ) (*OK, this list contains all the possible zones,
                     but some have to go.*)
              )
              @
                partial_zone_list
          )
          []
          zone_list
    )
    [zone]
    clock_constraint

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

let split_zone_list_on_constraint_list zone_list constraint_list ta =
  List.fold_left
    (function zone_list ->
      function clock_constraint ->
        List.fold_left
          (function partial_zone_list ->
            function zone ->
              (List.map
                 (function zone ->
                   {zone_location1 = zone.zone_location1;
                    zone_constraint1 =
                       (minimise_clock_constraint
                          ta.clock_names
                          zone.zone_constraint1
                       )
                   }
                 )
                 ((split_zone_on_clock_constraint zone clock_constraint ta.clock_names)
                  @
                    partial_zone_list)
              )
          )
          []
          zone_list
    )
    zone_list
    constraint_list
