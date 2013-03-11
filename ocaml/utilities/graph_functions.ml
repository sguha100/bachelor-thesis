open Grammar_types

let unit_clock_constraint_max unit_clock_constraint cn =
  match
    unit_clock_constraint
  with
    True -> Lstrict 0
  | False -> Lstrict 0
  | Lt (cn1, n1) -> if (cn1 = cn) then Lstrict n1 else Lstrict 0
  | Le (cn1, n1) -> if (cn1 = cn) then Lslack n1 else Lstrict 0
  | Eq (cn1, n1) -> if (cn1 = cn) then Lslack n1 else Lstrict 0
  | Ge (cn1, n1) -> if (cn1 = cn) then Lslack n1 else Lstrict 0
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
    True -> [True]
  | False -> [True]
  | Lt (cn1, n1) -> [Lt (cn1, n1); Ge (cn1, n1)]
  | Le (cn1, n1) -> [Le (cn1, n1); Gt (cn1, n1)]
  | Eq (cn1, n1) -> [Lt (cn1, n1); Eq (cn1, n1); Gt (cn1, n1)]
  | Ge (cn1, n1) -> [Lt (cn1, n1); Ge (cn1, n1)]
  | Gt (cn1, n1) -> [Le (cn1, n1); Gt (cn1, n1)]

let init_zone_array ta =
  (Array.init
     ta.numlocations
     (function i -> {zone_location = i; zone_constraint = [True]})
  )

let init_tree_array ta = 
  (Array.init
     ta.numlocations
     (function i -> [])
  )

let unit_constraint_intersection c1 c2 =
  match
    (c1, c2)
  with
    (True, _) -> [c2]
  | (False, _) -> [False]
  | (_, True) -> [c1]
  | (_, False) -> [False]
  | (Lt (cn1, n1), Lt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Lt (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Lt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [False]
      else
        [c2]
    else
      [c1; c2]
  | (Lt (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Lt (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Le (cn1, n1), Lt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Le (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Le (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [False]
      else
        [c2]
    else
      [c1; c2]
  | (Le (cn1, n1), Ge (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 < n2))
    then
      [False]
    else
      [c1; c2]
  | (Le (cn1, n1), Gt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 <= n2))
    then
      [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Lt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Le (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 == n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Eq (cn1, n1), Gt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 > n2)
      then
        [c1]
      else
        [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Lt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Le (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 > n2))
    then
      [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 <= n2)
      then
        [c2]
      else
        [False]
    else
      [c1; c2]
  | (Ge (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Ge (cn1, n1), Gt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 > n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Gt (cn1, n1), Lt (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Le (cn2, n2)) ->
    if
      ((cn1 = cn2) && (n1 >= n2))
    then
      [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Eq (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 < n2)
      then
        [c2]
      else
        [False]
    else
      [c1; c2]
  | (Gt (cn1, n1), Ge (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 >= n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]
  | (Gt (cn1, n1), Gt (cn2, n2)) ->
    if
      (cn1 = cn2)
    then
      if
        (n1 > n2)
      then
        [c1]
      else
        [c2]
    else
      [c1; c2]

let split_zone_on_clock_constraint zone clock_constraint =
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
                 {zone_location = zone.zone_location; zone_constraint
                   = (List.fold_left
                        (function partial_clock_constraint ->
                          function unit_of_zone_constraint ->
                            (unit_constraint_intersection unit_of_zone_constraint unit_clock_constraint)
                            @
                              partial_clock_constraint
                        )
                        []
                        zone.zone_constraint
                   )}
                   ::
                   partial_zone_list
          )
          []
          zone_list
    )
    [zone]
    clock_constraint
