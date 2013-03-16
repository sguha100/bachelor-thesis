open Grammar_types
open Unit_constraint_intersection

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
    True -> [True]
  | False -> [True]
  | Lt (cn1, n1)
  | Ge (cn1, n1) -> [Lt (cn1, n1); Ge (cn1, n1)]
  | Le (cn1, n1)
  | Gt (cn1, n1) -> [Le (cn1, n1); Gt (cn1, n1)]
  | Eq (cn1, n1) -> [Lt (cn1, n1); Eq (cn1, n1); Gt (cn1, n1)]

let init_zone_list_array ta =
  (Array.init
     ta.numlocations
     (function i -> [{zone_location = i; zone_constraint = [True]}])
  )

let init_tree_array ta = 
  (Array.init
     ta.numlocations
     (function i -> [])
  )

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

let split_zone_list_on_constraint_list zone_list constraint_list ta =
  List.fold_left
    (function zone_list ->
      function clock_constraint ->
        List.fold_left
          (function partial_zone_list ->
            function zone ->
              (split_zone_on_clock_constraint zone clock_constraint)
              @
                partial_zone_list
          )
          []
          zone_list
    )
    zone_list
    constraint_list

let dequeue ta (queue, zone_list_array, tree_array) =
  Printf.printf("dequeue got called!\n");
  match queue with
    [] -> (queue, zone_list_array, tree_array)
  | qhd::qtl -> (
    match
      tree_array.(qhd)
    with
      [] ->
        Printf.printf
          "qhd = %s, zone_list length = %s before split\n"
          (string_of_int qhd)
          (string_of_int (List.length zone_list_array.(qhd)));
        let
            constraint_list =
          (ta.locations.(qhd).invariant
           ::
             (List.fold_left
                (function partial_clock_constraint_list ->
                  function departure ->
                    ([departure.condition
                     ;
                      (List.filter
                         (function unit_clock_constraint ->
                           match
                             unit_clock_constraint
                           with
                             True
                           | False -> false
                           | Lt (cn, n)
                           | Le (cn, n)
                           | Eq (cn, n)
                           | Ge (cn, n)
                           | Gt (cn, n) ->
                             (not (List.exists
                                     ((=) cn)
                                     (Array.to_list departure.clock_resets)
                              ))
                         )
                         ta.locations.(departure.next_location).invariant
                      )
                     ]
                     @
                       partial_clock_constraint_list
                    )
                )
                []
                (Array.to_list ta.locations.(qhd).departures)
             )
          )
        in
        (Printf.printf
           "constraint_list length = %s\n"
           (string_of_int (List.length constraint_list))
        );
        (zone_list_array.(qhd) <-
           split_zone_list_on_constraint_list
            zone_list_array.(qhd)
            constraint_list
            ta
        ) ;
        Printf.printf
          "qhd = %s, zone_list length = %s after split\n"
          (string_of_int qhd)
          (string_of_int (List.length zone_list_array.(qhd)))
    | thd::ttl ->
      zone_list_array.(qhd) <-
        (split_zone_list_on_constraint_list
           zone_list_array.(qhd)
           (List.map
              (function zone -> zone.zone_constraint)
              zone_list_array.(thd)
           )
           ta)
  )
    ;
    (List.fold_left
       (function this_must_be_a_unit ->
         function tree_element ->
           this_must_be_a_unit;
           zone_list_array.(tree_element) <-
             (split_zone_list_on_constraint_list
                zone_list_array.(tree_element)
                (List.map
                   (function zone ->
                     zone.zone_constraint
                   )
                   zone_list_array.(qhd)
                )
                ta
             );
           if
             (List.exists
                ((=) qhd)
                tree_array.(tree_element)
             )
           then
             ()
           else
             (tree_array.(tree_element) <-
                qhd::tree_array.(tree_element)
             );
       )
       ()
       tree_array.(qhd)
    );
    let
        queue =
      (List.filter
         (function thd ->
           List.exists
             (function tree_element ->
               List.for_all
                 ((<>) tree_element)
                 tree_array.(qhd)
             )
             tree_array.(thd)
         )
         tree_array.(qhd)) @ qtl
    in
    let
        successors = (Array.to_list
                        ta.locations.(qhd).departures)
    in
    (List.fold_left
       (function this_must_be_a_unit ->
         function successor ->
           this_must_be_a_unit;
           zone_list_array.(successor) <-
             (split_zone_list_on_constraint_list
                zone_list_array.(successor)
                (List.map
                   (function zone ->
                     zone.zone_constraint
                   )
                   zone_list_array.(qhd)
                )
                ta
             );
           if
             (List.exists
                ((=) qhd)
                tree_array.(successor)
             )
           then
             ()
           else
             (tree_array.(successor) <-
                qhd::tree_array.(successor)
             );
       )
       ()
       (List.map
          (function departure -> departure.next_location)
          successors
       )
    );
    let
        queue =
      Printf.printf "queue length then = %s\n" (string_of_int
                                                  (List.length queue));
      (List.filter
         (function thd ->
           List.exists
             (function tree_element ->
               List.for_all
                 ((<>) tree_element)
                 tree_array.(qhd)
             )
             tree_array.(thd)
         )
         tree_array.(qhd)) @ queue
    in
    Printf.printf "queue length now = %s\n" (string_of_int
                                               (List.length queue));
    (queue,
     zone_list_array,
     tree_array)

let rec empty_queue ta (queue, zone_list_array, tree_array) =
  match queue with
    [] -> (queue, zone_list_array, tree_array)
  | qhd::qtl -> (empty_queue
                   ta
                   (dequeue
                      ta
                      (queue, zone_list_array, tree_array)
                   )
  )

let generate_zone_valuation_graph ta =
  match
    (empty_queue
       ta
       ([ta.numinit],
        (init_zone_list_array ta),
        (init_tree_array ta)
       )
    )
  with
    (_, zone_list_array, _) -> zone_list_array
