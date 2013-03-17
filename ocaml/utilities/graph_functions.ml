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
    True
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

let minimise_clock_constraint clock_constraint clock_name_list =
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
      phase4 clock_constraint =
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
      phase5 clock_constraint =
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
      phase6 clock_constraint =
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
  phase6 (phase5 (phase4 (phase3 (phase2 (phase1 clock_constraint)))))

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
              (List.map
                 (function unit_clock_constraint -> (*Yep, variable overuse.*)
                   {zone_location = zone.zone_location;
                    zone_constraint =
                       unit_clock_constraint :: zone.zone_constraint
                   }
                 )
                 (split_on_unit_clock_constraint unit_clock_constraint)
              )
              @
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
  flush stdout;
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
        flush stdout;
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
        flush stdout;
        (zone_list_array.(qhd) <-
           split_zone_list_on_constraint_list
            zone_list_array.(qhd)
            constraint_list
            ta
        ) ;
        Printf.printf
          "qhd = %s, zone_list length = %s after split\n"
          (string_of_int qhd)
          (string_of_int (List.length zone_list_array.(qhd)));
        flush stdout
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
    (Printf.printf "Starting with elements of the tree of qhd.\n");
    flush stdout;
    (List.fold_left
       (function this_must_be_a_unit ->
         function tree_element ->
           this_must_be_a_unit;
           let
               constraint_list =
             (List.map
                (function zone ->
                  zone.zone_constraint
                )
                zone_list_array.(qhd)
             )
           in
           zone_list_array.(tree_element) <-
             (split_zone_list_on_constraint_list
                zone_list_array.(tree_element)
                constraint_list
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
    (Printf.printf "Done with elements of the tree of qhd.\n");
    flush stdout;
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
    (Printf.printf "Starting with successors of qhd.\n");
    flush stdout;
    (List.fold_left
       (function this_must_be_a_unit ->
         function successor ->
           Printf.printf "A successor arrived.\n";
           flush stdout;
           this_must_be_a_unit;
           let
               constraint_list = (List.map
                                    (function zone ->
                                      zone.zone_constraint
                                    )
                                    zone_list_array.(qhd)
           )
           in
           Printf.printf
             "constraint_list length = %s\n"
             (string_of_int (List.length constraint_list));
           flush stdout;
           zone_list_array.(successor) <-
             (split_zone_list_on_constraint_list
                zone_list_array.(successor)
                constraint_list
                ta
             );
           Printf.printf "A successor left.\n";
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
    (Printf.printf "Done with the successors of qhd.\n");
    (flush stdout);
    let
        queue =
      Printf.printf "queue length then = %s\n" (string_of_int
                                                  (List.length
                                                     queue));
      flush stdout;
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
    flush stdout;
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
