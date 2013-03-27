open Grammar_types
open Unit_constraint_intersection
open Zone_stubs
open UDBM_utilities
open Clock_constraint_utilities

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

let dequeue ta (queue, zone_list_array, tree_array) =
  let queueref = ref queue in
  match !queueref with
    [] -> (!queueref, zone_list_array, tree_array)
  | qhd::qtl ->
    Printf.printf "dequeue %s!\n\n" (string_of_int qhd);
    flush stdout;
    queueref := qtl;
    flush stdout;
    (
      match
        tree_array.(qhd)
      with
        [] -> ()
      | thd::ttl ->
        zone_list_array.(qhd) <- (
          Printf.printf
            "qhd = %s, zone_list length = %s before split\n"
          (string_of_int qhd)
            (string_of_int (List.length zone_list_array.(qhd)));
          (Printf.printf
             "Tree top is %s, constraint_list length = %s\n"
             (string_of_int thd)
             (string_of_int (List.length zone_list_array.(thd)))
          );
          flush stdout;
          (split_zone_list_on_constraint_list
             zone_list_array.(qhd)
             (List.map
                (function zone -> zone.zone_constraint)
                zone_list_array.(thd)
             )
             ta)
        );
        Printf.printf
          "qhd = %s, zone_list length = %s after split\n"
          (string_of_int qhd)
          (string_of_int (List.length zone_list_array.(qhd)))
    ) ;
    flush stdout
    ;
    (if (not (List.mem qhd tree_array.(qhd)))
     then
        let
            constraint_list =
          (ta.locations.(qhd).invariant
           ::
             (List.fold_left
                (function partial_clock_constraint_list ->
                  function departure ->
                    ([departure.condition
                     ;
                      (clock_constraint_without_reset_clocks
                         ta.locations.(departure.next_location).invariant
                         departure.clock_resets
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
           "qhd = %s, zone_list length = %s before split\n"
           (string_of_int qhd)
           (string_of_int (List.length zone_list_array.(qhd))));
          (Printf.printf
             "Self-splitting, constraint_list length = %s\n"
             (string_of_int (List.length constraint_list))
          );
        flush stdout;
        (zone_list_array.(qhd) <-
           (split_zone_list_on_constraint_list
              zone_list_array.(qhd)
              constraint_list
              ta)
        );
        (Printf.printf
           "qhd = %s, zone_list length = %s after split\n"
           (string_of_int qhd)
           (string_of_int (List.length zone_list_array.(qhd))))
     else
        () 
    );
    (Printf.printf
       "Starting with the tree of qhd = [%s].\n"
       (String.concat "; " (List.map string_of_int tree_array.(qhd))));
    flush stdout;
    (List.fold_left
       (function this_must_be_a_unit ->
         function tree_element ->
           this_must_be_a_unit;
           (Printf.printf "constraint_list length = %s\n"
              (string_of_int (List.length zone_list_array.(qhd))));
           flush stdout;
           let
               constraint_list =
             (List.map
                (function zone ->
                  zone.zone_constraint
                )
                zone_list_array.(qhd)
             )
           in
           let
               changed_zone_list =
             (split_zone_list_on_constraint_list
                zone_list_array.(tree_element)
                constraint_list
                ta
             )
           in
           queueref :=
               if
                 (List.length changed_zone_list
                  <>
                    List.length zone_list_array.(tree_element)
                 )
                   &&
                   (List.for_all
                      ((<>) tree_element)
                      !queueref
                   )
               then
                 (Printf.printf "enqueue %s\n\n" (string_of_int
                                                    tree_element);
                  flush stdout;
                  tree_element::(!queueref))
               else
                 (!queueref)
           ;
           zone_list_array.(tree_element) <-
             changed_zone_list
           ;
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
    (Printf.printf "Starting with successors of qhd.\n");
    flush stdout;
    (List.fold_left
       (function this_must_be_a_unit ->
         function departure ->
           let successor = departure.next_location in
           Printf.printf "Successor=%s arrived.\n" (string_of_int successor);
           flush stdout;
           this_must_be_a_unit;
           let
               constraint_list = (List.map
                                    (function zone ->
                                      clock_constraint_after_clock_resets
                                        zone.zone_constraint
                                        departure.clock_resets
                                    )
                                    zone_list_array.(qhd)
           )
           in
           Printf.printf
             "constraint_list length = %s\n"
             (string_of_int (List.length constraint_list));
           flush stdout;
           let
               changed_zone_list =
             (split_zone_list_on_constraint_list
                zone_list_array.(successor)
                constraint_list
                ta
             )
           in
           queueref :=
               if
                 (List.length changed_zone_list
                  <>
                    List.length zone_list_array.(successor)
                 )
                   &&
                   (List.for_all
                      ((<>) successor)
                      !queueref
                   )
               then
                 (Printf.printf "enqueue successor %s\n\n"
                    (string_of_int successor);
                  flush stdout;
                  successor::(!queueref))
               else
                 (!queueref)
           ;
           zone_list_array.(successor) <-
             changed_zone_list;
           Printf.printf "Successor=%s left.\n" (string_of_int successor);
           flush stdout;
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
       (Array.to_list ta.locations.(qhd).departures)
    );
    (Printf.printf "Done with the successors of qhd.\n");
    (flush stdout);
    Printf.printf "queue now = [%s]\n" (String.concat "; " (List.map string_of_int queue));
    flush stdout;
    (!queueref,
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
  let zone_list_array = 
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
  in
  let
      graph = 
    Array.map
      (function zone_list ->
        List.map
          (function zone ->
            (zone,
             let
                 departures =
               {action = -1;
                condition = [True];
                clock_resets = [||];
                next_location = zone.zone_location
               } (*This one is a time transition.*)
               ::
               (List.filter
                 (function departure ->
                   clock_constraint_haveIntersection
                     ta.clock_names
                     zone.zone_constraint
                     departure.condition
                 )
                 (Array.to_list
                    ta.locations.(zone.zone_location).departures)
               )
             in
             List.map
               (function departure ->
                 (departure,
                  if (departure.action >= 0) then (*action transition*)
                    (List.filter
                       (function arrival_zone ->
                         clock_constraint_haveIntersection
                           ta.clock_names
                           (clock_constraint_after_clock_resets
                              zone.zone_constraint
                              departure.clock_resets
                           )
                           arrival_zone.zone_constraint
                       )
                       zone_list_array.(departure.next_location)
                    )
                  else (*time transition*)
                    (List.filter
                       (function arrival_zone ->
                         (* clock_constraint_haveIntersection *)
                         (*   ta.clock_names *)
                         (*   zone.zone_constraint (\*TODO: make this upward unbounded!*\) *)
                         (*   arrival_zone.zone_constraint *)
                         match
                           (clock_constraint_to_raw_t_option
                              ta.clock_names
                              zone.zone_constraint)
                         with
                           None -> false
                         | Some dst ->
                           (match
                               (clock_constraint_to_raw_t_option
                                  ta.clock_names
                                  arrival_zone.zone_constraint)
                            with
                              None -> false
                            | Some src ->
                              (dbm_haveIntersection
                                 (dbm_up dst (1 + Array.length ta.clock_names))
                                 src
                                 (1 + Array.length ta.clock_names))
                           )
                       )
                       zone_list_array.(departure.next_location)
                    )
                 )
               )
               departures
            )
          )
          zone_list
      )
      zone_list_array
  in
  graph

