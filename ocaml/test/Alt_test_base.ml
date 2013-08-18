open Alt_grammar_types

module TA = Timed_automaton

(* CAV paper example *)
let ta1 = TA.make_t {
  numlocations = 3;
  numtrans = 3;
  numclocks = 1;
  numactions = 3;
  numinit = {location_index = 0};
  clock_name_set = String_set.singleton "X";
  location_graph =
    (List.fold_left
       (function g ->
         function (src, condition, action, clock_resets, dst) ->
           (Location_graph.add_edge_e
              g
              (Location_graph.E.create
                 {location_index = src}
                 {condition = condition;
                  action = action;
                  clock_resets = clock_resets}
                 {location_index = dst}
              )
           )
       )
       Location_graph.empty
       [(0, [Gt ("X", 2)], 0, String_set.empty, 1);
        (1, [Gt ("X", 5)], 1, String_set.singleton "X", 2);
        (2, [Gt ("X", 8)], 2, String_set.empty, 0)]
    );
  location_invariant_map =
    List.fold_left
      (fun m i ->
        Location_invariant_map.add
          {location_index = i}
          [True]
          m
      )
      Location_invariant_map.empty
      [0; 1; 2]
}
