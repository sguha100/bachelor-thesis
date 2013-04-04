(* File calc.ml *)
open Parse_timed_automaton
open Clock_constraint_utilities
open Graph_functions
open Grammar_types
open UDBM_utilities
open Zone_stubs
open ZVG_modules

let text_dump ta g = 
  let txt_out = open_out "/tmp/lts.txt" in
  Printf.fprintf txt_out "#locations %s\n" (string_of_int ta.numlocations);
  Printf.fprintf txt_out "#trans %s\n" (string_of_int ta.numtrans);
  Printf.fprintf txt_out "#clocks %s\n" (string_of_int ta.numclocks);
  Printf.fprintf txt_out "#actions %s\n" (string_of_int ta.numactions);
  Printf.fprintf txt_out "#init %s\n" (string_of_int ta.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    List.iter
      (function (zone, edges_of_zone) ->
        Printf.fprintf txt_out "\nlocation: %s\n" (string_of_int i);
        Printf.fprintf txt_out
          "invar: %s\n"
          (string_of_clock_constraint zone.zone_constraint)
        ;
        Printf.fprintf txt_out "trans:\n";
        List.iter
          (function (departure, _) ->
            Printf.fprintf
              txt_out
              "ACT %s; RESET { %s }; goto %s\n"
              (string_of_int departure.action)
              (String.concat
                 " "
                 (Array.to_list departure.clock_resets)
              )
              (string_of_int departure.next_location)
          )
          edges_of_zone
      )
      g.(i)
    ;
  done;
  flush txt_out;
  close_out txt_out

let _ =
  let result = parse_timed_automaton stdin in
  let g = generate_zone_valuation_graph result in
  text_dump result g;
  let l = lts_of_zone_valuation_graph result in
  (ZVGLTS.print_dot
     l
     (ZVGLTS.fernandez_specifying_partition
        l
        (List.map
           (function ll ->
             ZVGLTS.get_block_from_node_refs
               l
               (List.map (function (zone, _) -> zone) ll)
           )
           (Array.to_list
              g
           )
        )
     )
     "/tmp/lts.dot");
  exit 0

