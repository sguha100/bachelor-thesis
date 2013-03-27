(* File calc.ml *)
open Parse_timed_automaton
open Graph_functions
open Grammar_types
open UDBM_utilities
open Zone_stubs
open ZVG_modules

let _ =
  let result = parse_timed_automaton stdin in
  let g = generate_zone_valuation_graph result in
  Printf.printf "#locations %s\n" (string_of_int result.numlocations);
  Printf.printf "#trans %s\n" (string_of_int result.numtrans);
  Printf.printf "#clocks %s\n" (string_of_int result.numclocks);
  Printf.printf "#actions %s\n" (string_of_int result.numactions);
  Printf.printf "#init %s\n" (string_of_int result.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    List.iter
      (function (zone, edges_of_zone) ->
        Printf.printf "\nlocation: %s\n" (string_of_int i);
        Printf.printf
          "invar: %s\n"
          (string_of_clock_constraint zone.zone_constraint)
        ;
        Printf.printf "trans:\n";
        List.iter
          (function (departure, _) ->
            Printf.printf
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
  flush stdout;
  let l = lts_of_zone_valuation_graph result in
  (ZVGLTS.print_dot l (ZVGLTS.fernandez l) "/tmp/something.dot");
  exit 0

