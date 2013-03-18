(* File calc.ml *)
open Parse_timed_automaton
open Graph_functions
open Grammar_types

let _ =
  let result = parse_timed_automaton stdin in
  let
      unit = (Printf.printf "Timed automaton parsed successfully!\n");
    (flush stdout)
  in
  let g = generate_zone_valuation_graph result in
  let
      unit = Printf.printf "Zone-valuation graph generated successfully!\n"
  in
  Printf.printf "#locations %s\n" (string_of_int result.numlocations);
  Printf.printf "#trans %s\n" (string_of_int result.numtrans);
  Printf.printf "#clocks %s\n" (string_of_int result.numclocks);
  Printf.printf "#actions %s\n" (string_of_int result.numactions);
  Printf.printf "#init %s\n" (string_of_int result.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    (* Printf.printf *)
    (*   "i = %s, %s\n" *)
    (*   (string_of_int *)
    (*      i) *)
    (*   (string_of_int *)
    (*      (List.length *)
    (*         g.(i) *)
    (*      )); *)
    List.iter
      (function zone ->
        Printf.printf "\nlocation: %s\n" (string_of_int i);
        Printf.printf
          "invar: %s\n"
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
                zone.zone_constraint
             )
          )
        ;
      )
      g.(i)
    ;
  done;
  flush stdout;
  exit 0
