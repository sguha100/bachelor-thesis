(* File calc.ml *)
open Parse_timed_automaton
open Graph_functions
open Grammar_types

let _ =
  let result = parse_timed_automaton stdin in
  let g = generate_zone_valuation_graph result in
  Printf.printf "numlocations = %s\n" (string_of_int result.numlocations);
  Printf.printf "numtrans = %s\n" (string_of_int result.numtrans);
  Printf.printf "numclocks = %s\n" (string_of_int result.numclocks);
  Printf.printf "numinit = %s\n" (string_of_int result.numinit);
  let len = (Array.length g) in
  for i = 0 to len - 1 do
    Printf.printf
      "i = %s, %s\n"
      (string_of_int
         i)
      (string_of_int
         (List.length
            g.(i)
         ));
  done;
  flush stdout;
  exit 0
