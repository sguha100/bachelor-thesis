(* File calc.ml *)
open Parse_timed_automaton

let _ =
  let result = parse_timed_automaton stdin in
  print_int (result.Grammar_types.numlocations); 
  print_newline(); 
  print_int (result.Grammar_types.numtrans); 
  print_newline(); 
  print_int (result.Grammar_types.numclocks); 
  print_newline(); 
  print_int (result.Grammar_types.numinit); 
  print_newline(); 
  print_int (Array.length result.Grammar_types.clock_names); 
  print_newline(); 
  print_int (Array.length result.Grammar_types.locations); 
  print_newline(); 
  flush stdout;
  exit 0
