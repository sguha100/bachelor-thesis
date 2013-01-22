(* File calc.ml *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  (let result = Parser.main Lexer.token lexbuf in
   print_int (result.Grammar_types.numlocations); 
   print_newline(); 
   print_int (result.Grammar_types.numtrans); 
   print_newline(); 
   print_int (result.Grammar_types.numclocks); 
   print_newline(); 
   print_int (result.Grammar_types.numinit); 
   print_newline(); 
   print_int (Array.length result.Grammar_types.clocks); 
   print_newline(); 
   print_int (Array.length result.Grammar_types.locations); 
   print_newline(); 
   flush stdout;
   exit 0)
