  (* File lexer.mll *)
  {
   open Parser        (* The type token is defined in parser.mli *)
exception Eof
 }
  let intexp = ['0'-'9']+
  let identexp = ['A'-'Z'](['A'-'Z']|['0'-'9'])*
  rule token = parse
  [' ' '\t']       { token lexbuf }     (* skip blanks *)
| ['\n' ]          { EOL }
| intexp as lxm    { INT(int_of_string lxm) }
| "TRUE"           { TRUE }
| "FALSE"          { FALSE }
| "RESET"          { RESET }
| identexp as lxm  { IDENT(lxm)}
| "=>"             { RARROW }
| ':'              { COLON }
| ';'              { SEMI }
| '{'              { LBRACE }
| '}'              { RBRACE }
| "#states"        { NUMSTATES }
| "#trans"         { NUMTRANS }
| "#clocks"        { NUMCLOCKS }
| "state"          { STATE }
| "prop"           { PROP }
| "invar"          { INVAR }
| "trans"          { TRANS }
| "goto"           { GOTO }
| "<="             { LE }
| ">="             { GE }
| '<'              { LT }
| '>'              { GT }
| '='              { EQ }
| "and"            { AND}
| eof              { raise Eof }
