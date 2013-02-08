let parse_timed_automaton channel =
  let
      lexbuf = Lexing.from_channel channel
  in
  let
      result = Parser.main Lexer.token lexbuf
  in
  result

