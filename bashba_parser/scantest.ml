open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokenseq = Parser.prog_rules Scanner.token lexbuf in
  print_endline (string_of_program tokenseq)
