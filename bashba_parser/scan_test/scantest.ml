open Scantestast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokenseq = Scantestparser.program Scantestscanner.token lexbuf in
  print_endline (string_of_program tokenseq)
