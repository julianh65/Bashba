type token =
  | SEMI
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | COMMA
  | AND
  | OR
  | NOT
  | EQ
  | LEQ
  | GEQ
  | NEQ
  | GT
  | LT
  | ASSIGN
  | COLON
  | WHILE
  | RETURN
  | IF
  | ELSE
  | EOF
  | LAMBDA
  | BREAK
  | CONTINUE
  | ARROW
  | LAMB
  | LBRACK
  | RBRACK
  | STRINGARRAY
  | INTARRAY
  | FILEARRAY
  | BOOL
  | INT
  | STRING
  | NONE
  | FILE
  | BLIT of (bool)
  | ID of (string)
  | STRINGLIT of (string)
  | LITERAL of (int)
  | FILELIT of (string)

val prog_rules :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
