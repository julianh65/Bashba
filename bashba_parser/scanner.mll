(* compile with $ocamllex scanner.mll *)
{ open Parser }

let dig = ['0'-'9']
let lit = ['a'-'z' 'A'-'Z']
let quote = "\""
let string_lit = quote (dig|lit|" ")* quote

rule token = parse 
 ['\n' '\t' '\r' ' '] {token lexbuf} 
 | "##" {comment lexbuf}
 | '+' { PLUS } 
 | '-' { MINUS } 
 | '*' { TIMES } 
 | '/' { DIVIDE } 
 | '%' { MOD }  
 | '{' { LBRACE } 
 | '}' { RBRACE } 
 | '(' { LPAREN } 
 | ')' { RPAREN } 
 | ',' { COMMA } 
 | ';' { SEMI } 
 | "and" { AND } 
 | "or" { OR } 
 | "not" { NOT } 
 | "==" { EQ } 
 | "<=" { LEQ } 
 | ">=" { GEQ } 
 | "!=" { NEQ } 
 | '>' { GT } 
 | '<' { LT } 
 | '=' { ASSIGN } 
 | "True" { BLIT(true) }
 | "False" { BLIT(false) }
 | "while" { WHILE } 
 | "return" { RETURN } 
 | "if"|"elif" { IF } 
 | "else" { ELSE } 
 | ':' { COLON } 
 | "lamb" { LAMB } 
 | "->" { ARROW }
 | "continue" { CONTINUE } 
 | "break" { BREAK } 
 | "none" { NONE }  
 | "int" { INT } 
 | "bool" { BOOL } 
 | "String" { STRING } 
 | string_lit as s { STRINGLIT(s) }
 | dig+ as d { LITERAL(int_of_string d) }
 | lit ( dig | lit | '_')* as d {ID(d)}
 | eof { EOF } 
 | _ as c { raise (Failure("illegal character " ^ Char.escaped c)) } 
and comment = parse 
'\n' { token lexbuf }
| _ { comment lexbuf }
