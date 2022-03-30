(* compile with $ocammlex scanner.mll *)
{ open Parser }

let dig = ['0'-'9']
let lit = ['a'-'z' 'A'-'Z']

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
 | "def" { DEF } 
 | "lambda" { LAMBDA } 
 | "continue" { CONTINUE } 
 | "break" { BREAK } 
 | "none" { NONE }  
 | "int" { INT } 
 | "bool" { BOOL } 
 | "String" { STRING } 
 | dig+ as d { LIT(int_of_string d) }
 | lit ( dig | lit | '_')* as d {ID(d)}
 | eof { EOF } 
 | _ as c { raise (Failure("illegal character " ^ Char.escaped char)) } 
and comment = parse 
'\n' { token lexbuf }
| _ { comment lexbuf }
