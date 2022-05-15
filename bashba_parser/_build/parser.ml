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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 54 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* PLUS *);
  259 (* MINUS *);
  260 (* TIMES *);
  261 (* DIVIDE *);
  262 (* MOD *);
  263 (* LBRACE *);
  264 (* RBRACE *);
  265 (* LPAREN *);
  266 (* RPAREN *);
  267 (* COMMA *);
  268 (* AND *);
  269 (* OR *);
  270 (* NOT *);
  271 (* EQ *);
  272 (* LEQ *);
  273 (* GEQ *);
  274 (* NEQ *);
  275 (* GT *);
  276 (* LT *);
  277 (* ASSIGN *);
  278 (* COLON *);
  279 (* WHILE *);
  280 (* RETURN *);
  281 (* IF *);
  282 (* ELSE *);
    0 (* EOF *);
  283 (* LAMBDA *);
  284 (* BREAK *);
  285 (* CONTINUE *);
  286 (* ARROW *);
  287 (* LAMB *);
  288 (* LBRACK *);
  289 (* RBRACK *);
  290 (* STRINGARRAY *);
  291 (* INTARRAY *);
  292 (* BOOL *);
  293 (* INT *);
  294 (* STRING *);
  295 (* NONE *);
  296 (* FILE *);
    0|]

let yytransl_block = [|
  297 (* BLIT *);
  298 (* ID *);
  299 (* STRINGLIT *);
  300 (* LITERAL *);
  301 (* FILELIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\004\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\007\000\
\007\000\013\000\013\000\008\000\008\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\008\000\000\000\
\001\000\001\000\003\000\000\000\001\000\001\000\003\000\000\000\
\001\000\001\000\003\000\000\000\002\000\002\000\003\000\011\000\
\007\000\005\000\003\000\002\000\002\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\004\000\
\007\000\003\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\014\000\013\000\009\000\008\000\010\000\
\012\000\064\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\004\000\007\000\003\000\000\000\000\000\025\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\038\000\
\000\000\040\000\039\000\041\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\036\000\037\000\
\000\000\000\000\000\000\017\000\000\000\021\000\000\000\000\000\
\000\000\015\000\029\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\054\000\
\000\000\035\000\000\000\000\000\000\000\059\000\058\000\000\000\
\000\000\061\000\000\000\000\000\043\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\023\000\000\000\056\000\000\000\034\000\000\000\063\000\
\000\000\000\000\000\000\000\000\057\000\000\000\000\000\000\000\
\032\000"

let yydgoto = "\002\000\
\010\000\011\000\021\000\013\000\029\000\014\000\045\000\046\000\
\059\000\060\000\061\000\062\000\023\000\047\000\048\000\089\000\
\090\000"

let yysindex = "\005\000\
\075\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\029\255\075\000\228\254\000\000\075\000\
\075\000\000\000\000\000\000\000\020\255\026\255\000\000\075\000\
\045\255\000\000\075\000\058\255\098\000\075\000\098\000\154\000\
\053\255\154\000\061\255\070\255\075\255\014\255\000\000\000\000\
\248\254\000\000\000\000\000\000\051\255\078\255\098\000\048\255\
\000\000\085\255\205\255\154\000\072\255\154\000\000\000\000\000\
\086\255\091\255\050\255\000\000\063\255\000\000\154\000\154\000\
\075\000\000\000\000\000\000\000\154\000\154\000\154\000\154\000\
\154\000\154\000\154\000\154\000\154\000\154\000\000\000\000\000\
\224\255\000\000\243\255\066\255\073\255\000\000\000\000\006\000\
\095\255\000\000\025\000\090\255\000\000\000\000\039\000\039\000\
\062\255\062\255\062\255\062\255\062\255\062\255\098\000\117\255\
\000\000\000\000\154\000\000\000\101\255\000\000\098\000\000\000\
\098\000\118\255\121\255\106\255\000\000\133\255\098\000\125\255\
\000\000"

let yyrindex = "\000\000\
\143\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\143\000\000\000\000\000\143\000\
\137\255\000\000\000\000\000\000\002\255\000\000\000\000\000\000\
\000\000\000\000\137\000\000\000\255\254\137\000\255\254\120\255\
\000\000\120\255\000\000\000\000\000\000\115\255\000\000\000\000\
\008\255\000\000\000\000\000\000\000\000\000\000\251\254\000\000\
\000\000\000\000\000\000\120\255\000\000\120\255\000\000\000\000\
\122\255\129\255\000\000\000\000\000\000\000\000\005\255\120\255\
\000\000\000\000\000\000\000\000\120\255\120\255\120\255\120\255\
\120\255\120\255\120\255\120\255\120\255\120\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\154\255\
\000\000\000\000\068\255\000\000\000\000\000\000\103\255\141\255\
\088\255\110\255\126\255\148\255\164\255\186\255\251\254\000\000\
\000\000\000\000\120\255\000\000\000\000\000\000\255\254\000\000\
\007\255\000\000\000\000\059\000\000\000\000\000\255\254\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\040\000\019\001\000\000\139\000\105\000\161\000\225\255\
\000\000\087\000\000\000\095\000\157\000\000\000\226\255\000\000\
\078\000"

let yytablesize = 455
let yytable = "\050\000\
\063\000\051\000\028\000\053\000\028\000\001\000\028\000\015\000\
\042\000\042\000\042\000\026\000\064\000\019\000\060\000\067\000\
\028\000\042\000\042\000\042\000\042\000\081\000\042\000\083\000\
\024\000\042\000\042\000\042\000\024\000\016\000\024\000\026\000\
\088\000\091\000\024\000\025\000\024\000\017\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\068\000\069\000\070\000\027\000\018\000\042\000\042\000\020\000\
\057\000\058\000\030\000\071\000\072\000\052\000\073\000\069\000\
\070\000\074\000\075\000\076\000\053\000\054\000\055\000\110\000\
\082\000\069\000\070\000\056\000\088\000\053\000\053\000\114\000\
\065\000\115\000\086\000\071\000\072\000\066\000\073\000\120\000\
\045\000\074\000\075\000\076\000\079\000\077\000\078\000\087\000\
\084\000\045\000\045\000\045\000\045\000\085\000\045\000\051\000\
\108\000\045\000\045\000\045\000\057\000\113\000\046\000\109\000\
\051\000\051\000\051\000\051\000\058\000\077\000\078\000\046\000\
\046\000\046\000\046\000\111\000\046\000\116\000\048\000\046\000\
\046\000\046\000\117\000\118\000\121\000\045\000\045\000\048\000\
\048\000\048\000\048\000\119\000\048\000\052\000\002\000\048\000\
\048\000\048\000\024\000\016\000\047\000\024\000\052\000\052\000\
\052\000\052\000\018\000\046\000\046\000\047\000\047\000\047\000\
\047\000\022\000\047\000\062\000\049\000\047\000\047\000\047\000\
\049\000\092\000\105\000\048\000\048\000\049\000\049\000\049\000\
\049\000\022\000\049\000\106\000\026\000\049\000\049\000\049\000\
\112\000\000\000\050\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\047\000\050\000\050\000\050\000\050\000\000\000\
\050\000\000\000\000\000\050\000\050\000\050\000\069\000\070\000\
\000\000\049\000\049\000\000\000\000\000\000\000\080\000\000\000\
\071\000\072\000\000\000\073\000\000\000\000\000\074\000\075\000\
\076\000\069\000\070\000\000\000\000\000\000\000\000\000\050\000\
\050\000\103\000\000\000\071\000\072\000\000\000\073\000\000\000\
\000\000\074\000\075\000\076\000\069\000\070\000\000\000\000\000\
\000\000\000\000\077\000\078\000\104\000\000\000\071\000\072\000\
\000\000\073\000\000\000\000\000\074\000\075\000\076\000\069\000\
\070\000\000\000\000\000\000\000\000\000\077\000\078\000\000\000\
\107\000\071\000\072\000\012\000\073\000\000\000\000\000\074\000\
\075\000\076\000\069\000\070\000\000\000\000\000\000\000\012\000\
\077\000\078\000\012\000\000\000\071\000\072\000\000\000\073\000\
\069\000\070\000\074\000\075\000\076\000\028\000\000\000\000\000\
\028\000\000\000\000\000\077\000\078\000\073\000\000\000\000\000\
\074\000\075\000\076\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\033\000\033\000\033\000\000\000\077\000\078\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\033\000\033\000\077\000\078\000\033\000\033\000\
\033\000\033\000\033\000\000\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\031\000\003\000\032\000\000\000\004\000\005\000\006\000\007\000\
\008\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\033\000\034\000\035\000\000\000\000\000\036\000\037\000\000\000\
\003\000\038\000\000\000\004\000\005\000\006\000\007\000\008\000\
\039\000\009\000\040\000\041\000\042\000\043\000\044\000\005\000\
\005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\005\000\005\000\032\000\000\000\005\000\005\000\005\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
\003\000\038\000\000\000\004\000\005\000\006\000\007\000\008\000\
\039\000\009\000\040\000\041\000\042\000\043\000\044\000"

let yycheck = "\031\000\
\009\001\032\000\008\001\034\000\010\001\001\000\008\001\000\000\
\001\001\002\001\003\001\010\001\021\001\042\001\010\001\047\000\
\010\001\010\001\011\001\012\001\013\001\052\000\015\001\054\000\
\030\001\018\001\019\001\020\001\030\001\001\001\011\001\030\001\
\063\000\064\000\030\001\010\001\030\001\009\001\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\001\001\002\001\003\001\007\001\013\000\046\001\047\001\016\000\
\043\001\044\001\001\001\012\001\013\001\009\001\015\001\002\001\
\003\001\018\001\019\001\020\001\001\001\009\001\001\001\103\000\
\001\001\002\001\003\001\001\001\107\000\010\001\011\001\111\000\
\030\001\113\000\033\001\012\001\013\001\008\001\015\001\119\000\
\001\001\018\001\019\001\020\001\008\001\046\001\047\001\033\001\
\011\001\010\001\011\001\012\001\013\001\011\001\015\001\001\001\
\010\001\018\001\019\001\020\001\043\001\009\001\001\001\022\001\
\010\001\011\001\012\001\013\001\044\001\046\001\047\001\010\001\
\011\001\012\001\013\001\007\001\015\001\008\001\001\001\018\001\
\019\001\020\001\010\001\026\001\008\001\046\001\047\001\010\001\
\011\001\012\001\013\001\007\001\015\001\001\001\000\000\018\001\
\019\001\020\001\010\001\033\001\001\001\030\001\010\001\011\001\
\012\001\013\001\033\001\046\001\047\001\010\001\011\001\012\001\
\013\001\033\001\015\001\010\001\001\001\018\001\019\001\020\001\
\030\000\065\000\084\000\046\001\047\001\010\001\011\001\012\001\
\013\001\017\000\015\001\085\000\024\000\018\001\019\001\020\001\
\107\000\255\255\001\001\255\255\255\255\255\255\255\255\255\255\
\255\255\046\001\047\001\010\001\011\001\012\001\013\001\255\255\
\015\001\255\255\255\255\018\001\019\001\020\001\002\001\003\001\
\255\255\046\001\047\001\255\255\255\255\255\255\010\001\255\255\
\012\001\013\001\255\255\015\001\255\255\255\255\018\001\019\001\
\020\001\002\001\003\001\255\255\255\255\255\255\255\255\046\001\
\047\001\010\001\255\255\012\001\013\001\255\255\015\001\255\255\
\255\255\018\001\019\001\020\001\002\001\003\001\255\255\255\255\
\255\255\255\255\046\001\047\001\010\001\255\255\012\001\013\001\
\255\255\015\001\255\255\255\255\018\001\019\001\020\001\002\001\
\003\001\255\255\255\255\255\255\255\255\046\001\047\001\255\255\
\011\001\012\001\013\001\001\000\015\001\255\255\255\255\018\001\
\019\001\020\001\002\001\003\001\255\255\255\255\255\255\013\000\
\046\001\047\001\016\000\255\255\012\001\013\001\255\255\015\001\
\002\001\003\001\018\001\019\001\020\001\027\000\255\255\255\255\
\030\000\255\255\255\255\046\001\047\001\015\001\255\255\255\255\
\018\001\019\001\020\001\255\255\255\255\255\255\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\046\001\047\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\023\001\024\001\025\001\046\001\047\001\028\001\029\001\
\030\001\031\001\032\001\255\255\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\007\001\031\001\009\001\255\255\034\001\035\001\036\001\037\001\
\038\001\255\255\040\001\255\255\255\255\255\255\255\255\255\255\
\023\001\024\001\025\001\255\255\255\255\028\001\029\001\255\255\
\031\001\032\001\255\255\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\009\001\255\255\028\001\029\001\030\001\255\255\
\032\001\255\255\255\255\255\255\255\255\255\255\255\255\039\001\
\255\255\041\001\042\001\043\001\044\001\045\001\255\255\255\255\
\031\001\032\001\255\255\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\045\001"

let yynames_const = "\
  SEMI\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  AND\000\
  OR\000\
  NOT\000\
  EQ\000\
  LEQ\000\
  GEQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  ASSIGN\000\
  COLON\000\
  WHILE\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  EOF\000\
  LAMBDA\000\
  BREAK\000\
  CONTINUE\000\
  ARROW\000\
  LAMB\000\
  LBRACK\000\
  RBRACK\000\
  STRINGARRAY\000\
  INTARRAY\000\
  BOOL\000\
  INT\000\
  STRING\000\
  NONE\000\
  FILE\000\
  "

let yynames_block = "\
  BLIT\000\
  ID\000\
  STRINGLIT\000\
  LITERAL\000\
  FILELIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
            ( _1)
# 369 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( ([], [])               )
# 375 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 383 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 38 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 391 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
              ( [] )
# 397 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 43 "parser.mly"
                           (  _1 :: _3 )
# 405 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
         ( (_1, _2) )
# 413 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
          ( Int   )
# 419 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
          ( Bool  )
# 425 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
           ( String )
# 431 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
         ( Lamb )
# 437 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
         ( File )
# 443 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
             ( IntArray )
# 449 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                (StringArray )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 60 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals= _3;
      locals = _6;
      body= _7
    }
  )
# 473 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
  ( [] )
# 479 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 72 "parser.mly"
                (_1)
# 486 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
            ( [_1] )
# 493 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 76 "parser.mly"
                                (_1::_3)
# 501 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
  ( [] )
# 507 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 80 "parser.mly"
             (_1)
# 514 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
          ( [_1] )
# 521 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 84 "parser.mly"
                           (_1::_3)
# 529 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
              ( [] )
# 535 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 89 "parser.mly"
                 ( _1 )
# 542 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 92 "parser.mly"
        ( [_1] )
# 549 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 93 "parser.mly"
                             ( _1::_3 )
# 557 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                ( [] )
# 563 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 97 "parser.mly"
                    ( _1::_2 )
# 571 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 100 "parser.mly"
                                                                                          ( Expr _1             )
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 101 "parser.mly"
                                                                                          ( Block _2            )
# 585 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 594 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 103 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 104 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 610 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 105 "parser.mly"
                                                                                          ( Return _2           )
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                                                                                          ( Break               )
# 623 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                                                                                          ( Continue            )
# 629 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 111 "parser.mly"
                                  ( BoolLit _1            )
# 636 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
                                  ( Literal _1            )
# 643 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
                                  ( StringLit _1          )
# 650 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
                                  ( FileLit _1            )
# 657 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "parser.mly"
                                  ( Id _1                 )
# 664 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 116 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 672 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 117 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 680 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 118 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 688 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 119 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 696 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 120 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 704 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 121 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 712 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 122 "parser.mly"
                                  ( Binop (_1, Leq, _3)   )
# 720 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 123 "parser.mly"
                                  ( Binop (_1, Geq, _3)   )
# 728 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 124 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 736 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 125 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 744 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 126 "parser.mly"
                                  ( Assign (_1, _3)       )
# 752 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 127 "parser.mly"
                                  ( _2                    )
# 759 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
                                  ( None                  )
# 765 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 129 "parser.mly"
                                  ( Call (_1, _3)  )
# 773 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 131 "parser.mly"
                                                        ( Lamb (
    {
      rtyp=_3;
      lambname="";
      formals=_1;
      body=_6;
    })
  )
# 789 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'int_array) in
    Obj.repr(
# 139 "parser.mly"
                                  ( IntArray _2           )
# 796 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string_array) in
    Obj.repr(
# 140 "parser.mly"
                                  ( StringArray _2        )
# 803 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
              ( [] )
# 809 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 145 "parser.mly"
         ( _1 )
# 816 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 148 "parser.mly"
             ( [_1] )
# 823 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 149 "parser.mly"
                         ( _1::_3 )
# 831 "parser.ml"
               : 'args))
(* Entry prog_rules *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog_rules (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
