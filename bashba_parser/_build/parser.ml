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
  | BOOL
  | INT
  | STRING
  | NONE
  | LBRACK
  | RBRACK
  | STRINGARRAY
  | INTARRAY
  | BLIT of (bool)
  | ID of (string)
  | STRINGLIT of (string)
  | LITERAL of (int)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 52 "parser.ml"
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
  288 (* BOOL *);
  289 (* INT *);
  290 (* STRING *);
  291 (* NONE *);
  292 (* LBRACK *);
  293 (* RBRACK *);
  294 (* STRINGARRAY *);
  295 (* INTARRAY *);
    0|]

let yytransl_block = [|
  296 (* BLIT *);
  297 (* ID *);
  298 (* STRINGLIT *);
  299 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\006\000\006\000\003\000\
\007\000\007\000\007\000\007\000\007\000\007\000\004\000\005\000\
\010\000\010\000\011\000\011\000\013\000\013\000\014\000\014\000\
\008\000\008\000\015\000\015\000\009\000\009\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\017\000\017\000\018\000\018\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\002\000\000\000\003\000\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\008\000\009\000\
\000\000\001\000\001\000\003\000\000\000\001\000\001\000\003\000\
\000\000\001\000\001\000\003\000\000\000\002\000\002\000\003\000\
\011\000\007\000\005\000\003\000\002\000\002\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\004\000\
\003\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\010\000\009\000\011\000\014\000\013\000\
\063\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\004\000\005\000\008\000\003\000\000\000\000\000\
\026\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\000\000\039\000\000\000\041\000\
\040\000\000\000\000\000\000\000\000\000\007\000\000\000\000\000\
\000\000\000\000\000\000\037\000\038\000\000\000\018\000\000\000\
\000\000\022\000\000\000\000\000\015\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\032\000\054\000\000\000\036\000\000\000\058\000\
\000\000\057\000\000\000\000\000\060\000\000\000\043\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\000\000\000\000\020\000\024\000\000\000\056\000\035\000\
\000\000\062\000\000\000\000\000\000\000\000\000\000\000\033\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\012\000\013\000\034\000\014\000\024\000\
\050\000\062\000\063\000\051\000\065\000\066\000\025\000\052\000\
\092\000\093\000"

let yysindex = "\007\000\
\075\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\004\255\075\000\075\000\232\254\000\000\075\000\
\075\000\075\000\000\000\000\000\000\000\000\000\009\255\012\255\
\000\000\250\254\075\000\038\255\075\000\000\000\075\000\027\255\
\049\255\143\000\042\255\075\000\143\000\155\000\043\255\155\000\
\050\255\061\255\065\255\000\000\155\000\000\000\254\254\000\000\
\000\000\059\255\189\255\143\000\143\000\000\000\069\255\228\255\
\155\000\209\255\155\000\000\000\000\000\046\255\000\000\247\255\
\048\255\000\000\155\000\155\000\000\000\000\000\155\000\155\000\
\155\000\155\000\155\000\155\000\155\000\155\000\155\000\155\000\
\000\000\058\255\000\000\000\000\010\000\000\000\029\000\000\000\
\155\000\000\000\048\000\076\255\000\000\067\000\000\000\000\000\
\081\000\081\000\031\255\031\255\031\255\031\255\031\255\031\255\
\000\000\143\000\080\255\000\000\000\000\155\000\000\000\000\000\
\143\000\000\000\084\255\062\255\093\255\143\000\099\255\000\000"

let yyrindex = "\000\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\091\000\091\000\000\000\000\000\091\000\
\096\255\085\255\000\000\000\000\000\000\000\000\252\254\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\120\000\000\000\
\000\000\106\255\000\000\120\000\106\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\082\255\000\000\045\255\000\000\
\000\000\000\000\000\000\249\254\115\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\095\255\
\000\000\000\000\123\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\124\255\000\000\000\000\136\255\000\000\000\000\
\017\255\168\255\060\255\083\255\098\255\111\255\139\255\152\255\
\000\000\249\254\000\000\000\000\000\000\000\000\000\000\000\000\
\106\255\000\000\000\000\095\000\000\000\106\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\254\255\208\000\000\000\000\000\100\000\109\000\121\000\
\219\255\000\000\052\000\220\255\000\000\055\000\118\000\000\000\
\000\000\050\000"

let yytablesize = 454
let yytable = "\055\000\
\029\000\056\000\029\000\058\000\016\000\027\000\067\000\001\000\
\064\000\019\000\020\000\015\000\017\000\022\000\081\000\082\000\
\021\000\051\000\068\000\027\000\085\000\028\000\087\000\029\000\
\018\000\027\000\051\000\051\000\051\000\051\000\091\000\094\000\
\071\000\072\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\031\000\042\000\042\000\042\000\
\035\000\036\000\053\000\057\000\064\000\051\000\042\000\042\000\
\042\000\042\000\059\000\042\000\045\000\060\000\042\000\042\000\
\042\000\061\000\069\000\105\000\112\000\045\000\045\000\045\000\
\045\000\091\000\045\000\115\000\083\000\045\000\045\000\045\000\
\119\000\042\000\088\000\046\000\090\000\111\000\113\000\117\000\
\042\000\042\000\002\000\116\000\046\000\046\000\046\000\046\000\
\045\000\046\000\048\000\118\000\046\000\046\000\046\000\045\000\
\045\000\025\000\120\000\048\000\048\000\048\000\048\000\047\000\
\048\000\029\000\025\000\048\000\048\000\048\000\017\000\046\000\
\047\000\047\000\047\000\047\000\029\000\047\000\046\000\046\000\
\047\000\047\000\047\000\019\000\059\000\061\000\048\000\054\000\
\053\000\032\000\026\000\049\000\108\000\048\000\048\000\109\000\
\030\000\053\000\053\000\047\000\049\000\049\000\049\000\049\000\
\050\000\049\000\047\000\047\000\049\000\049\000\049\000\114\000\
\000\000\050\000\050\000\050\000\050\000\000\000\050\000\000\000\
\052\000\050\000\050\000\050\000\053\000\000\000\000\000\049\000\
\000\000\052\000\052\000\052\000\052\000\000\000\049\000\049\000\
\000\000\000\000\000\000\000\000\050\000\070\000\071\000\072\000\
\000\000\000\000\000\000\050\000\050\000\000\000\000\000\000\000\
\073\000\074\000\000\000\075\000\052\000\000\000\076\000\077\000\
\078\000\086\000\071\000\072\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\073\000\074\000\000\000\075\000\
\023\000\023\000\076\000\077\000\078\000\071\000\072\000\000\000\
\079\000\080\000\023\000\000\000\000\000\084\000\033\000\073\000\
\074\000\000\000\075\000\033\000\000\000\076\000\077\000\078\000\
\071\000\072\000\000\000\000\000\079\000\080\000\000\000\000\000\
\000\000\089\000\073\000\074\000\000\000\075\000\000\000\000\000\
\076\000\077\000\078\000\071\000\072\000\000\000\000\000\079\000\
\080\000\000\000\000\000\106\000\000\000\073\000\074\000\000\000\
\075\000\000\000\000\000\076\000\077\000\078\000\071\000\072\000\
\000\000\000\000\079\000\080\000\000\000\000\000\107\000\000\000\
\073\000\074\000\000\000\075\000\000\000\000\000\076\000\077\000\
\078\000\071\000\072\000\000\000\000\000\079\000\080\000\000\000\
\000\000\000\000\110\000\073\000\074\000\000\000\075\000\000\000\
\000\000\076\000\077\000\078\000\071\000\072\000\000\000\000\000\
\079\000\080\000\000\000\000\000\000\000\000\000\073\000\074\000\
\000\000\075\000\071\000\072\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\079\000\080\000\000\000\000\000\075\000\
\000\000\000\000\076\000\077\000\078\000\034\000\034\000\034\000\
\034\000\003\000\004\000\005\000\006\000\000\000\079\000\080\000\
\007\000\008\000\000\000\000\000\000\000\034\000\034\000\034\000\
\000\000\000\000\034\000\034\000\079\000\080\000\006\000\006\000\
\006\000\034\000\034\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\000\000\000\000\000\000\000\000\006\000\006\000\
\006\000\000\000\000\000\006\000\006\000\037\000\000\000\038\000\
\000\000\000\000\006\000\006\000\000\000\000\000\000\000\006\000\
\006\000\006\000\006\000\038\000\000\000\039\000\040\000\041\000\
\000\000\000\000\042\000\043\000\000\000\000\000\000\000\000\000\
\000\000\044\000\045\000\000\000\000\000\000\000\046\000\047\000\
\048\000\049\000\000\000\000\000\000\000\044\000\045\000\000\000\
\000\000\000\000\046\000\047\000\048\000\049\000"

let yycheck = "\037\000\
\008\001\038\000\010\001\040\000\001\001\010\001\009\001\001\000\
\045\000\012\000\013\000\000\000\009\001\016\000\052\000\053\000\
\041\001\001\001\021\001\011\001\057\000\010\001\059\000\030\001\
\021\001\030\001\010\001\011\001\012\001\013\001\067\000\068\000\
\002\001\003\001\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\007\001\001\001\002\001\003\001\
\022\001\001\001\009\001\009\001\089\000\037\001\010\001\011\001\
\012\001\013\001\009\001\015\001\001\001\001\001\018\001\019\001\
\020\001\001\001\008\001\010\001\106\000\010\001\011\001\012\001\
\013\001\110\000\015\001\113\000\008\001\018\001\019\001\020\001\
\118\000\037\001\037\001\001\001\037\001\010\001\007\001\026\001\
\044\001\045\001\000\000\008\001\010\001\011\001\012\001\013\001\
\037\001\015\001\001\001\007\001\018\001\019\001\020\001\044\001\
\045\001\010\001\008\001\010\001\011\001\012\001\013\001\001\001\
\015\001\008\001\030\001\018\001\019\001\020\001\037\001\037\001\
\010\001\011\001\012\001\013\001\010\001\015\001\044\001\045\001\
\018\001\019\001\020\001\037\001\010\001\010\001\037\001\036\000\
\001\001\029\000\018\000\001\001\089\000\044\001\045\001\089\000\
\027\000\010\001\011\001\037\001\010\001\011\001\012\001\013\001\
\001\001\015\001\044\001\045\001\018\001\019\001\020\001\110\000\
\255\255\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\001\001\018\001\019\001\020\001\037\001\255\255\255\255\037\001\
\255\255\010\001\011\001\012\001\013\001\255\255\044\001\045\001\
\255\255\255\255\255\255\255\255\037\001\001\001\002\001\003\001\
\255\255\255\255\255\255\044\001\045\001\255\255\255\255\255\255\
\012\001\013\001\255\255\015\001\037\001\255\255\018\001\019\001\
\020\001\001\001\002\001\003\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\012\001\013\001\255\255\015\001\
\017\000\018\000\018\001\019\001\020\001\002\001\003\001\255\255\
\044\001\045\001\027\000\255\255\255\255\010\001\031\000\012\001\
\013\001\255\255\015\001\036\000\255\255\018\001\019\001\020\001\
\002\001\003\001\255\255\255\255\044\001\045\001\255\255\255\255\
\255\255\011\001\012\001\013\001\255\255\015\001\255\255\255\255\
\018\001\019\001\020\001\002\001\003\001\255\255\255\255\044\001\
\045\001\255\255\255\255\010\001\255\255\012\001\013\001\255\255\
\015\001\255\255\255\255\018\001\019\001\020\001\002\001\003\001\
\255\255\255\255\044\001\045\001\255\255\255\255\010\001\255\255\
\012\001\013\001\255\255\015\001\255\255\255\255\018\001\019\001\
\020\001\002\001\003\001\255\255\255\255\044\001\045\001\255\255\
\255\255\255\255\011\001\012\001\013\001\255\255\015\001\255\255\
\255\255\018\001\019\001\020\001\002\001\003\001\255\255\255\255\
\044\001\045\001\255\255\255\255\255\255\255\255\012\001\013\001\
\255\255\015\001\002\001\003\001\018\001\019\001\020\001\255\255\
\255\255\255\255\255\255\044\001\045\001\255\255\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\007\001\008\001\009\001\
\010\001\031\001\032\001\033\001\034\001\255\255\044\001\045\001\
\038\001\039\001\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\044\001\045\001\007\001\008\001\
\009\001\035\001\036\001\255\255\255\255\255\255\040\001\041\001\
\042\001\043\001\255\255\255\255\255\255\255\255\023\001\024\001\
\025\001\255\255\255\255\028\001\029\001\007\001\255\255\009\001\
\255\255\255\255\035\001\036\001\255\255\255\255\255\255\040\001\
\041\001\042\001\043\001\009\001\255\255\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\255\255\255\255\255\255\255\255\
\255\255\035\001\036\001\255\255\255\255\255\255\040\001\041\001\
\042\001\043\001\255\255\255\255\255\255\035\001\036\001\255\255\
\255\255\255\255\040\001\041\001\042\001\043\001"

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
  BOOL\000\
  INT\000\
  STRING\000\
  NONE\000\
  LBRACK\000\
  RBRACK\000\
  STRINGARRAY\000\
  INTARRAY\000\
  "

let yynames_block = "\
  BLIT\000\
  ID\000\
  STRINGLIT\000\
  LITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "parser.mly"
            ( _1)
# 360 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                 ( ([], [])               )
# 366 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 36 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 374 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 382 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'lambdadecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 38 "parser.mly"
                    ( (fst _2, (_1 :: snd _2)) )
# 390 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
              ( [] )
# 396 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 42 "parser.mly"
                           (  _1 :: _3 )
# 404 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
         ( (_1, _2) )
# 412 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
          ( Int   )
# 418 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
          ( Bool  )
# 424 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
           ( String )
# 430 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
         ( Lamb )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
             ( IntArray )
# 442 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                (StringArray )
# 448 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 59 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals= _3;
      locals= _6;
      body= _7
    }
  )
# 466 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 73 "parser.mly"
  (
    {
      rtyp= _5;
      fname= snd _1;
      formals= _3;
      locals= [];
      body= _8
    }
  )
# 484 "parser.ml"
               : 'lambdadecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
  ( [] )
# 490 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 85 "parser.mly"
                (_1)
# 497 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 88 "parser.mly"
            ( [_1] )
# 504 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 89 "parser.mly"
                                (_1::_3)
# 512 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
  ( [] )
# 518 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 93 "parser.mly"
             (_1)
# 525 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "parser.mly"
            ( [_1] )
# 532 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 97 "parser.mly"
                             (_1::_3)
# 540 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
              ( [] )
# 546 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 102 "parser.mly"
                 ( _1 )
# 553 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 105 "parser.mly"
        ( [_1] )
# 560 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 106 "parser.mly"
                             ( _1::_3 )
# 568 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                ( [] )
# 574 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 110 "parser.mly"
                    ( _1::_2 )
# 582 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 113 "parser.mly"
                                                                                          ( Expr _1             )
# 589 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 114 "parser.mly"
                                                                                          ( Block _2            )
# 596 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 115 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 605 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 116 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 613 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 117 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 621 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 118 "parser.mly"
                                                                                          ( Return _2           )
# 628 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                                                                          ( Break               )
# 634 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
                                                                                          ( Continue            )
# 640 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 124 "parser.mly"
                                  ( BoolLit _1            )
# 647 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 125 "parser.mly"
                                  ( Literal _1            )
# 654 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                                  ( StringLit _1          )
# 661 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 127 "parser.mly"
                                  ( Id _1                 )
# 668 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 128 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 676 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 129 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 684 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 130 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 692 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 131 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 700 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 132 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 708 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 133 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 716 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 134 "parser.mly"
                                  ( Binop (_1, Leq, _3)   )
# 724 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 135 "parser.mly"
                                  ( Binop (_1, Geq, _3)   )
# 732 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 136 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 740 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 137 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 748 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 138 "parser.mly"
                                  ( Assign (_1, _3)       )
# 756 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 139 "parser.mly"
                                  ( _2                    )
# 763 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
                                  ( None                  )
# 769 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 141 "parser.mly"
                                  ( Call (_1, _3)         )
# 777 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'int_array) in
    Obj.repr(
# 142 "parser.mly"
                                  ( IntArray _2           )
# 784 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string_array) in
    Obj.repr(
# 143 "parser.mly"
                                  ( StringArray _2        )
# 791 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
              ( [] )
# 797 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 148 "parser.mly"
         ( _1 )
# 804 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 151 "parser.mly"
             ( [_1] )
# 811 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 152 "parser.mly"
                         ( _1::_3 )
# 819 "parser.ml"
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
