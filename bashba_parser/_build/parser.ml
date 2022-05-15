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
# 50 "parser.ml"
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
  292 (* FILE *);
    0|]

let yytransl_block = [|
  293 (* BLIT *);
  294 (* ID *);
  295 (* STRINGLIT *);
  296 (* LITERAL *);
  297 (* FILELIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\004\000\007\000\007\000\009\000\
\009\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\012\000\
\012\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\008\000\000\000\001\000\001\000\
\003\000\000\000\002\000\002\000\003\000\011\000\007\000\005\000\
\003\000\002\000\002\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\001\000\004\000\007\000\000\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\009\000\008\000\010\000\012\000\052\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\004\000\
\007\000\003\000\000\000\000\000\015\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\028\000\000\000\030\000\029\000\
\031\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\026\000\027\000\000\000\000\000\000\000\
\013\000\019\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\000\044\000\000\000\
\025\000\000\000\000\000\000\000\049\000\000\000\000\000\033\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\000\000\000\024\000\000\000\
\051\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
\000\000\022\000"

let yydgoto = "\002\000\
\008\000\009\000\019\000\011\000\027\000\012\000\042\000\043\000\
\021\000\044\000\045\000\076\000\077\000"

let yysindex = "\002\000\
\062\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\005\255\062\255\228\254\000\000\062\255\062\255\000\000\
\000\000\000\000\011\255\037\255\000\000\062\255\047\255\000\000\
\062\255\065\255\077\000\062\255\077\000\123\000\074\255\123\000\
\076\255\073\255\085\255\000\000\000\000\039\255\000\000\000\000\
\000\000\070\255\091\255\077\000\089\255\000\000\095\255\214\255\
\123\000\115\255\123\000\000\000\000\000\123\000\123\000\062\255\
\000\000\000\000\000\000\123\000\123\000\123\000\123\000\123\000\
\123\000\123\000\123\000\123\000\123\000\000\000\000\000\235\255\
\000\000\249\255\012\000\087\255\000\000\026\000\083\255\000\000\
\000\000\010\255\010\255\048\255\048\255\048\255\048\255\048\255\
\048\255\077\000\099\255\123\000\000\000\101\255\000\000\077\000\
\000\000\077\000\105\255\104\255\093\255\000\000\108\255\077\000\
\112\255\000\000"

let yyrindex = "\000\000\
\121\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\121\000\000\000\000\000\121\000\113\255\000\000\
\000\000\000\000\014\255\000\000\000\000\000\000\000\000\000\000\
\112\000\000\000\012\255\112\000\012\255\092\255\000\000\092\255\
\000\000\000\000\000\000\000\000\000\000\069\255\000\000\000\000\
\000\000\000\000\000\000\001\255\000\000\000\000\000\000\000\000\
\092\255\000\000\092\255\000\000\000\000\033\255\092\255\000\000\
\000\000\000\000\000\000\092\255\092\255\092\255\092\255\092\255\
\092\255\092\255\092\255\092\255\092\255\000\000\000\000\000\000\
\000\000\000\000\114\255\000\000\000\000\067\255\000\000\000\000\
\000\000\006\255\045\255\128\255\141\255\154\255\167\255\180\255\
\193\255\001\255\000\000\092\255\000\000\000\000\000\000\012\255\
\000\000\035\255\000\000\000\000\042\000\000\000\000\000\012\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\048\000\000\000\097\000\070\000\122\000\227\255\
\114\000\000\000\228\255\000\000\052\000"

let yytablesize = 420
let yytable = "\047\000\
\013\000\048\000\001\000\050\000\016\000\014\000\041\000\018\000\
\018\000\017\000\018\000\060\000\061\000\015\000\058\000\041\000\
\041\000\041\000\041\000\018\000\072\000\022\000\074\000\016\000\
\064\000\075\000\078\000\065\000\066\000\067\000\014\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\014\000\048\000\016\000\018\000\042\000\023\000\054\000\
\010\000\060\000\061\000\068\000\069\000\025\000\042\000\042\000\
\042\000\042\000\010\000\055\000\095\000\010\000\014\000\075\000\
\014\000\028\000\099\000\043\000\100\000\032\000\032\000\032\000\
\026\000\052\000\105\000\026\000\043\000\043\000\032\000\032\000\
\032\000\032\000\049\000\032\000\051\000\053\000\032\000\032\000\
\032\000\059\000\060\000\061\000\003\000\004\000\005\000\006\000\
\093\000\007\000\057\000\056\000\062\000\063\000\070\000\064\000\
\094\000\096\000\065\000\066\000\067\000\098\000\032\000\032\000\
\101\000\102\000\104\000\073\000\060\000\061\000\103\000\106\000\
\002\000\014\000\014\000\050\000\046\000\079\000\062\000\063\000\
\035\000\064\000\068\000\069\000\065\000\066\000\067\000\024\000\
\020\000\035\000\035\000\035\000\035\000\036\000\035\000\097\000\
\000\000\035\000\035\000\035\000\000\000\000\000\036\000\036\000\
\036\000\036\000\038\000\036\000\068\000\069\000\036\000\036\000\
\036\000\000\000\000\000\038\000\038\000\038\000\038\000\037\000\
\038\000\035\000\035\000\038\000\038\000\038\000\000\000\000\000\
\037\000\037\000\037\000\037\000\039\000\037\000\036\000\036\000\
\037\000\037\000\037\000\000\000\000\000\039\000\039\000\039\000\
\039\000\040\000\039\000\038\000\038\000\039\000\039\000\039\000\
\000\000\000\000\040\000\040\000\040\000\040\000\000\000\040\000\
\037\000\037\000\040\000\040\000\040\000\000\000\000\000\060\000\
\061\000\000\000\000\000\000\000\000\000\039\000\039\000\071\000\
\000\000\062\000\063\000\000\000\064\000\000\000\000\000\065\000\
\066\000\067\000\040\000\040\000\060\000\061\000\000\000\000\000\
\000\000\000\000\000\000\000\000\090\000\000\000\062\000\063\000\
\000\000\064\000\060\000\061\000\065\000\066\000\067\000\068\000\
\069\000\000\000\091\000\000\000\062\000\063\000\000\000\064\000\
\000\000\000\000\065\000\066\000\067\000\060\000\061\000\000\000\
\000\000\000\000\000\000\000\000\068\000\069\000\092\000\062\000\
\063\000\000\000\064\000\060\000\061\000\065\000\066\000\067\000\
\000\000\000\000\068\000\069\000\000\000\062\000\063\000\000\000\
\064\000\000\000\000\000\065\000\066\000\067\000\000\000\000\000\
\023\000\023\000\023\000\023\000\000\000\068\000\069\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\023\000\023\000\068\000\069\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\029\000\000\000\030\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\032\000\033\000\000\000\000\000\
\034\000\035\000\000\000\003\000\004\000\005\000\006\000\036\000\
\007\000\037\000\038\000\039\000\040\000\041\000\005\000\005\000\
\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\005\000\005\000\
\005\000\000\000\000\000\005\000\005\000\005\000\000\000\000\000\
\000\000\000\000\005\000\000\000\005\000\005\000\005\000\005\000\
\005\000\003\000\004\000\005\000\006\000\036\000\007\000\037\000\
\038\000\039\000\040\000\041\000"

let yycheck = "\029\000\
\000\000\030\000\001\000\032\000\011\000\001\001\001\001\014\000\
\008\001\038\001\010\001\002\001\003\001\009\001\044\000\010\001\
\011\001\012\001\013\001\008\001\049\000\011\001\051\000\010\001\
\015\001\054\000\055\000\018\001\019\001\020\001\030\001\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\030\001\010\001\030\001\010\001\001\001\010\001\009\001\
\001\000\002\001\003\001\042\001\043\001\007\001\010\001\011\001\
\012\001\013\001\011\000\021\001\090\000\014\000\030\001\092\000\
\030\001\001\001\096\000\001\001\098\000\001\001\002\001\003\001\
\025\000\001\001\104\000\028\000\010\001\011\001\010\001\011\001\
\012\001\013\001\009\001\015\001\009\001\001\001\018\001\019\001\
\020\001\001\001\002\001\003\001\031\001\032\001\033\001\034\001\
\010\001\036\001\008\001\030\001\012\001\013\001\008\001\015\001\
\022\001\007\001\018\001\019\001\020\001\009\001\042\001\043\001\
\008\001\010\001\007\001\001\001\002\001\003\001\026\001\008\001\
\000\000\030\001\010\001\010\001\028\000\056\000\012\001\013\001\
\001\001\015\001\042\001\043\001\018\001\019\001\020\001\022\000\
\015\000\010\001\011\001\012\001\013\001\001\001\015\001\092\000\
\255\255\018\001\019\001\020\001\255\255\255\255\010\001\011\001\
\012\001\013\001\001\001\015\001\042\001\043\001\018\001\019\001\
\020\001\255\255\255\255\010\001\011\001\012\001\013\001\001\001\
\015\001\042\001\043\001\018\001\019\001\020\001\255\255\255\255\
\010\001\011\001\012\001\013\001\001\001\015\001\042\001\043\001\
\018\001\019\001\020\001\255\255\255\255\010\001\011\001\012\001\
\013\001\001\001\015\001\042\001\043\001\018\001\019\001\020\001\
\255\255\255\255\010\001\011\001\012\001\013\001\255\255\015\001\
\042\001\043\001\018\001\019\001\020\001\255\255\255\255\002\001\
\003\001\255\255\255\255\255\255\255\255\042\001\043\001\010\001\
\255\255\012\001\013\001\255\255\015\001\255\255\255\255\018\001\
\019\001\020\001\042\001\043\001\002\001\003\001\255\255\255\255\
\255\255\255\255\255\255\255\255\010\001\255\255\012\001\013\001\
\255\255\015\001\002\001\003\001\018\001\019\001\020\001\042\001\
\043\001\255\255\010\001\255\255\012\001\013\001\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\002\001\003\001\255\255\
\255\255\255\255\255\255\255\255\042\001\043\001\011\001\012\001\
\013\001\255\255\015\001\002\001\003\001\018\001\019\001\020\001\
\255\255\255\255\042\001\043\001\255\255\012\001\013\001\255\255\
\015\001\255\255\255\255\018\001\019\001\020\001\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\042\001\043\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\023\001\024\001\025\001\042\001\043\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\007\001\255\255\009\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\023\001\024\001\025\001\255\255\255\255\
\028\001\029\001\255\255\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\041\001\007\001\008\001\
\009\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\009\001\255\255\255\255\023\001\024\001\
\025\001\255\255\255\255\028\001\029\001\030\001\255\255\255\255\
\255\255\255\255\035\001\255\255\037\001\038\001\039\001\040\001\
\041\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001"

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
# 30 "parser.mly"
            ( _1)
# 339 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
                 ( ([], [])               )
# 345 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 34 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 353 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 35 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 361 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
              ( [] )
# 367 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 40 "parser.mly"
                           (  _1 :: _3 )
# 375 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
         ( (_1, _2) )
# 383 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
          ( Int   )
# 389 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
          ( Bool  )
# 395 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
           ( String )
# 401 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
         ( Lamb )
# 407 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
         ( File )
# 413 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 55 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals= _3;
      locals = _6;
      body= _7
    }
  )
# 431 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
              ( [] )
# 437 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 69 "parser.mly"
                 ( _1 )
# 444 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 72 "parser.mly"
        ( [_1] )
# 451 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 73 "parser.mly"
                             ( _1::_3 )
# 459 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                ( [] )
# 465 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 77 "parser.mly"
                    ( _1::_2 )
# 473 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 80 "parser.mly"
                                                                                          ( Expr _1             )
# 480 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 81 "parser.mly"
                                                                                          ( Block _2            )
# 487 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 82 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 496 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 83 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 84 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 85 "parser.mly"
                                                                                          ( Return _2           )
# 519 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                                                                          ( Break               )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                                                                          ( Continue            )
# 531 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "parser.mly"
                                  ( BoolLit _1            )
# 538 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
                                  ( Literal _1            )
# 545 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                  ( StringLit _1          )
# 552 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "parser.mly"
                                  ( FileLit _1            )
# 559 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "parser.mly"
                                  ( Id _1                 )
# 566 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 574 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 582 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 590 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 598 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 606 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 614 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "parser.mly"
                                  ( Binop (_1, Leq, _3)   )
# 622 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Binop (_1, Geq, _3)   )
# 630 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 638 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 104 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 646 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 105 "parser.mly"
                                  ( Assign (_1, _3)       )
# 654 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 106 "parser.mly"
                                  ( _2                    )
# 661 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                                  ( None                  )
# 667 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 108 "parser.mly"
                                  ( Call (_1, _3)  )
# 675 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 110 "parser.mly"
                                                        ( Lamb (
    {
      rtyp=_3;
      lambname="";
      formals=_1;
      body=_6;
    })
  )
# 691 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
              ( [] )
# 697 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 122 "parser.mly"
         ( _1 )
# 704 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 125 "parser.mly"
             ( [_1] )
# 711 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 126 "parser.mly"
                         ( _1::_3 )
# 719 "parser.ml"
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
