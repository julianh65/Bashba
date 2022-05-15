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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 49 "parser.ml"
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
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\004\000\007\000\007\000\009\000\
\009\000\008\000\008\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\008\000\000\000\001\000\001\000\
\003\000\000\000\002\000\002\000\003\000\011\000\007\000\005\000\
\003\000\002\000\002\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\004\000\007\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\009\000\008\000\010\000\012\000\051\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\004\000\
\007\000\003\000\000\000\000\000\015\000\000\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\000\028\000\000\000\030\000\029\000\
\000\000\000\000\000\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\026\000\027\000\000\000\000\000\000\000\013\000\
\019\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\043\000\000\000\025\000\
\000\000\000\000\000\000\048\000\000\000\000\000\032\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\000\000\024\000\000\000\050\000\
\000\000\000\000\000\000\000\000\046\000\000\000\000\000\000\000\
\022\000"

let yydgoto = "\002\000\
\008\000\009\000\019\000\011\000\027\000\012\000\041\000\042\000\
\021\000\043\000\044\000\075\000\076\000"

let yysindex = "\044\000\
\061\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\000\004\255\061\255\027\255\000\000\061\255\061\255\000\000\
\000\000\000\000\056\255\063\255\000\000\061\255\069\255\000\000\
\061\255\076\255\085\000\061\255\085\000\064\000\073\255\064\000\
\075\255\084\255\095\255\000\000\000\000\254\254\000\000\000\000\
\072\255\090\255\085\000\088\255\000\000\091\255\226\255\064\000\
\113\255\064\000\000\000\000\000\064\000\064\000\061\255\000\000\
\000\000\000\000\064\000\064\000\064\000\064\000\064\000\064\000\
\064\000\064\000\064\000\064\000\000\000\000\000\245\255\000\000\
\003\000\022\000\094\255\000\000\036\000\083\255\000\000\000\000\
\009\255\009\255\007\255\007\255\007\255\007\255\007\255\007\255\
\085\000\104\255\064\000\000\000\103\255\000\000\085\000\000\000\
\085\000\105\255\107\255\092\255\000\000\112\255\085\000\114\255\
\000\000"

let yyrindex = "\000\000\
\120\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\120\000\000\000\000\000\120\000\111\255\000\000\
\000\000\000\000\011\255\000\000\000\000\000\000\000\000\000\000\
\119\000\000\000\000\255\119\000\000\255\093\255\000\000\093\255\
\000\000\000\000\000\000\000\000\000\000\068\255\000\000\000\000\
\000\000\000\000\249\254\000\000\000\000\000\000\000\000\093\255\
\000\000\093\255\000\000\000\000\032\255\093\255\000\000\000\000\
\000\000\000\000\093\255\093\255\093\255\093\255\093\255\093\255\
\093\255\093\255\093\255\093\255\000\000\000\000\000\000\000\000\
\000\000\124\255\000\000\000\000\046\255\000\000\000\000\000\000\
\005\255\042\255\126\255\146\255\159\255\179\255\192\255\212\255\
\249\254\000\000\093\255\000\000\000\000\000\000\000\255\000\000\
\034\255\000\000\000\000\051\000\000\000\000\000\000\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\035\000\047\000\000\000\096\000\080\000\125\000\227\255\
\121\000\000\000\228\255\000\000\057\000"

let yytablesize = 415
let yytable = "\046\000\
\018\000\047\000\018\000\049\000\014\000\040\000\053\000\018\000\
\059\000\060\000\059\000\060\000\015\000\057\000\040\000\040\000\
\040\000\040\000\054\000\071\000\016\000\073\000\014\000\063\000\
\074\000\077\000\064\000\065\000\066\000\014\000\079\000\080\000\
\081\000\082\000\083\000\084\000\085\000\086\000\087\000\088\000\
\016\000\047\000\041\000\018\000\001\000\016\000\042\000\010\000\
\018\000\067\000\068\000\041\000\041\000\041\000\041\000\042\000\
\042\000\010\000\013\000\094\000\010\000\014\000\074\000\014\000\
\017\000\098\000\022\000\099\000\031\000\031\000\031\000\026\000\
\023\000\104\000\026\000\025\000\028\000\031\000\031\000\031\000\
\031\000\048\000\031\000\050\000\051\000\031\000\031\000\031\000\
\058\000\059\000\060\000\003\000\004\000\005\000\006\000\052\000\
\007\000\056\000\069\000\061\000\062\000\055\000\063\000\092\000\
\093\000\064\000\065\000\066\000\031\000\031\000\095\000\097\000\
\100\000\072\000\059\000\060\000\101\000\102\000\103\000\002\000\
\014\000\105\000\014\000\045\000\061\000\062\000\034\000\063\000\
\067\000\068\000\064\000\065\000\066\000\049\000\078\000\034\000\
\034\000\034\000\034\000\020\000\034\000\000\000\024\000\034\000\
\034\000\034\000\035\000\096\000\000\000\000\000\000\000\000\000\
\000\000\067\000\068\000\035\000\035\000\035\000\035\000\037\000\
\035\000\000\000\000\000\035\000\035\000\035\000\034\000\034\000\
\037\000\037\000\037\000\037\000\000\000\037\000\000\000\000\000\
\037\000\037\000\037\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\035\000\035\000\036\000\036\000\036\000\036\000\
\038\000\036\000\000\000\000\000\036\000\036\000\036\000\037\000\
\037\000\038\000\038\000\038\000\038\000\000\000\038\000\000\000\
\000\000\038\000\038\000\038\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\039\000\039\000\039\000\
\039\000\000\000\039\000\059\000\060\000\039\000\039\000\039\000\
\038\000\038\000\000\000\070\000\000\000\061\000\062\000\000\000\
\063\000\000\000\000\000\064\000\065\000\066\000\059\000\060\000\
\000\000\000\000\000\000\000\000\039\000\039\000\089\000\000\000\
\061\000\062\000\000\000\063\000\059\000\060\000\064\000\065\000\
\066\000\000\000\067\000\068\000\090\000\000\000\061\000\062\000\
\000\000\063\000\000\000\000\000\064\000\065\000\066\000\059\000\
\060\000\000\000\000\000\000\000\000\000\067\000\068\000\000\000\
\091\000\061\000\062\000\000\000\063\000\059\000\060\000\064\000\
\065\000\066\000\000\000\067\000\068\000\000\000\000\000\061\000\
\062\000\000\000\063\000\000\000\000\000\064\000\065\000\066\000\
\000\000\023\000\023\000\023\000\023\000\000\000\067\000\068\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\023\000\023\000\023\000\067\000\068\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\029\000\000\000\030\000\003\000\004\000\
\005\000\006\000\036\000\007\000\037\000\038\000\039\000\040\000\
\000\000\000\000\000\000\031\000\032\000\033\000\000\000\000\000\
\034\000\035\000\000\000\003\000\004\000\005\000\006\000\036\000\
\007\000\037\000\038\000\039\000\040\000\005\000\005\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
\000\000\000\000\005\000\005\000\005\000\000\000\000\000\000\000\
\000\000\005\000\000\000\005\000\005\000\005\000\005\000"

let yycheck = "\029\000\
\008\001\030\000\010\001\032\000\001\001\001\001\009\001\008\001\
\002\001\003\001\002\001\003\001\009\001\043\000\010\001\011\001\
\012\001\013\001\021\001\048\000\010\001\050\000\030\001\015\001\
\053\000\054\000\018\001\019\001\020\001\030\001\059\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\030\001\010\001\001\001\010\001\001\000\011\000\001\001\001\000\
\014\000\041\001\042\001\010\001\011\001\012\001\013\001\010\001\
\011\001\011\000\000\000\089\000\014\000\030\001\091\000\030\001\
\038\001\095\000\011\001\097\000\001\001\002\001\003\001\025\000\
\010\001\103\000\028\000\007\001\001\001\010\001\011\001\012\001\
\013\001\009\001\015\001\009\001\001\001\018\001\019\001\020\001\
\001\001\002\001\003\001\031\001\032\001\033\001\034\001\001\001\
\036\001\008\001\008\001\012\001\013\001\030\001\015\001\010\001\
\022\001\018\001\019\001\020\001\041\001\042\001\007\001\009\001\
\008\001\001\001\002\001\003\001\010\001\026\001\007\001\000\000\
\010\001\008\001\030\001\028\000\012\001\013\001\001\001\015\001\
\041\001\042\001\018\001\019\001\020\001\010\001\055\000\010\001\
\011\001\012\001\013\001\015\000\015\001\255\255\022\000\018\001\
\019\001\020\001\001\001\091\000\255\255\255\255\255\255\255\255\
\255\255\041\001\042\001\010\001\011\001\012\001\013\001\001\001\
\015\001\255\255\255\255\018\001\019\001\020\001\041\001\042\001\
\010\001\011\001\012\001\013\001\255\255\015\001\255\255\255\255\
\018\001\019\001\020\001\001\001\255\255\255\255\255\255\255\255\
\255\255\255\255\041\001\042\001\010\001\011\001\012\001\013\001\
\001\001\015\001\255\255\255\255\018\001\019\001\020\001\041\001\
\042\001\010\001\011\001\012\001\013\001\255\255\015\001\255\255\
\255\255\018\001\019\001\020\001\001\001\255\255\255\255\255\255\
\255\255\255\255\255\255\041\001\042\001\010\001\011\001\012\001\
\013\001\255\255\015\001\002\001\003\001\018\001\019\001\020\001\
\041\001\042\001\255\255\010\001\255\255\012\001\013\001\255\255\
\015\001\255\255\255\255\018\001\019\001\020\001\002\001\003\001\
\255\255\255\255\255\255\255\255\041\001\042\001\010\001\255\255\
\012\001\013\001\255\255\015\001\002\001\003\001\018\001\019\001\
\020\001\255\255\041\001\042\001\010\001\255\255\012\001\013\001\
\255\255\015\001\255\255\255\255\018\001\019\001\020\001\002\001\
\003\001\255\255\255\255\255\255\255\255\041\001\042\001\255\255\
\011\001\012\001\013\001\255\255\015\001\002\001\003\001\018\001\
\019\001\020\001\255\255\041\001\042\001\255\255\255\255\012\001\
\013\001\255\255\015\001\255\255\255\255\018\001\019\001\020\001\
\255\255\007\001\008\001\009\001\010\001\255\255\041\001\042\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\009\001\023\001\024\001\025\001\041\001\042\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\007\001\255\255\009\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\255\255\255\255\255\255\023\001\024\001\025\001\255\255\255\255\
\028\001\029\001\255\255\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\007\001\008\001\009\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\030\001\255\255\255\255\255\255\
\255\255\035\001\255\255\037\001\038\001\039\001\040\001"

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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 29 "parser.mly"
            ( _1)
# 334 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                 ( ([], [])               )
# 340 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 348 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 34 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 356 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
              ( [] )
# 362 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 39 "parser.mly"
                           (  _1 :: _3 )
# 370 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
         ( (_1, _2) )
# 378 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
          ( Int   )
# 384 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
          ( Bool  )
# 390 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
           ( String )
# 396 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
         ( Lamb )
# 402 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
         ( File )
# 408 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 54 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals= _3;
      locals = _6;
      body= _7
    }
  )
# 426 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
              ( [] )
# 432 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 68 "parser.mly"
                 ( _1 )
# 439 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 71 "parser.mly"
        ( [_1] )
# 446 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 72 "parser.mly"
                             ( _1::_3 )
# 454 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
                ( [] )
# 460 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 76 "parser.mly"
                    ( _1::_2 )
# 468 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 79 "parser.mly"
                                                                                          ( Expr _1             )
# 475 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 80 "parser.mly"
                                                                                          ( Block _2            )
# 482 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 81 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 491 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 82 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 499 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 83 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 507 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 84 "parser.mly"
                                                                                          ( Return _2           )
# 514 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                                                                                          ( Break               )
# 520 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                                                                          ( Continue            )
# 526 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 89 "parser.mly"
                                  ( BoolLit _1            )
# 533 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 90 "parser.mly"
                                  ( Literal _1            )
# 540 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
                                  ( StringLit _1          )
# 547 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                                  ( Id _1                 )
# 554 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 93 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 562 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 94 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 570 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 578 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 586 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 594 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 602 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "parser.mly"
                                  ( Binop (_1, Leq, _3)   )
# 610 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "parser.mly"
                                  ( Binop (_1, Geq, _3)   )
# 618 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 626 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 634 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "parser.mly"
                                  ( Assign (_1, _3)       )
# 642 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 104 "parser.mly"
                                  ( _2                    )
# 649 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                  ( None                  )
# 655 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 106 "parser.mly"
                                  ( Call (_1, _3)  )
# 663 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 108 "parser.mly"
                                                        ( Lamb (
    {
      rtyp=_3;
      lambname="";
      formals=_1;
      body=_6;
    })
  )
# 679 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
              ( [] )
# 685 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 120 "parser.mly"
         ( _1 )
# 692 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 123 "parser.mly"
             ( [_1] )
# 699 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 124 "parser.mly"
                         ( _1::_3 )
# 707 "parser.ml"
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
