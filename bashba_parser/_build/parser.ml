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
  | BLIT of (bool)
  | ID of (string)
  | STRINGLIT of (string)
  | LITERAL of (int)

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 48 "parser.ml"
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
    0|]

let yytransl_block = [|
  292 (* BLIT *);
  293 (* ID *);
  294 (* STRINGLIT *);
  295 (* LITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\004\000\007\000\007\000\009\000\009\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\012\000\012\000\013\000\
\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\003\000\011\000\007\000\005\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\009\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\001\000\004\000\000\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\009\000\008\000\010\000\050\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\004\000\007\000\
\003\000\000\000\000\000\014\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\026\000\000\000\044\000\027\000\000\000\029\000\028\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\018\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\043\000\000\000\024\000\000\000\000\000\000\000\
\000\000\047\000\000\000\032\000\033\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\023\000\000\000\000\000\049\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\000\000\021\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\026\000\011\000\019\000\041\000\
\020\000\042\000\043\000\073\000\074\000"

let yysindex = "\010\000\
\047\255\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\051\255\047\255\242\254\000\000\047\255\047\255\000\000\000\000\
\000\000\018\255\032\255\000\000\047\255\036\255\000\000\047\255\
\048\255\126\000\047\255\126\000\083\000\061\255\083\000\064\255\
\000\000\000\000\047\255\000\000\000\000\249\254\000\000\000\000\
\077\255\126\000\074\255\000\000\080\255\228\255\083\000\098\255\
\083\000\060\255\083\000\083\000\000\000\000\000\000\000\083\000\
\083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
\083\000\000\000\000\000\252\255\000\000\015\000\047\255\029\000\
\081\255\000\000\048\000\000\000\000\000\056\000\056\000\045\255\
\045\255\045\255\045\255\045\255\045\255\126\000\088\255\075\255\
\083\000\000\000\000\000\126\000\087\255\000\000\090\255\047\255\
\076\255\126\000\096\255\094\255\126\000\000\000\097\255\000\000"

let yyrindex = "\000\000\
\106\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\106\000\000\000\000\000\106\000\099\255\000\000\000\000\
\000\000\251\254\000\000\000\000\000\000\000\000\000\000\151\000\
\000\000\100\255\103\000\100\255\000\000\000\000\000\000\000\000\
\000\000\000\000\089\255\000\000\000\000\043\255\000\000\000\000\
\000\000\020\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\110\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\115\255\
\000\000\000\000\040\255\000\000\000\000\007\255\056\255\111\255\
\135\255\148\255\172\255\185\255\209\255\020\255\000\000\000\000\
\000\000\000\000\000\000\100\255\000\000\000\000\000\000\174\000\
\070\000\117\255\000\000\000\000\100\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\242\255\000\000\232\255\036\000\093\000\229\255\
\111\000\000\000\231\255\000\000\044\000"

let yytablesize = 469
let yytable = "\018\000\
\045\000\051\000\044\000\046\000\015\000\048\000\018\000\040\000\
\015\000\025\000\001\000\017\000\025\000\052\000\054\000\012\000\
\040\000\040\000\040\000\040\000\018\000\068\000\016\000\070\000\
\015\000\072\000\075\000\017\000\021\000\017\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
\042\000\022\000\024\000\030\000\030\000\030\000\056\000\057\000\
\027\000\042\000\042\000\013\000\030\000\030\000\030\000\030\000\
\041\000\030\000\091\000\014\000\030\000\030\000\030\000\072\000\
\095\000\041\000\041\000\041\000\041\000\047\000\100\000\098\000\
\049\000\103\000\055\000\056\000\057\000\003\000\004\000\005\000\
\006\000\025\000\030\000\030\000\053\000\058\000\059\000\066\000\
\060\000\071\000\090\000\061\000\062\000\063\000\092\000\096\000\
\093\000\097\000\069\000\056\000\057\000\099\000\101\000\102\000\
\104\000\002\000\088\000\017\000\013\000\058\000\059\000\034\000\
\060\000\064\000\065\000\061\000\062\000\063\000\013\000\046\000\
\034\000\034\000\034\000\034\000\048\000\034\000\017\000\050\000\
\034\000\034\000\034\000\023\000\094\000\000\000\000\000\035\000\
\000\000\064\000\065\000\000\000\000\000\000\000\000\000\000\000\
\035\000\035\000\035\000\035\000\037\000\035\000\034\000\034\000\
\035\000\035\000\035\000\000\000\000\000\037\000\037\000\037\000\
\037\000\000\000\037\000\000\000\000\000\037\000\037\000\037\000\
\000\000\000\000\000\000\000\000\036\000\000\000\035\000\035\000\
\000\000\000\000\000\000\000\000\000\000\036\000\036\000\036\000\
\036\000\038\000\036\000\037\000\037\000\036\000\036\000\036\000\
\000\000\000\000\038\000\038\000\038\000\038\000\000\000\038\000\
\000\000\000\000\038\000\038\000\038\000\000\000\000\000\000\000\
\000\000\039\000\000\000\036\000\036\000\000\000\000\000\000\000\
\000\000\000\000\039\000\039\000\039\000\039\000\000\000\039\000\
\038\000\038\000\039\000\039\000\039\000\056\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\067\000\000\000\058\000\
\059\000\000\000\060\000\000\000\000\000\061\000\062\000\063\000\
\039\000\039\000\000\000\000\000\000\000\056\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\086\000\000\000\058\000\
\059\000\000\000\060\000\064\000\065\000\061\000\062\000\063\000\
\056\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
\087\000\000\000\058\000\059\000\000\000\060\000\056\000\057\000\
\061\000\062\000\063\000\064\000\065\000\000\000\000\000\089\000\
\058\000\059\000\000\000\060\000\000\000\000\000\061\000\062\000\
\063\000\056\000\057\000\000\000\000\000\000\000\064\000\065\000\
\000\000\056\000\057\000\058\000\059\000\000\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\065\000\060\000\000\000\
\000\000\061\000\062\000\063\000\022\000\022\000\022\000\022\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\064\000\
\065\000\000\000\000\000\029\000\022\000\022\000\022\000\064\000\
\065\000\022\000\022\000\000\000\022\000\000\000\000\000\000\000\
\022\000\022\000\022\000\022\000\022\000\005\000\005\000\005\000\
\005\000\035\000\000\000\000\000\000\000\036\000\037\000\038\000\
\039\000\040\000\000\000\000\000\000\000\005\000\005\000\005\000\
\000\000\000\000\005\000\005\000\028\000\000\000\029\000\000\000\
\000\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\030\000\031\000\032\000\000\000\
\000\000\033\000\034\000\000\000\035\000\005\000\005\000\005\000\
\036\000\037\000\038\000\039\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\005\000\005\000\
\000\000\000\000\005\000\005\000\005\000\000\000\005\000\005\000\
\000\000\005\000\005\000\005\000\005\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\005\000\005\000\000\000\
\000\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
\005\000\005\000\005\000\005\000\005\000"

let yycheck = "\014\000\
\028\000\009\001\027\000\029\000\010\001\031\000\021\000\001\001\
\010\000\024\000\001\000\013\000\027\000\021\001\042\000\000\000\
\010\001\011\001\012\001\013\001\035\000\047\000\037\001\049\000\
\030\001\051\000\052\000\008\001\011\001\010\001\056\000\057\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\001\001\010\001\007\001\001\001\002\001\003\001\002\001\003\001\
\001\001\010\001\011\001\001\001\010\001\011\001\012\001\013\001\
\001\001\015\001\086\000\009\001\018\001\019\001\020\001\089\000\
\092\000\010\001\011\001\012\001\013\001\009\001\098\000\096\000\
\009\001\101\000\001\001\002\001\003\001\031\001\032\001\033\001\
\034\001\096\000\040\001\041\001\008\001\012\001\013\001\008\001\
\015\001\030\001\010\001\018\001\019\001\020\001\007\001\009\001\
\022\001\008\001\001\001\002\001\003\001\026\001\007\001\010\001\
\008\001\000\000\071\000\008\001\010\001\012\001\013\001\001\001\
\015\001\040\001\041\001\018\001\019\001\020\001\030\001\010\001\
\010\001\011\001\012\001\013\001\010\001\015\001\010\001\035\000\
\018\001\019\001\020\001\021\000\089\000\255\255\255\255\001\001\
\255\255\040\001\041\001\255\255\255\255\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001\001\001\015\001\040\001\041\001\
\018\001\019\001\020\001\255\255\255\255\010\001\011\001\012\001\
\013\001\255\255\015\001\255\255\255\255\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\001\001\255\255\040\001\041\001\
\255\255\255\255\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\001\001\015\001\040\001\041\001\018\001\019\001\020\001\
\255\255\255\255\010\001\011\001\012\001\013\001\255\255\015\001\
\255\255\255\255\018\001\019\001\020\001\255\255\255\255\255\255\
\255\255\001\001\255\255\040\001\041\001\255\255\255\255\255\255\
\255\255\255\255\010\001\011\001\012\001\013\001\255\255\015\001\
\040\001\041\001\018\001\019\001\020\001\002\001\003\001\255\255\
\255\255\255\255\255\255\255\255\255\255\010\001\255\255\012\001\
\013\001\255\255\015\001\255\255\255\255\018\001\019\001\020\001\
\040\001\041\001\255\255\255\255\255\255\002\001\003\001\255\255\
\255\255\255\255\255\255\255\255\255\255\010\001\255\255\012\001\
\013\001\255\255\015\001\040\001\041\001\018\001\019\001\020\001\
\002\001\003\001\255\255\255\255\255\255\255\255\255\255\255\255\
\010\001\255\255\012\001\013\001\255\255\015\001\002\001\003\001\
\018\001\019\001\020\001\040\001\041\001\255\255\255\255\011\001\
\012\001\013\001\255\255\015\001\255\255\255\255\018\001\019\001\
\020\001\002\001\003\001\255\255\255\255\255\255\040\001\041\001\
\255\255\002\001\003\001\012\001\013\001\255\255\015\001\255\255\
\255\255\018\001\019\001\020\001\040\001\041\001\015\001\255\255\
\255\255\018\001\019\001\020\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\040\001\
\041\001\255\255\255\255\009\001\023\001\024\001\025\001\040\001\
\041\001\028\001\029\001\255\255\031\001\255\255\255\255\255\255\
\035\001\036\001\037\001\038\001\039\001\007\001\008\001\009\001\
\010\001\031\001\255\255\255\255\255\255\035\001\036\001\037\001\
\038\001\039\001\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\007\001\255\255\009\001\255\255\
\255\255\035\001\036\001\037\001\038\001\039\001\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\024\001\025\001\255\255\
\255\255\028\001\029\001\255\255\031\001\007\001\008\001\009\001\
\035\001\036\001\037\001\038\001\039\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\007\001\255\255\009\001\010\001\
\255\255\035\001\036\001\037\001\038\001\039\001\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\024\001\025\001\255\255\
\255\255\028\001\029\001\255\255\255\255\255\255\255\255\255\255\
\035\001\036\001\037\001\038\001\039\001"

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
# 342 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                 ( ([], [])               )
# 348 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 356 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 34 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 364 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
              ( [] )
# 370 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 38 "parser.mly"
                           (  _1 :: _3 )
# 378 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
         ( (_1, _2) )
# 386 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
          ( Int   )
# 392 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
          ( Bool  )
# 398 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
           ( String )
# 404 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
         ( Lamb )
# 410 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 53 "parser.mly"
  (
    {
      rtyp=fst _1;
      fname=snd _1;
      formals= _3;
      locals= _6;
      body= _7
    }
  )
# 428 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
              ( [] )
# 434 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 66 "parser.mly"
                 ( _1 )
# 441 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 69 "parser.mly"
        ( [_1] )
# 448 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 70 "parser.mly"
                             ( _1::_3 )
# 456 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                ( [] )
# 462 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 74 "parser.mly"
                    ( _1::_2 )
# 470 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 77 "parser.mly"
                                                                                          ( Expr _1             )
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 78 "parser.mly"
                                                                                          ( Block _2            )
# 484 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 79 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 493 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 80 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 501 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 81 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 509 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 82 "parser.mly"
                                                                                          ( Return _2           )
# 516 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                                                                          ( Break               )
# 522 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                                                                          ( Continue            )
# 528 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 87 "parser.mly"
                                  ( BoolLit _1            )
# 535 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
                                  ( Literal _1            )
# 542 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                                  ( StringLit _1          )
# 549 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser.mly"
                                  ( Id _1                 )
# 556 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'typ) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 92 "parser.mly"
                                                                        ( Lamb ({
      rtyp = _4;
      formals = _2;
      locals = _7;
      body = _8;
    }) )
# 571 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 579 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 587 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 595 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 603 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 611 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 619 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 104 "parser.mly"
                                  ( Binop (_1, Leq, _3)  )
# 627 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 105 "parser.mly"
                                  ( Binop (_1, Geq, _3)  )
# 635 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 106 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 643 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 107 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 651 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 108 "parser.mly"
                                  ( Assign (_1, _3)       )
# 659 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 109 "parser.mly"
                                  ( _2                    )
# 666 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                                  ( None                  )
# 672 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 111 "parser.mly"
                                  ( Call (_1, _3)  )
# 680 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
              ( [] )
# 686 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 116 "parser.mly"
         ( _1 )
# 693 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 119 "parser.mly"
             ( [_1] )
# 700 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 120 "parser.mly"
                         ( _1::_3 )
# 708 "parser.ml"
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
