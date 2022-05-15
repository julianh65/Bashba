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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 53 "parser.ml"
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
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\004\000\009\000\
\009\000\010\000\010\000\011\000\011\000\012\000\012\000\007\000\
\007\000\013\000\013\000\008\000\008\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\008\000\000\000\
\001\000\001\000\003\000\000\000\001\000\001\000\003\000\000\000\
\001\000\001\000\003\000\000\000\002\000\002\000\003\000\011\000\
\007\000\005\000\003\000\002\000\002\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\004\000\007\000\
\003\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\011\000\014\000\013\000\009\000\008\000\010\000\
\012\000\063\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\004\000\007\000\003\000\000\000\000\000\025\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\054\000\038\000\
\000\000\040\000\039\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\000\000\000\000\000\000\036\000\037\000\000\000\
\000\000\000\000\017\000\000\000\021\000\000\000\000\000\000\000\
\015\000\029\000\030\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\000\053\000\000\000\
\035\000\000\000\000\000\000\000\058\000\057\000\000\000\000\000\
\060\000\000\000\000\000\042\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\023\000\000\000\055\000\000\000\034\000\000\000\062\000\000\000\
\000\000\000\000\000\000\056\000\000\000\000\000\000\000\032\000"

let yydgoto = "\002\000\
\010\000\011\000\021\000\013\000\029\000\014\000\044\000\045\000\
\058\000\059\000\060\000\061\000\023\000\046\000\047\000\088\000\
\089\000"

let yysindex = "\002\000\
\057\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\028\255\057\000\221\254\000\000\057\000\
\057\000\000\000\000\000\000\000\019\255\014\255\000\000\057\000\
\044\255\000\000\057\000\069\255\080\000\057\000\080\000\135\000\
\068\255\135\000\073\255\074\255\077\255\013\255\000\000\000\000\
\248\254\000\000\000\000\050\255\080\255\080\000\047\255\000\000\
\090\255\187\255\135\000\071\255\135\000\000\000\000\000\089\255\
\094\255\075\255\000\000\076\255\000\000\135\000\135\000\057\000\
\000\000\000\000\000\000\135\000\135\000\135\000\135\000\135\000\
\135\000\135\000\135\000\135\000\135\000\000\000\000\000\206\255\
\000\000\227\255\063\255\078\255\000\000\000\000\246\255\097\255\
\000\000\009\000\099\255\000\000\000\000\023\000\023\000\066\255\
\066\255\066\255\066\255\066\255\066\255\080\000\120\255\000\000\
\000\000\135\000\000\000\125\255\000\000\080\000\000\000\080\000\
\127\255\126\255\111\255\000\000\132\255\080\000\133\255\000\000"

let yyrindex = "\000\000\
\140\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\140\000\000\000\000\000\140\000\
\134\255\000\000\000\000\000\000\001\255\000\000\000\000\000\000\
\000\000\000\000\118\000\000\000\254\254\118\000\254\254\112\255\
\000\000\112\255\000\000\000\000\000\000\110\255\000\000\000\000\
\007\255\000\000\000\000\000\000\000\000\004\255\000\000\000\000\
\000\000\000\000\112\255\000\000\112\255\000\000\000\000\121\255\
\131\255\000\000\000\000\000\000\000\000\006\255\112\255\000\000\
\000\000\000\000\000\000\112\255\112\255\112\255\112\255\112\255\
\112\255\112\255\112\255\112\255\112\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\143\255\000\000\
\000\000\053\255\000\000\000\000\000\000\243\255\047\000\084\255\
\100\255\113\255\137\255\150\255\166\255\004\255\000\000\000\000\
\000\000\112\255\000\000\000\000\000\000\254\254\000\000\025\255\
\000\000\000\000\042\000\000\000\000\000\254\254\000\000\000\000"

let yygindex = "\000\000\
\000\000\045\000\017\001\000\000\136\000\107\000\155\000\225\255\
\000\000\090\000\000\000\091\000\150\000\000\000\226\255\000\000\
\074\000"

let yytablesize = 435
let yytable = "\049\000\
\062\000\050\000\001\000\052\000\015\000\028\000\019\000\041\000\
\041\000\041\000\026\000\028\000\063\000\028\000\066\000\059\000\
\041\000\041\000\041\000\041\000\080\000\041\000\082\000\025\000\
\041\000\041\000\041\000\024\000\016\000\024\000\026\000\087\000\
\090\000\024\000\028\000\024\000\017\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\067\000\
\068\000\069\000\027\000\041\000\041\000\052\000\024\000\056\000\
\057\000\018\000\070\000\071\000\020\000\072\000\052\000\052\000\
\073\000\074\000\075\000\068\000\069\000\030\000\109\000\081\000\
\068\000\069\000\054\000\087\000\051\000\055\000\113\000\064\000\
\114\000\053\000\070\000\071\000\044\000\072\000\119\000\065\000\
\073\000\074\000\075\000\076\000\077\000\044\000\044\000\044\000\
\044\000\078\000\044\000\083\000\045\000\044\000\044\000\044\000\
\084\000\056\000\107\000\085\000\086\000\045\000\045\000\045\000\
\045\000\047\000\045\000\076\000\077\000\045\000\045\000\045\000\
\108\000\057\000\047\000\047\000\047\000\047\000\110\000\047\000\
\044\000\044\000\047\000\047\000\047\000\112\000\115\000\116\000\
\117\000\046\000\118\000\002\000\120\000\024\000\016\000\024\000\
\045\000\045\000\046\000\046\000\046\000\046\000\048\000\046\000\
\061\000\018\000\046\000\046\000\046\000\047\000\047\000\048\000\
\048\000\048\000\048\000\022\000\048\000\048\000\049\000\048\000\
\048\000\048\000\091\000\022\000\104\000\026\000\105\000\049\000\
\049\000\049\000\049\000\111\000\049\000\046\000\046\000\049\000\
\049\000\049\000\000\000\000\000\068\000\069\000\000\000\000\000\
\000\000\000\000\048\000\048\000\079\000\000\000\070\000\071\000\
\000\000\072\000\000\000\000\000\073\000\074\000\075\000\068\000\
\069\000\000\000\049\000\049\000\000\000\000\000\000\000\102\000\
\000\000\070\000\071\000\000\000\072\000\000\000\000\000\073\000\
\074\000\075\000\000\000\000\000\068\000\069\000\000\000\076\000\
\077\000\000\000\000\000\000\000\103\000\000\000\070\000\071\000\
\000\000\072\000\000\000\050\000\073\000\074\000\075\000\068\000\
\069\000\000\000\076\000\077\000\050\000\050\000\050\000\050\000\
\106\000\070\000\071\000\000\000\072\000\000\000\000\000\073\000\
\074\000\075\000\068\000\069\000\000\000\000\000\000\000\076\000\
\077\000\012\000\000\000\000\000\070\000\071\000\000\000\072\000\
\068\000\069\000\073\000\074\000\075\000\012\000\000\000\000\000\
\012\000\000\000\076\000\077\000\000\000\072\000\000\000\000\000\
\073\000\074\000\075\000\028\000\000\000\000\000\028\000\051\000\
\033\000\033\000\033\000\033\000\000\000\076\000\077\000\000\000\
\051\000\051\000\051\000\051\000\000\000\000\000\000\000\000\000\
\033\000\033\000\033\000\076\000\077\000\033\000\033\000\033\000\
\033\000\033\000\000\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\031\000\003\000\
\032\000\000\000\004\000\005\000\006\000\007\000\008\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\033\000\034\000\
\035\000\000\000\000\000\036\000\037\000\000\000\003\000\038\000\
\000\000\004\000\005\000\006\000\007\000\008\000\039\000\009\000\
\040\000\041\000\042\000\043\000\005\000\005\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\005\000\005\000\032\000\
\000\000\005\000\005\000\005\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\000\000\000\005\000\005\000\
\005\000\005\000\000\000\000\000\000\000\003\000\038\000\000\000\
\004\000\005\000\006\000\007\000\008\000\039\000\009\000\040\000\
\041\000\042\000\043\000"

let yycheck = "\031\000\
\009\001\032\000\001\000\034\000\000\000\008\001\042\001\001\001\
\002\001\003\001\010\001\008\001\021\001\010\001\046\000\010\001\
\010\001\011\001\012\001\013\001\051\000\015\001\053\000\010\001\
\018\001\019\001\020\001\030\001\001\001\011\001\030\001\062\000\
\063\000\030\001\010\001\030\001\009\001\068\000\069\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\001\001\
\002\001\003\001\007\001\045\001\046\001\001\001\030\001\043\001\
\044\001\013\000\012\001\013\001\016\000\015\001\010\001\011\001\
\018\001\019\001\020\001\002\001\003\001\001\001\102\000\001\001\
\002\001\003\001\001\001\106\000\009\001\001\001\110\000\030\001\
\112\000\009\001\012\001\013\001\001\001\015\001\118\000\008\001\
\018\001\019\001\020\001\045\001\046\001\010\001\011\001\012\001\
\013\001\008\001\015\001\011\001\001\001\018\001\019\001\020\001\
\011\001\043\001\010\001\033\001\033\001\010\001\011\001\012\001\
\013\001\001\001\015\001\045\001\046\001\018\001\019\001\020\001\
\022\001\044\001\010\001\011\001\012\001\013\001\007\001\015\001\
\045\001\046\001\018\001\019\001\020\001\009\001\008\001\010\001\
\026\001\001\001\007\001\000\000\008\001\030\001\033\001\010\001\
\045\001\046\001\010\001\011\001\012\001\013\001\001\001\015\001\
\010\001\033\001\018\001\019\001\020\001\045\001\046\001\010\001\
\011\001\012\001\013\001\033\001\015\001\030\000\001\001\018\001\
\019\001\020\001\064\000\017\000\083\000\024\000\084\000\010\001\
\011\001\012\001\013\001\106\000\015\001\045\001\046\001\018\001\
\019\001\020\001\255\255\255\255\002\001\003\001\255\255\255\255\
\255\255\255\255\045\001\046\001\010\001\255\255\012\001\013\001\
\255\255\015\001\255\255\255\255\018\001\019\001\020\001\002\001\
\003\001\255\255\045\001\046\001\255\255\255\255\255\255\010\001\
\255\255\012\001\013\001\255\255\015\001\255\255\255\255\018\001\
\019\001\020\001\255\255\255\255\002\001\003\001\255\255\045\001\
\046\001\255\255\255\255\255\255\010\001\255\255\012\001\013\001\
\255\255\015\001\255\255\001\001\018\001\019\001\020\001\002\001\
\003\001\255\255\045\001\046\001\010\001\011\001\012\001\013\001\
\011\001\012\001\013\001\255\255\015\001\255\255\255\255\018\001\
\019\001\020\001\002\001\003\001\255\255\255\255\255\255\045\001\
\046\001\001\000\255\255\255\255\012\001\013\001\255\255\015\001\
\002\001\003\001\018\001\019\001\020\001\013\000\255\255\255\255\
\016\000\255\255\045\001\046\001\255\255\015\001\255\255\255\255\
\018\001\019\001\020\001\027\000\255\255\255\255\030\000\001\001\
\007\001\008\001\009\001\010\001\255\255\045\001\046\001\255\255\
\010\001\011\001\012\001\013\001\255\255\255\255\255\255\255\255\
\023\001\024\001\025\001\045\001\046\001\028\001\029\001\030\001\
\031\001\032\001\255\255\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001\042\001\043\001\044\001\007\001\031\001\
\009\001\255\255\034\001\035\001\036\001\037\001\038\001\255\255\
\040\001\255\255\255\255\255\255\255\255\255\255\023\001\024\001\
\025\001\255\255\255\255\028\001\029\001\255\255\031\001\032\001\
\255\255\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\044\001\007\001\008\001\009\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\023\001\024\001\025\001\009\001\
\255\255\028\001\029\001\030\001\255\255\032\001\255\255\255\255\
\255\255\255\255\255\255\255\255\039\001\255\255\041\001\042\001\
\043\001\044\001\255\255\255\255\255\255\031\001\032\001\255\255\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001"

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
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "parser.mly"
            ( _1)
# 359 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                 ( ([], [])               )
# 365 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 36 "parser.mly"
                    ( ((_1 :: fst _3), snd _3) )
# 373 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 37 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 381 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
              ( [] )
# 387 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 42 "parser.mly"
                           (  _1 :: _3 )
# 395 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
         ( (_1, _2) )
# 403 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
          ( Int   )
# 409 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
          ( Bool  )
# 415 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
           ( String )
# 421 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
         ( Lamb )
# 427 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
         ( File )
# 433 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
             ( IntArray )
# 439 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                (StringArray )
# 445 "parser.ml"
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
      locals = _6;
      body= _7
    }
  )
# 463 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
  ( [] )
# 469 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 71 "parser.mly"
                (_1)
# 476 "parser.ml"
               : 'string_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
            ( [_1] )
# 483 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'string_list) in
    Obj.repr(
# 75 "parser.mly"
                                (_1::_3)
# 491 "parser.ml"
               : 'string_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
  ( [] )
# 497 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 79 "parser.mly"
             (_1)
# 504 "parser.ml"
               : 'int_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "parser.mly"
          ( [_1] )
# 511 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'int_list) in
    Obj.repr(
# 83 "parser.mly"
                           (_1::_3)
# 519 "parser.ml"
               : 'int_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
              ( [] )
# 525 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 88 "parser.mly"
                 ( _1 )
# 532 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 91 "parser.mly"
        ( [_1] )
# 539 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 92 "parser.mly"
                             ( _1::_3 )
# 547 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                ( [] )
# 553 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 96 "parser.mly"
                    ( _1::_2 )
# 561 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 99 "parser.mly"
                                                                                          ( Expr _1             )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 100 "parser.mly"
                                                                                          ( Block _2            )
# 575 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt_list) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 101 "parser.mly"
                                                                                          ( IfElse(_3, _6, _10) )
# 584 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 102 "parser.mly"
                                                                                          ( If(_3, _6)          )
# 592 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 103 "parser.mly"
                                                                                          ( While (_3, _5)      )
# 600 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 104 "parser.mly"
                                                                                          ( Return _2           )
# 607 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                                                                                          ( Break               )
# 613 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                                                                                          ( Continue            )
# 619 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 110 "parser.mly"
                                  ( BoolLit _1            )
# 626 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
                                  ( Literal _1            )
# 633 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
                                  ( StringLit _1          )
# 640 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
                                  ( Id _1                 )
# 647 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 114 "parser.mly"
                                  ( Binop (_1, Add, _3)   )
# 655 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 115 "parser.mly"
                                  ( Binop (_1, Sub, _3)   )
# 663 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 116 "parser.mly"
                                  ( Binop (_1, Equal, _3) )
# 671 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 117 "parser.mly"
                                  ( Binop (_1, Neq, _3)   )
# 679 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 118 "parser.mly"
                                  ( Binop (_1, Less, _3)  )
# 687 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 119 "parser.mly"
                                  ( Binop (_1, Great, _3) )
# 695 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 120 "parser.mly"
                                  ( Binop (_1, Leq, _3)   )
# 703 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 121 "parser.mly"
                                  ( Binop (_1, Geq, _3)   )
# 711 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 122 "parser.mly"
                                  ( Binop (_1, And, _3)   )
# 719 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 123 "parser.mly"
                                  ( Binop (_1, Or, _3)    )
# 727 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 124 "parser.mly"
                                  ( Assign (_1, _3)       )
# 735 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 125 "parser.mly"
                                  ( _2                    )
# 742 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                  ( None                  )
# 748 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 127 "parser.mly"
                                  ( Call (_1, _3)  )
# 756 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'formals_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 129 "parser.mly"
                                                        ( Lamb (
    {
      rtyp=_3;
      lambname="";
      formals=_1;
      body=_6;
    })
  )
# 772 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'int_array) in
    Obj.repr(
# 137 "parser.mly"
                                  ( IntArray _2           )
# 779 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'string_array) in
    Obj.repr(
# 138 "parser.mly"
                                  ( StringArray _2        )
# 786 "parser.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
              ( [] )
# 792 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 143 "parser.mly"
         ( _1 )
# 799 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 146 "parser.mly"
             ( [_1] )
# 806 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 147 "parser.mly"
                         ( _1::_3 )
# 814 "parser.ml"
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
