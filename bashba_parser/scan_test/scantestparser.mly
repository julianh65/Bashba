/* Ocamlyacc parser for NanoC */

%{
open Scantestast
%}

/* TODO: String literals */

%token PLUS MINUS TIMES DIVIDE MOD LBRACE RBRACE LPAREN RPAREN
%token COMMA SEMI AND OR NOT EQ LEQ GEQ NEQ GT LT ASSIGN COLON
%token WHILE RETURN IF ELSE LAMBDA BREAK CONTINUE ARROW LAMB
%token BOOL INT STRING NONE
%token <bool> BLIT
%token <string> ID
%token <int> LITERAL
%token <string> STRINGLIT
%token EOF

/*
%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT BOOL
%token RETURN COMMA
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF
*/

%start program
%type <Scantestast.tokenseq> program

%%

program:
  tokens EOF { $1}

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | TIMES { "TIMES" }
  | DIVIDE { "DIVIDE " }
  | MOD { "MOD" }
  | NOT { "NOT" }
  | LEQ { "LEQ" }
  | GEQ { "GEQ" }
  | GT { "GT" }
  | ASSIGN { "ASSIGN" }
  | COLON { "COLON" }
  | LAMBDA { "LAMB" }
  | BREAK { "BREAK" }
  | CONTINUE { "CONTINUE" }
  | ARROW { "ARROW" }
  | LAMBDA { "LAMBDA" }
  | LAMB { "LAMB" }
  | STRING { "STRING" }
  | NONE { "NONE" }
  | SEMI  {  "SEMI" }
  | LPAREN { "LPAREN" }
  | RPAREN { "RPAREN" }
  | LBRACE { "LBRACE" }
  | RBRACE { "RBRACE" }
  | COMMA { "COMMA" }
  | PLUS { "PLUS" }
  | MINUS { "MINUS" }
  | ASSIGN { "ASSIGN" }
  | EQ { "EQ" }
  | NEQ { "NEQ" }
  | LT { "LT" }
  | AND { "AND" }
  | OR { "OR" }
  | IF { "IF" }
  | ELSE { "ELSE" }
  | WHILE { "WHILE" }
  | RETURN { "RETURN" }
  | INT { "INT" }
  | BOOL { "BOOL" }
  | BLIT { "BOOL: " ^ string_of_bool $1 }
  | LITERAL { "LITERAL: " ^ string_of_int $1 }
  | STRINGLIT { "STRINGLIT: " ^ $1 }
  | ID { "ID: " ^ $1 }
  
