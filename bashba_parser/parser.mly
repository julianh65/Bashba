 
%{
open Ast
%}

%token PLUS MINUS TIMES DIVIDE MOD LBRACE RBRACE LPAREN RPAREN
%token COMMA SEMI AND OR NOT EQ LTEQ GTEQ NEQ GT LT ASSIGN 
%token WHILE RETURN IF ELSE EOF
%token <bool> BLIT
%token <string> IF
%token <int> LIT

%start prog_rules 
%type <Ast.prog> prog_rules

%right ASSIGN
%left OR AND NOT
%left EQ NEQ LT GT LTEQ GTEQ 
%left PLUS MINUS 
%left TIMES DIVIDE MOD

%%

