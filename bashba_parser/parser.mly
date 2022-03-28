 
%{
open Ast
%}

%token PLUS MINUS TIMES DIVIDE MOD LBRACE RBRACE LPAREN RPAREN
%token COMMA SEMI AND OR NOT EQ LTEQ GTEQ NEQ GT LT ASSIGN 
%token WHILE RETURN IF ELSE EOF
%token BOOL INT
%token <bool> BLIT
%token <string> IF
%token <string> ID
%token <int> LITERAL

%start prog_rules 
%type <Ast.prog> prog_rules

%right ASSIGN
%left OR AND NOT
%left EQ NEQ LT GT LTEQ GTEQ 
%left PLUS MINUS 
%left TIMES DIVIDE MOD

%%

prog_rules:
  vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule vdecl_list_rule  { $1 :: $2 }

vdecl_rule:
  typ_rule ID SEMI { ($1, $2) }

typ_rule:
  INT       { Int  }
  | BOOL    { Bool }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | LITERAL                       { Literal $1            }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule GT expr_rule        { Binop ($1, Great, $3) }
  | expr_rule LTEQ expr_rule      { Binop ($1, Leq, $3)  }
  | expr_rule GTEQ expr_rule      { Binop ($1, Geq, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
