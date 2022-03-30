%{
open Ast
%}

%token PLUS MINUS TIMES DIVIDE MOD LBRACE RBRACE LPAREN RPAREN
%token COMMA SEMI AND OR NOT EQ LTEQ GTEQ NEQ GT LT ASSIGN COLON
%token WHILE RETURN IF ELSE EOF LAMBDA BREAK CONTINUE DEF
%token BOOL INT STRING NONE
%token <bool> BLIT
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

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | STRING { String }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals= $3;
      locals= $6;
      body= $7
    }
  }
  | LAMBDA vdecl COLON LPAREN vdecl_list stmt_list RPAREN
  { {
    rypt = fst $3;
    fname = "";
    formals = $3;
    locals = $7;
    body = $8
  }}

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }
  | fdecl { [$1] }
  | fdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | RETURN expr SEMI                        { Return $2      }
  | BREAK                                   { Break }
  | CONTINUE                                { Continue }

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
  | NONE                        { None                  }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | LAMBDA vdecl COLON LPAREN vdecl_list stmt_list RPAREN  { Lambda ($2, $5, $6)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }