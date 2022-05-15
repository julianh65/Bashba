// compile with ocamlyacc parser.mly

%{
open Ast
%}

%token SEMI PLUS MINUS TIMES DIVIDE MOD LBRACE RBRACE LPAREN RPAREN
%token COMMA AND OR NOT EQ LEQ GEQ NEQ GT LT ASSIGN COLON
%token WHILE RETURN IF ELSE EOF LAMBDA BREAK CONTINUE ARROW LAMB
%token BOOL INT STRING NONE FILE
%token <bool> BLIT
%token <string> ID
%token <string> STRINGLIT
%token <int> LITERAL
%token <string> FILELIT

%start prog_rules 
%type <Ast.program> prog_rules

%right ASSIGN
%left OR AND NOT
%left EQ NEQ LT GT LTEQ GTEQ 
%left PLUS MINUS 
%left TIMES DIVIDE MOD

%%

/* add function declarations*/
prog_rules:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }
//  | lambdadecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

vdecl:
  typ ID { ($1, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | STRING { String }
  | LAMB { Lamb }
  | FILE { File }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals= $3;
      locals = $6;
      body= $7
    }
  }


/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr_rule SEMI                                                                        { Expr $1             }
  | LBRACE stmt_list RBRACE                                                               { Block $2            }
  | IF LPAREN expr_rule RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE       { IfElse($3, $6, $10) }
  | IF LPAREN expr_rule RPAREN LBRACE stmt_list RBRACE                                    { If($3, $6)          }
  | WHILE LPAREN expr_rule RPAREN stmt_list                                               { While ($3, $5)      }
  | RETURN expr_rule SEMI                                                                 { Return $2           }
  | BREAK SEMI                                                                            { Break               }
  | CONTINUE SEMI                                                                         { Continue            }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | LITERAL                       { Literal $1            }
  | STRINGLIT                     { StringLit $1          }
  | FILELIT                       { FileLit $1            }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule GT expr_rule        { Binop ($1, Great, $3) }
  | expr_rule LTEQ expr_rule      { Binop ($1, Leq, $3)   }
  | expr_rule GTEQ expr_rule      { Binop ($1, Geq, $3)   }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
  | NONE                          { None                  }
  | ID LPAREN args_opt RPAREN     { Call ($1, $3)  }
  /* x = int x, int y -> int : (body) */
  | formals_opt ARROW typ COLON LPAREN stmt_list RPAREN { Lamb (
    {
      rtyp=$3;
      lambname="";
      formals=$1;
      body=$6;
    })
  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr_rule  { [$1] }
  | expr_rule COMMA args { $1::$3 }