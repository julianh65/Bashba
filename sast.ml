open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SStringLit of string
  | SFileLit of string
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SLamb of slamb_def
  | SIntArray of sexpr list
  | SStringArray of sexpr list
  | SFileArray of sexpr list
  | SNone
and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIfElse of sexpr * sstmt list * sstmt list
  | SIf of sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SReturn of sexpr
  | SBreak
  | SContinue
and slamb_def = {
  srtyp: typ;
  slambname : string;
  sformals: bind list;
  sbody: sstmt list;
}

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SStringLit(s) -> "\"" ^ s ^ "\""
      | SFileLit(s) -> "\"" ^ s ^ "\""
      | SId(s) -> s
      | SIntArray (l) -> "[" ^ "intarr" ^ "]"
      | SStringArray (l) -> "[" ^ "strarr" ^ "]"
      | SFileArray (l) -> "[" ^ "filearr" ^ "]"
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SLamb(l) -> "lamb"
      | SNone -> "None"
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1) -> "if (" ^ string_of_sexpr e ^ ") {\n" ^ 
      String.concat "" (List.map string_of_sstmt s1) ^ "\n }\n"
  | SIfElse(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ") {\n" ^
                       String.concat "" (List.map string_of_sstmt s1) ^ "else{ \n" ^ 
                        String.concat "" (List.map string_of_sstmt s2) ^ "\n}\n"
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ 
      String.concat "" (List.map string_of_sstmt s)
  | SBreak -> "Break; \n"
  | SContinue -> "Continue; \n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
