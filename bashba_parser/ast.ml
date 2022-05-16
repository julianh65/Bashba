type bop = Add | Sub | Times | Divide | Mod | Equal | Neq | Leq | Geq | Less | Great | And | Or

type typ = Int | Bool | String | Lamb | None | File | IntArray | StringArray

type bind = typ * string

type expr =
  | Literal of int
  | BoolLit of bool
  | StringLit of string
  | FileLit of string
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Lamb of lamb_def
  | IntArray of int list
  | StringArray of string list
  | None
and stmt =
  | Block of stmt list
  | Expr of expr
  | IfElse of expr * stmt list * stmt list
  | If of expr * stmt list
  | While of expr * stmt list
  | Break
  | Continue
  | Return of expr
and lamb_def = {
  rtyp: typ;
  lambname : string;
  formals: bind list;
  body: stmt list;
}

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}
open Printf
type program = bind list * func_def list

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | String -> "String"
  | Lamb -> "lamb"
  | None -> "None"
  | File -> "File"
<<<<<<< HEAD
  | IntArray -> "int Array"
  | StringArray -> "String Array"
=======
  | IntArray -> "int[]"
  | StringArray -> "String[]"
>>>>>>> 9aa6ff1b6f19ac2170d9661c8bf54c8e5064cee6

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Leq -> "<="
  | Geq -> ">="
  | Less -> "<"
  | Great -> ">"
  | And -> "&&"
  | Or -> "||"
let rec string_of_expr = function
  Literal(l) -> string_of_int l
| BoolLit(true) -> "true"
| BoolLit(false) -> "false"
| StringLit(s) -> s
| FileLit(s) -> s
| Id(s) -> s
| Binop(e1, o, e2) ->
  string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
| Assign(v, e) -> v ^ " = " ^ string_of_expr e
| Call(s, l) -> "(" ^ s ^ ")\n"
| IntArray(s) -> "[" ^ (String.concat ", " (List.map string_of_int s)) ^ "]"
| StringArray(s) -> "[" ^ (String.concat ", " s) ^ "]"
| Lamb(s) -> String.concat "" (List.map string_of_bind s.formals) ^ "->" ^
  string_of_typ s.rtyp ^ " : " ^ "(" ^ 
  String.concat "" (List.map string_of_stmt s.body) ^ ")"
| None -> "None"

and string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | IfElse(e, b1, b2) ->  "if (" ^ string_of_expr e ^ ") {\n" ^
                      String.concat "" (List.map string_of_stmt b1) ^ "} else {\n" ^ 
                      String.concat "" (List.map string_of_stmt b2)
  | If(e, b1) -> "if (" ^ string_of_expr e ^ ") {\n" ^ String.concat "" (List.map string_of_stmt b1) ^ "\n}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat "" (List.map string_of_stmt s)
  | Break -> "Break\n"
  | Continue -> "Continue\n"
  | Return(e) -> "return " ^ string_of_expr e ^ "\n"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | String -> "String"
  | Lamb -> "lamb"
  | File -> "File"
  | IntArray -> "int[]"
  | StringArray -> "String[]"
  | None -> "None"

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_func func = 
  "" ^ (string_of_typ func.rtyp) ^ " " ^ func.fname ^ "(" ^
  String.concat "," (List.map string_of_bind func.formals) ^ ") {\n" ^
  String.concat "" (List.map string_of_vdecl func.locals) ^ 
  String.concat "" (List.map string_of_stmt func.body) ^
  "}\n"

let string_of_program prog =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl (fst prog)) ^
  String.concat "" (List.map string_of_func (snd prog)) ^
  "\n"
