type bop = Add | Sub | Times | Divide | Mod | Equal | Neq | Leq | Geq | Less | Great | And | Or

type typ = Int | Bool | String | Lamb | IntArray | StringArray | None

type bind = typ * string

type expr =
  | Literal of int
  | BoolLit of bool
  | StringLit of string
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Lamb of lamb_def
  | IntArray of expr list
  | StringArray of expr list
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
  fname: string;
  formals: bind list;
  locals: bind list;
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
| Id(s) -> s
| Binop(e1, o, e2) ->
  string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
| Assign(v, e) -> v ^ " = " ^ string_of_expr e
| Call(s, l) -> "(" ^ s ^ ")\n"
| Lamb(s) -> "lambda func"
| IntArray(s) -> "int array"
| StringArray(s) -> "string array"
| None -> "None"

let rec string_of_stmt = function
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
