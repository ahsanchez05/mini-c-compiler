(* Abstract syntax tree and pretty printer for the C compiler *)

open Printf


(** Binary operators *)
type binop =
  | Add | Sub | Mul | Div | Mod
  | Gt | Lt | Ge | Le | Eq | Ne
  | BAnd | BOr | LAnd | LOr

(** Expressions: variables, literals, binary ops and function calls *)
type expr = 
  | EVar of string
  | EInt of int
  | EChar of char
  | EBinOp of binop * expr * expr
  | ECall of string * expr list

(** Types *)
type ty =
  | TVoid
  | TInt
  | TChar

(** Statements: expressions, variable definitions/assignments, 
scopes (blocks), conditionals, loops, returns, and breaks *)
type stmt =
  | SExpr of expr
  | SVarDef of ty * string * expr
  | SVarAssign of string * expr
  | SScope of stmt list
  | SIf of expr * stmt * stmt option
  | SWhile of expr * stmt
  | SReturn of expr option
  | SBreak

(** Global declarations: function definitions and declarations *)
type global =
  | GFuncDef of ty * string * (ty * string) list * stmt
  | GFuncDecl of ty * string * (ty * string) list

(** Abstract syntax tree *)
type ast = global list



(**----------------Pretty printer for the abstract syntax tree----------------*)

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="
  | Eq -> "=="
  | Ne -> "!="
  | BAnd -> "&"
  | BOr -> "|"
  | LAnd -> "&&"
  | LOr -> "||"

let string_of_ty = function
  | TVoid -> "void"
  | TInt -> "int"
  | TChar -> "char"

(* Escape characters so printed chars stay parseable *)
let escape_char = function
  | '\'' -> "\\'"
  | '\"' -> "\\\""
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | c -> Char.escaped c

let rec pp_expr = function
  | EVar name -> sprintf "EVar(%s)" ("\"" ^ name ^ "\"")
  | EInt i -> sprintf "EInt(%d)" i
  | EChar c -> sprintf "EChar(%s)" ("'" ^ escape_char c ^ "'")
  | EBinOp (bop, o1, o2) -> sprintf "EBinOp(%s, %s, %s)" (string_of_binop bop) (pp_expr o1) (pp_expr o2)
  | ECall (name, args) -> 
    (* Print function calls as ECall(name, {arg1 arg2 ...}) *)
    let string_args = String.concat " " (List.map pp_expr args) in
    sprintf "ECall(%s, {%s})" ("\"" ^ name ^ "\"") string_args

let rec pp_stmt = function
  | SExpr expr -> "SExpr(" ^ pp_expr expr ^ ")"
  | SVarDef (ty, name, expr) -> sprintf "SVarDef(%s, %s, %s)" (string_of_ty ty) ("\"" ^ name ^ "\"") (pp_expr expr)
  | SVarAssign (name, expr) -> sprintf "SVarAssign(%s, %s)" ("\"" ^ name ^ "\"") (pp_expr expr)
  | SScope stmts ->
    (
      match stmts with
        | [] -> "SScope({})"
        | _ ->
          let stmt = String.concat " " (List.map pp_stmt stmts) in
          sprintf "SScope({%s})" stmt
    )
  | SIf (expr, stmt, opt_stmt) -> 
    (
      match opt_stmt with
        | None -> sprintf "SIf(%s, %s, )" (pp_expr expr) (pp_stmt stmt)
        | Some opt_stmt -> sprintf "SIf(%s, %s, %s)" (pp_expr expr) (pp_stmt stmt) (pp_stmt opt_stmt)
    )
  | SWhile (expr, stmt) -> sprintf "SWhile(%s, %s)" (pp_expr expr) (pp_stmt stmt)
  | SBreak -> "SBreak"
  | SReturn None -> "SReturn()"
  | SReturn (Some expr) -> sprintf "SReturn(%s)" (pp_expr expr)

let pp_params params = 
  (* Print a list of parameters as a space-separated touple list *)
  let extract_params (ty, name) = sprintf "(%s, %s)" (string_of_ty ty) ("\"" ^ name ^ "\"") in
  "{" ^ String.concat " " (List.map extract_params params) ^ "}"

let pp_global = function
  | GFuncDef (ty, name, params, body) -> sprintf "GFuncDef(%s, %s, %s, %s)" (string_of_ty ty) ("\"" ^ name ^ "\"") (pp_params params) (pp_stmt body) 
  | GFuncDecl (ty, name, params) -> sprintf "GFuncDecl(%s, %s, %s)" (string_of_ty ty) ("\"" ^ name ^ "\"") (pp_params params)

let pp_program globals =
  String.concat "\n\n" (List.map pp_global globals)