(* Translation from AST to IR (intermediate representation), before instruction selection *)

open Ast
open Printf


(** Intermediate representation definition *)
type ir_stmt = 
  | IRSExpr of expr
  | IRSVarAssign of string * expr
  | IRSVarDecl of string * ty
  
type ir_blockend = 
  | IRSReturn of expr option

type ir_block = IRBlock of string * (ir_stmt list * ir_blockend)

type param = ty * string

type ir_global = IRFunc of string * (ty * param list * ir_block list)

type ir_program = ir_global list


(*------------------------Translation from AST to IR--------------------------*)


(* Traduce from AST statements to a list of IR statements *)
let rec ir_stmt_list_of_stmt = function
  | SExpr e -> [IRSExpr e]
  | SVarDef(ty, name, expr) -> [IRSVarDecl(name, ty); IRSVarAssign(name, expr)]
  | SVarAssign(name, expr) -> [IRSVarAssign(name, expr)]
  | SScope stmts -> List.concat (List.map ir_stmt_list_of_stmt stmts)
  | SReturn _ -> [] (*ignore: handled by next nethod*)
  | SBreak | SIf (_, _, _) | SWhile (_, _) -> exit 1 (*we are not taking into account branches in this implementation for now *)

(* Build a block from a statement list, collecting everything until the (optional) return
Optional return handled here *)
let ir_block_of_stmt_list stmts =
  let rec aux acc = function
    | [] -> (List.rev acc, IRSReturn None)
    | SReturn(opt)::[] -> (List.rev acc, IRSReturn opt)
    | stmt::tail ->
      let ir_stmt_list = ir_stmt_list_of_stmt stmt in
      aux (List.rev ir_stmt_list @ acc) tail
  in
  aux [] stmts

(* Entry point for scopes *)
let ir_scope_of_stmt stmt = 
  match stmt with
  | SScope stmts -> ir_block_of_stmt_list stmts
  | _ -> exit 1 (* everything else that is not SScope *)

(* Translate globals into an IR function with a single block *)
let ir_of_global = function
  | GFuncDef(ty, name, params, body) ->
    let stmt_list, blockend = ir_scope_of_stmt body in
    let block = IRBlock(name, (stmt_list, blockend)) in
    IRFunc(name, (ty, params, [block]))
  | _ -> exit 1 (* ignore function declarations in this implementation for now *)

(* Translate the whole program *)
let ir_of_program globals = 
  List.map ir_of_global globals


(*----------------------------IR Pretty Printer--------------------------------*)

(* Pretty printer for debugging *)
let pp_ir_stmt = function
  | IRSExpr e -> sprintf "IRSExpr(%s)" (pp_expr e)
  | IRSVarAssign (ty, e) -> sprintf "IRSVarAssign(%s, %s)" ("\"" ^ ty ^ "\"") (pp_expr e)
  | IRSVarDecl (s, ty) -> sprintf "IRSVarDecl(%s, %s)" ("\"" ^ s ^ "\"") (string_of_ty ty)


let pp_ir_blockend = function
  | IRSReturn None -> "IRSReturn()"
  | IRSReturn (Some e) -> sprintf "IRSReturn(%s)" (pp_expr e)


let pp_ir_block = function
  | IRBlock(key, (stmts, return)) ->
    let body = String.concat "\n\t" (List.map pp_ir_stmt stmts) in
    sprintf "IRBlock({%s,\n\t%s\n\t%s})" key body (pp_ir_blockend return)

let pp_ir_global = function
  | IRFunc(name, (ty, param_list, block_list)) ->
    let blocks = String.concat "\n" (List.map pp_ir_block block_list) in
    sprintf "IRFunc(%s, %s, {},{\n\t%s})" (string_of_ty ty) name blocks

let pp_program ir_globals = String.concat "\n\n" (List.map pp_ir_global ir_globals) 
