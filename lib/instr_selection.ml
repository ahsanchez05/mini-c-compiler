(* Instruction selection: translates the IR into the assembly IR, mapping variables to virtual registers *)

open Ast
open Ir
open Asm_ir


  (*
  env is the environment (an association list), 
  mapping names of variables to a
  tuple with a unique integer and the
  type of the variable

  env: (string * (int * ty)) list


  n is the integer count, used when
  generating new virtual registers


  acc is the accumulator list of generated
  instructions. It has type inst list
  *)


(* Make a temporary register with a unique number n *)
let tmp_reg n = TReg((n, QWord), Printf.sprintf "tmp_%d" n)

(* Make a register for a variable given the environment 
(list mapping names of variables to a tuple with a unique integer and the
type of the variable) *)
let make_reg key env =
  let (n, _ty) = List.assoc key env in
  TReg((n, QWord), Printf.sprintf "%s_%d" key n) (* We can always use QWord for this first implementation *)


let rec inst_select_expr env n acc reg = function
    (*
    1. instruction selection on an expression. arguments:
      - env
      - n
      - acc of instructions
      - register the expression will be assigned to
      - IR expression
    *)

  (* Int *)
  | EInt(i) -> (BinOp(Mov, reg, Imm(i))::acc, n)

  (* Var *)
  | EVar(v) -> (BinOp(Mov, reg, (make_reg v env))::acc, n)

  (* BinOp: x = x + 1 or x = 1 + x *)
  | EBinOp(Add, EVar(x), EInt(v)) | EBinOp(Add, EInt(v), EVar(x)) when reg = make_reg x env -> 
    (BinOp(Add, reg, Imm(v))::acc, n)

  (* BinOp: x = x + y *)
  | EBinOp(Add, EVar(x), EVar(y)) when reg = make_reg x env -> 
    (BinOp(Add, reg, make_reg y env)::acc, n)

  (* BinOp: x = y + x *)
  | EBinOp(Add, EVar(y), EVar(x)) when reg = make_reg x env -> 
    (BinOp(Add, reg, make_reg y env)::acc, n)

  (* Binop: x = e1 + e2 *)
  | EBinOp(Add, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Mov, reg, r1)::BinOp(Add, reg, r2)::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
    inst_select_expr env n2 acc2 r1 e1

  (* BinOp: x = x - 1 or x = 1 - x *)
  | EBinOp(Sub, EVar(x), EInt(v)) | EBinOp(Sub, EInt(v), EVar(x)) when reg = make_reg x env -> 
    (BinOp(Sub, reg, Imm(v))::acc, n)

  (* BinOp: x = x - y *)
  | EBinOp(Sub, EVar(x), EVar(y)) when reg = make_reg x env -> 
    (BinOp(Sub, reg, make_reg y env)::acc, n)

  (* BinOp: x = y - x *)
  | EBinOp(Sub, EVar(y), EVar(x)) when reg = make_reg x env -> 
    (BinOp(Sub, reg, make_reg y env)::acc, n)

  (* Binop: x = e1 - e2 *)
  | EBinOp(Sub, e1, e2) ->
    let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
    let n1 = n + 2 in
    let acc1 = BinOp(Mov, reg, r1)::BinOp(Sub, reg, r2)::acc in
    let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
    inst_select_expr env n2 acc2 r1 e1

  | _ -> exit 1 (* Other operators not handled for part S*)


let rec inst_select_ir_stmts env n acc = function
  (*
  env is the environment (an association list), 
  mapping names of variables to a
  tuple with a unique integer and the
  type of the variable

  env: (string * (int * ty)) list


  n is the integer count, used when
  generating new virtual registers


  acc is the accumulator list of generated
  instructions. It has type inst list


  The fourth argument is pattern matched on.
  It is a list of IR statements: ir_stmt list
  *)
  | IRSVarAssign(x, expr)::xs -> 
    let (expr_acc, n2) = inst_select_expr env n [] (make_reg x env) expr in
    inst_select_ir_stmts env n2 (List.rev_append expr_acc acc) xs (*List.rev_append l1 l2 reverses l1 and concatenates it with l2*)

  | IRSVarDecl(x, ty)::xs -> 
    inst_select_ir_stmts ((x, (n, ty))::env) (n+1) acc xs
    (*
    IRSVarDecl adds the variable to the environment,
    increments n, and proceeds with the rest of
    the instructions (xs), without generating an instruction 
    *)

  | [] -> (env, n, List.rev acc)
  | _ -> exit 1 (* error in other case *)
  (*
  Return a 3-tuple, with the elements: 
  environment, number of generated virtual registers, 
  and the list of generated instructions 
  (after reversing the list)
  *)

let inst_select_block env n (IRBlock(name, (stmt_list, blockend))) =
  (* Translate one block to asm code *)
  let env1, n1, insts = inst_select_ir_stmts env n [] stmt_list in
  let insts, n2 =
    match blockend with
    | IRSReturn None -> insts, n1
    | IRSReturn (Some e) -> 
      let return_reg = Reg(0, QWord) in (* The return value will be stored in register rax (index 0) *)
      let acc, n3 = inst_select_expr env1 n1 [] return_reg e in
      insts @ List.rev acc, n3
  in
  (Block(name, (insts, Ret)), env1, n2) 


let instr_select_func (IRFunc(name, (_ty, _param_list, block_list))) =
  match block_list with
  | [blk] ->
    let block, _env, _n = inst_select_block [] 0 blk in
    Func(name, [block])
  | _ -> exit 1 (* we only expect one block in this implementation *)

let instr_select_program prog =
  (* Translate the program to asm code *)
  List.map instr_select_func prog
