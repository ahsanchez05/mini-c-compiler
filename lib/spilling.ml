(* Spilling/Register Allocation for x86-64 assembly *)

open Asm_ir


(* Helper function that adds a temporal register to the accumulator 
acc is an association list from temp_reg name to its index *)
let add_temp_reg acc = function
  | TReg(_reg, name) when not (List.mem_assoc name acc) -> (* Add register only if the register is not in the accumulator *)
    (name, List.length acc)::acc 
  | _ -> acc (* Don't add reg and return the accumulator in any other case *)


(* Function that goes through all the asm instructions and collects all the temporal registers *)
let rec collect_temp_regs acc = function
  (* If the instruction is a unary operator, collect from its operand *)
  | UnOp(_unop, op)::xs -> collect_temp_regs (add_temp_reg acc op) xs

  (* For binary operators, collect from both operands *)
  | BinOp(_binop, op1, op2)::xs -> collect_temp_regs (add_temp_reg (add_temp_reg acc op1) op2) xs

  (* Ignore call and sign extension (cqo) instructions *)
  | _::xs -> collect_temp_regs acc xs

  (* When no instructions left, return the inversed list of temporal registers (name, index) *)
  | [] -> List.rev acc


(* Function that goes through an operand and replaces virtual memory (temporal registers) 
with explicit memory (Mem(bitsize, reg, reg option, scale, displacement)) *)
let spill_op temp_reg_list = function
  | TReg(_reg, name) -> 
    let id = List.assoc name temp_reg_list in (* List.assoc finds the index of the temporal register *)
    Mem(QWord, (4, QWord), None, 1, 8*id) (* 4 is the index of rsp. we use rsp as base pointer of the stack for spills *)
  | other -> other (* If not a temporal register, return the operand *)


(* Spill a whole instruction
Avoid Mem-to-Mem operations by using register r10 when needed
Return a list of instructions *)
let spill_instrs temp_reg_list = function
  | UnOp(unop, op) -> [UnOp(unop, spill_op temp_reg_list op)]
  | BinOp(binop, op1, op2) -> 
    let op1_ = spill_op temp_reg_list op1 in
    let op2_ = spill_op temp_reg_list op2 in
    (match op1_, op2_ with
    | Mem(_) , Mem(_) ->  (* If both operands are memory locations, use r10 as a temporary register *)
      [BinOp(Mov, Reg(10, QWord), op2_); BinOp(binop, op1_, Reg(10, QWord))]
    | _ -> 
      [BinOp(binop, op1_, op2_)]
    )
  | Call s -> [Call s]
  | Cqo -> [Cqo]


(* Function that reserves space in the stack for all the temporal registers *)
let reserve_space size_in_bytes = 
  if size_in_bytes = 0 then [] 
  else                                                  (* If 0 bytes requested, do nothing *)
    [BinOp(Sub, Reg(4, QWord), Imm(size_in_bytes))]     (* If not 0, we compute rsp = rsp - size_in_bytes to reserve space *)


(* Function that restores the stack as it was at the beginning *)
let restore_stack size_in_bytes = 
  if size_in_bytes = 0 then [] 
  else
    [BinOp(Add, Reg(4, QWord), Imm(size_in_bytes))]    (* We compute rsp = rsp + size_in_bytes in order to restore the stack as it was at the beginning*)



let spill_block (Block(name, (instrs, blockend))) = 
  (* Spill every temp in the block to stack slots below rsp *)
  let temp_reg_list = collect_temp_regs [] instrs in
  let size_in_bytes = 8 * List.length temp_reg_list in
  let instrs2 = List.concat (List.map (spill_instrs temp_reg_list) instrs) in
  Block(name, (reserve_space size_in_bytes @ instrs2 @ restore_stack size_in_bytes, blockend))


let spill_program functions = 
  (* Apply spilling over all functions/blocks. *)
  List.map (fun (Func(name, blocks)) -> Func(name, List.map spill_block blocks)) functions
