(* IR for x86-64 assembly and pretty-printer *)

open Printf

(* The assembly IR is a simplified version of the x86-64 assembly language. 
   It is designed to be easy to generate from the IR and easy to translate to actual assembly code *)
type unop = Inc | Dec | Push | Pop | IMul | IDiv | Not | Neg |
            Setg | Setl | Setge | Setle | Sete | Setne

type binop = Add | Sub | Cmp | Mov | And | Or | Xor

(* Concrete operand sizes we care about *)
type bitsize =
  | Byte 
  | Word 
  | DWord 
  | QWord

type displacement = int
type scale = int
type reg = int * bitsize

(* Operands: immediates, registers (anonymous or named temps), memory references, or a hole *)
type op =
  | Imm of int
  | Reg of reg
  | TReg of reg * string
  | Mem of bitsize * reg * reg option * scale * displacement
  | NoOp

(* Instruction shapes supported by the compiler backend. *)
type inst =
  | UnOp of unop * op
  | BinOp of binop * op * op
  | Call of string
  | Cqo

type jbinop = Jl | Jg | Jle | Jge | Je | Jne

(* Currently only 'ret' but leaves room for other terminators. *)
type blockend =
| Ret

type block = Block of string * (inst list * blockend)

type func = Func of string * block list

(*----------------------------ASM Pretty Printer--------------------------------*)

let string_of_unop = function
  | Inc -> "inc"
  | Dec -> "dec"
  | Push -> "push"
  | Pop -> "pop"
  | IMul -> "imul"
  | IDiv -> "idiv"
  | Not -> "not"
  | Neg -> "neg"
  | Setg -> "setg"
  | Setl -> "setl"
  | Setge -> "setge"
  | Setle -> "setle"
  | Sete -> "sete"
  | Setne -> "setne"

let string_of_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Cmp -> "cmp"
  | Mov -> "mov"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"

let string_of_bitsize = function
  | Byte -> "byte"
  | Word -> "word"
  | DWord -> "dword"
  | QWord -> "qword"

let string_of_jbinop = function
  | Jl -> "jl"
  | Jg -> "jg"
  | Jle -> "jle"
  | Jge -> "jge"
  | Je -> "je"
  | Jne -> "jne"

let bit_regs_8 = ["al"; "cl"; "dl"; "bl"; "spl"; "bpl"; "sil"; "dil"; "r8b"; "r9b"; "r10b"; "r11b"; "r12b"; "r13b"; "r14b"; "r15b"]

let bit_regs_16 = ["ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"; "r8w"; "r9w"; "r10w"; "r11w"; "r12w"; "r13w"; "r14w"; "r15w"]

let bit_regs_32 = ["eax"; "ecx"; "edx"; "ebx"; "esp"; "ebp"; "esi"; "edi"; "r8d"; "r9d"; "r10d"; "r11d"; "r12d"; "r13d"; "r14d"; "r15d"]

let bit_regs_64 = ["rax"; "rcx"; "rdx"; "rbx"; "rsp"; "rbp"; "rsi"; "rdi"; "r8"; "r9"; "r10"; "r11"; "r12"; "r13"; "r14"; "r15"]


let string_of_reg (id, size) =
  (* Registers are stored by index
  pick the right name for the requested size. *)
  let reg_list = 
    match size with
    | Byte -> bit_regs_8
    | Word -> bit_regs_16
    | DWord -> bit_regs_32
    | QWord -> bit_regs_64
  in
  if id >= 0 && id <= 15 then
    sprintf "%s" (List.nth reg_list id)
  else "error"

let string_of_op = function
  | Imm i -> sprintf "%d" i
  | Reg r -> string_of_reg r
  | TReg (_r, name) -> name
  | Mem (size, r1, r2, scale, disp) ->
    (* NASM addressing form: [base + index*scale + disp] with an explicit size prefix *)
    let reg1 = string_of_reg r1 in
    let reg2 =
      match r2 with
      | None -> ""
      | Some r -> 
        let aux = string_of_reg r in
        let scale_ = if scale = 1 then "" else "*" ^ string_of_int scale in
        "+" ^ aux ^ scale_
    in
    let disp_ = 
      match disp with
      | 0 -> ""
      | d when d > 0 -> "+" ^ string_of_int d
      | d when d < 0 -> "-" ^ string_of_int (-d)
      | _ -> ""
    in sprintf "%s [%s%s%s]" (string_of_bitsize size) reg1 reg2 disp_
  | NoOp -> ""


let pp_inst = function
  | UnOp(unop, op) -> sprintf ("%s\t%s") (string_of_unop unop) (string_of_op op)
  | BinOp(binop, op1, op2) -> sprintf "%s\t%s, %s" (string_of_binop binop) (string_of_op op1) (string_of_op op2)
  | Call s -> sprintf "call\t%s" s
  | Cqo -> "cqo"

let pp_blockend = function
  | Ret -> ["ret"]

let pp_block (Block (name, (instrs, blockend))) =
  let instrs_str = List.map (fun x -> "\t" ^ pp_inst x) instrs in
  let blockend_str = List.map (fun y -> "\t" ^ y) (pp_blockend blockend) in
  String.concat "\n" ((name ^ ":") :: instrs_str @ blockend_str)

let pp_func (Func(name, blocks)) =
  let blocks_string = String.concat "\n" (List.map pp_block blocks) in
  sprintf "\t\t; function '%s'\n%s" name blocks_string


let pp_program funcs = 
  let globals = 
    String.concat "\n" (List.map (fun (Func(name, _blocks)) -> "global " ^ name) funcs)
  in
  let body = String.concat "\n\n" (List.map pp_func funcs) in
  sprintf "\t%s\n\tsection .text\n\t%s" globals body
