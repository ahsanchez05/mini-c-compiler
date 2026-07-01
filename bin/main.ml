open Mini_c_compiler
open Printf

(* Obtain the program arguments *)
let program_args = Sys.argv

(* Get the number of program arguments *)
let n_program_args = Array.length program_args

(* Main function *)
let () =
  (* Three arguments required *)
  if n_program_args = 3 then (
      let arg1 = Array.get program_args 1 in
      let arg2 = Array.get program_args 2 in
      match (arg1, arg2) with

      (* First option: --pretty-print tag -> pretty print AST *)
      | ("--pretty-print", filename) | (filename, "--pretty-print") ->
        (* Get file name *)
        let file = open_in filename in
        (* Get lexer buffer *)
        let lexbuf = Lexing.from_channel file in
        let ast =
          try
            (* Get sequence of tokens (Lexer.token lexbuf) and parse tokens
            Generate AST *)
            Parser.main Lexer.token lexbuf
          with
          | Parser.Error -> exit 1
        in
        (* Pretty print AST *)
        printf "%s\n" (Ast.pp_program ast)

      (* Second option: --ir tag -> pretty print the IR () *)
      | ("--ir", filename) | (filename, "--ir") ->
        (* Get file name *)
        let file = open_in filename in
        (* Get lexer buffer *)
        let lexbuf = Lexing.from_channel file in
        let ast =
          try
            (* Get sequence of tokens (Lexer.token lexbuf) and parse tokens
            Generate AST *)
            Parser.main Lexer.token lexbuf
          with
          | Parser.Error -> exit 1
        in
        (* Traduce AST to IR *)
        let ir = Ir.ir_of_program ast in
        (* Pretty print IR *)
        printf "%s\n" (Ir.pp_program ir)

      (* Third option: --asm tag -> generate asm code given a C file
      and pretty print it *)
      | ("--asm", filename) | (filename, "--asm") ->
        (* Get file name *)
        let file = open_in filename in
        (* Get lexer buffer *)
        let lexbuf = Lexing.from_channel file in
        (* Get AST *)
        let ast =
          try
            (* Get sequence of tokens (Lexer.token lexbuf) and parse tokens
            Generate AST *)
            Parser.main Lexer.token lexbuf
          with
          | Parser.Error -> exit 1
        in
        (* Traduce AST to IR *)
        let ir = Ir.ir_of_program ast in
        (* Generate asm code (instruction selection) from IR *)
        let instr_select = Instr_selection.instr_select_program ir in
        (* Spilling: generate final asm code *)
        let asm_final = Spilling.spill_program instr_select in
        (* Pretty print asm code *)
        printf "%s\n" (Asm_ir.pp_program asm_final)

      | _ -> exit 1
  )
  else exit 1