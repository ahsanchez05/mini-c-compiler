(* Lexer for the C compiler using OCamllex *)

{
    open Parser
}


let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
    | [' ' '\t' '\r'] { token lexbuf } (* Ignore whitespace *)
    | ['\n'] { Lexing.new_line lexbuf; token lexbuf } (* Ignore newlines *)
    | '#' [^ '\n']* { token lexbuf } (* Ignore preprocessor directives *)
    | "//" [^ '\n']* { token lexbuf } (* Ignore line comments *)
    | "/*" { comment lexbuf; token lexbuf } (* Ignore multi-line comments *)

    (* Identifiers *)
    | ident as i {
        match i with 
        | "break" -> KW_BREAK
        | "char" -> KW_CHAR
        | "else" -> KW_ELSE
        | "extern" -> KW_EXTERN
        | "if" -> KW_IF
        | "int" -> KW_INT
        | "return" -> KW_RETURN
        | "void" -> KW_VOID
        | "while" -> KW_WHILE
        | _ -> IDENT(i)
    }

    (* Int literals *)
    | '0'|['1'-'9']['0'-'9']* as lxm
        { UINT (int_of_string lxm) }

    (* Separators *)
    | '('{ LPAREN }
    | ')' { RPAREN }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ',' { COMMA }
    | ';' { SEMICOLON }

    (* Binary operators *)
    | ">"  { GT }
    | "<"  { LT }
    | ">=" { GE }
    | "<=" { LE }
    | "==" { EQ }
    | "!=" { NEQ }
    | '='  { ASSIGN }
    | '+'  { PLUS }
    | '-'  { MINUS }
    | '*'  { MUL }
    | '/'  { DIV }
    | '%'  { MOD }
    | '&'  { AND }
    | "&&" { ANDAND }
    | '|'  { OR }
    | "||" { OROR }

    (* Allowed characters: from 0X20 to 0X7E excluding 0X5C, 0X27 AND 0X22 *)
    | '\'' ([' '-'!''#'-'&''('-'['']'-'~'] as ch) '\'' 
        { CHAR ch }

    (* Characters that need to be scaped *)
    | '\'' '\\' (['n''t''\\''\'''"'] as ch2) '\'' {
        let c =
        match ch2 with
        | 'n' -> '\n'
        | 't' -> '\t'
        | '\'' -> '\''
        | '\\' -> '\\'
        | '\"' -> '\"'  
        | _ -> exit 1
        in CHAR c
        }   
    | eof
        { EOF } 
    | _
        { exit 1 }


and comment = parse
    | "*/" { () }
    | eof  { exit 1 }
    | '\n' { Lexing.new_line; comment lexbuf }
    | _    { comment lexbuf }