(* Parser for the C compiler using Menhir *)

%{
    open Ast
%}


(* Tokens *)
%token <int> UINT
%token <string> IDENT
%token <char> CHAR
%token LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON
%token GT LT GE LE EQ NEQ ASSIGN
%token PLUS MINUS MUL DIV MOD
%token AND ANDAND OR OROR
%token KW_BREAK KW_CHAR KW_ELSE KW_EXTERN KW_IF KW_INT KW_RETURN KW_VOID KW_WHILE
%token EOF

(* Define a fake precedence level to resolve the dangling-else ambiguity *)
%nonassoc FAKE_ELSE 
%nonassoc KW_ELSE


(* Define operator precedence and associativity *)
(* least precedence *)
%left OROR
%left ANDAND
%left OR
%left AND
%left EQ NEQ
%left GT LT GE LE
%left PLUS MINUS 
%left MUL DIV MOD
(* most precedence *)


%start main
%type <Ast.ast> main

%%