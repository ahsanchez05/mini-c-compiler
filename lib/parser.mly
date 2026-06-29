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

main:
  | globals EOF
    { $1 }

globals:
  | 
    { [] }
  | global globals
    { $1::$2 }

global:
  | ty IDENT LPAREN params RPAREN LBRACE statements RBRACE
    { GFuncDef($1, $2, $4, SScope($7)) }
  | KW_EXTERN ty IDENT LPAREN params RPAREN SEMICOLON
    { GFuncDecl($2, $3, $5) }

ty:
  | KW_VOID
    { TVoid }
  | KW_INT 
    { TInt }
  | KW_CHAR
    { TChar }

params:
  | 
    { [] }
  | param_list
    { $1 }

param_list:
  | param
    { [$1] }
  | param COMMA param_list
    { $1::$3 }

param:
  | ty IDENT
    { ($1, $2) }

statements:
  |
    { [] }
  | statement statements
    { $1::$2 }

statement:
  | ty IDENT ASSIGN expression SEMICOLON
    { SVarDef($1, $2, $4) }
  | IDENT ASSIGN expression SEMICOLON
    { SVarAssign($1, $3) }
  | expression SEMICOLON
    { SExpr($1) }
  | LBRACE statements RBRACE 
    { SScope($2) }
  | KW_IF LPAREN expression RPAREN statement KW_ELSE statement
    { SIf($3, $5, Some $7) }
  | KW_IF LPAREN expression RPAREN statement %prec FAKE_ELSE 
    { SIf($3, $5, None) } (* By using %prec, we are forcing to first check if/else, and if not KW_ELSE token found, then proceed with this production *)
  | KW_WHILE LPAREN expression RPAREN statement 
    { SWhile($3, $5) }
  | KW_BREAK SEMICOLON
    { SBreak }
  | KW_RETURN SEMICOLON
    { SReturn None }
  | KW_RETURN expression SEMICOLON
    { SReturn(Some $2) }

expression:
  | IDENT 
    { EVar $1 }
  | UINT
    { EInt $1 }
  | CHAR
    { EChar $1 }
  | IDENT LPAREN args RPAREN
    { ECall($1, $3) }
  | LPAREN expression RPAREN
    { $2 }
  | expression PLUS expression
    { EBinOp(Add, $1, $3) }
  | expression MINUS expression
    { EBinOp(Sub, $1, $3) }
  | expression MUL expression
    { EBinOp(Mul, $1, $3) }
  | expression DIV expression
    { EBinOp(Div, $1, $3) }
  | expression MOD expression
    { EBinOp(Mod, $1, $3) }
  | expression GT expression
    { EBinOp(Gt, $1, $3) }
  | expression LT expression
    { EBinOp(Lt, $1, $3) }
  | expression GE expression
    { EBinOp(Ge, $1, $3) }
  | expression LE expression
    { EBinOp(Le, $1, $3) }
  | expression EQ expression
    { EBinOp(Eq, $1, $3) }
  | expression NEQ expression
    { EBinOp(Ne, $1, $3) }
  | expression AND expression
    { EBinOp(BAnd, $1, $3) }
  | expression ANDAND expression
    { EBinOp(LAnd, $1, $3) }
  | expression OR expression
    { EBinOp(BOr, $1, $3) }
  | expression OROR expression
    { EBinOp(LOr, $1, $3) }

args:
  | 
    { [] }
  | arg_list
    { $1 }

arg_list:
  | expression
    { [$1] }
  | expression COMMA arg_list
    { $1::$3 }