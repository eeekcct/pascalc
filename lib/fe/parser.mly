%{
  open Ast
%}

%token PROGRAM BEGIN END WRITELN VAR INTEGER
%token SEMI COLON COMMA DOT ASSIGN LPAREN RPAREN
%token PLUS MINUS STAR SLASH
%token <string> IDENT
%token <int> INT
%token EOF

%start <Ast.program> program

// %left PLUS MINUS
// %left STAR SLASH

%%
program:
  | PROGRAM IDENT SEMI stmt DOT EOF { Program ($2, $4) }

block:
  | VAR decs BEGIN stmt_list_opt END { Block ($2, $4) }
  | BEGIN stmt_list_opt END { Block ([], $2) }

ty:
  | INTEGER { IntType }

decs:
  | dec SEMI decs { $1 :: $3 }
  | dec SEMI { [$1] }

dec:
  | IDENT COLON ty { VarDec ($3, $1) }

stmt_list_opt:
  | { [] }
  | stmt_list { List.rev $1 }

stmt_list:
  | stmt { [$1] }
  | stmt_list SEMI stmt { $3 :: $1 }
  | stmt_list SEMI { $1 }

stmt:
  | IDENT ASSIGN expr { Assign ($1, $3) }
  | WRITELN LPAREN expr RPAREN { Writeln $3 }
  | block { $1 }

expr:
  | term { $1 }
  | expr PLUS term { Binop (Add,$1, $3) }
  | expr MINUS term { Binop (Sub,$1, $3) }

term:
  | factor { $1 }
  | term STAR factor { Binop (Mul,$1, $3) }
  | term SLASH factor { Binop (Div,$1, $3) }

factor:
  | INT { Int $1 }
  | IDENT { Var $1 }
  | LPAREN expr RPAREN { $2 }
