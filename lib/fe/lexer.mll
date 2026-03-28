{
  open Parser
  exception Lexing_error of string
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let ws = [' ' '\t' '\n' '\r']+

rule token = parse
  | ws { token lexbuf } (* Skip whitespace *)
  | "program" { PROGRAM }
  | "begin" { BEGIN }
  | "end" { END }
  | "writeln" { WRITELN }
  | "var" { VAR }
  | "integer" { INTEGER }
  | "boolean" { BOOLEAN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | ":=" { ASSIGN }
  | ';' { SEMI }
  | ':' { COLON }
  | '.' { DOT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '=' { EQ }
  | "<>" { NEQ }
  | '>' { GT }
  | '<' { LT }
  | ">=" { GE }
  | "<=" { LE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | digit+ as n { INT (int_of_string n) }
  | id as s { IDENT s }
  | eof { EOF }
  | _ {raise (Lexing_error ("Unexpected character: " ^ (String.make 1 (Lexing.lexeme_char lexbuf 0)))) }
