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
  | ":=" { ASSIGN }
  | ';' { SEMI }
  | ':' { COLON }
  | '.' { DOT }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | digit+ as n { INT (int_of_string n) }
  | id as s { IDENT s }
  | eof { EOF }
  | _ {raise (Lexing_error ("Unexpected character: " ^ (String.make 1 (Lexing.lexeme_char lexbuf 0)))) }
