open Pascalc

let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "Line %d, Column %d" line col

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let program = Parser.program Lexer.token lexbuf in
    close_in ic;
    program
  with
  | Lexer.Lexing_error msg ->
      close_in ic;
      failwith (Printf.sprintf "Lexing error at %s: %s" (position_string lexbuf) msg)
  | Parser.Error ->
      close_in ic;
      failwith (Printf.sprintf "Parsing error at %s" (position_string lexbuf))
  | e ->
      close_in_noerr ic;
      raise e


let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <source.pas>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  try
    let program = parse_file filename in 
    Interp.run program
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Interp.Runtime_error msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
