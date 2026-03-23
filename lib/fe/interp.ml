open Ast

exception Runtime_error of string

let rec eval_expr env = function
  | Int n -> n
  | Var x -> Hashtbl.find env x
  | Binop (Add, e1, e2) -> eval_expr env e1 + eval_expr env e2
  | Binop (Sub, e1, e2) -> eval_expr env e1 - eval_expr env e2
  | Binop (Mul, e1, e2) -> eval_expr env e1 * eval_expr env e2
  | Binop (Div, e1, e2) ->
      let v2 = eval_expr env e2 in
      if v2 = 0 then raise (Runtime_error "Division by zero")
      else eval_expr env e1 / v2
  | _ -> failwith "Unsupported expression"

let rec eval_stmt env = function
  | Assign (x,e) ->
      let v = eval_expr env e in
      Hashtbl.replace env x v
  | Writeln e ->
      Printf.printf "%d\n" (eval_expr env e)
  | Block stmts ->
      List.iter (eval_stmt env) stmts

let run = function
  | Program (name, stmt) ->
      let env = Hashtbl.create 16 in
      eval_stmt env stmt
