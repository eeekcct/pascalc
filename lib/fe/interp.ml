open Ast

exception Runtime_error of string

let rec eval_expr env = function
  | Int n -> n
  | Bool b -> if b then 1 else 0
  | Var x -> Hashtbl.find env x
  | Binop (Add, e1, e2) -> eval_expr env e1 + eval_expr env e2
  | Binop (Sub, e1, e2) -> eval_expr env e1 - eval_expr env e2
  | Binop (Mul, e1, e2) -> eval_expr env e1 * eval_expr env e2
  | Binop (Div, e1, e2) ->
      let v2 = eval_expr env e2 in
      if v2 = 0 then raise (Runtime_error "Division by zero")
      else eval_expr env e1 / v2
  | Binop (Eq, e1, e2) -> if eval_expr env e1 = eval_expr env e2 then 1 else 0
  | Binop (Neq, e1, e2) -> if eval_expr env e1 <> eval_expr env e2 then 1 else 0
  | Binop (Gt, e1, e2) -> if eval_expr env e1 > eval_expr env e2 then 1 else 0
  | Binop (Lt, e1, e2) -> if eval_expr env e1 < eval_expr env e2 then 1 else 0
  | Binop (Ge, e1, e2) -> if eval_expr env e1 >= eval_expr env e2 then 1 else 0
  | Binop (Le, e1, e2) -> if eval_expr env e1 <= eval_expr env e2 then 1 else 0
  | Unop (Neg, e) -> - (eval_expr env e)
  | _ -> failwith "Unsupported expression"

let rec eval_stmt env = function
  | Assign (x,e) ->
      let v = eval_expr env e in
      Hashtbl.replace env x v
  | Writeln e ->
      Printf.printf "%d\n" (eval_expr env e)
  | If (cond, then_stmt, else_stmt) ->
      if eval_expr env cond <> 0 then eval_stmt env then_stmt
      else (match else_stmt with
            | Some stmt -> eval_stmt env stmt
            | None -> ())
  | Compound stmts ->
      List.iter (eval_stmt env) stmts
  | While (cond, body) ->
      while eval_expr env cond <> 0 do
        eval_stmt env body
      done

let rec eval_block env (Block (decs, stmt)) =
  List.iter(fun dec ->
    match dec with
    | VarDec (IntType, id) -> Hashtbl.add env id 0
    | VarDec (BoolType, id) -> Hashtbl.add env id 0
  ) decs;
  eval_stmt env stmt

let run = function
  | Program (name, block) ->
      let env = Hashtbl.create 16 in
      eval_block env block
