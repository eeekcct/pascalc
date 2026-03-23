type binop =
  | Add | Sub | Mul | Div
  | Eq | Gt

type expr = 
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of binop * expr * expr
  | Let of string * expr * expr

type stmt =
  | Assign of string * expr
  | Writeln of expr
  | Block of stmt list

type program =
  Program of string * stmt
