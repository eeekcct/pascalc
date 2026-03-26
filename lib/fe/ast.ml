type id = string

type binop =
  | Add | Sub | Mul | Div
  | Eq | Gt

type typ = 
  | IntType

type dec =
  | VarDec of typ * id

type expr = 
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of binop * expr * expr
  | Let of string * expr * expr

type stmt =
  | Assign of string * expr
  | Writeln of expr
  | Block of dec list * stmt list

type program =
  Program of string * stmt
