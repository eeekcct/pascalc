type id = string

type binop =
  | Add | Sub | Mul | Div
  | Eq | Gt | Neq | Lt | Ge | Le

type unop =
  | Neg

type typ = 
  | IntType

type dec =
  | VarDec of typ * id

type expr = 
  | Int of int
  | Bool of bool
  | Var of string
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Let of string * expr * expr

type stmt =
  | Assign of string * expr
  | Writeln of expr
  | Block of dec list * stmt list
  | If of expr * stmt * (stmt option)

type program =
  Program of string * stmt
