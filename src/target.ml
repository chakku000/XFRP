type id = string
type const = string
type ast =
  | Empty
  | Const of const (* 定数 *)
  | Var of id
  | Binop of id * ast * ast
  | FunCall of id * ast list
  | CodeSeq of ast list
