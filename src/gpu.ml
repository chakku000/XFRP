open Target

let self_index_expr = 
  "int self = blockIdx.x * blockDim.x + threadIdx.x;"

let rec transform_to_c (expr : Syntax.expr) : Target.ast * Target.ast = 
  match expr with 
  | EConst(c) -> (Empty,Const c)
  | Eid(i) -> (Empty, Var i)
  | Ebin(op,expr1,expr2) ->
      let op_symbol = Syntax.string_of_binop op in
      let pre1, post1 = transform_to_c expr1 in
      let pre2, post2 = transform_to_c expr2 in
