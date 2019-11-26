open Syntax

type id = string

type const = string

type ast =
  | Empty
  | Const of const (* 定数 *)
  | Var of id
  | VarDec of Type.t * string (* 変数宣言 *)
  | Assignment of Type.t option * string * ast
  | Binop of id * ast * ast
  | FunCall of id * ast list
  | If of
      (Type.t * string)
      * (* if式の結果を保存する変数 *)
        string
        (* condの結果を保存する変数. 型はType.TBool *)
      * (ast * ast)
      * (* cond *)
      (ast * ast)
      * (* then *)
      (ast * ast) (* else *)
  | CodeSeq of ast list

(* let xfrp_to_g_ast (expr : Syntax.gexpr) : ast * ast = *)
(*   match expr with *)
(*   | GSelf -> (Empty, Const("self")) *)
(*   | GConst e -> (Empty, Const(e)) *)
(*   | Gid id -> (Empty, Var(id)) *)
(*   | GAnnot(id,_) -> (Empty, *)
(*   | _ -> Empty (1* TODO 実装 *1) *)
