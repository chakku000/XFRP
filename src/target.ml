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

let unique_index = ref 0

let get_unique_name () : string =
  let id = string_of_int !unique_index in
  unique_index := !unique_index + 1 ;
  "tmp_" ^ id

(* GPUノードとCPUノードではノードへのアクセス方法が異なるのでXFRPの式からCUDAへの変換はXFRPからC言語への変換とは異なる関数で実装するべき *)
let rec xfrp_to_g_ast (expr : Syntax.gexpr) : ast * ast =
  match expr with
  | GSelf ->
      (Empty, Const "self")
  | GConst e ->
      (Empty, Const (Syntax.string_of_const e))
  | Gid id ->
      (Empty, Var id)
  | GAnnot (id, _) ->
      (Empty, Var (id ^ "_ATLAST"))
  | GIdAt (id, e_index) ->
      (* GPUノードへのアクセス. 事前にindexの値を計算するべき *)
      let index_ast_pre, index_ast_post = xfrp_to_g_ast e_index in
      let index_temp = get_unique_name () in
      ( Assignment (Some Type.TInt, index_temp, index_ast_post)
      , Var (Printf.sprintf "%s_ATLAST[%s]" id index_temp) )
  | _ ->
      (Empty, Empty)

(* TODO 実装 *)
