open Target
open Syntax
module StringSet = Set.Make (String)

let self_index_expr = "int self = blockIdx.x * blockDim.x + threadIdx.x;"

(* let rec transform_to_c (expr : Syntax.expr) : Target.ast * Target.ast = *)
(*   match expr with *)
(*   | GSelf -> (Empty, Const("self")) *)
(*   | GConst(c) -> (Empty,Const c) *)
(*   | Gid(i) -> (Empty, Var i) *)
(*   | Gbin(op,expr1,expr2) -> *)
(*       let op_symbol = Syntax.string_of_binop op in *)
(*       let pre1, post1 = transform_to_c expr1 in *)
(*       let pre2, post2 = transform_to_c expr2 in *)

let generate_gnode_update_kernel (name : string) (expr : Syntax.gexpr)
    (ast : Syntax.ast) (program : Module.program) : string =
  let modification = "__global__" in
  let kernel_name = Printf.sprintf "%s_kernel" name in
  let id_to_type_table =
    (* Nodeから型への辞書 TODO これを毎回構築してるの効率が悪い *)
    let tbl = Hashtbl.create 10 in
    List.iter
      (function Node ((i, t), _, _) -> Hashtbl.add tbl i t | _ -> ())
      ast.definitions ;
    List.iter (function i, t -> Hashtbl.add tbl i t) ast.in_nodes ;
    List.iter
      (function GNode ((i, t), _, _, _) -> Hashtbl.add tbl i t | _ -> ())
      ast.definitions ;
    tbl
  in
  let rec collect_node_deps e =
    match e with
    | GSelf ->
        StringSet.empty
    | GConst _ ->
        StringSet.empty
    | Gid id ->
        if List.exists (String.equal id) program.node then
          StringSet.singleton id
        else StringSet.empty
    | GAnnot (_, _) ->
        StringSet.empty
    | Gbin (_, e1, e2) ->
        StringSet.union (collect_node_deps e1) (collect_node_deps e2)
    | GApp (_, el) ->
        List.fold_left
          (fun acc elm -> StringSet.union acc (collect_node_deps elm))
          StringSet.empty el
    | Gif (ce, e1, e2) ->
        StringSet.union (collect_node_deps ce)
          (StringSet.union (collect_node_deps e1) (collect_node_deps e2))
  in
  let rec collect_node_atlast_deps e =
    match e with
    | GSelf ->
        StringSet.empty
    | GConst _ ->
        StringSet.empty
    | Gid _ ->
        StringSet.empty
    | GAnnot (id, _) ->
        if List.exists (String.equal id) program.node then
          StringSet.singleton id
        else StringSet.empty
    | Gbin (_, e1, e2) ->
        StringSet.union
          (collect_node_atlast_deps e1)
          (collect_node_atlast_deps e2)
    | GApp (_, el) ->
        List.fold_left
          (fun acc elm -> StringSet.union acc (collect_node_atlast_deps elm))
          StringSet.empty el
    | Gif (ce, e1, e2) ->
        StringSet.union
          (collect_node_atlast_deps ce)
          (StringSet.union
             (collect_node_atlast_deps e1)
             (collect_node_atlast_deps e2))
  in
  let rec collect_gnode_deps e =
    match e with
    | GSelf ->
        StringSet.empty
    | GConst _ ->
        StringSet.empty
    | Gid _ ->
        StringSet.empty
    | GAnnot _ ->
        StringSet.empty
    | Gbin (_, e1, e2) ->
        StringSet.union (collect_gnode_deps e1) (collect_gnode_deps e2)
    | GApp (_, el) ->
        List.fold_left
          (fun acc elm -> StringSet.union acc (collect_gnode_deps elm))
          StringSet.empty el
    | Gif (ce, e1, e2) ->
        StringSet.union (collect_gnode_deps ce)
          (StringSet.union (collect_gnode_deps e1) (collect_gnode_deps e2))
  in
  let node_set = collect_node_deps expr in
  let node_atlast_set = collect_node_atlast_deps expr in
  let gnode_set = collect_gnode_deps expr in
  Hashtbl.iter
    (fun k v -> Printf.printf "%s - %s\n" k (Type.of_string v))
    id_to_type_table ;
  let normal_args =
    StringSet.elements node_set
    |> List.map (fun nd ->
           Printf.sprintf "%s %s"
             (Hashtbl.find id_to_type_table nd |> Type.of_string)
             nd)
    |> String.concat ", "
  in
  let last_args =
    StringSet.elements node_atlast_set
    |> List.map (fun nd ->
           Printf.sprintf "%s %s_ATLAST"
             (Hashtbl.find id_to_type_table nd |> Type.of_string)
             nd)
    |> String.concat ","
  in
  let gpu_args = 
    StringSet.elements gnode_set
    |> List.map (fun nd -> 
        Printf.sprintf "%s* %s"
        (Hashtbl.find id_to_type_table nd |> Type.of_string)
        nd)
    |> String.concat "n"
  in
  let args =
    (* 順序は @lastがついてないノード -> @lastがついているノード -> gnodeの引数 となる. それぞれの中では辞書順で並んでいる *)
    "("
    ^ String.concat ","
        (List.filter (fun l -> String.length l > 0) [normal_args; last_args; gpu_args])
    ^ ")"
  in
  modification ^ " " ^ kernel_name ^ args ^ ";"
