open Syntax
module IntSet = Set.Make (Int)

(* 依存グラフの構築 *)
(* TODO : Module.program.graphは不要なのでこの実装が終わり次第消しても良い *)
(* 返り値はノードのIDを使った隣接リスト  *)
(* a->bと依存関係がある場合, graph[a] = {b} となる *)
let construct_graph (ast : Syntax.ast) (program : Module.program) =
  let ptbl = Hashtbl.create 128 in
  (* 親ノードの集合. a->bならptbl[b]={a} *)
  (* Inputノード *)
  List.iter
    (fun inode ->
      Hashtbl.add ptbl (Hashtbl.find program.id_table inode) IntSet.empty)
    program.input ;
  (* Internal/Outputノード *)
  let rec collect_id_of_cpu expr =
    (* exprの部分木に含まれるIDの集合 *)
    match expr with
    | ESelf | EConst _ | EAnnot _ | EAnnotA _ ->
        IntSet.empty
    | Eid i ->
        Hashtbl.find program.id_table i |> IntSet.singleton
    | EidA (i, e) ->
        let id1 = Hashtbl.find program.id_table i in
        let index_set = collect_id_of_cpu e in
        IntSet.add id1 index_set
    | Ebin (_, e1, e2) ->
        IntSet.union (collect_id_of_cpu e1) (collect_id_of_cpu e2)
    | EUni (_, e) ->
        collect_id_of_cpu e
    | EApp (_, args) ->
        List.fold_left
          (fun acc elm -> IntSet.union acc (collect_id_of_cpu elm))
          IntSet.empty args
    | Eif (cond, e1, e2) ->
        IntSet.union (collect_id_of_cpu cond)
          (IntSet.union (collect_id_of_cpu e1) (collect_id_of_cpu e2))
  in
  (* TODO GPUノードの解析が未実装 *)
  (* let rec collect_id_of_gpu gexp = *)
  List.iter
    (function
      | Node ((node, t), _, e) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_id_of_cpu e)
      | NodeA ((node, t), _, _, e) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_id_of_cpu e)
      | GNode _ ->
          ())
    ast.definitions ;
  let ctbl = Hashtbl.create 128 in
  (* 子ノードの集合. a->bならctbl[a]={b} *)
  Hashtbl.iter
    (fun key set ->
      IntSet.iter
        (fun v ->
          let cur =
            if Hashtbl.mem ctbl v then Hashtbl.find ctbl v else IntSet.empty
          in
          Hashtbl.replace ctbl v (IntSet.add key cur))
        set)
    ptbl ;
  ptbl
