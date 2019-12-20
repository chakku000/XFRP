open Syntax
module IntSet = Set.Make (Int)

(* 依存グラフの構築 *)
(* TODO : Module.program.graphは不要なのでこの実装が終わり次第消しても良い *)
(* 返り値はノードのIDを使った隣接リスト  *)
(* a->bと依存関係がある場合, graph[a] = {b} となる *)
let construct_graph (ast : Syntax.ast) (program : Module.program) : (int,IntSet.t) Hashtbl.t =
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
  (* この状態ではctblにoutputノードについての定義がないので追加 *)
  List.iter
    (fun output -> 
        let id = Hashtbl.find program.id_table output in
        Hashtbl.replace ctbl id IntSet.empty)
    program.output;

  ctbl

(* 各ノードのFSDを求める関数 *)
let calc_fsd (ast : Syntax.ast) (prog : Module.program) : (int,int) Hashtbl.t = 
    let graph = construct_graph ast prog in

    (* test output*)
    print_endline "-----> Graph";
    Hashtbl.iter
        (fun key set -> Printf.printf "%d : " key; Utils.print_set (module IntSet) set print_int; print_newline ())
        graph;
    print_endline "Graph <-----";

    let fsd = Hashtbl.create 128 in
    let rec dfs (cur : int) : int =
        if (Hashtbl.mem fsd cur) then (Hashtbl.find fsd cur)
        else begin
            let children = Hashtbl.find graph cur in
            let fsd_value =
                if (IntSet.is_empty children) then 0
                else (((IntSet.map dfs children) |> IntSet.max_elt) + 1)
            in
            Hashtbl.replace fsd cur fsd_value;
            fsd_value
        end
    in
    Hashtbl.iter (fun k _ -> let fsdval = dfs k in Hashtbl.replace fsd k fsdval) graph;
    fsd

(* 各距離のノードを集約する関数 *)
let collect_same_fsd (ast : Syntax.ast) (prog : Module.program) : (int list) array = 
    let fsd_table = calc_fsd ast prog in
    let max_dist = Hashtbl.fold (fun _ v acc -> max v acc) fsd_table 0 in
    Printf.printf "Max_distance : %d\n" max_dist;
    let dist_array = Array.make (max_dist + 1) [] in
    Hashtbl.iter
        (fun k v -> dist_array.(v) <- dist_array.(v) @ [k])
        fsd_table;
    dist_array
