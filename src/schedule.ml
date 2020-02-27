open Syntax
module IntSet = Set.Make (Int)

(* CPUコアへのノードの割当に使うデータ *)
(* 通常のノードならばSingleでノードIDを保存する *)
(* ノード配列ならばArrayで区間も保存する *)
type assign_node =
    | Single of int 
    | Array of int * (int * int) (* ノードID * (開始インデックス * 修了インデックス) *)
    | GArray of int

(* Construct dependency graph
 * This function returns the adjency list of the graph. The list uses the `node id`.
 * If there is a dependency such as `a` -> `b`, `graph` which the data this function returns is graph[a] = {b}.
 * *)
let construct_graph (ast : Syntax.ast) (program : Module.program) : (int,IntSet.t) Hashtbl.t =
  (* The set of parent nodes. *)
  let ptbl = Hashtbl.create 128 in

  (* Add input node to `ptbl` *)
  (* List.iter (fun inode -> Hashtbl.add ptbl (Hashtbl.find program.id_table inode) IntSet.empty) program.input; *)

  (* The function that collects the set of id. *)
  let rec collect_nodeid expr =
    match expr with
    | ESelf | EConst _ | EAnnot _ | EAnnotA _ ->
        IntSet.empty
    | Eid i ->
        if List.mem i program.input
          then IntSet.empty
          else Hashtbl.find program.id_table i |> IntSet.singleton
    | EidA (i, e) ->
        let index_set = collect_nodeid e in
        let set1 =
          if List.mem i program.input
            then IntSet.empty
            else let id1 = Hashtbl.find program.id_table i in IntSet.singleton id1
        in
        IntSet.union set1 index_set
    | Ebin (_, e1, e2) ->
        IntSet.union (collect_nodeid e1) (collect_nodeid e2)
    | EUni (_, e) ->
        collect_nodeid e
    | EApp (_, args) ->
        List.fold_left
          (fun acc elm -> IntSet.union acc (collect_nodeid elm))
          IntSet.empty args
    | Eif (cond, e1, e2) ->
        IntSet.union (collect_nodeid cond)
          (IntSet.union (collect_nodeid e1) (collect_nodeid e2))
  in
  let rec collect_gnodeid gexpr = 
    match gexpr with
    | GSelf -> IntSet.empty
    | GConst _ -> IntSet.empty
    (* CPUノード *)
    | Gid nodename -> Hashtbl.find program.id_table nodename|> IntSet.singleton
    (* CPUノード+@last *)
    | GAnnot _ -> IntSet.empty
    (* ノード配列に対する参照 *)
    | GIdAt (nodesymbol, ge_index) -> 
        let id = Hashtbl.find program.id_table nodesymbol |> IntSet.singleton in
        let index_set = collect_gnodeid ge_index in
        IntSet.union id index_set
    | GIdAtAnnot _ -> IntSet.empty
    | Gbin (op, ge1, ge2) -> IntSet.union (collect_gnodeid ge1) (collect_gnodeid ge2)
    | GApp (funname, args) -> List.fold_left (fun acc ge -> IntSet.union acc (collect_gnodeid ge)) IntSet.empty args
    | Gif (ge_if, ge_then, ge_else) -> IntSet.union (collect_gnodeid ge_if) (IntSet.union (collect_gnodeid ge_then) (collect_gnodeid ge_else))
  in

  (* ptbl(親ノードの隣接リスト)を構築 *)
  List.iter
    (function
      | Node ((node, t), _, e) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_nodeid e)
      | NodeA ((node, t), _, _, e, _) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_nodeid e)
      | GNode ((nodename,t), _, _, _, ge) -> 
          let id = Hashtbl.find program.id_table nodename in
          Hashtbl.add ptbl id (collect_gnodeid ge)
      | Func _ -> ()
    ) ast.definitions ;

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

(* Calculate FSD of each node. *)
(* returns the Hashtable whose key is the value of node_id and vwhose value is  *)
let get_fsd_hashtbl(ast : Syntax.ast) (prog : Module.program) : (int,int) Hashtbl.t = 
    let graph : (int, IntSet.t) Hashtbl.t = construct_graph ast prog in
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

(* This function collects the node whose fsd values are same. *)
(* The return value `a` is a array. And, the `a[i]` is the list of node whose fsd value is `i` *)
let collect_same_fsd (ast : Syntax.ast) (prog : Module.program) : (int list) array = 
    (* Hashtbl that contains the fsd of each node. *)
    (* The key of tbl is the node of id, and the value is the fsd of the node *)
    let fsd_table = get_fsd_hashtbl ast prog in

    let max_dist = Hashtbl.fold (fun _ v acc -> max v acc) fsd_table 0 in
    let dist_array = Array.make (max_dist + 1) [] in
    Hashtbl.iter
        (fun k v -> dist_array.(v) <- dist_array.(v) @ [k])
        fsd_table;
    dist_array

(*各FSDの値でスケジューリングする 
  引数のnodesはfsdが相当のノードのidリスト. IDリストなのでprogram.info_tableから直接検索できる
  返り値retに対してret[th]がスレッドthの担当するノードのリスト *)
let schedule_fsd (fsd : int) (nodes : int list) (thread : int) (program : Module.program) : (assign_node list) array = 
  (* let node_sum = *) 
  (*   List.fold_left *)
  (*     (fun acc node -> *)
  (*       let nodeinfo = Hashtbl.find program.info_table node in *)
  (*       acc + nodeinfo.number) *)
  (*     0 *)
  (*     nodes *) 
  (*   in *)
  let singles = List.filter (* ノード配列でないノードのIDリスト *)
    (fun node ->
      let info = Hashtbl.find program.info_table node in
      info.number = 1) nodes in
  let arrays = List.filter (* ノード配列のIDリスト *)
    (fun node ->
      let info = Hashtbl.find program.info_table node in
      let is_cpu = List.exists (fun name -> name = info.name) program.node in
      info.number > 1 && is_cpu) nodes in


  (* 各coreの更新するノード群 *)
  (* cores[i] indicates the list of node which are updated by core i. *)
  let cores = Array.make thread [] in
  (* start_core_index番目のコアからノードを振り当てる.
   * これは更新するノードの数がThread数の倍数でないことがあるので必要 *)
  let start_core_index = ref 0 in 

  (* スケジューリング方針(Arrayの割当) *)
  (* 各コアの計算コストをできるだけ同じにするために、各ノード配列ごとにそれぞれのコアに分配する *)
  List.iter (* 各ノード配列をコアに割り当てる *)
        (fun nodeid ->
          let info = Hashtbl.find program.info_table nodeid in
          let node_number = info.number in
          let d = node_number / thread in
          let r = node_number mod thread in
          let (rs,re) = (!start_core_index, !start_core_index + r) in (* 管理の個数が+1される区間[rs,re) *)
          let start_node_id = ref 0 in (* ノード配列の何番目からを管理するか? *)
          List.iter (* CPUコアに割り当てる *)
                (fun coreid ->
                  (* is_plus_one : coreid番目のコアの管理個数が+1されるかどうか *)
                  let is_plus_one = (rs <= coreid && coreid < re) 
      || (rs <= coreid+thread && coreid+thread<re) in
                  (* 管理するノードの個数 *)
                  let number = d + (if is_plus_one then 1 else 0) in
                  (* coreidに相当するコアはノード配列(id)の[sid,eid)を更新する *)
                  let sid = !start_node_id in 
                  let eid = !start_node_id + number in
                  (* cores[coreid](CPUコアcoreidが担当する更新リスト)にノードIDがidの配列の[sid,eid)を追加 *)
                  cores.(coreid) <- cores.(coreid) @ [Array(nodeid,(sid,eid))];
                    (* 次のCPUコアが開始する添字を更新 *)
                    start_node_id := eid)
                (List.init thread (fun id -> id)); (* [0,thread)のリスト *)
            start_core_index := re mod thread
        ) arrays;

    (* スケジューリング方針(Signleの割当) *)
    List.iter (* 各ノードをコアに割り当てる *)
        (fun nodeid -> 
          cores.(!start_core_index) <- cores.(!start_core_index) @ [Single(nodeid)];
            start_core_index := (!start_core_index + 1) mod thread)
        singles;

  (* GPU Node is always updated by CPU core0. *)
  let gpunodes = (* List of GPU Node *)
    List.filter
      (fun node -> 
          let info = Hashtbl.find program.info_table node in
          let is_gpu = List.exists (fun name -> name= info.name) program.gnode in
          is_gpu)
      nodes (* the list of nodes whose fsd value collesponds to fsd value *)
  in
  List.iter
    (fun node_id -> 
      cores.(0) <- cores.(0) @ [GArray(node_id)];)
    gpunodes;

  (* 各コアが担当するノードの情報を返す *)
  cores


    (* どのノードをどのCPUコアが更新するかを決定する *)
let assign_to_cpu (ast : Syntax.ast) (program: Module.program) (thread : int) : (int * ((assign_node list) array)) array =
  let dist_array = collect_same_fsd ast program in (* dist_array[i] : FSDがiのノードの集合 *)

  (* (FSDの値, 各CPUコアが担当するノードのリストの配列)を返す *)
  Array.mapi
    (fun fsd node_list (* fsdに相当するノードのリスト *) ->
      (* assigned[th]: スレッドthがFSD=iで担当するノードの配列*)
      let assigned : (assign_node list) array = schedule_fsd fsd node_list thread program in
  (* (FSD,各コアのFSD(i)で担当するノードのリスト)のtuple*)
      (fsd, assigned)
    ) dist_array
