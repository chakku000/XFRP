open Batteries
open Syntax
module IntSet = Set.Make (Int)

(* CPUコアへのノードの割当に使うデータ *)
(* 通常のノードならばSingleでノードIDを保存する *)
(* ノード配列ならばArrayで区間も保存する *)
type assign_node =
    | Single of int 
    | Array of int * (int * int) (* ノードID * (開始インデックス * 修了インデックス) *)

(* 依存グラフの構築 *)
(* 返り値はノードのIDを使った隣接リスト  *)
(* a->bと依存関係がある場合, graph[a] = {b} となる *)
let construct_graph (ast : Syntax.ast) (program : Module.program) : (int,IntSet.t) Hashtbl.t =
  let ptbl = Hashtbl.create 128 in (* 親ノードの集合. a->bならptbl[b]={a} *)
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

  (* ptbl(親ノードの隣接リスト)を構築 *)
  List.iter
    (function
      | Node ((node, t), _, e) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_id_of_cpu e)
      | NodeA ((node, t), _, _, e) ->
          let id = Hashtbl.find program.id_table node in
          Hashtbl.add ptbl id (collect_id_of_cpu e)
      | GNode _ -> () (* TODO GPUノードに対しても実装する必要がある *)
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

(* 各ノードのFSDを求める関数 *)
let calc_fsd (ast : Syntax.ast) (prog : Module.program) : (int,int) Hashtbl.t = 
    let graph = construct_graph ast prog in

    (* test output*)
    (* print_endline "-----> Graph";
    Hashtbl.iter
        (fun key set -> Printf.printf "%d : " key; Utils.print_set (module IntSet) set print_int; print_newline ())
        graph;
    print_endline "Graph <-----"; *)

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
(* 返り値retにたいしてret[d]でFSDがdのノードの集合 *)
let collect_same_fsd (ast : Syntax.ast) (prog : Module.program) : (int list) array = 
    let fsd_table = calc_fsd ast prog in
    let max_dist = Hashtbl.fold (fun _ v acc -> max v acc) fsd_table 0 in
    (* test output *)
    (* Printf.printf "Max_distance : %d\n" max_dist; *)
    let dist_array = Array.make (max_dist + 1) [] in
    Hashtbl.iter
        (fun k v -> dist_array.(v) <- dist_array.(v) @ [k])
        fsd_table;
    dist_array

(*各FSDの値でスケジューリングする 
  引数のnodesはノードのidリスト. IDリストなのでprogram.info_tableから直接検索できる
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
      info.number > 1) nodes in

    (* 各coreの更新するノード群 *)
    let cores = Array.make thread [] in
    (* start_core_index番目のコアからノードを振り当てる.
     * これは更新するノードの数がThread数の倍数でないことがあるので必要 *)
    let start_core_index = ref 0 in 

    (* スケジューリング方針(Arrayの割当) *)
    (* 各コアの計算コストをできるだけ同じにするために、各ノード配列ごとにそれぞれのコアに分配する *)
    List.iter (* 各ノード配列をコアに割り当てる *)
        (fun nodeid ->
            let info = Hashtbl.find program.info_table nodeid in
            let node_number = info.number in (* *)
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
                (List.init thread identity); (* [0,thread)のリスト *)
            start_core_index := re mod thread
        ) arrays;

    (* スケジューリング方針(Signleの割当) *)
    List.iter (* 各ノードをコアに割り当てる *)
        (fun nodeid -> 
            cores.(!start_core_index) <- cores.(!start_core_index) @ [Single(nodeid)];
            start_core_index := (!start_core_index + 1) mod thread)
        singles;

    (* 各コアが担当するノードの情報を返す *)
    cores


(* どのノードをどのCPUコアが更新するかを決定する *)
let assign_to_cpu (ast : Syntax.ast) (program: Module.program) (thread : int) : (int * ((assign_node list) array)) array =
  let dist_array = collect_same_fsd ast program in (* dist_array[i] : FSDがiのノードの集合 *)
  (* デバッグ出力 *)
  (* let max_distance = Array.length dist_array -1 in *)(*{{{*)
  (* Array.iteri *)
  (*       (fun i lst -> *)
  (*         let assigned = schedule_fsd i lst thread program in *)
  (*         Array.iteri (1* 出力 *1) *)
  (*               (fun id lst -> *)
  (*                 Printf.printf "core%d : " id; *)
  (*                   List.iter (function *)
  (*                     | Single id -> Printf.printf "%d," id *)
  (*                       | Array (id,(s,t)) -> Printf.printf "%d(%d,%d)," id s t) *)
  (*                   lst; *)
  (*                   Printf.printf "\n" *)
  (*               ) assigned) *)
  (*       dist_array; *)(*}}}*)

  (* (FSDの値, 各CPUコアが担当するノードのリストの配列)を返す *)
  Array.mapi
    (fun i (* fsd *) node_list (* fsdに相当するノードのリスト *) ->
      (* assigned[th]: スレッドthがFSD=iで担当するノードの配列*)
      let assigned : (assign_node list) array = schedule_fsd i node_list thread program in
      (* (FSD,各コアのFSD(i)で担当するノードのリスト)のtuple*)
      (i, assigned)
    ) dist_array
