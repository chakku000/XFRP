exception Unreachable of string

module IntSet = Set.Make (Int)
let print_intset st = 
  print_char '{';
  IntSet.iter (fun i -> print_int i; print_char ',') st;
  print_char '}'

type node_t =
  { id: string
  ; typ: Type.t option ref
  ; init: Syntax.expr option
  ; expr: Syntax.expr }

type program =
  { id: Syntax.moduleid
  ; input: Syntax.id list
  ; (* list of identifier of input node *)
    output: Syntax.id list
  ; (* list of identifier of output node *)
    node: Syntax.id list
  ; (* list of identifier of nodes. It includes input, output, and othre nodes. *)
    gnode: Syntax.id list
  ; (* list of gpu node *)
    id_table: (string, int) Hashtbl.t
  ; (* Dictinary from node and gnode to index *)
    graph: (int, IntSet.t) Hashtbl.t (* graph[i] is the set of parent nodes *)
  }


(* Node/Gnode(string)からID(int)への辞書を構築する関数 *)
let construct_id_table (nodes : Syntax.id list) (gnodes : Syntax.id list) :
    (string, int) Hashtbl.t =
  let table = Hashtbl.create (List.length nodes + List.length gnodes) in
  List.iteri (fun i n -> Hashtbl.add table n i) (nodes @ gnodes) ;
  table


(* 依存関係を表すグラフ(隣接リスト)を構築 *)
(* 依存関係は親ノードの集合を持つ *)
let construct_graph :
    Syntax.ast -> (string, int) Hashtbl.t -> (int, IntSet.t) Hashtbl.t =
 fun ast idtable ->
  let graph : (int, IntSet.t) Hashtbl.t = Hashtbl.create 1024 in
  (* 通常ノードの親ノードの集合 *)
  let rec nodeexpr_to_parent : Syntax.expr -> IntSet.t = function
    | Eid id ->
        Hashtbl.find idtable id |> IntSet.singleton
    | Ebin (_, e1, e2) ->
        IntSet.union (nodeexpr_to_parent e1) (nodeexpr_to_parent e2)
    | _ ->
        IntSet.empty
  in
  let rec gnodeexpr_to_parent : Syntax.gexpr -> IntSet.t = function
    | Gid id ->
        Hashtbl.find idtable id |> IntSet.singleton
    | Gbin (_, e1, e2) ->
        IntSet.union (gnodeexpr_to_parent e1) (gnodeexpr_to_parent e2)
    | _ ->
        IntSet.empty
  in
  let to_parentset : Syntax.definition -> IntSet.t = function
    | Node (_, _, e) ->
        nodeexpr_to_parent e
    | GNode (_, _, _, e) ->
        gnodeexpr_to_parent e
  in
  (* get_name: ノードの定義から名前を取ってくる *)
  let get_name : Syntax.definition -> Syntax.id = function
    | Node ((i, _), _, _) ->
        i
    | GNode ((i, _), _, _, _) ->
        i
  in
  (* Utils.print_hstbl idtable (fun x -> print_string x) (fun x -> print_int x) ; *)
  let update_table def =
    let id = Hashtbl.find idtable (get_name def) in
    Hashtbl.add graph id (to_parentset def)
  in
  List.iter update_table ast.definitions ;
  graph

(* ASTから依存関係を抽出 *)
let ast_to_program : Syntax.ast -> program =
 fun ast ->
  let input = List.map (fun (i, t) -> i) ast.in_nodes in
  let output = List.map (fun (i, t) -> i) ast.out_nodes in
  (* nodeのリストを構築 *)
  let node =
    let filter_function = function
      (* Nodeだけ取得する関数 *)
      | Syntax.Node (_, _, _) ->
          true
      | _ ->
          false
    in
    let node_list = List.filter filter_function ast.definitions in
    let map_function = function
      | Syntax.Node ((i, t), _, _) ->
          i
      | _ ->
          raise (Unreachable "unreachable code")
    in
    input @ List.map map_function node_list
    (* definitionsのNodeにinputノードを追加したもの*)
  in
  (* gpu nodeのリストを構築 *)
  let gnode =
    let filter_function = function
      | Syntax.GNode (_, _, _, _) ->
          true
      | _ ->
          false
    in
    let map_function = function
      | Syntax.GNode ((i, t), _, _, _) ->
          i
      | _ ->
          raise (Unreachable "unreachable code")
    in
    let filtered = List.filter filter_function ast.definitions in
    List.map map_function filtered
  in
  let id_table = construct_id_table node gnode in
  let graph : (int, IntSet.t) Hashtbl.t = construct_graph ast id_table in
  Hashtbl.iter (fun n i -> Printf.printf "%d %s\n" i n) id_table ;
  {id= ast.module_id; input; output; node; gnode; id_table; graph}

let print_program prog : unit =
  Printf.printf "Module : %s\n" prog.id ;
  Printf.printf "Input : %s\n" (String.concat ", " prog.input) ;
  Printf.printf "Output: %s\n" (String.concat ", " prog.output) ;
  Printf.printf "Node : %s\n" (String.concat ", " prog.node) ;
  Printf.printf "GNode : %s\n" (String.concat ", " prog.gnode)

(* プログラム中のノードの数. Gnodeは展開前の1つで計算する *)
let node_num prog : int = 
  Hashtbl.length prog.id_table
