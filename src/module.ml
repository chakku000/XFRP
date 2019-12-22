open Syntax

exception Unreachable of string

module IntSet = Set.Make (Int)

let print_intset st =
  print_char '{' ;
  IntSet.iter (fun i -> print_int i ; print_char ',') st ;
  print_char '}'

type node_t =
    {
        name : string;
        t : Type.t;
        number : int;
    }

type program =
  { id: Syntax.moduleid
  ; input: Syntax.id list
  ; (* list of identifier of input node *)
    output: Syntax.id list
  ; (* list of identifier of output node *)
    node: Syntax.id list
  ; (* list of identifier of nodes. It includes input, output, and internal nodes. Not that gnode is not contained *)
    gnode: Syntax.id list
  ; (* list of gpu node *)
    id_table: (string, int) Hashtbl.t
  ;
    info_table : (int,node_t) Hashtbl.t
  }

(* Node/Gnode(string)からID(int)への辞書を構築する関数 *)
let construct_id_table (nodes : Syntax.id list) (gnodes : Syntax.id list) :
    (string, int) Hashtbl.t =
  let table = Hashtbl.create (List.length nodes + List.length gnodes) in
  List.iteri (fun i n -> Hashtbl.add table n i) (nodes @ gnodes) ;
  table

(* 各ノード(ID)とそのノードの情報のテーブルを構築する関数 *)
let construct_nodeinfo_table (ast : Syntax.ast)
                             (id_table : (string,int) Hashtbl.t)
                             : (int,node_t) Hashtbl.t = 
    let table = Hashtbl.create 128 in

    (* Internal/Output Nodeをテーブルに追加 *)
    List.iter
        (function
            | Node ((name,t),_,_) ->
                let id = Hashtbl.find id_table name in
                Hashtbl.add table id { name; t; number=1; }
            | NodeA ((name,t),number,_,_) -> 
                let id = Hashtbl.find id_table name in
                Hashtbl.add table id { name; t; number; }
            | GNode ((name,t), number, _, _) -> 
                let id = Hashtbl.find id_table name in
                Hashtbl.add table id { name; t; number; })
        ast.definitions;

    (* Input Nodeがまだテーブルに含まれていないので追加 *)
    List.iter
        (function
            | Single (name,t) ->
                let id = Hashtbl.find id_table name in
                Hashtbl.add table id { name; t; number=1; }
            | Array ((name,t),number) -> 
                let id = Hashtbl.find id_table name in
                Hashtbl.add table id { name; t; number; })
        ast.in_nodes;
    table

(* ASTから依存関係を抽出 *)
let ast_to_program : Syntax.ast -> program =
 fun ast ->
  let input = List.map Syntax.name_of_cpunode ast.in_nodes in
  let output = List.map Syntax.name_of_cpunode ast.out_nodes in
  (* nodeのリストを構築 *)
  let node =
    let internal_and_output =
      List.filter_map
        (function
          | Node ((i, _), _, _) ->
              Some i
          | NodeA ((i, _), _, _, _) ->
              Some i
          | GNode _ ->
              None)
        ast.definitions
    in
    input @ internal_and_output
  in
  (* gpu nodeのリストを構築 *)
  let gnode =
    List.filter_map
      (function GNode ((i, _), _, _, _) -> Some i | _ -> None)
      ast.definitions
  in
  let id_table = construct_id_table node gnode in
  let info_table = construct_nodeinfo_table ast id_table in
  (* Hashtbl.iter (fun n i -> Printf.printf "%d %s\n" i n) id_table ; *)
  {id= ast.module_id; input; output; node; gnode; id_table; info_table; }

let print_program prog : unit =
  Printf.printf "Module : %s\n" prog.id ;
  Printf.printf "Input : %s\n" (String.concat ", " prog.input) ;
  Printf.printf "Output: %s\n" (String.concat ", " prog.output) ;
  Printf.printf "Node : %s\n" (String.concat ", " prog.node) ;
  Printf.printf "GNode : %s\n" (String.concat ", " prog.gnode)

(* プログラム中のノードの数. Gnodeは展開前の1つで計算する *)
let node_num prog : int = Hashtbl.length prog.id_table
