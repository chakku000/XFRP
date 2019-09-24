exception Unreachable of string

type node_t = {
  id : string;
  typ : Type.t option ref;
  init : Syntax.expr option;
  expr : Syntax.expr;
}

type program = {
  id : Syntax.moduleid;
  input : Syntax.id list; (* list of identifier of input node *)
  output : Syntax.id list;(* list of identifier of output node *)
  node : Syntax.id list; (* list of identifier of nodes. It includes input, output, and othre nodes. *)
  gnode : Syntax.id list; (* list of gpu node *)
}


let ast_to_program : Syntax.ast -> program =  fun ast ->
  let input = List.map (fun (i,t) -> i) ast.in_nodes in
  let output = List.map (fun (i,t) -> i) ast.out_nodes in
  (* nodeの構築 *)
  let node = 
    let filter_function = function (* Nodeだけ取得する関数 *)
      | Syntax.Node (_,_,_)  -> true
      | _ -> false in
    let node_list = List.filter filter_function ast.definitions in
    let map_function = function
      | Syntax.Node ((i,t),_,_) -> i
      | _ -> raise(Unreachable("unreachable code")) in
    input @ List.map map_function node_list  (* definitionsのNodeにinputノードを追加したもの*)
  in
  (* gpu nodeの構築 *)
  let gnode = 
    let filter_function = function
      | Syntax.GNode (_,_,_,_) -> true
      | _ -> false in
    let map_function = function
      | Syntax.GNode ((i,t),_,_,_) -> i
      | _ -> raise(Unreachable("unreachable code")) in
    let filtered = List.filter filter_function ast.definitions in
    List.map map_function filtered
  in
  {
    id = ast.module_id;
    input = input;
    output = output;
    node = node;
    gnode = gnode;
  }

let print_program prog : unit = 
  Printf.printf "Module : %s\n" prog.id;
  Printf.printf "Input : %s\n" (String.concat ", " prog.input);
  Printf.printf "Output: %s\n" (String.concat ", " prog.output);
  Printf.printf "Node : %s\n" (String.concat ", " prog.node);
  Printf.printf "GNode : %s\n" (String.concat ", " prog.gnode)
