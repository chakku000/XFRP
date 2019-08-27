open Syntax

type node_t = {
  id : string;
  typ : Type.t option ref;
  init : Syntax.expr option;
  expr : Syntax.expr;
}

type program = {
  id : moduleid;
  input : id list; (* list of identifier of input node *)
  output : id list;(* list of identifier of output node *)
  (* gpu : id list;   (1* list of identifierof gpu node *1) *)
  (* node : id list;  (1* list of identifier of all node. *1) *)
}


let ast_to_program : Syntax.ast -> program =  fun abstract_syntax_tree ->
  let input = List.map (fun (i,t) -> i) abstract_syntax_tree.in_nodes in
  let output = List.map (fun (i,t) -> i) abstract_syntax_tree.out_nodes in
  (* let node = List.filter (function |Node -> true | GPU -> true | _ -> false) *) 
  {
    id = abstract_syntax_tree.module_id;
    input = input;
    output = output;
  }

let print_program prog : unit = 
  Printf.printf "Module : %s\n" prog.id;
  Printf.printf "Input : %s\n" (String.concat ", " prog.input);
  Printf.printf "Output: %s\n" (String.concat ", " prog.output)
