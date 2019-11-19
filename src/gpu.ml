open Target
open Syntax

module StringSet = Set.Make(String);;

let self_index_expr = 
  "int self = blockIdx.x * blockDim.x + threadIdx.x;"

(* let rec transform_to_c (expr : Syntax.expr) : Target.ast * Target.ast = *) 
(*   match expr with *) 
(*   | GSelf -> (Empty, Const("self")) *)
(*   | GConst(c) -> (Empty,Const c) *)
(*   | Gid(i) -> (Empty, Var i) *)
(*   | Gbin(op,expr1,expr2) -> *)
(*       let op_symbol = Syntax.string_of_binop op in *)
(*       let pre1, post1 = transform_to_c expr1 in *)
(*       let pre2, post2 = transform_to_c expr2 in *)



let generate_gnode_update_kernel (name : string) (expr : Syntax.gexpr) (program: Module.program) : string = 
  let modification = "__global__" in
  let kernel_name = Printf.sprintf "%s_kernel" name in
  let rec collect_node_deps e = (* TODO: fix. GNodeからアクセスするNodeを列挙しているが、名前しか取っていないので型が必要 *)
    match e with
    | GSelf -> StringSet.empty
    | GConst(_) -> StringSet.empty
    | Gid(id) -> 
        if (List.exists (String.equal id) program.node) then (StringSet.singleton id)
        else StringSet.empty
    | GAnnot(id,_) -> 
        if (List.exists (String.equal id) program.node) then (StringSet.singleton (id ^ "_at_last")) (* めんどいから適当に_at_lastとか付けてみた *)
        else StringSet.empty
    | Gbin(_,e1,e2) -> 
        StringSet.union (collect_node_deps e1) (collect_node_deps e2)
    | GApp(_,el) -> 
        List.fold_left (fun acc elm -> StringSet.union acc (collect_node_deps elm)) StringSet.empty el
    | Gif (ce,e1,e2) -> 
        StringSet.union (collect_node_deps ce) (StringSet.union (collect_node_deps e1) (collect_node_deps e2))
    | GAnnot (_, _) -> 
        StringSet.empty
  in
  let node_set = collect_node_deps expr in
  let args = "(" ^ (String.concat ", " (StringSet.elements node_set)) ^ ")" in
  modification ^ " " ^ kernel_name ^ args ^ ";"
