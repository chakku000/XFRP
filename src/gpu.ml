open Target
open Syntax
module StringSet = Set.Make (String)
module IntSet = Set.Make (Int)

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


(* The kernel function for updating gpu node array requires following information for argument. *)
(*    - CPU Node and its @last *)
(*    - CPU Node array and its @last *)
(*    - GPU Node array and its @last *)
let collect_argument (gexpr : Syntax.gexpr) (program : Module.program) = 
  (* This function `collect_single_node` collect the single cpu node contained in the expression. *)
  (* The return value is 2-tuple. The first is the present node and the second is the @last node. *)
  let rec collect_single_node e = match e with
    | GSelf -> (IntSet.empty, IntSet.empty)
    | GConst _ -> (IntSet.empty, IntSet.empty)
    | Gid symbol ->
        let nodeid = Hashtbl.find program.id_table symbol in
        let first = if (IntSet.mem nodeid program.single_nodes) then IntSet.singleton nodeid else IntSet.empty in
        (first, IntSet.empty)
    | GAnnot (symbol, _) -> 
        let nodeid = Hashtbl.find program.id_table symbol in
        let second = if IntSet.mem nodeid program.single_nodes then IntSet.singleton nodeid else IntSet.empty in
        (IntSet.empty, second)
    | GIdAt (_ , ge) -> collect_single_node ge
    | GIdAtAnnot (_, ge, _) -> collect_single_node ge
    | Gbin (_, ge1, ge2) ->
        let f1, s1 = collect_single_node ge1 in
        let f2, s2 = collect_single_node ge2 in
        (IntSet.union f1 f2, IntSet.union s1 s2)
    | GApp (_, args) -> 
        List.fold_left
          (fun (accf,accs) arg_ge -> 
            let f, s = collect_single_node arg_ge in
            (IntSet.union accf f, IntSet.union accs s))
          (IntSet.empty, IntSet.empty) (* accumulator *)
          args
    | Gif (ge1, ge2, ge3) -> 
        List.fold_left
          (fun (accf,accs) arg_ge -> 
            let f, s = collect_single_node arg_ge in
            (IntSet.union accf f, IntSet.union accs s))
          (IntSet.empty, IntSet.empty) (* accumulator *)
          [ge1; ge2; ge3]
  in
  let rec collect_node_array e = match e with
  | GSelf -> (IntSet.empty, IntSet.empty)
  | GConst _ -> (IntSet.empty, IntSet.empty)
  | Gid _ -> (IntSet.empty, IntSet.empty)
  | GAnnot _ ->  (IntSet.empty, IntSet.empty)
  | GIdAt (symbol, ge) -> 
      let set = if (List.mem symbol program.gnode)
                  then IntSet.empty
                  else IntSet.singleton (Hashtbl.find program.id_table symbol)
      in
      let f,s = collect_node_array ge in
      (IntSet.union f set, s)
  | GIdAtAnnot (symbol, ge, _) ->
      let set = if (List.mem symbol program.gnode)
                  then IntSet.empty
                  else IntSet.singleton (Hashtbl.find program.id_table symbol)
      in
      let f,s = collect_node_array ge in
      (IntSet.union f set, s)
  | Gbin (_, ge1, ge2) -> 
      let f1, s1 = collect_node_array ge1 in
      let f2, s2 = collect_node_array ge2 in
      (IntSet.union f1 f2, IntSet.union s1 s2)
  | GApp (_, args) -> 
      List.fold_left
        (fun (accf, accs) arg_ge -> 
          let f, s = collect_node_array arg_ge in
          (IntSet.union accf f, IntSet.union accs s))
        (IntSet.empty, IntSet.empty)
        args
  | Gif (ge1, ge2, ge3) ->
      List.fold_left
        (fun (accf,accs) arg_ge -> 
          let f, s = collect_node_array arg_ge in
          (IntSet.union accf f, IntSet.union accs s))
        (IntSet.empty, IntSet.empty)
        [ge1; ge2; ge3]
  in
  let rec collect_gnode e = match e with
  | GSelf -> (IntSet.empty, IntSet.empty) 
  | GConst _ -> (IntSet.empty, IntSet.empty)
  | Gid _ -> (IntSet.empty, IntSet.empty)
  | GAnnot _ -> (IntSet.empty, IntSet.empty)
  | GIdAt (symbol, ge) -> 
      let set = if (List.mem symbol program.gnode)
                  then IntSet.singleton (Hashtbl.find program.id_table symbol)
                  else IntSet.empty
      in
      let f,s = collect_node_array ge in
      (IntSet.union f set, s)
  | GIdAtAnnot (symbol, ge, _) ->
      let set = if (List.mem symbol program.gnode)
                  then  IntSet.singleton (Hashtbl.find program.id_table symbol)
                  else IntSet.empty
      in
      let f,s = collect_node_array ge in
      (IntSet.union f set, s)
  | Gbin (_, ge1, ge2) -> 
      let f1, s1 = collect_gnode ge1 in
      let f2, s2 = collect_gnode ge2 in
      (IntSet.union f1 f2, IntSet.union s1 s2)
  | GApp (_, args) -> 
      List.fold_left
        (fun (accf, accs) arg_ge -> 
          let f, s = collect_gnode arg_ge in
          (IntSet.union accf f, IntSet.union accs s))
        (IntSet.empty, IntSet.empty)
        args
  | Gif (ge1, ge2, ge3) ->
      List.fold_left
        (fun (accf,accs) arg_ge -> 
          let f, s = collect_gnode arg_ge in
          (IntSet.union accf f, IntSet.union accs s))
        (IntSet.empty, IntSet.empty)
        [ge1; ge2; ge3]
  in
  (collect_single_node gexpr, collect_node_array gexpr, collect_gnode gexpr)


let generate_gnode_update_kernel (name : string) (expr : Syntax.gexpr)
    (ast : Syntax.ast) (program : Module.program) : string =
  let kernel_name = Printf.sprintf "__global__ void %s_kernel(){" name in
  let id_to_type_table = Utils.create_id_type_dict ast in
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
    | GIdAt (i, idx) ->
        StringSet.empty
    | GIdAtAnnot (i, idx, _) ->
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
    | GSelf | GConst _ | Gid _ ->
        StringSet.empty
    | GAnnot (id, _) ->
        if List.exists (String.equal id) program.node then
          StringSet.singleton id
        else StringSet.empty
    | GIdAt _ | GIdAtAnnot _ ->
        StringSet.empty
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
    | GSelf | GConst _ | Gid _ | GAnnot _ ->
        StringSet.empty
    | GIdAt (i, _) ->
        StringSet.singleton i
    | GIdAtAnnot _ ->
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
  let rec collect_gnode_atlast_deps e =
    match e with
    | GSelf | GConst _ | Gid _ | GAnnot _ | GIdAt _ ->
        StringSet.empty
    | GIdAtAnnot (id, _, _) ->
        StringSet.singleton id
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
  let gnode_atlast_ast = collect_gnode_atlast_deps expr in
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
  let gpu_atlast_args =
    StringSet.elements gnode_atlast_ast
    |> List.map (fun nd ->
           Printf.sprintf "%s* %s_ATLAST"
             (Hashtbl.find id_to_type_table nd |> Type.of_string)
             nd)
    |> String.concat "\n"
  in
  let args =
    (* 順序は @lastがついてないノード -> @lastがついているノード -> gnodeの引数 となる. それぞれの中では辞書順で並んでいる *)
    "("
    ^ String.concat ","
        (List.filter
           (fun l -> String.length l > 0)
           [normal_args; last_args; gpu_args; gpu_atlast_args])
    ^ ")"
  in
  kernel_name ^ args ^ "{\n" ^ "\t" ^ self_index_expr
  ^ "\n" ^ "}"


(* Return the update function of gpu node array. *)
(* For the gpu node array `x`, this function reaturns the function `x_update()`. *)
let generate_gpu_node_array_update (name : string) (gexpr : Syntax.gexpr) (ast : Syntax.ast) (program : Module.program) = 
  let kernel_function = generate_gnode_update_kernel name gexpr ast program in
  let declare = Printf.sprintf "void %s_update(){" name in
  let kernel_arguments = collect_argument gexpr program in
  let single, nodearray, gnode = kernel_arguments in
  let single_now, single_last = single in
  let array_now, array_last = nodearray in
  let gnode_now, gnode_last = gnode in
  Printf.eprintf "===== GNode %s =====\n"  name;
  Printf.eprintf "single(now) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  single_now;
  Printf.eprintf "\n";
  Printf.eprintf "single(last) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  single_last;
  Printf.eprintf "\n";
  Printf.eprintf "array(now) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  array_now;
  Printf.eprintf "\n";
  Printf.eprintf "array(last) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  array_last;
  Printf.eprintf "\n";
  Printf.eprintf "gnode(now) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  gnode_now;
  Printf.eprintf "\n";
  Printf.eprintf "gnode(last) : ";
  IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  gnode_last;
  Printf.eprintf "\n";
  Utils.concat_without_empty "\n" [kernel_function; declare; "}"]
