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


(* This function returns the head of kernel function. *)
(* It means that this returns, for example, "void update_kernel(int arg1, int arg2,...){" *)
let generate_kernel_head (name : string) (program : Module.program)
                                          (arg_single_now : IntSet.t) (arg_single_last : IntSet.t)
                                          (arg_array_now : IntSet.t) (arg_array_last : IntSet.t)
                                          (arg_gnode_now : IntSet.t) (arg_gnode_last : IntSet.t)
                                         : string = 
  let types_single_now = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = Type.of_string info.t in
        let arg_name = info.name in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_single_now
      ""
  in
  let types_single_last = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = Type.of_string info.t in
        let arg_name = info.name ^ "_ATLAST" in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_single_last
      ""
  in
  let types_array_now = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = (Type.of_string info.t) ^ "*" in
        let arg_name = info.name in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_array_now
      ""
  in
  let types_array_last = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = (Type.of_string info.t) ^ "*" in
        let arg_name = info.name ^ "_ATLAST" in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_array_last
      ""
  in
  let types_gnode_now = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = (Type.of_string info.t) ^ "*" in
        let arg_name = info.name in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_gnode_now
      ""
  in
  let types_gnode_last = 
    IntSet.fold
      (fun id acc ->
        let info = Hashtbl.find program.info_table id in
        let t = (Type.of_string info.t) ^ "*" in
        let arg_name = info.name ^ "_ATLAST" in
        let arg = t ^ " " ^ arg_name in
        if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
      arg_gnode_last
      ""
  in
  let args = Utils.concat_without_empty ", " [types_single_now; types_single_last; types_array_now; types_array_last; types_gnode_now; types_gnode_last]
  in
  Printf.sprintf "__global__ void %s_kernel(%s){" name args

(* Generate the cuda kernel for updating the gpu node. *)
(* The argument `head` is the first line of the definition of kernel *)
let generate_gnode_update_kernel (name : string) (gexpr : Syntax.gexpr) (ast : Syntax.ast) (program : Module.program) (head: string) : string =
  let kernel_name = head in
  let self_index_expr = "\tint self = blockIdx.x * blockDim.x + threadIdx.x;" in
  let self_restriction = Printf.sprintf "\tif(self < %d){" (let id = Hashtbl.find program.id_table name in let info = Hashtbl.find program.info_table id in info.number ) in
  let cuda_ast_pre, cuda_ast_post = Target.convert_from_gexpr_to_cudaAST gexpr program in
  let cuda_code_pre = Target.convert_cudaAST_to_code cuda_ast_pre 2 in
  let cuda_code_post = Target.convert_cudaAST_to_code cuda_ast_post 0 in
  let cuda_code_post_assign = Printf.sprintf "\t\t%s[self] = %s;" name cuda_code_post in
  Utils.concat_without_empty "\n" [kernel_name; self_index_expr; self_restriction; cuda_code_pre; cuda_code_post_assign ; "\t}"; "}"]


(* Return the update function of gpu node array. *)
(* For the gpu node array `x`, this function reaturns the function `x_update()`. *)
(* transfer_host_set : The set of gpu node which is required to transfer the data from device to host. *)
let generate_gpu_node_array_update (name : string) (gexpr : Syntax.gexpr) (ast : Syntax.ast) (program : Module.program) (transer_host_set : IntSet.t) :string = 
  let gnodid = Hashtbl.find program.id_table name in
  let info = Hashtbl.find program.info_table gnodid in
  (* Declaration of update function that is called from CPU. *)
  let declare = Printf.sprintf "void %s_update(){" name in

  (* Argument Information for Kernel Function. *)
  let kernel_arguments = collect_argument gexpr program in
  let single, nodearray, gnode = kernel_arguments in
  let single_now, single_last = single in
  let array_now, array_last = nodearray in
  let gnode_now, gnode_last = gnode in
  (* Printf.eprintf "===== GNode %s =====\n"  name; *)(*{{{*)
  (* Printf.eprintf "single(now) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  single_now; *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "single(last) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  single_last; *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "array(now) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  array_now; *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "array(last) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  array_last; *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "gnode(now) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  gnode_now; *)
  (* Printf.eprintf "\n"; *)
  (* Printf.eprintf "gnode(last) : "; *)
  (* IntSet.iter (fun i -> let info = Hashtbl.find program.info_table i in Printf.eprintf "%s, " info.name)  gnode_last; *)
  (* Printf.eprintf "\n"; *)(*}}}*)

  (* Create Kernel Function *)
  let kernel = 
    let head = generate_kernel_head name program single_now single_last array_now array_last gnode_now gnode_last in
    generate_gnode_update_kernel name gexpr ast program head
  in

  (* Call Kernel Function *)
  let call_kernel : string =
    let required_thread_num = info.number
    in
    (* Construct the arguments of the kernel function *)
    let arguments =(*{{{*)
      let arg_single_now = IntSet.fold
                            (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                            let arg = Printf.sprintf "%s[turn]" info.name in
                                            if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                            single_now
                            ""
      in
      let arg_single_last = IntSet.fold
                              (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                              let arg = Printf.sprintf "%s[turn^1]" info.name in
                                              if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                              single_last
                              ""
      in
      let arg_array_now = IntSet.fold
                            (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                            let arg = Printf.sprintf "%s[turn]" info.name in
                                            if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                            array_now
                            ""
      in
      let arg_array_last = IntSet.fold
                            (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                            let arg = Printf.sprintf "%s[turn]" info.name in
                                            if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                            array_last
                            ""
      in
      let arg_gnode_now = IntSet.fold
                            (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                            let arg = Printf.sprintf "%s[turn]" info.name in
                                            if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                            gnode_now
                            ""
      in
      let arg_gnode_last = IntSet.fold
                            (fun id acc ->  let info = Hashtbl.find program.info_table id in
                                            let arg = Printf.sprintf "%s[turn]" info.name in
                                            if acc = "" then arg else Printf.sprintf "%s, %s" acc arg)
                            gnode_last
                            ""
      in
      Utils.concat_without_empty ", " [arg_single_now; arg_single_last; arg_array_now; arg_array_last; arg_gnode_now; arg_gnode_last]
    in(*}}}*)
    let dim_block = 512 in
    let dim_grid = (required_thread_num + 511) / 512 in
    Printf.sprintf "\t%s_kernel<<<%d,%d>>>(%s);" name dim_grid dim_block arguments
  in
  let transfer_to_host : string =
    if IntSet.mem gnodid transer_host_set
    then
      let dst = Printf.sprintf "%s[turn]" name in
      let src = Printf.sprintf "g_%s[turn]" name in
      let size = Printf.sprintf "%d * sizeof(%s)" info.number (Type.of_string info.t) in
      Printf.sprintf "\tcudaMemcpy(%s,%s,%s, cudaMemcpyDeviceToHost);" dst src size 
    else ""
  in
  Utils.concat_without_empty "\n" [kernel ; declare; call_kernel; transfer_to_host; "}"]
