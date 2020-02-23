open Syntax
open Type

exception TypeError of string
exception Unreachable of string

module IntSet = Set.Make (Int)

module String = struct
  include String

  let is_not_empty s = String.length s > 0
end

(* memo: 現在の実装ではNodeはc_astに変換される. 時間があればtarget.astに変換するようにしたほうが良いかもしれない *)
type c_ast =
  | Empty
  | Const of string
  | Variable of string (* 変数 *)
  | VariableA of string * c_ast (* 配列 *)
  | VarDec of Type.t * string (* 変数宣言 *)
  | Assignment of Type.t option * string * c_ast (* 変数への代入. もしType.tがNoneならば宣言済みの変数. そうでなければ変数宣言と同時に代入 *)
  | Uniop of string * c_ast
  | Binop of string * c_ast * c_ast
  | Call of string * c_ast list
  | If of
      (Type.t * string) * (* If式の結果に対応する変数名とその型 *)
      (c_ast * c_ast) * (* cond式のC言語でのAST(pre,post) *)
      (c_ast * c_ast) * (* then (pre,post) *)
      (c_ast * c_ast) (* else (pre,post) *)
  | CodeList of c_ast list

(* 唯一の識別子を与える *)
let unique_index = ref 0

let get_unique_name () : string =
  let id = string_of_int !unique_index in
  unique_index := !unique_index + 1 ;
  "tmp_" ^ id

(* C言語のASTのpretty printer *)
let rec string_of_c_ast (ast : c_ast) : string =(*{{{*)
  match ast with
  | Empty ->
      "Empty"
  | Const s ->
      Printf.sprintf "Const[ %s ]" s
  | VarDec (t, i) ->
      Printf.sprintf "VarDec[ %s ][ %s ]" (Type.of_string t) i
  | Variable v ->
      Printf.sprintf "Var[ %s ]" v
  | VariableA (i, c) ->
      Printf.sprintf "%s[%s]" i (string_of_c_ast c)
  | Assignment (t, var, ast) ->
      let typename =
        match t with None -> "None" | Some t -> Type.of_string t
      in
      Printf.sprintf "Assignment[ %s ][ %s ][ %s ]" typename var
        (string_of_c_ast ast)
  | Uniop (uniop, e) ->
      let c = string_of_c_ast e in
      Printf.sprintf "%s(%s)" uniop c
  | Binop (op, ast1, ast2) ->
      Printf.sprintf "Binop[ %s ][ %s ][ %s ]" op (string_of_c_ast ast1)
        (string_of_c_ast ast2)
  | Call (f, args) ->
      Printf.sprintf "Call[ %s ][ %s ]" f
        (List.map string_of_c_ast args |> String.concat " , ")
  | If ((_,resvar), (cond_pre, cond_post), _, _) ->
      Printf.sprintf "If(%s) [cond_pre: [%s], cond_post:[%s]] [] []"
        resvar 
        (string_of_c_ast cond_pre)
        (string_of_c_ast cond_post)
  | CodeList codes ->
      List.map string_of_c_ast codes |> String.concat " :: "(*}}}*)

(* Header周り *)
let header_list = ["stdio.h"; "stdlib.h"](*{{{*)
let header_list2 = ["setting.h"]
let header_code () =
  List.map (fun s -> "#include<" ^ s ^ ">") header_list |> String.concat "\n"
let header_code2 () =
  List.map (fun s -> Printf.sprintf "#include \"%s\"" s) header_list2 |> Utils.concat_without_empty "\n"
let macros = ["#define bool char"; "#define true 1"; "#define false 0"]
let macro_code () = Utils.concat_without_empty "\n" macros(*}}}*)

(* ノードの値を保存するグローバルな変数を定義 *)
let global_variable (ast : Syntax.ast) (prg : Module.program) (nodearrays_accessed_from_gnode : IntSet.t) : string =(*{{{*)
  let turn_vairable = "char turn = 0;" in
  let cpunode_to_variable = function
    | Single (i, t) ->
        Printf.sprintf "%s %s[2];" (Type.of_string t) i
    | Array ((i, t), n, _) ->
        let host : string = Printf.sprintf "%s %s[2][%d];" (Type.of_string t) i n in
        let device : string = Printf.sprintf "%s* g_%s[2];" (Type.of_string t) i in
        host ^ "\n" ^  device
  in
  let input =
    List.map cpunode_to_variable ast.in_nodes |> String.concat "\n"
  in
  let node =
    List.filter_map
      (function
        | Node ((i, t), _, _) ->
            Some (Printf.sprintf "%s %s[2];" (Type.of_string t) i)
        | NodeA ((i, t), n, _, _, _) ->
            Some (Printf.sprintf "%s %s[2][%d];" (Type.of_string t) i n)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  let gnode =
    List.filter_map
      (function
        | Syntax.GNode ((i, t), _, _, _, _) ->
            Some (Printf.sprintf "%s* g_%s[2];" (Type.of_string t) i)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  List.filter String.is_not_empty [turn_vairable; input; node; gnode]
  |> String.concat "\n"(*}}}*)

(* XFRPの式の型を取得する関数 *)
let rec get_xfrp_expr_type (e : expr) (program : Module.program) : Type.t = (*{{{*)
    match e with
    | ESelf -> Type.TInt
    | EConst value -> type_of_const value
    | Eid name -> 
        let id = Hashtbl.find program.id_table name in
        let node = Hashtbl.find program.info_table id in
        node.t
    | EidA (name,_) -> 
        let id = Hashtbl.find program.id_table name in
        let node = Hashtbl.find program.info_table id in
        node.t
    | EAnnot (name,_) -> 
        let id = Hashtbl.find program.id_table name in
        let node = Hashtbl.find program.info_table id in
        node.t
    | EAnnotA (name,_,_) -> 
        let id = Hashtbl.find program.id_table name in
        let node = Hashtbl.find program.info_table id in
        node.t
    | EUni _ -> TInt
    | Ebin (op,e1,e2) -> 
        (match op with 
        | BEq |BOr |BLte |BLt |BRte |BRt -> Type.TBool
        | _ -> 
            let t1 = get_xfrp_expr_type e1 program in
            let t2 = get_xfrp_expr_type e2 program in
            let typ = (match (t1,t2) with
                        | (TFloat,_) -> TFloat
                        | (_,TFloat) -> TFloat
                        | (TInt,_) -> TInt
                        | (_,TInt) -> TInt
                        | (TChar,_) -> TChar
                        | (_,TChar) -> TChar
                        | _ -> TBool)
            in typ)
    | EApp (id,_) ->
        (* TODO 関数を実装していないのでここも実装できない. 関数を実装したら関数の型から返り値を適切に決定できるようにする *)
        TInt
    | Eif (_,e1,e2) -> 
        let t1 = get_xfrp_expr_type e1 program in
        let t2 = get_xfrp_expr_type e2 program in
        if t1 = t2 then t1
                    else raise (TypeError ("In If-expr, the value of then and else are not matched"))(*}}}*)

(* XFRPの式 -> C言語のAST *)
let rec expr_to_clang (e : expr) (program : Module.program) : c_ast * c_ast =(*{{{*)
  match e with
  | ESelf ->
      (Empty, Variable "self")
  | EConst e ->
      (Empty, Const (Syntax.string_of_const e))
  | Eid i ->
      (Empty, Variable (i ^ "[turn]"))
  | EidA (i, e) -> (* e:添字 *)
      let index_pre, index_post = expr_to_clang e program in
      let node_id = Hashtbl.find program.id_table i in
      let node = Hashtbl.find program.info_table node_id in
      let result_variable = get_unique_name () in
      (*
       * int index_post = ...
       * <Type.t> tmp;
       * if(index_post < size){
       *    tmp = i[turn][index_post];
       * }else{
       *    tmp = iの定数;
       * }
       * <post> := tmp
       *)
      let default_value = Option.get node.default in
      let check_index_code = If((node.t,result_variable),
                                (Empty, Binop("<",index_post, Variable(string_of_int node.number))),
                                (Empty, VariableA(Printf.sprintf "%s[turn]" i, index_post)),
                                (Empty,Const(Syntax.string_of_const default_value))) in
      (CodeList [index_pre; check_index_code], Variable(result_variable))
  | EAnnot (id, annot) ->
      (Empty, Variable (id ^ "[turn^1]"))
  | EAnnotA (i, _, indexe) ->
      let pre, post = expr_to_clang indexe program in
      (pre, VariableA (Printf.sprintf "%s[turn^1]" i, post))
  | Ebin (op, e1, e2) ->
      let op_symbol = string_of_binop op in
      let pre1, cur1 = expr_to_clang e1 program in
      let pre2, cur2 = expr_to_clang e2 program in
      let pre =
        match (pre1, pre2) with
        | Empty, Empty ->
            Empty
        | pre1, Empty ->
            pre1
        | Empty, pre2 ->
            pre2
        | pre1, pre2 ->
            CodeList [pre1; pre2]
      in
      (pre, Binop (op_symbol, cur1, cur2))
  | EUni (uni, e) ->
      let opsym = string_of_uniop uni in
      let pre, post = expr_to_clang e program in
      (pre, Uniop (opsym, post))
  | EApp (f, args) ->
      let maped = List.map (fun v -> expr_to_clang v program) args in
      let pre : c_ast list = List.map (fun (p, _) -> p) maped in
      let a : c_ast list = List.map (fun (_, a) -> a) maped in
      (CodeList pre, Call (f, a))
  | Eif (cond_expr, then_expr, else_expr) ->
      let res_var = get_unique_name () in (* res_var: if式の結果を保存する変数 *)
      let then_type = get_xfrp_expr_type then_expr program in
      let else_type = get_xfrp_expr_type else_expr program in
      let res_type = Type.union_type then_type else_type in
      (* res_varの型はthen_exprとelse_exprの型に一致する *)
      (* もしthen_exprとelse_exprの型が異なればコンパイルエラーにしてよい *)
      ( If
          ( ( res_type, res_var )
          , expr_to_clang cond_expr program
          , expr_to_clang then_expr program
          , expr_to_clang else_expr program )
      , Variable res_var )
(*}}}*)

(* XFRPの式 -> C言語のAST. 関数のbodyの生成に使う *)
let rec expr_to_clang_of_func (e : expr) (program : Module.program) : c_ast * c_ast =(*{{{*)
  match e with
  | ESelf ->
      (Empty, Variable "self")
  | EConst e ->
      (Empty, Const (Syntax.string_of_const e))
  | Eid i ->
      (Empty, Variable(i))
  | EidA (i, e) -> (* e:添字 *)
      raise (Unreachable "In function EidA is not used")
  | EAnnot (id, annot) ->
      raise (Unreachable "In function EAnnot is not used")
  | EAnnotA (i, _, indexe) ->
      raise (Unreachable "In function EAnnotA is not used")
  | Ebin (op, e1, e2) ->
      let op_symbol = string_of_binop op in
      let pre1, cur1 = expr_to_clang_of_func e1 program in
      let pre2, cur2 = expr_to_clang_of_func e2 program in
      let pre =
        match (pre1, pre2) with
        | Empty, Empty ->
            Empty
        | pre1, Empty ->
            pre1
        | Empty, pre2 ->
            pre2
        | pre1, pre2 ->
            CodeList [pre1; pre2]
      in
      (pre, Binop (op_symbol, cur1, cur2))
  | EUni (uni, e) ->
      let opsym = string_of_uniop uni in
      let pre, post = expr_to_clang_of_func e program in
      (pre, Uniop (opsym, post))
  | EApp (f, args) ->
      let maped = List.map (fun v -> expr_to_clang_of_func v program) args in
      let pre : c_ast list = List.map (fun (p, _) -> p) maped in
      let a : c_ast list = List.map (fun (_, a) -> a) maped in
      (CodeList pre, Call (f, a))
  | Eif (cond_expr, then_expr, else_expr) ->
      let res_var = get_unique_name () in (* res_var: if式の結果を保存する変数 *)
      let then_type = get_xfrp_expr_type then_expr program in
      let else_type = get_xfrp_expr_type else_expr program in
      let res_type = Type.union_type then_type else_type in
      (* res_varの型はthen_exprとelse_exprの型に一致する *)
      (* もしthen_exprとelse_exprの型が異なればコンパイルエラーにしてよい *)
      ( If
          ( ( res_type, res_var )
          , expr_to_clang_of_func cond_expr program
          , expr_to_clang_of_func then_expr program
          , expr_to_clang_of_func else_expr program )
      , Variable res_var )
(*}}}*)

(* C言語のASTからC言語のコード *)
let rec code_of_c_ast (ast : c_ast) (tabs : int) : string =(*{{{*)
  let tab = String.make tabs '\t' in
  match ast with
  | Empty -> ""
  | Const s -> s
  | Variable v -> v
  | VariableA (i, c) ->
      Printf.sprintf "%s[%s]" i (code_of_c_ast c 0)
  | VarDec (t, v) ->
      Printf.sprintf "%s %s;" (Type.of_string t) v
  | Assignment (_, var, ca) ->
      (* int a = ??? *)
      let right = code_of_c_ast ca tabs in
      tab ^ var ^ "=" ^ right ^ ";"
  | Uniop (op, c) ->
      Printf.sprintf "%s(%s)" op (code_of_c_ast c 0)
  | Binop (op, ast1, ast2) ->
      Printf.sprintf "(%s %s %s)" (code_of_c_ast ast1 0) op
        (code_of_c_ast ast2 0)
  | Call (f, args) ->
      Printf.sprintf "%s(%s)" f
        (List.map (fun ast -> code_of_c_ast ast 0) args |> String.concat ", ")
  | If
      ( (t, res)
      , (cond_pre, cond_post)
      , (then_pre, then_post)
      , (else_pre, else_post) ) ->
      let cond_pre_code = code_of_c_ast cond_pre tabs in
      (* if文の結果を保存する変数宣言 *)
      let if_var_dec = tab ^ Printf.sprintf "%s %s;" (Type.of_string t) res in
      (* if文の条件式の結果を保存する変数の宣言と計算 *)
      let cond_var = get_unique_name () in
      let cond_var_dec =
        tab ^ Printf.sprintf "bool %s = %s;" cond_var (code_of_c_ast cond_post 0)
      in
      let if_st = tab ^ Printf.sprintf "if(!!%s){" cond_var in
      let then_st1 = code_of_c_ast then_pre (tabs + 1) in
      let then_st2 =
        tab ^ Printf.sprintf "\t%s = %s;" res (code_of_c_ast then_post 0)
      in
      let else_st = tab ^ "}else{" in
      let else_st1 = code_of_c_ast else_pre (tabs + 1) in
      let else_st2 =
        tab ^ Printf.sprintf "\t%s = %s;" res (code_of_c_ast else_post 0)
      in
      let else_st3 = tab ^ "}" in
      (* 
       * cond_pre_code...
       * `t` `res`; // if_var_dec
       * bool `cond_var` = `cond_post` // cond_var_dec
       * if(!!`cond_var`){ // if_1
       *    then_pre...
       *    res = then_post;
       * }else{
       *    else_pre...
       *    res = else_post;
       * }
       * *)
      Utils.concat_without_empty "\n"
        [cond_pre_code
        ; if_var_dec
        ; cond_var_dec
        ; if_st
        ; then_st1; then_st2
        ; else_st; else_st1; else_st2; else_st3 ]
      (* cond_pre_code ^ "\n" ^ if_var_dec ^ "\n" ^ cond_var_dec ^ "\n" ^ if_st ^ "\n" ^ then_st1 ^ "\n" ^ then_st2 ^ "\n" *)
      (* ^ else_st ^ "\n" ^ else_st1 ^ "\n" ^ else_st2 ^ "\n" ^ else_st3 *)
  | CodeList lst ->
      (* List.iter (fun ast -> Printf.printf "--> %s\n" (string_of_c_ast ast)) lst ; *)
      List.filter_map
        (function Empty -> None | ast -> Some (code_of_c_ast ast tabs))
        lst
      |> String.concat "\n"
(*}}}*)

(* 関数定義を生成する関数 *)
let generate_functions (funcname : string) (typ : Type.t) (args : id_and_type list) (expr : Syntax.expr) (program : Module.program) = 
  let args_code = 
    List.map (fun (i,t) -> (Type.of_string t) ^ " " ^ i) args
    |> String.concat ", "
  in
  let declare = Printf.sprintf "%s %s(%s){" (Type.of_string typ) funcname args_code in
  let foward, backward = expr_to_clang_of_func expr program in
  let code1 = code_of_c_ast foward 1 in
  let code2 = code_of_c_ast backward 0 in
  Utils.concat_without_empty "\n" [declare; code1; "\treturn " ^ code2 ^ ";"; "}"]

(* ノード更新関数を生やす関数 *)
let generate_node_update_function (name : string) (expr : Syntax.expr) (program:Module.program) =
  let declare = Printf.sprintf "void %s_update(){" name in
  let foward, backward = expr_to_clang expr program in
  let code1 = code_of_c_ast foward 1 in
  let code2 = code_of_c_ast backward 0 in
  Utils.concat_without_empty "\n" [declare; code1; Printf.sprintf "\t%s[turn] = %s ;" name code2; "}"]

(* ノード配列の更新関数を生成する関数 *)
let generate_nodearray_update (name : string) (expr : Syntax.expr) (program : Module.program) (device_memory_require : IntSet.t) : string =(*{{{*)
  let declare = Printf.sprintf "void %s_update(int self){" name in
  let pre, post = expr_to_clang expr program in
  let code_pre = code_of_c_ast pre 1 in
  let code_post = Printf.sprintf "\t%s[turn][self] = %s;" name (code_of_c_ast post 1) in
  Utils.concat_without_empty "\n" [declare; code_pre; code_post; "}"](*}}}*)

(* Return the update function of gpu node array. *)
(* For the gpu node array `x`, this function reaturns the function `x_update()`. *)
(* let generate_gpu_node_array_update (name : string) (gexpr : Syntax.gexpr) (ast : Syntax.ast) (program : Module.program) = *) 
(*   let kernel_function = Gpu.generate_gnode_update_kernel name gexpr ast program in *)
(*   let declare = Printf.sprintf "void %s_update(){" name in *)
(*   Utils.concat_without_empty "\n" [kernel_function; "\n"; declare; "}"] *)

(* 各ノードの初期化をするC言語のコードを出力する関数 *)
let setup_code (ast : Syntax.ast) (prg : Module.program) (thread : int) : string =(*{{{*)
  (* GNodeの初期化 *)
  let init_gnode =
    List.filter_map
      (function
        | GNode ((i, t), n, init, _, _) ->
            let malloc =
              Printf.sprintf
                "\tfor(int i=0;i<2;i++) \
                 cudaMalloc((void**)&g_%s[i],%d*sizeof(%s));"
                i n (Type.of_string t)
            in
            if Option.is_none init then Some malloc
            else
              let tmp = get_unique_name () in
              let preast, curast = expr_to_clang (Option.get init) prg in
              let precode = code_of_c_ast preast 1 in
              let curcode =
                "\tint " ^ tmp ^ " = " ^ code_of_c_ast curast 1 ^ ";"
              in
              let ini_code =
                Printf.sprintf "\tcudaMemSet(%s[1],%s,sizeof(%s)*%d)" i tmp
                  (Type.of_string t) n
              in
              Some
                ( malloc ^ "\n" ^ precode
                ^ (if precode = "" then "" else "\n")
                ^ curcode ^ "\n" ^ ini_code )
        | _ ->
            None)
      ast.definitions
    |> Utils.concat_without_empty "\n"
  in
  (* CPUノードの初期化を行うコードを出力 *)
  let init_node =
    List.filter_map
      (function
        | Node ((i, t), Some(init), _)  ->
            let preast, curast = expr_to_clang init prg in
            let precode = code_of_c_ast preast 1 in
            let curcode = code_of_c_ast curast 1 in
            Some (Utils.concat_without_empty "\n" [precode; "\t"^i^"[1]="^curcode^";"])
        | NodeA((i,t),num, Some(init), _, _) -> 
            let pre_ast, cur_ast = expr_to_clang init prg in
            let pre_code = code_of_c_ast pre_ast 1 in
            let cur_code = code_of_c_ast cur_ast 1 in
            let assign_code = Printf.sprintf "\t\t%s[1][self] = %s;" i cur_code in
            let code = Utils.concat_without_empty "\n" [pre_code; assign_code] in
            let for_stub = Printf.sprintf "\tfor(int self=0;self<%d;self++){\n%s\n\t}" num code in
            Some(for_stub)
        | _ -> None)
      ast.definitions
    |> Utils.concat_without_empty "\n"
  in
  (* スレッドが2以上のときにスレッドをforkするコード *)
  let thread_fork = 
    if thread = 1 then ""
    else List.init (thread-1) (fun i -> Printf.sprintf "\tfork(%d);" (i+1)) |> String.concat "\n"
  in
  (* 同期機構の初期化 *)
  let init_sync = 
    if thread = 1 then ""
    else Printf.sprintf "\tinit_barrier(%d);" thread
  in
  (* コードの結合 *)
  Utils.concat_without_empty "\n" ["void setup(){"; "\tturn=0;"; init_node; init_gnode; init_sync; thread_fork; "}"] (*}}}*)

(* loop関数を生成 *)
(* 返り値 (max_fsd, assign_array2d) 
 *  max_fsd : FSDの最大値
 *  assign_array2d : FSDがiのときにスレッドjが更新する更新関数
 *  nodearray_accessed_by_gpunode : 
 *)
let create_update_th_fsd_function (ast : Syntax.ast) (program:Module.program) (thread : int) (nodearray_accessed_by_gpunode : IntSet.t) : int * string array array = 
  (* scheduled : スケジューリングされたノード *)
  (* scheduled.(i) :=  FSDがiのときの各スレッドが担当するノードの集合 *)
  (* scheduled.(i).(j) := FSDがiのときにスレッドjが更新するノードのリスト *)
  let scheduled : (int * ((Schedule.assign_node list) array)) array = Schedule.assign_to_cpu ast program thread in


  let max_fsd = Array.length scheduled - 1 in
  (* update_x_y関数を生成*)
  (* update_x_y関数はスレッドxが担当するFSDがyのノード集合 *)
  (* update_functions.(i).(j)にはスレッドjが担当するFSDがiのノードを更新する関数がリストになっている *)
  let update_functions = 
    Array.map (fun (assign_of_each_fsd : (int * ((Schedule.assign_node list) array))) -> 
      let fsd, assign_fsd_array = assign_of_each_fsd in
      Array.mapi (fun th list_of_each_thread -> 
        (* FSD=i, Thread=thが更新するノードの更新関数(update_<node名>)のリスト *)
        let update_function_list = 
          List.map (fun nodeinfo -> 
            match nodeinfo with
            | Schedule.Single (nodeid) ->
                (* Return the update method of cpu node which corresponds to `nodeid`.  *)
                (* Returns the following code. *)
                (* node_udpate(); *)
                let info = Hashtbl.find program.info_table nodeid in
                Printf.sprintf "\t%s_update();" info.name
            | Schedule.Array (nodeid, (startindex, endindex))->
                (* Return the update method of cpu node array which corresponds to `nodeid`. *)
                (* The update of cpu node array is done by using loop iteration.
                 * It means that the cpu node array update is done by following code. *)
                (* for(int i=0;i<N; i++) node_update(i); *)
                let info = Hashtbl.find program.info_table nodeid in
                let update_code = Printf.sprintf "\tfor(int i=%d;i<%d;i++) %s_update(i);" startindex endindex info.name in
                let transfer_to_device_code = 
                  if IntSet.mem nodeid nodearray_accessed_by_gpunode
                  then
                    let src = Printf.sprintf "%s[turn]" info.name in
                    let dst = Printf.sprintf "g_%s[turn]" info.name in
                    let size = Printf.sprintf "%d * sizeof(%s)" info.number (Type.of_string info.t) in
                    Printf.sprintf "\tcudaMemcpy(%s, %s, %s, cudaMemcpyHostToDevice);" dst src size 
                  else
                    ""
                in
                let code = Utils.concat_without_empty "\n" [update_code; transfer_to_device_code] in
                code
            | Schedule.GArray nodeid ->
                (* Return the update method of gpu node corresponds to `nodeid`. *)
                (* Returns the following code. *)
                (* gnode_update(); *)
                let info = Hashtbl.find program.info_table nodeid in
                Printf.sprintf "\t%s_update();" info.name
          ) list_of_each_thread
        in
        (* 更新関数のリストを結合すればよい *)
        let body = Utils.concat_without_empty "\n" update_function_list in
        let head = 
          Printf.sprintf "void update_%d_%d(){" th fsd in
        head ^ "\n" ^ body ^ "\n}"
      ) assign_fsd_array
    ) scheduled
  in
  (* update_x_yのデバッグ出力 *)(*{{{*)
  (* Printf.printf "-----------\n"; *)
  (* Array.iteri (fun fsd a -> *) 
  (*   Printf.printf "----- fsd = %d ------\n" fsd; *)
  (*   Array.iteri (fun thread v -> *) 
  (*     Printf.printf "%d :" thread; *)
  (*       Printf.printf "%s, " v ; *)
  (*     Printf.printf "\n" *)
  (*   ) a; *)
  (* ) update_functions; *)
  (* Printf.printf "-----------\n"; *)(*}}}*)
  (* return *)
  (max_fsd, update_functions)

(* loop,loop1,...関数を生成する *)
let create_loop_function (ast : Syntax.ast) (program : Module.program)(*{{{*)
                          (thread : int) (max_fsd : int) 
                        : string array =
  (* 返り値 *)
  (* loop_function.(i)にはスレッドiが実行するloop関数 *)
  let loop_functions = Array.make thread "" in
  (* 各loop関数を実装 *)
  for i = 0 to thread-1 do
    let head =
      let first_sync = if thread = 1 then "" else Printf.sprintf "\tsynchronization(%d);\n" i in
      "void loop" ^ (if i=0 then "" else string_of_int i) ^ (Printf.sprintf "(){\n%s" first_sync) in
    let body = 
      let concat_delm = if thread = 1 then "\n" else Printf.sprintf "\n\tsynchronization(%d);\n" i in
      List.init (max_fsd+1) (fun i -> max_fsd - i) |> 
      List.map (fun fsd -> Printf.sprintf "\tupdate_%d_%d();" i fsd) |>
      String.concat concat_delm
    in
    let tail = 
      if thread = 1 then "\n\tturn^=1;\n}" 
                    else Printf.sprintf "\n\tsynchronization(%d);\n%s}" i (if i==0 then "\tturn^=1;\n" else "")
    in
    loop_functions.(i) <- head ^ body ^ tail
  done;
  loop_functions(*}}}*)

let main_code = "int main()\n{\n  setup();\n  loop();\n}"

(* 全体のC言語のコードを文字列として出力する *)
(* 外部からはこの関数を呼べばいい *)
let code_of_ast (ast:Syntax.ast) (prg:Module.program) (thread:int) : string =(*{{{*)
  let header = header_code () in
  let header2 = header_code2 () in
  let macros = macro_code () in
  let require_host_to_device_node =  (* The set of node array accessed from gpu node *)
    let rec traverse_gexpr gexpr = 
      match gexpr with
      | GIdAt (sym, g) | GIdAtAnnot(sym,g,_) ->
          let id = Hashtbl.find prg.id_table sym in
          let set = if IntSet.mem id prg.node_arrays then IntSet.singleton id else IntSet.empty in
          IntSet.union set (traverse_gexpr g)
      | Gbin (_, g1,g2) -> 
          IntSet.union (traverse_gexpr g1) (traverse_gexpr g2)
      | GApp (_, args) -> 
          List.fold_left (fun acc g -> IntSet.union acc (traverse_gexpr g)) IntSet.empty args
      | Gif (cond1, cond2, cond3) ->
          List.fold_left (fun acc g -> IntSet.union acc (traverse_gexpr g)) IntSet.empty [cond1; cond2; cond3]
      | _ -> IntSet.empty
    in
    List.filter_map
      (function
        | GNode (_,_,_,_,g) -> Some(traverse_gexpr g)
        | _ -> None)
      ast.definitions
    |>  List.fold_left (fun set acc -> IntSet.union set acc) IntSet.empty
  in
  (* The set of gpu node, which are accessed by cpu node or cpu node array.  *)
  (* The gpu node in the set must move the value from the device to the host. *)
  let gpunodes_accessed_by_cpunode : IntSet.t =
    let rec traverse_expr expr : IntSet.t = 
      match expr with
      | EidA (sym, index_e) | EAnnotA (sym, _, index_e) -> 
          let id = Hashtbl.find prg.id_table sym in
          let set1 = if List.mem sym prg.gnode then IntSet.singleton id else IntSet.empty in
          IntSet.union set1 (traverse_expr index_e)
      | Ebin(_, e1, e2) ->
          IntSet.union (traverse_expr e1) (traverse_expr e2)
      | EApp(_, args) -> 
          List.fold_left (fun acc e -> IntSet.union acc (traverse_expr e)) IntSet.empty args
      | Eif(e1,e2,e3) -> 
          List.fold_left (fun acc e -> IntSet.union acc (traverse_expr e) ) IntSet.empty [e1; e2; e3]
      | _ -> IntSet.empty
    in
    List.filter_map (function | Node (_, _, e) -> Some(traverse_expr e)
                              | NodeA (_,_,_,e,_) -> Some(traverse_expr e)
                              | _ -> None)
                    ast.definitions
    |> List.fold_left (fun set acc -> IntSet.union set acc) IntSet.empty
  in
  let variables = global_variable ast prg require_host_to_device_node in
  let functions = (* 関数定義 *)
    List.filter_map
      (function
        | Func ((i,t),args,e) -> Some (generate_functions i t args e prg)
        | _ -> None
      ) ast.definitions
        |> String.concat "\n\n"
  in
  let node_update = (* Internal/Outputノードの更新関数を定義 *)
    List.filter_map
      (function
        | Node ((i, t), _, e) -> Some (generate_node_update_function i e prg)
        | _ -> None)
      ast.definitions
    |> String.concat "\n\n"
  in
  let node_array_update : string = (* Internal/Output配列ノードの更新関数を定義 *)
    List.filter_map
      (function
        | NodeA ((i, t), _, _, e, _) -> Some (generate_nodearray_update i e prg require_host_to_device_node)
        | _ -> None)
      ast.definitions
    |> Utils.concat_without_empty "\n\n"
  in
  (* let gpu_kernel : string = *) 
  (*   List.filter_map *)
  (*     (function *)
  (*       | GNode ((i,t), _, _, _, e) -> *) 
  (*           Some(Gpu.generate_gnode_update_kernel i e ast prg) *)
  (*       | _ -> None) *)
  (*     ast.definitions *)
  (*   |>  Utils.concat_without_empty "\n" *)
  (* in *)
  let gnode_update = (* GPUノードの更新関数を定義 *)
    List.filter_map
      (function
        | GNode ((i, t), _, _, _, e) ->
            Some (Gpu.generate_gpu_node_array_update i e ast prg)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n\n"
  in
  let input_node_update_functions =
    List.map
      (function
        | Single (i,_) -> Printf.sprintf "void %s_update(){\n\t%s[turn]=definition_of_%s();\n}" i i i
        | Array ((i,t),num,_) -> Printf.sprintf "void %s_update(int self){\n\t%s[turn][self] = definition_of_%s(self);\n}" i i i)
      ast.in_nodes
        |> String.concat "\n\n" in
  let main = main_code in
  let setup = setup_code ast prg thread in
  let maxfsd, update_function_array = create_update_th_fsd_function ast prg thread require_host_to_device_node in
  let updates =
    Array.map (fun a -> Array.to_list a |> String.concat "\n") update_function_array |> Array.to_list |> String.concat "\n" in
  let loops = create_loop_function ast prg thread maxfsd |> Array.to_list |> Utils.concat_without_empty "\n"
  in
  List.filter
    (fun s -> s != "")
    [ header
    ; header2
    ; macros
    ; variables
    ; functions
    (* ; gpu_kernel *)
    ; input_node_update_functions
    ; node_update
    ; node_array_update
    ; gnode_update
    ; updates 
    ; loops
    ; setup
    ; main ]
  |> Utils.concat_without_empty "\n\n"(*}}}*)
