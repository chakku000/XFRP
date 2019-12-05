open Syntax

(* memo: 現在の実装ではNodeはc_astに変換される. 時間があればtarget.astに変換するようにしたほうが良いかもしれない *)
type c_ast =
  | Empty
  | Const of string
  | Variable of string (* 変数 *)
  | VarDec of Type.t * string (* 変数宣言 *)
  | Assignment of Type.t option * string * c_ast (* 変数への代入. もしType.tがNoneならば宣言済みの変数. そうでなければ変数宣言と同時に代入 *)
  | Binop of string * c_ast * c_ast
  | Call of string * c_ast list
  | If of
      (Type.t * string)
      * (* If式の結果に対応する変数名とその型 *)
      (c_ast * c_ast)
      * (* cond式のC言語でのAST(pre,post) *)
      (c_ast * c_ast)
      * (* then (pre,post) *)
      (c_ast * c_ast) (* else (pre,post) *)
  | CodeList of c_ast list


(* 唯一の識別子を与える *)
let unique_index = ref 0
let get_unique_name () : string =
  let id = string_of_int !unique_index in
  unique_index := !unique_index + 1 ;
  "tmp_" ^ id

let rec string_of_c_ast (ast : c_ast) : string =
  match ast with
  | Empty ->
      "Empty"
  | Const s ->
      Printf.sprintf "Const[ %s ]" s
  | VarDec (t, i) ->
      Printf.sprintf "VarDec[ %s ][ %s ]" (Type.of_string t) i
  | Variable v ->
      Printf.sprintf "Var[ %s ]" v
  | Assignment (t, var, ast) ->
      let typename =
        match t with None -> "None" | Some t -> Type.of_string t
      in
      Printf.sprintf "Assignment[ %s ][ %s ][ %s ]" typename var
        (string_of_c_ast ast)
  | Binop (op, ast1, ast2) ->
      Printf.sprintf "Binop[ %s ][ %s ][ %s ]" op (string_of_c_ast ast1)
        (string_of_c_ast ast2)
  | Call (f, args) ->
      Printf.sprintf "Call[ %s ][ %s ]" f
        (List.map string_of_c_ast args |> String.concat " , ")
  | If (_, (cond_pre, cond_post), _, _) ->
      Printf.sprintf "If [cond_pre: [%s], cond_post:[%s]] [] []"
        (string_of_c_ast cond_pre)
        (string_of_c_ast cond_post)
  | CodeList codes ->
      List.map string_of_c_ast codes |> String.concat " :: "


let header_list = ["stdio.h"; "stdlib.h"]

let header_code () =
  List.map (fun s -> "#include<" ^ s ^ ">") header_list |> String.concat "\n"

let global_variable (ast : Syntax.ast) (prg : Module.program) =
  let cpunode_to_variable = function
    | Single(i,t) -> 
        Printf.sprintf "%s %s[2];" (Type.of_string t) i
    | Array((i,t),n) -> 
        Printf.sprintf "%s %s[2][%d];" (Type.of_string t) i n
  in
  let input =
    List.map
    cpunode_to_variable
      ast.in_nodes
    |> String.concat "\n"
  in
  let node =
    List.filter_map
      (function
        | Syntax.Node ((i, t), _, _) ->
            Some (Printf.sprintf "%s %s[2];" (Type.of_string t) i)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  let gnode =
    List.filter_map
      (function
        | Syntax.GNode ((i, t), _, _, _) ->
            Some (Printf.sprintf "%s* g_%s[2];" (Type.of_string t) i)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  input ^ "\n" ^ node ^ "\n" ^ gnode

(* XFRPの式 -> C言語のAST *)
let rec expr_to_clang (e : expr) : c_ast * c_ast =
  match e with
  | ESelf -> (Empty, Variable("self"))
  | EConst e ->
      (Empty, Const (Syntax.string_of_const e))
  | Eid i ->
      (Empty, Variable (i ^ "[turn]"))
  | EAnnot (id, annot) ->
      (Empty, Variable (id ^ "[turn^1]"))
  | Ebin (op, e1, e2) ->
      let op_symbol = string_of_binop op in
      let pre1, cur1 = expr_to_clang e1 in
      let pre2, cur2 = expr_to_clang e2 in
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
  | EApp (f, args) ->
      let maped = List.map expr_to_clang args in
      let pre : c_ast list = List.map (fun (p, _) -> p) maped in
      let a : c_ast list = List.map (fun (_, a) -> a) maped in
      (CodeList pre, Call (f, a))
  | Eif (cond_expr, then_expr, else_expr) ->
      let res_var = get_unique_name () in
      ( If
          ( ( Type.TInt
              (* TODO この変数の型を確認. 今は暫定的にType.TIntにしている. *)
            , res_var )
          , expr_to_clang cond_expr
          , expr_to_clang then_expr
          , expr_to_clang else_expr )
      , Variable res_var )

(* C言語のASTからC言語のコード *)
let rec code_of_c_ast (ast : c_ast) (tabs : int) : string =
  let tab = String.make tabs '\t' in
  match ast with
  | Empty ->
      ""
  | Const s ->
      s
  | Variable v ->
      v
  | VarDec (t, v) ->
      Printf.sprintf "%s %s;" (Type.of_string t) v
  | Assignment (_, var, ca) ->
      (* int a = ??? *)
      let right = code_of_c_ast ca tabs in
      tab ^ var ^ "=" ^ right ^ ";"
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
      Printf.printf "check : %s\n" (string_of_c_ast cond_pre) ;
      Printf.printf "check2 : [%s]\n" cond_pre_code ;
      let if_var_dec =
        tab ^ Printf.sprintf "%s %s;\n" (Type.of_string t) res
      in
      (* Ifの結果の宣言 *)
      let cond_var = get_unique_name () in
      let cond_var_dec =
        tab
        ^ Printf.sprintf "%s %s = %s;\n"
            (Type.of_string Type.TBool)
            cond_var
            (code_of_c_ast cond_post 0)
      in
      (* 条件の結果をcond_varに保存 *)
      let if_st = tab ^ Printf.sprintf "if(!!%s){\n" cond_var in
      let then_st1 = code_of_c_ast then_pre (tabs + 1) in
      let then_st2 =
        tab ^ Printf.sprintf "\t%s = %s;\n" res (code_of_c_ast then_post 0)
      in
      let else_st = tab ^ "else{\n" in
      let else_st1 = code_of_c_ast else_pre (tabs + 1) in
      let else_st2 =
        tab ^ Printf.sprintf "\t%s = %s;\n" res (code_of_c_ast else_post 0)
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
      cond_pre_code ^ if_var_dec ^ cond_var_dec ^ if_st ^ then_st1 ^ then_st2
      ^ else_st ^ else_st1 ^ else_st2 ^ else_st3
  | CodeList lst ->
      List.iter (fun ast -> Printf.printf "--> %s\n" (string_of_c_ast ast)) lst ;
      List.filter_map
        (function Empty -> None | ast -> Some (code_of_c_ast ast tabs))
        lst
      |> String.concat "\n<>"

(* ノード更新関数を生やす関数 *)
let generate_node_update_function (name : string) (expr : Syntax.expr) =
  let declare = Printf.sprintf "void %s_update(){\n" name in
  let foward, backward = expr_to_clang expr in
  let code1 = code_of_c_ast foward 1 in
  let code2 = code_of_c_ast backward 0 in
  Printf.printf "----- %s ----- \n" name ;
  Printf.printf "expr: %s\n" (string_of_expr expr) ;
  Printf.printf "foward : %s\n" (string_of_c_ast foward) ;
  Printf.printf "backward : %s\n" (string_of_c_ast backward) ;
  Printf.printf "cod1: -> %d <-\n" (String.length code1) ;
  Printf.printf "cod2: -> %d : %s <-\n" (String.length code2) code2 ;
  String.concat ""
    [ declare
    ; (code1 ^ if code1 = "" then "" else "\n")
    ; Printf.sprintf "\t%s[turn] = %s ;" name code2
    ; "\n}" ]

let setup_code (ast : Syntax.ast) (prg : Module.program) : string =
  (* GNodeの初期化 *)
  let init_gnode =
    List.filter_map
      (function
        | GNode ((i, t), n, init, _) ->
            let malloc =
              Printf.sprintf
                "\tfor(int i=0;i<2;i++) \
                 cudaMalloc((void**)&g_%s[i],%d*sizeof(%s));"
                i n (Type.of_string t)
            in
            if Option.is_none init then Some malloc
            else
              let tmp = get_unique_name () in
              let preast, curast = expr_to_clang (Option.get init) in
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
    |> String.concat "\n"
  in
  let init_node =
    List.filter_map
      (function
        | Node ((i, t), init, _) ->
            if Option.is_none init then None
              (* 初期化指定子が無い場合 *)
            else
              let preast, curast = expr_to_clang (Option.get init) in
              let precode = code_of_c_ast preast 1 in
              let curcode = code_of_c_ast curast 1 in
              Some
                ( precode
                ^ (if precode = "" then "" else "\n")
                ^ "\t" ^ i ^ "[1] = " ^ curcode )
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  "void setup(){\n" ^ "\tturn=0;\n" ^ init_node
  ^ (if init_node = "" then "" else "\n")
  ^ init_gnode ^ "\n" ^ "}"

let main_code = "int main()\n{\n  setup();\n  loop();\n}"

let code_of_ast : Syntax.ast -> Module.program -> string =
 fun ast prg ->
  let header = header_code () in
  let variables = global_variable ast prg in
  let node_update =
    List.filter_map
      (function
        | Node ((i, t), _, e) ->
            Some (generate_node_update_function i e)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n\n"
  in
  let gnode_update_kernel =
    List.filter_map
      (function
        | GNode ((i, t), _, _, e) ->
            Some (Gpu.generate_gnode_update_kernel i e ast prg)
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n\n"
  in
  let main = main_code in
  let setup = setup_code ast prg in
  String.concat "\n\n"
    [header; variables; node_update; gnode_update_kernel; setup; main]
