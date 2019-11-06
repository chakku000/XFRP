open Syntax

let header_list = ["stdio.h"; "stdlib.h"]

let header_code () =
  List.map (fun s -> "#include<" ^ s ^ ">") header_list |> String.concat "\n"

let global_variable (ast : Syntax.ast) (prg : Module.program) =
  let input =
    List.map
      (fun (id, typ) -> Printf.sprintf "%s %s[2];" (Type.of_string typ) id)
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

let setup_code (ast : Syntax.ast) (prg : Module.program) : string =
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
            else begin
              let ini_code = Printf.sprintf "\tcudaMemSet(%s,%s,sizeof(%s)*%d)" i "12345" (Type.of_string t) n in (* TODO init->stringが必要.暫定的に定数12345を入れてる *)
              Some(malloc^"\n"^ini_code)
            end
        | _ ->
            None)
      ast.definitions
    |> String.concat "\n"
  in
  "void setup(){\n" ^ "\tturn=0;\n" ^ init_gnode ^ "\n" ^ "}"

let main_code = "int main()\n{\n  setup();\n  loop();\n}"

let code_of_ast : Syntax.ast -> Module.program -> string =
 fun ast prg ->
  let header = header_code () in
  let variables = global_variable ast prg in
  let main = main_code in
  let setup = setup_code ast prg in
  String.concat "\n" [header; variables; setup; main]
