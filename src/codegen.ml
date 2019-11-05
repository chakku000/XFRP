let header_list = ["stdio.h"; "stdlib.h"; ]
let header_code () = List.map (fun s -> "#include<" ^ s ^ ">") header_list |> String.concat "\n"

let global_variable (ast : Syntax.ast) (prg : Module.program) = 
  (* Input Node *)
  let input = List.map (fun (id,typ) -> Printf.sprintf "%s %s[2];" (Type.of_string typ) id) ast.in_nodes |> String.concat "\n" in
  input

let setup_code (prg : Module.program) : string =
"void setup()
{
  turn = 0;
}"

let main_code =
"int main()
{
  setup();
  loop();
}"

let code_of_ast : Syntax.ast -> Module.program -> string = fun ast prg ->
  let header = header_code () in
  let variables = global_variable ast prg in
  let main = main_code in
  let setup = setup_code prg in
  String.concat "\n" [header; variables; main; setup]
