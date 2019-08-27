let header_list = ["stdio.h"; "stdlib.h"; ]
let header_code () = List.map (fun s -> "#include<" ^ s ^ ">") header_list |> String.concat "\n"

let main_code =
"int main()
{
  setup();
  loop();
}"

let code_of_ast : Syntax.ast -> string = fun ast ->
  let header = header_code () in
  let main = main_code in
  String.concat "\n" [header; main]
