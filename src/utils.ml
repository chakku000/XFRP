let print_list (lst : 'a list) (print_value : 'a -> unit) : unit = 
  print_char '[';
  List.iter (fun v -> print_value v; print_char ',') lst;
  print_char ':'

let print_hstbl (tbl : ('a,'b) Hashtbl.t) (print_key : 'a -> unit) (print_value : 'b -> unit) : unit = 
  let print k v : unit =
    print_char '(';
    print_key k;
    print_char ' ';
    print_value v;
    print_char ')'
  in
  Hashtbl.iter (fun k v -> print k v; print_newline ()) tbl
