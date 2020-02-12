open Syntax

(* CPUノード,GPUノードのノード名からその型へのマップ *)
let create_id_type_dict (ast : Syntax.ast) : (Syntax.id, Type.t) Hashtbl.t =
  let tbl = Hashtbl.create 128 in
  (* check Input Node *)
  List.iter
    (function
      | Single (i, t) ->
          Hashtbl.add tbl i t
      | Array ((i, t), _, _) ->
          Hashtbl.add tbl i t)
    ast.in_nodes ;
  (* TODO check Output/Internal Node *)
  List.iter
    (function
      | Node ((i, t), _, _) ->
          Hashtbl.add tbl i t
      | NodeA ((i, t), _, _, _, _) ->
          Hashtbl.add tbl i t
      | GNode ((i, t), _, _, _, _) ->
          Hashtbl.add tbl i t
      | Func _ -> ()
    )ast.definitions ;
  tbl

let print_list (lst : 'a list) (print_value : 'a -> unit) : unit =
  print_char '[' ;
  List.iter (fun v -> print_value v ; print_char ',') lst ;
  print_char ':'

let print_hstbl (tbl : ('a, 'b) Hashtbl.t) (print_key : 'a -> unit)
    (print_value : 'b -> unit) : unit =
  let print k v : unit =
    print_char '(' ;
    print_key k ;
    print_string " : " ;
    print_value v ;
    print_char ')'
  in
  Hashtbl.iter (fun k v -> print k v ; print_newline ()) tbl

let print_set (type s t) (module S : Set.S with type elt = s and type t = t)
    (set : t) (p : s -> unit) : unit =
        S.iter (fun v -> p v; print_char ',') set

(* String.concatをするがその際にemptyな文字列は無視する *)
let concat_without_empty (del : string) (lst : string list) =
  List.filter (fun s -> String.length s > 0) lst |> String.concat del
