type moduleid = string
type id = string
type id_and_type = id * Type.t
let string_of_id_and_type (i,t) =  "id_and_type(" ^ i ^ " , " ^ (Type.of_string t) ^ ")"

type self_id = string

type id_and_type_opt = id * Type.t option
let string_of_id_and_type_opt ito = 
  match ito with
  | (id,Some t) -> "id_and_type_opt( " ^ id ^ " ," ^ (Type.of_string t) ^ ")"
  | (id,None)   -> "id_and_type_opt( " ^ id ^ ", NoType)"


type annot = ALast

let string_of_annot : annot -> string = function
  | ALast -> "@last"

type const = 
  | CBool of bool
  | CInt of int
  | CFloat of float

let string_of_const = function
  | CBool b -> string_of_bool b
  | CInt i  -> string_of_int i
  | CFloat f -> string_of_float f

type binop = 
  | BAdd
  | BMinus
  | BMul
  | BDiv
  | BMod
  | BEq
  | BOr
  | BLte
  | BLt
  | BRte
  | BRt

let string_of_binop : binop -> string =  function
  | BAdd -> "+"
  | BMinus -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BEq -> "=="
  | BOr -> "||"
  | BLte -> "<="
  | BLt -> "<"
  | BRte -> ">="
  | BRt -> ">"

type expr = 
  | EConst of const 
  | Eid of id
  | EAnnot of id * annot
  | Ebin of binop * expr * expr 
  | EApp of id * expr list
  | Eif of expr * expr * expr

type gexpr = 
  | GSelf
  | GConst of const
  | Gid of id
  | GAnnot of id * annot
  | Gbin of binop * gexpr * gexpr
  | GApp of id * gexpr list
  | Gif of gexpr * gexpr * gexpr

let rec string_of_expr = function
  | EConst c -> "EConst( " ^ (string_of_const c) ^ " )"
  | Eid i    -> Printf.printf "poi %s\n" i;  "Eid( " ^ i ^ ")"
  | Ebin (op,e1,e2) -> "Ebin (" ^ (string_of_binop op) ^ "){ " ^ (string_of_expr e1) ^ " op "^ (string_of_expr e2) ^ " }"
  | EApp (i,es) -> "EApp(" ^ i ^ " , " ^ (String.concat "," (List.map string_of_expr es)) ^ ")"
  | Eif (cond,e1,e2) -> "Eif{ cond =  " ^ (string_of_expr cond) ^ " }{ then = " ^ (string_of_expr e1) ^ "}{ else = " ^ (string_of_expr e2)  ^ "}" 
  | EAnnot (id,an) -> "EAnnot(" ^ id ^ (string_of_annot an) ^ ")"

let rec string_of_gexpr = function
  | GSelf -> "Self"
  | GConst c -> "EConst( " ^ (string_of_const c) ^ " )"
  | Gid i    -> Printf.printf "poi %s\n" i;  "Eid( " ^ i ^ ")"
  | Gbin (op,e1,e2) -> "Ebin (" ^ (string_of_binop op) ^ "){ " ^ (string_of_gexpr e1) ^ " op "^ (string_of_gexpr e2) ^ " }"
  | GApp (i,es) -> "EApp(" ^ i ^ " , " ^ (String.concat "," (List.map string_of_gexpr es)) ^ ")"
  | Gif (cond,e1,e2) -> "Eif{ cond =  " ^ (string_of_gexpr cond) ^ " }{ then = " ^ (string_of_gexpr e1) ^ "}{ else = " ^ (string_of_gexpr e2)  ^ "}" 
  | GAnnot (id,an) -> "EAnnot(" ^ id ^ (string_of_annot an) ^ ")"

type definition = 
  | Node of id_and_type * expr option (* init *) * expr
  | GNode of id_and_type * int * expr option (* init *) * gexpr
  (* | Fun  of (id * Type.t * id list * Type.t list) * expr *)
  (* | Const of id_and_type * expr *)

let string_of_definition = function
  | Node (it, Some ie , e) -> "Node {\n\t" ^ (string_of_id_and_type it) ^ " ,\n\tinit = " ^ (string_of_expr ie) ^ "\n\texpr = " ^ (string_of_expr e) ^ "\n}"
  | Node (it, None, e) -> "Node {\n\t" ^ (string_of_id_and_type it) ^ " ,\n\tinit = " ^ "NONE" ^ "\n\texpr = " ^ (string_of_expr e) ^ "\n}"
  | GNode (it, n, Some ie , e) -> "GNode {\n\t" ^ (string_of_id_and_type it) ^ " ,\n\tinit = " ^ (string_of_expr ie) ^ "\n\texpr = " ^ (string_of_gexpr e) ^ "\n}"
  | GNode (it, n, None, e) -> "GNode {\n\t" ^ (string_of_id_and_type it) ^ " ,\n\tinit = " ^ "NONE" ^ "\n\texpr = " ^ (string_of_gexpr e) ^ "\n}"


type ast = {
  module_id : moduleid;
  in_nodes : id_and_type list;
  out_nodes : id_and_type list;
  use : moduleid list;
  definitions : definition list;
}
