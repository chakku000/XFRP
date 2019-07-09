type moduleid = string
type id = string
type id_and_type = id * Type.t


type id_and_type_opt = id * Type.t option
let string_of_id_and_type_opt ito = 
  match ito with
  | (id,Some t) -> "id_and_type_opt( " ^ id ^ " ," ^ (Type.of_string t) ^ ")"
  | (id,None)   -> "id_and_type_opt( " ^ id ^ ", NoType)"


type annot = ALast

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
  (* | EAnnot of id * annot *)
  (* | Ebin of binop * expr * expr *) 
  (* | Eif of expr * expr * expr *)
  | EApp of id * expr list

let rec string_of_expr = function
  | EConst c -> "EConst( " ^ (string_of_const c) ^ " )"
  | Eid i    -> "Eid( " ^ i ^ ")"
  | EApp (i,es) -> "EApp(" ^ i ^ " , " ^ (String.concat "," (List.map string_of_expr es)) ^ ")"

type definition = 
  | Node of id_and_type_opt * expr option (* init *) * expr
  (* | GPU  of id_and_type_opt * expr option (1* init *1) * expr *)
  (* | Fun  of (id * Type.t * id list * Type.t list) * expr *)
  (* | Const of id_and_type * expr *)

let string_of_definition = function
  | Node (ito , Some ie , e) -> "Node {\n\t" ^ (string_of_id_and_type_opt ito) ^ " ,\n\t init = " ^ (string_of_expr ie) ^ "\n\texpr = " ^ (string_of_expr e) ^ "\n}"
  | Node (ito , None, e) -> "Node {\n\t" ^ (string_of_id_and_type_opt ito) ^ " ,\n\tinit = " ^ "NONE" ^ "\n\texpr = " ^ (string_of_expr e) ^ "\n}"

type ast = {
  module_id : moduleid;
  in_nodes : id_and_type list;
  out_nodes : id_and_type list;
  use : moduleid list;
  definitions : definition list;
}
