type moduleid = string

type id = string

type id_and_type = id * Type.t

type const = CBool of bool | CInt of int | CFloat of float
let string_of_const = function
  | CBool b ->
      string_of_bool b
  | CInt i ->
      string_of_int i
  | CFloat f ->
      string_of_float f

type cpu_node_type = Single of id_and_type | Array of id_and_type * int * const option

let name_of_cpunode = function Single (i, _) -> i | Array ((i, _), _, _) -> i

let string_of_id_and_type (i, t) =
  "id_and_type(" ^ i ^ " , " ^ Type.of_string t ^ ")"

type self_id = string

type id_and_type_opt = id * Type.t option

let string_of_id_and_type_opt ito =
  match ito with
  | id, Some t ->
      "id_and_type_opt( " ^ id ^ " ," ^ Type.of_string t ^ ")"
  | id, None ->
      "id_and_type_opt( " ^ id ^ ", NoType)"

type annot = ALast

let string_of_annot : annot -> string = function ALast -> "@last"



let type_of_const = function
    | CBool _ -> Type.TBool
    | CInt _ -> Type.TInt
    | CFloat _ -> Type.TFloat

type binop =
  | BAdd
  | BMinus
  | BMul
  | BDiv
  | BMod
  | BEq
  | BNeq
  | BOr
  | BLte
  | BLt
  | BRte
  | BRt

let string_of_binop : binop -> string = function
  | BAdd -> "+"
  | BMinus -> "-"
  | BMul -> "*"
  | BDiv -> "/"
  | BMod -> "%"
  | BEq -> "=="
  | BNeq -> "!="
  | BOr -> "||"
  | BLte -> "<="
  | BLt -> "<"
  | BRte -> ">="
  | BRt -> ">"

type uniop = UNeg

let string_of_uniop = function UNeg -> "-"

type expr =
  | ESelf
  | EConst of const
  | Eid of id
  | EidA of id * expr
  | EAnnot of id * annot
  | EAnnotA of id * annot * expr
  | Ebin of binop * expr * expr
  | EUni of uniop * expr
  | EApp of id * expr list
  | Eif of expr * expr * expr

type gexpr =
  | GSelf
  | GConst of const (* 定数1,2,3とか *)
  | Gid of id (* CPUノードに対する参照 *)
  | GAnnot of id * annot
  | GIdAt of id * gexpr (* GPUノードに対する参照. gnodeの右辺に出現するid[self+1]みたいなの *)
  | GIdAtAnnot of id * gexpr * annot
  | Gbin of binop * gexpr * gexpr
  | GApp of id * gexpr list
  | Gif of gexpr * gexpr * gexpr

let rec string_of_expr = function
  | ESelf ->
      "ESelf"
  | EConst c ->
      "EConst( " ^ string_of_const c ^ " )"
  | Eid i ->
      "Eid( " ^ i ^ ")"
  | EidA (i, e) ->
      Printf.sprintf "EidA( %s , %s )" i (string_of_expr e)
  | EAnnot (id, an) ->
      "EAnnot(" ^ id ^ string_of_annot an ^ ")"
  | EAnnotA (i, _, e) ->
      Printf.sprintf "EAnnotA(%s, %s)" i (string_of_expr e)
  | EUni (u, e) ->
      Printf.sprintf "EUni(%s, %s)" (string_of_uniop u) (string_of_expr e)
  | Ebin (op, e1, e2) ->
      "Ebin (" ^ string_of_binop op ^ "){ " ^ string_of_expr e1 ^ " op "
      ^ string_of_expr e2 ^ " }"
  | EApp (i, es) ->
      "EApp(" ^ i ^ " , "
      ^ String.concat "," (List.map string_of_expr es)
      ^ ")"
  | Eif (cond, e1, e2) ->
      "Eif{ cond =  " ^ string_of_expr cond ^ " }{ then = " ^ string_of_expr e1
      ^ "}{ else = " ^ string_of_expr e2 ^ "}"

let rec string_of_gexpr = function
  | GSelf ->
      "Self"
  | GConst c ->
      "EConst( " ^ string_of_const c ^ " )"
  | Gid i ->
      "Eid( " ^ i ^ ")"
  | GIdAt (i, idx) ->
      Printf.sprintf "GIdAt(%s , %s)" i (string_of_gexpr idx)
  | GIdAtAnnot (i, idx, _) ->
      Printf.sprintf "GIdAtAnnot(%s , %s)" i (string_of_gexpr idx)
  | Gbin (op, e1, e2) ->
      "Ebin (" ^ string_of_binop op ^ "){ " ^ string_of_gexpr e1 ^ " op "
      ^ string_of_gexpr e2 ^ " }"
  | GApp (i, es) ->
      "EApp(" ^ i ^ " , "
      ^ String.concat "," (List.map string_of_gexpr es)
      ^ ")"
  | Gif (cond, e1, e2) ->
      "Eif{ cond =  " ^ string_of_gexpr cond ^ " }{ then = "
      ^ string_of_gexpr e1 ^ "}{ else = " ^ string_of_gexpr e2 ^ "}"
  | GAnnot (id, an) ->
      "EAnnot(" ^ id ^ string_of_annot an ^ ")"

type definition =
  | Node of id_and_type * expr option (* init *) * expr
  | NodeA of id_and_type * int * expr option * expr * const
  | GNode of id_and_type * int * expr option (* init *) * gexpr
  | Func of id_and_type * (id_and_type list) * expr

(* | Fun  of (id * Type.t * id list * Type.t list) * expr *)
(* | Const of id_and_type * expr *)

let string_of_definition = function
  | Node (it, Some ie, e) ->
      "Node {\n\t" ^ string_of_id_and_type it ^ " ,\n\tinit = "
      ^ string_of_expr ie ^ "\n\texpr = " ^ string_of_expr e ^ "\n}"
  | Node (it, None, e) ->
      "Node {\n\t" ^ string_of_id_and_type it ^ " ,\n\tinit = " ^ "NONE"
      ^ "\n\texpr = " ^ string_of_expr e ^ "\n}"
  | GNode (it, n, Some ie, e) ->
      "GNode {\n\t" ^ string_of_id_and_type it ^ " ,\n\tinit = "
      ^ string_of_expr ie ^ "\n\texpr = " ^ string_of_gexpr e ^ "\n}"
  | GNode (it, n, None, e) ->
      "GNode {\n\t" ^ string_of_id_and_type it ^ " ,\n\tinit = " ^ "NONE"
      ^ "\n\texpr = " ^ string_of_gexpr e ^ "\n}"
  | NodeA _ ->
      "NodeA is notimplmented"
  | Func _ -> 
      "Func is not implemented"

type ast =
  { module_id: moduleid
  ; in_nodes: cpu_node_type list
  ; out_nodes: cpu_node_type list
  ; use: moduleid list
  ; definitions: definition list }
