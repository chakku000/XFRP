%{
    open Syntax
    open Type
    exception UnknownTypeDeclaration of string
%}

(* 予約後 *)
%token MODULE IN OUT USE INIT NODE GNODE TRUE FALSE IF THEN ELSE AT LAST SELF
(* 括弧 *)
%token LBRACKET RBRACKET LPAREN RPAREN
(* 記号 *)
%token COMMA COLON (* AT SEMICOLON *)
(* 演算 *)
%token EQUAL PLUS MINUS PERCENT SLASH ASTERISK (* EQUAL2 OR LTE LT RTE RT(1* XOR AND LOR LAND NEQ LSHIFT RSHIFT *1) *)
(* 識別子 *)
%token <string> ID
(* 数値 *)
%token <float> FLOAT
%token <int> INT
(* その他 *)
%token EOF

(* 構文解析開始地点 *)
 %start <Syntax.ast> top

(* 下のほうが優先順位が高い *)
%right prec_if
(* %left  OR *)
(* %left  EQUAL2 *)
(* %left  LTE LT RTE RT *)
%left  PLUS MINUS
%left  ASTERISK SLASH PERCENT

%%

top :
  | MODULE id = ID
    IN innodes  = separated_list(COMMA,id_and_type)
    OUT outnodes= separated_list(COMMA,id_and_type)
    USE modules = separated_list(COMMA,ID)
    defs = nonempty_list(definition)
  EOF
  { (* Syntax.top *)
    {
      module_id = id;
      in_nodes = innodes;
      out_nodes = outnodes;
      use = modules;
      definitions = defs;
    }
  }

definition :
  | NODE 
      init = option(INIT LBRACKET ie = init_expr RBRACKET {ie})
      (* ito = id_and_type_opt EQUAL e = expr *)
      it = id_and_type EQUAL e = expr
      { Node(it,init,e) }
  | GNODE (* gnode@1024 init[0] x: Int = ... *)
      AT n = INT
      init = option(INIT LBRACKET ie = init_expr RBRACKET {ie})
      it = id_and_type EQUAL ge = gexpr
      { GNode(it,n,init,ge) }

(* ---------- Node or Function Expression ---------- *)
expr :
  | constant        { EConst($1) }
  | id = ID         { Eid(id) }
  | id = ID AT a = annotation { EAnnot(id,a) }
  | expr binop expr { Ebin($2,$1,$3) }
  | LPAREN expr RPAREN { $2 }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr %prec prec_if { Eif(cond,e1,e2) } (* %prec prec_if down the priority of if statement *)

gexpr : 
  | SELF            { GSelf }
  | constant        { GConst($1) }
  | id = ID         { Gid(id) }
  | id = ID AT a = annotation { GAnnot(id,a) }
  | gexpr binop gexpr   { Gbin($2,$1,$3) }
  | LPAREN gexpr RPAREN  { $2 }
  | IF cond = gexpr THEN e1 = gexpr ELSE e2 = gexpr %prec prec_if { Gif(cond,e1,e2) }

(* ---------- Initialize Node value -------------------- *)
(* restricted expression for initialize the node value *)
init_expr :
  | constant  { EConst($1) }
  | id = ID LPAREN args = init_args RPAREN { EApp(id,args) } (* Function Call *)

init_args :
  | args = separated_list(COMMA,constant) { List.map (fun const_arg -> EConst(const_arg))  args}

(* ---------- Type Specific -------------------- *)
id_and_type : 
  | id = ID COLON t = type_specific { (id,t) }

(* id_and_type_opt: *)
(*   | id = ID COLON t = type_specific { (id,Some t) } *)
(*   | id = ID { (id,None) } *)

type_specific :
  | t = prime_type_specific { t }

prime_type_specific :
  | t = ID
    {
      match t with
      | "Bool" -> TBool
      | "Char" -> TChar
      | "Int"  -> TInt
      | "Float"-> TFloat
      | _ -> raise (UnknownTypeDeclaration t)
    }


(* -------------- Operator ---------------- *)
%inline
binop:
  | PLUS    { BAdd }
  | MINUS   { BMinus }
  | ASTERISK{ BMul }
  | SLASH   { BDiv }
  | PERCENT { BMod }


annotation:
      | LAST { ALast }

(* -------------- Constant ---------------- *)
constant:
  | TRUE  { CBool(true) }
  | FALSE { CBool(false)}
  | INT   { CInt($1) }
  | FLOAT { CFloat($1) }
