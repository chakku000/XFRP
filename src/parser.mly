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
%token EQUAL PLUS MINUS PERCENT SLASH ASTERISK EQUAL2 LT RT LTE RTE (* OR LTE LT RTE RT(1* XOR AND LOR LAND NEQ LSHIFT RSHIFT *1) *)
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
%left  EQUAL2
%left  LTE LT RTE RT
%left  PLUS MINUS
%left  ASTERISK SLASH PERCENT
%right prec_uni

%%

top :
  | MODULE id = ID
    IN innodes  = separated_list(COMMA,input_definition)
    OUT outnodes= separated_list(COMMA,output_definition)
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

(* Internal / Output Node *)
definition :
  | NODE 
      init = option(INIT LBRACKET ie = init_expr RBRACKET {ie})
      i = ID n = option(AT num = INT {num}) COLON t = type_specific EQUAL e = expr
      {
        match n with
        | None -> Node((i,t),init,e)
        | Some(n) -> NodeA((i,t),n,init,e)
      }
  | GNODE (* gnode@1024 init[0] x: Int = ... *)
      AT n = INT
      init = option(INIT LBRACKET ie = init_expr RBRACKET {ie})
      it = id_and_type EQUAL ge = gexpr
      { GNode(it,n,init,ge) }

(* ---------- Node or Function Expression ---------- *)
expr :
  | SELF            { ESelf }
  | constant        { EConst($1) }
  | MINUS expr %prec prec_uni { EUni(UNeg,$2) }
  | id = ID         { Eid(id) }
  | id = ID LBRACKET e = expr RBRACKET { EidA(id,e) }
  | id = ID AT a = annotation { EAnnot(id,a) }
  | id = ID AT a = annotation LBRACKET e = expr RBRACKET { EAnnotA(id,a,e) }
  | expr binop expr { Ebin($2,$1,$3) }
  | LPAREN expr RPAREN { $2 }
  | IF cond = expr THEN e1 = expr ELSE e2 = expr %prec prec_if { Eif(cond,e1,e2) } (* %prec prec_if down the priority of if statement *)

gexpr : 
  | SELF            { GSelf }
  | constant        { GConst($1) }
  (* ここらへんのパースはもう少し上手くやれそうな気がする *)
  | id = ID         { Gid(id) }
  | id = ID AT a = annotation { GAnnot(id,a) }
  | id = ID LBRACKET index = gexpr RBRACKET { GIdAt(id,index) }
  | id = ID LBRACKET index = gexpr RBRACKET AT a = annotation { GIdAtAnnot(id,index,a) }
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


(* --------------- Input --------------- *)
input_definition:
  | i = ID n = option(AT num=INT {num}) COLON t = type_specific
      {
        match n with 
        | None -> Single(i,t)
        | Some(num) -> Array((i,t),num)
      }

(* --------------- Output --------------- *)
output_definition:
  | i = ID n = option(AT num=INT {num}) COLON t = type_specific
      {
        match n with 
        | None -> Single(i,t)
        | Some(num) -> Array((i,t),num)
      }

(* -------------- Operator ---------------- *)
%inline
binop:
  | PLUS    { BAdd }
  | MINUS   { BMinus }
  | ASTERISK{ BMul }
  | SLASH   { BDiv }
  | PERCENT { BMod }
  | EQUAL2  { BEq }
  | RTE     { BRte }
  | RT      { BRt }
  | LTE     { BLte }
  | LT      { BLt }


annotation:
      | LAST { ALast }

(* -------------- Constant ---------------- *)
constant:
  | TRUE  { CBool(true) }
  | FALSE { CBool(false)}
  | INT   { CInt($1) }
  | FLOAT { CFloat($1) }
