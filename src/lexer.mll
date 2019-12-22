{
  open Parser
  open Lexing
  exception Error of string
}
(* 空白 *)
let space = [' ' '\r' '\t']+

(* 改行 *)
let newline = '\r' | '\n' | "\r\n"

(* identifier *)
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

(* 整数/実数 *)
let digits = '0' | ['1'-'9']['0'-'9']*
let fdigits = ('0' | ['1'-'9']['0'-'9']*) '.' ['0'-'9']+

(* In rule, the priority is higher if written before *)
rule read = parse
  | space             { read lexbuf } (* 空白無視 *)
  | newline           { Lexing.new_line lexbuf; read lexbuf } (* 改行無視 *)
  | '#'               { read_comment lexbuf; read lexbuf } (* コメントをパースし,そのあとに字句解析再開 *)
  | ','               { COMMA }
  | ':'               { COLON }
  | '['               { LBRACKET }
  | ']'               { RBRACKET }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | "!="              { NEQ }
  | "=="              { EQUAL2 }
  | '='               { EQUAL }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '%'               { PERCENT }
  | '*'               { ASTERISK }
  | '/'               { SLASH }
  | ">="              { RTE }
  | "<="              { LTE }
  | '>'               { RT }
  | '<'               { LT }
  | '@'               { AT }
  | "module"          { MODULE }
  | "in"              { IN }
  | "out"             { OUT }
  | "use"             { USE }
  | "node"            { NODE }
  | "gnode"           { GNODE}
  | "init"            { INIT }
  | "true"            { TRUE }
  | "false"           { FALSE }
  | "if"              { IF }
  | "then"            { THEN }
  | "else"            { ELSE }
  | "last"            { LAST }
  | "self"            { SELF }
  (* | "fun"             { FUNCTION } *)
  | id                { ID (Lexing.lexeme lexbuf) }
  | fdigits           { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | digits            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof               { EOF }
  | _                 { raise (Error ("Unexpected Char: " ^ Lexing.lexeme lexbuf ^ " line: " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum) ^ " column: " ^ (string_of_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)))) }
and read_comment = parse
  | '\n'              { new_line lexbuf }
  | eof               { () }
  | _                 { read_comment lexbuf }
