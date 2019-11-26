type t = TUnit | TBool | TChar | TInt | TFloat

let of_string = function
  | TUnit ->
      "void"
  | TBool ->
      "bool"
  | TChar ->
      "char"
  | TInt ->
      "int"
  | TFloat ->
      "float"
