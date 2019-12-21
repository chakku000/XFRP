exception TypeError of string

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

let union_type t1 t2 = 
    if t1 = t2 then t1
    else
        (match (t1,t2) with
            | (TFloat,_) when (t2 = TInt || t2 = TChar) -> TFloat
            | (_,TFloat) when (t1 = TInt || t1 = TChar) -> TFloat
            | (TInt,TChar) | (TChar,TInt) -> TInt
            | _ ->
                let t1s = of_string t1 in
                let t2s = of_string t2 in
                raise (TypeError(Printf.sprintf "Types can't merged(%s,%s)" t1s t2s) )
        )
