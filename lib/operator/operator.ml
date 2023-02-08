exception Error of string

type t = LESS_THAN | GREATER_THAN | EQUAL | LESS_THAN_OR_EQ | GREATER_THAN_OR_EQ

let from_string  = function
  | ">" -> GREATER_THAN
  | ">=" -> GREATER_THAN_OR_EQ 
  | "<" -> LESS_THAN
  | "<=" -> LESS_THAN_OR_EQ
  | "=" -> EQUAL
  | _ -> raise (Error "Unkown operator :(")



