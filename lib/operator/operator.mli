type t = LESS_THAN | GREATER_THAN | EQUAL | LESS_THAN_OR_EQ | GREATER_THAN_OR_EQ

val from_string : string -> t
val execute_binary_operator : int -> int -> t -> bool