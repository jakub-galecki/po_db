type selectivity = float
type int_hist_v
type range
type int_hist_struct

type op = LESS_THAN | GREATER_THAN | EQUAL | LESS_THAN_OR_EQ | GREATER_THAN_OR_EQ

val load_histogram_from_file : string -> string -> int_hist_struct
val print_int_hist : int_hist_struct -> unit





