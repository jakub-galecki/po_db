type selectivity = float
type hist_v
(* type range *)
type equi_depth_hist
type bucket

val print_bucket : bucket -> unit
val load_histogram_from_file : string -> string -> equi_depth_hist
val print : equi_depth_hist -> unit
val find_bucket_for_value :  equi_depth_hist -> int -> hist_v
val get_selectivity_restriction :  string -> string -> int ->  Operator.t -> selectivity
val exists_for_relation_attr : string -> string -> bool
val get_selectivity_join :  string -> string -> string -> string ->  Operator.t -> selectivity

