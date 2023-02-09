type selectivity = float
type hist_v
(* type range *)
type t = {
    t_name: string;
    attr: string;
    data: hist_v list;
    n_buckets: int;
    total: int;
  }
type bucket

val print_bucket : bucket -> unit
val load_histogram_from_file : string -> string -> t
val print : t -> unit
val find_bucket_for_value :  t -> int -> hist_v
val get_selectivity_restriction :  t -> int ->  Operator.t -> selectivity
val exists_for_relation_attr : string -> string -> bool
val get_selectivity_join :  t -> t ->  Operator.t -> selectivity

