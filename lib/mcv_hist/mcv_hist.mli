type t

type selectivity = float

val get_selectivity_restriction : t -> int -> Operator.t -> selectivity
val load_histogram_from_file : string -> string -> t
val exists_for_retlation : string -> string -> bool
val mcv_selectivity_join : t -> t -> Operator.t -> selectivity