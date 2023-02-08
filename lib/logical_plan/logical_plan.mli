type alias =  string * string [@@deriving show] 
type property = string * string [@@deriving show]

type tpredicate = {
  tp1: property; 
  top: string;
  tp2: property;
} [@@deriving show] 

type cpredicate = {
  cp1: property; 
  cop: string;
  cp2: int;
} [@@deriving show]

type method_ = string [@@deriving show]
type tuples = (string * string) list [@@deriving show]
type froms = alias list [@@deriving show]
type joins = (string * tpredicate) list [@@deriving show]
type tpredicates = tpredicate list [@@deriving show]
type cpredicates = cpredicate list [@@deriving show]

type logical_plan = {
  met: method_ ;
  tups: tuples ;
  fms: froms ;
  jns: joins ;
  tps : tpredicates;
  cps : cpredicates;
  (* env : (string, string) Hashtbl.t *)
} [@@deriving show]

val is_cpredicate : string -> bool
val cpredicate_from_string : string -> cpredicate
val is_tpredicate : string -> bool
val tpredicate_from_string : string -> tpredicate
val get_relation_from_property : property -> string
val get_attribute_from_property : property -> string