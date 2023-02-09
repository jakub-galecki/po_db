exception Error of string

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

type _ predicate = 
  | Cpred : cpredicate -> cpredicate predicate
  | Tpred : tpredicate -> tpredicate predicate

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
} [@@deriving show]


let is_cpredicate (str: string) : bool =  
  let regx = Str.regexp "^[a-zA-z0-9]+\\.[a-zA-z0-9]+ ..? [a-zA-Z0-9]+$" in
Str.string_match regx str 0

(* a.tuple <operator> constant*)
let cpredicate_from_string (str: string) : cpredicate = 
let split_by_dot = Str.split (Str.regexp "\\.") in
match Str.split (Str.regexp " ") str with 
| x :: op :: y :: _ -> 
  begin
  let a1 = split_by_dot x  in 
    match a1, y with 
    | tname1 :: property1 :: _, const -> 
      begin 
      {cp1 = ((String.lowercase_ascii tname1),property1); cop= op; cp2=(int_of_string (String.trim const)) }
      end
    | _ -> raise (Error  "79: Wrong predicate")
  end
| _ -> raise (Error  "81: Wrong predicate")



(* tpredicate TableName.property operator TableName.property *)
let is_tpredicate (str: string) : bool =
  let regx = Str.regexp "^[a-zA-z0-9]+\\.[a-zA-z0-9]+ ..? [a-zA-z0-9]+\\.[a-zA-z0-9]+$" in
Str.string_match regx str 0

let tpredicate_from_string (str: string) : tpredicate = 
let split_by_dot = Str.split (Str.regexp "\\.") in
let splitted_pred = Str.split (Str.regexp " ") str in
match splitted_pred with 
| x :: op :: y :: _ -> 
  begin
  let a1 = split_by_dot x and a2 = split_by_dot y in 
    match a1, a2 with 
    | tname1 :: property1 :: _, tname2 :: property2 :: _ -> 
      begin 
      {tp1 = ((String.lowercase_ascii tname1),property1); top= op; tp2= ((String.lowercase_ascii tname2), property2)}
      end
    | _ -> raise (Error  "52: Wrong predicate")
  end
| _ -> raise (Error  "54: Wrong predicate")


let get_relation_from_property (prop: property) = 
  (fst prop)

let get_attribute_from_property (prop: property) =
  (snd prop)