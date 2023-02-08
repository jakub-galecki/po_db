exception Error of string

type selectivity = float

type entry = {
  value: int;
  frequency: int;
  index: int;
}

type mcv_hist = {
  t_name: string;
  attr: string;
  entries: entry list;
  n_entries: int;
  total: int;
}

type operator = LESS_THAN | GREATER_THAN | EQUAL | LESS_THAN_OR_EQ | GREATER_THAN_OR_EQ

let exists_for_retlation (relation: string) (attr: string) =
  Sys.file_exists ("statistics/" ^ relation ^ "/histograms/mcv" ^ attr)


let entry_from_string (str: string) (idx: int) : entry = 
  match Str.split (Str.regexp ";") str with
  |  v :: frequency :: _ -> {value = (int_of_string (String.trim v)); frequency=(int_of_string (String.trim frequency)); index=idx}
  | _ -> raise (Error "Failed to parse histogram")


let load_from_file (relation: string) (attr: string) : mcv_hist =
  let lines = BatFile.lines_of ("statistics/" ^ relation ^ attr ^ "/histograms/mcv/" ^ attr) in 
    let idx = ref 0 in 
    let total = ref 0 in 
    let data = (BatList.of_enum (BatEnum.map (fun h ->
    let e = (entry_from_string h !idx) in idx := !idx + 1; total := (!total + e.frequency);  e) lines)) in 
    {t_name=relation; attr=attr; entries= data; n_entries=(List.length data); total=(!total)}


let execute_operator op x y = failwith "todo"
let get_frac n v = (float_of_int v) /. (float_of_int v)

let  mcv_selectivity_restriction (mcv: mcv_hist) (constant: int) (op: operator) = 
      let rec loop acc = function 
      | h :: tl -> if (execute_operator op (h.value) constant) then (loop (get_frac mcv.total h.frequency) tl)
      else (loop acc tl)
      | _ -> acc in loop 0.0 mcv.entries

(* let mcv_selectivity_join (mcv1: mcv_hist) (mcv2: mcv_hist) (op: operator) = 
  let rec loop acc =  *)






