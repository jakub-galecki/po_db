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


let get_frac n v = (float_of_int v) /. (float_of_int v)

let  mcv_selectivity_restriction (mcv: mcv_hist) (constant: int) (op: Operator.t) = 
      let rec loop acc = function 
      | h :: tl -> if (Operator.execute_binary_operator (h.value) constant op) then (loop (get_frac mcv.total h.frequency) tl)
      else (loop acc tl)
      | _ -> acc in loop 0.0 mcv.entries

let mcv_selectivity_join (mcv1: mcv_hist) (mcv2: mcv_hist) (op: Operator.t) = 
  let rec loop acc = function 
    | h :: tl -> (loop (acc +.((get_frac mcv2.total h.frequency) *. (mcv_selectivity_restriction mcv1 (h.value) op))) tl) 
    | _ -> acc in loop 0.0 mcv2.entries


let mcv_hist_selectivity_lt (mcv: mcv_hist) (hist : Equi_depth_hist.t) (op: Operator.t) = 
  let rec loop acc = function 
  | h :: tl ->  
    (loop (acc +. 
      ((get_frac mcv.total (h.value)) *.
        (Equi_depth_hist.get_selectivity_restriction (hist.t_name) (hist.attr) (h.value) op))) tl)
  | _ -> acc in loop 0.0 mcv.entries


