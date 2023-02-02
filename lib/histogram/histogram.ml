type selectivity = float

exception Error of string

(* equi height hist *)

type bukcet = {
  lower_bound: int;
  upper_boud: int;
} 

type operator = LESS_THAN | GREATER_THAN | EQUAL | LESS_THAN_OR_EQ | GREATER_THAN_OR_EQ


let bukcet_from_str (str: string) : bukcet = 
  let splitted_str = Str.split (Str.regexp "-") (String.trim str) in 
    match splitted_str with
    | lower :: upper :: _ -> {lower_bound = (int_of_string lower); upper_boud = (int_of_string upper)}
    | _ -> raise (Error "Failed to parse lower and upper bound")


type int_hist_v = {
  value: bukcet;
  frequency: int;
}


let int_hist_v_from_str (str: string) : int_hist_v = 
  match Str.split (Str.regexp ";") str with
  |  rng :: frequency :: _ -> {value = (bukcet_from_str rng); frequency=(int_of_string (String.trim frequency))}
  | _ -> raise (Error "Failed to parse histogram")

type int_hist_struct = {
  t_name: string;
  attr: string;
  data: int_hist_v BatEnum.t;
}


let min_selectivity = 0.01;; (* Minimum selectivity to avoid underestimation *)


let load_histogram_from_file (t_name: string) (attr: string) : int_hist_struct = 
  (* fixme: do not use magic strings *)
  let lines = BatFile.lines_of ("histograms/" ^ t_name ^ "/" ^ attr) in 
    {t_name=t_name; attr=attr; data=(BatEnum.map int_hist_v_from_str lines)} 
;;


(* todo create nice formatter *)
let print_int_hist hist = 
  print_endline "INT HISTOGRAM";
  print_endline ("table name: " ^ hist.t_name);
  print_endline ("attribute:" ^ hist.attr);
  BatEnum.iter (fun v ->
    print_string "lower bound: "; print_int v.value.lower_bound; print_newline ();
    print_string "upper bound: "; print_int v.value.upper_boud; print_newline ();
    print_string "frequency: "; print_int v.frequency; print_newline ();
  ) hist.data;
  print_endline "END HISTOGRAM"



let get_bucket_lower_bound (value: int) : int = failwith "not implemented";;
let get_bucket_upper_bound (value: int) : int = failwith "not implemented";;


let get_selectivity_eq (value: int) : selectivity = failwith "not implemented";;
let get_selectivity_less (value: int) : selectivity = failwith "not implemented";;
let get_selectivity_greater (value: int) : selectivity = failwith "not implemented";;


(* create abstraction for int *)
let get_selectivity  (value: int) = function
| LESS_THAN -> get_selectivity_less(value)
| GREATER_THAN -> get_selectivity_greater(value)
| EQUAL -> get_selectivity_eq(value)
| LESS_THAN_OR_EQ -> get_selectivity_eq(value) +. get_selectivity_greater(value)
| GREATER_THAN_OR_EQ -> get_selectivity_eq(value) +. get_selectivity_less(value) 
