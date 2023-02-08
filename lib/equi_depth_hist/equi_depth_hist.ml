(* BASED ON https://doi.org/10.48550/arXiv.2206.07396 *)

type selectivity = float

exception Error of string

(* equi depth hist *)

let exists_for_relation_attr (relation: string) (attr: string) : bool = 
  let found = Sys.file_exists ("statistics/" ^ relation ^ "/" ^ attr ^ "/histograms/equi_depth/" ^ attr) in found

type bucket = {
  lower_bound: int; (* exclusive *)
  upper_boud: int; (* inclusive *)
  index: int;
}

let print_bucket b = 
  print_string "lower_bound: "; print_int b.lower_bound; print_newline ();
  print_string "upper_bound: "; print_int b.upper_boud; print_newline ();
;;

let bukcet_from_str (str: string) (index: int) : bucket = 
  let splitted_str = Str.split (Str.regexp "-") (String.trim str) in 
    match splitted_str with
    | lower :: upper :: _ -> {lower_bound = (int_of_string lower); upper_boud = (int_of_string upper); index=index}
    | _ -> raise (Error "Failed to parse lower and upper bound")


type hist_v = {
  n_distinct_values: int;
  value: bucket;
  frequency: int;
}

let hist_v_from_str (str: string) (index: int) : hist_v = 
  match Str.split (Str.regexp ";") str with
  |  rng :: frequency :: distinct :: _ -> {value = (bukcet_from_str rng index); frequency=(int_of_string (String.trim frequency)); n_distinct_values=(int_of_string (String.trim distinct))}
  | _ -> raise (Error "Failed to parse histogram")


type equi_depth_hist = {
  t_name: string;
  attr: string;
  data: hist_v list;
  n_buckets: int;
  total: int;
}

(* Function performs binary search on histogram buckets - todo *)
(* not its O(n) but we could do better *)
let find_bucket_for_value (hist: equi_depth_hist) (v: int) : hist_v = 
   List.find (fun b -> ( b.value.lower_bound < v) && (b.value.upper_boud >= v)) hist.data 

let min_selectivity = 0.01;; (* Minimum selectivity to avoid underestimation *)

let load_histogram_from_file (t_name: string) (attr: string) : equi_depth_hist = 
  (* fixme: do not use magic strings *)
  let file = ("statistics/" ^ t_name ^ "/" ^ attr ^ "/histograms/equi_depth/" ^ attr) in 
  let lines = BatFile.lines_of file  in 
    let idx = ref 0 and total = ref 0 in 
    let data = (BatList.of_enum (BatEnum.map (fun h ->
    let hv = (hist_v_from_str h !idx) in  idx := !idx + 1; total := (!total + hv.frequency); hv) lines)) in 
    {t_name=t_name; attr=attr; data= data; n_buckets=(List.length data); total=(!total)}
;;

(* todo create formatter *)
let print hist = 
  print_endline "INT HISTOGRAM";
  print_endline ("table name: " ^ hist.t_name);
  print_endline ("attribute:" ^ hist.attr);
  print_string "n_bukcets: "; print_int hist.n_buckets; print_newline (); 
  List.iter (fun v ->
    print_string "lower bound: "; print_int v.value.lower_bound; print_newline ();
    print_string "upper bound: "; print_int v.value.upper_boud; print_newline ();
    print_string "bucket index: "; print_int v.value.index; print_newline ();
    print_string "frequency: "; print_int v.frequency; print_newline ();
  ) hist.data;
  print_endline "END HISTOGRAM"


(* RESTRICTION SELECTIVITIES i.e. calculates selectivity for R.X <op> const *)
let restriction_selectivity_lt (hist: equi_depth_hist) (const: int) : selectivity = 
  try let hist_v = find_bucket_for_value hist const in 
    match hist_v.value.index with 
    | i when (i < 0) -> float_of_int 0
    | i when i >= (hist.n_buckets - 1) -> float_of_int 1 
    | i -> let sel = (float_of_int i) /. (float_of_int hist.n_buckets) in 
        let lin_interpol = (float_of_int (const - hist_v.value.lower_bound)) /. (float_of_int (hist_v.value.upper_boud - hist_v.value.lower_bound)) /. (float_of_int hist.n_buckets) in (sel +. lin_interpol) 
  with Not_found -> min_selectivity
;;

let restriction_selectivity_eq (hist: equi_depth_hist) (value: int) : selectivity =
  let hist_v = find_bucket_for_value hist value in 
    (float_of_int hist_v.frequency) /. (float_of_int hist_v.n_distinct_values) /. (float_of_int hist.total)


let get_selectivity_restriction (table: string) (attr: string) (value: int) (op: Operator.t) =
  let hist = load_histogram_from_file table attr in match op with
    | LESS_THAN -> restriction_selectivity_lt hist value
    | GREATER_THAN ->  (float_of_int 1) -. (restriction_selectivity_lt hist value) -. (restriction_selectivity_eq hist value )
    | EQUAL -> restriction_selectivity_eq hist value
    | LESS_THAN_OR_EQ -> (restriction_selectivity_eq hist value) +. (restriction_selectivity_lt hist value)
    | GREATER_THAN_OR_EQ -> (float_of_int 1) -. (restriction_selectivity_lt hist value)

(* 
histx_x = [10,20,25,45]  h𝑖𝑠𝑡_y = [15, 20, 38, 50]
MERGED = [10, 15, 20, 25, 38, 45, 50]

let merge_hists (h1: equi_depth_hist) (h2: equi_depth_hist) : (int * int) list 
*)

(* Could be optimized *)
let join_selectivity_lt (_: equi_depth_hist) (_: equi_depth_hist) : selectivity =  0.3333
    (* let sync = failwith "merge two histograms" in 
      let rec calculate k (cur_fx, cur_fy) (next_fx, next_fy) sel_acc = 
        if k >= (hist_x.n_buckets + hist_y.n_buckets) then sel_acc else
          let computed_next_fx = restriction_selectivity_lt hist_x (List.nth sync k) and 
          computed_next_fy = restriction_selectivity_lt hist_y (List.nth sync k) in 
            let sel_prod = (cur_fx +. next_fx) *. (next_fy -. cur_fy) in 
          (calculate (k + 1) (computed_next_fx, computed_next_fy) (0.0, 0.0) (sel_acc +. sel_prod)) in
      let init_fx = restriction_selectivity_lt hist_x (List.nth sync 0) and init_fy = restriction_selectivity_lt hist_y (List.nth sync 0) in ((calculate 1 (init_fx, init_fy) (0.0, 0.0) 0.0) /. 2.0)  *)


(* todo *)
let join_selectivity_eq _ _ = min_selectivity

let get_selectivity_join  (table_x: string) (attr_x: string) (table_y: string) (attr_y: string) (op: Operator.t)  =
    let hist_x = load_histogram_from_file table_x attr_x and hist_y = load_histogram_from_file table_y attr_y in
    match op with 
    | LESS_THAN -> join_selectivity_lt hist_x hist_y
    | GREATER_THAN -> join_selectivity_lt hist_y hist_x
    | EQUAL -> join_selectivity_eq hist_x hist_y
    | LESS_THAN_OR_EQ -> (join_selectivity_eq hist_x hist_y) +. (join_selectivity_lt hist_x hist_y)
    | GREATER_THAN_OR_EQ -> (float_of_int 1) -. (join_selectivity_lt hist_x hist_y)