exception Error of string

type selectivity = float
let min_selectivity = 0.01;; (* Minimum selectivity to avoid underestimation *)

type _ histogram = 
  | MCV : Mcv_hist.t -> 'a histogram
  | EQUI_D : Equi_depth_hist.t -> 'a histogram

let get_selectivity_restriction : type a. a histogram -> int -> Operator.t -> float = fun his const op -> match his with
  | MCV hist -> Mcv_hist.get_selectivity_restriction hist const op
  | EQUI_D hist -> Equi_depth_hist.get_selectivity_restriction hist const op

let get_selectivity_join : type a b. a histogram ->  b histogram -> Operator.t -> float = fun h1 h2 op ->
  match h1, h2 with 
  | (MCV hist1, MCV hist2) -> Mcv_hist.mcv_selectivity_join hist1 hist2 op
  | (EQUI_D hist1, EQUI_D hist2) -> Equi_depth_hist.get_selectivity_join hist1 hist2 op
  | (_, _) -> min_selectivity
  (* | (EQUI_D hist1, MCV hist2) -> failwith "todo" *)

let chose_hist : string -> string -> 'a histogram = fun relation attribute ->
  if Equi_depth_hist.exists_for_relation_attr relation attribute then 
    let hist = Equi_depth_hist.load_histogram_from_file relation attribute in 
      EQUI_D(hist)
  else  if Mcv_hist.exists_for_retlation relation attribute then 
          let hist = Mcv_hist.load_histogram_from_file relation attribute in MCV(hist)
        else  raise (Error "No histograms found")

let get_cpredicate_estimation (cp: Logical_plan.cpredicate): int = 
  let relation = Logical_plan.get_relation_from_property cp.cp1 and attribute = Logical_plan.get_attribute_from_property cp.cp1 in
  let hist = chose_hist relation attribute in
  let sel = get_selectivity_restriction hist cp.cp2 (Operator.from_string cp.cop) in
  let cardinatlity = Statistics.get_ncard_only  relation attribute in 
  (int_of_float (sel *. (float_of_int cardinatlity)))

let get_tpredicate_estimation (tp: Logical_plan.tpredicate) : int =
  let rel1 = Logical_plan.get_relation_from_property tp.tp1 and attr1 = Logical_plan.get_attribute_from_property tp.tp1
  and rel2 = Logical_plan.get_relation_from_property tp.tp2 and attr2 = Logical_plan.get_attribute_from_property tp.tp2 in 
  let hist1 = chose_hist rel1 attr1 and hist2 = chose_hist rel2 attr2 in 
  let sel =  get_selectivity_join hist1 hist2 (Operator.from_string tp.top)  in 
  let card1 = Statistics.get_ncard_only  rel1 attr1 and 
      card2 = Statistics.get_ncard_only  rel2 attr2 in 
  (int_of_float (sel *. (float_of_int (card1 * card2)))) 


(* let order_joins _ = failwith "todo" *)

let get_best_plan (lp: Logical_plan.logical_plan) : unit = 
  match lp.met with
  | "select" -> 
    let total_cpreds = List.fold_left (fun acc cp -> acc + (get_cpredicate_estimation cp)) 0 lp.cps and 
    total_tpreds =  List.fold_left (fun acc tp -> acc + (get_tpredicate_estimation tp)) 0 lp.tps in 
    print_endline ("ESTIMTED TOTAL CPREDS: " ^ (string_of_int total_cpreds));
    print_endline ("ESTIMTED TOTAL TPREDS: " ^ (string_of_int total_tpreds))
  | _ -> prerr_endline "Oooops";