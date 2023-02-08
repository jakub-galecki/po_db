exception Error of string

type selectivity = float
let min_selectivity = 0.01;; (* Minimum selectivity to avoid underestimation *)

let get_cpredicate_estimation (cp: Logical_plan.cpredicate): int = 
  let relation = Logical_plan.get_relation_from_property cp.cp1 and attribute = Logical_plan.get_attribute_from_property cp.cp1 in
  let sel = if Equi_depth_hist.exists_for_relation_attr relation attribute then 
              Equi_depth_hist.get_selectivity_restriction relation attribute cp.cp2 (Operator.from_string cp.cop)
            else raise (Error "Histogram doesnt exist for this property") in
  let cardinatlity = Statistics.get_ncard (Statistics.get_stats relation attribute) in 
  (int_of_float (sel *. (float_of_int cardinatlity)))

let get_tpredicate_estimation (tp: Logical_plan.tpredicate) : int =
  let rel1 = Logical_plan.get_relation_from_property tp.tp1 and attr1 = Logical_plan.get_relation_from_property tp.tp1
  and rel2 = Logical_plan.get_relation_from_property tp.tp2 and attr2 = Logical_plan.get_attribute_from_property tp.tp2 in 
  let sel = if (Equi_depth_hist.exists_for_relation_attr rel1 attr1) && (Equi_depth_hist.exists_for_relation_attr rel2 attr2) then 
    Equi_depth_hist.get_selectivity_join rel1 attr1 rel2 attr2 (Operator.from_string tp.top)
  else raise (Error "Histogram doesnt exist for this property") in 
  let card1 = Statistics.get_ncard (Statistics.get_stats rel1 attr1) and 
      card2 = Statistics.get_ncard (Statistics.get_stats rel2 attr2) in 
  (int_of_float (sel *. (float_of_int (card1 * card2)))) 



let get_best_plan (lp: Logical_plan.logical_plan) : unit = 
  print_endline "Finding the best plan";
  match lp.met with
  | "select" -> 
    let total_cpreds = List.fold_left (fun acc cp -> acc + (get_cpredicate_estimation cp)) 0 lp.cps and 
    total_tpreds =  List.fold_left (fun acc tp -> acc + (get_tpredicate_estimation tp)) 0 lp.tps in 
    print_endline ("ESTIMTED TOTAL CPREDS: " ^ (string_of_int total_cpreds));
    print_endline ("ESTIMTED TOTAL TPREDS: " ^ (string_of_int total_tpreds))
  | _ -> prerr_endline "Oooops";