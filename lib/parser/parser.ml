module Json = Yojson.Basic
module JsonUtil = Yojson.Basic.Util

exception Error of string

(*
JSON REQUEST
{
  method : "select",
  tuples: [alias.tuple, ...],
  froms: [tableName(alias), ...]
  joins: [alias<predicate>, ...]
  predicates: [predicate, ...]
}

Where predicate looks as following
1) a.tuple <operator> b.tuple
2) a.tuple <operator> constant

If alias doesnt exists it equals to the table name
*)
  
let print_json (json :Json.t) = 
  Logs.info(fun m -> m "Parsed json: %a" Json.pp json)

let try_retrive member json =
  let j = Json.Util.member member json in match j with 
  | `Null -> None
  | _ -> Some(j)

let remove_last_char str = 
  let n = String.length str in 
    if n = 0 then 
      raise (Error  "Can not remove char from empty string")
    else String.sub str 0 (n - 1) 


let handle_method json : Logical_plan.method_ = 
  match try_retrive "method" json with
  | None -> raise (Error  "Method not specified")
  | Some(m) -> m |> JsonUtil.to_string


let handle_tuples json : Logical_plan.tuples = 
  let is_tuple str = 
    let rgx = Str.regexp "^.+\\..+$" in 
    Str.string_match rgx str 0 in
  match try_retrive "tuples" json with 
  | None -> raise(Error  "Tuples not specified")
  | Some(arr_t) -> 
    (* todo make this better *)
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_t) in
      let filtered_arr = List.filter is_tuple arr in 
      let split_str = Str.split (Str.regexp "\\.") in
      let helper str = 
        let splitted_arr = split_str str in 
          if List.length splitted_arr < 2 then raise (Error  "Wrong tuple definition")
          else ((List.nth splitted_arr 0), (List.nth splitted_arr 1)) in
        List.map helper filtered_arr


let handle_froms json : Logical_plan.froms =
  let is_from str =
    let rgx = Str.regexp "^[a-zA-Z]*([a-zA-Z]+)" in 
    Str.string_match rgx str 0 in 
  match try_retrive "froms" json with 
  | None -> raise (Error  "Froms not specified")
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in
    let filtered_arr = List.filter is_from arr in 
    (* This should have following form : ["table name"; "alias>"] *)
    let split_str = Str.split (Str.regexp "(") in
    let helper str = 
      let splitted = split_str str in 
        if List.length splitted < 2 then raise (Error  "Wrong from definition")
        else 
          let tname = (List.nth splitted 0) and a = (List.nth splitted 1) in 
          let n = String.length a in if n = 0 then raise (Error  "Wrong alias") else 
            ((String.lowercase_ascii tname), remove_last_char a) in 
    List.map helper filtered_arr


let handle_joins json : (string * Logical_plan.tpredicate) list =
  (*  joins: [alias<predicate>, ...] *)
  (* let is_join str =
    let rgx = Str.regexp "^[a-zA-Z]*<[a-zA-Z]+>" in 
    Str.string_match rgx str 0 in  *)
  match try_retrive "joins" json with 
  | None -> []
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in
    (* let filtered_arr = List.filter (fun s ->
      let _s = remove_last_char s in
      if (is_join _s) || (is_tpredicate _s) then true else false  
    ) arr in  *)
    (* Logs.info(fun m -> m "Filtered array for joins: %a" (Fmt.list Fmt.string) filtered_arr); *)
    (* This should have following form : ["table name"; "alias>"] *)
    let split_str = Str.split (Str.regexp "<") in
    let helper str = 
      let splitted = split_str str in 
        if List.length splitted < 2 then raise (Error  "Wrong join definition")
        else 
          let tname = (List.nth splitted 0) and p = (List.nth splitted 1) in 
          let n = String.length p in if n = 0 then raise (Error  "191: Wrong predicate") else 
            let predicate = remove_last_char p in 
              if Logical_plan.is_tpredicate predicate then 
                ((String.lowercase_ascii tname), (Logical_plan.tpredicate_from_string predicate))
              else raise (Error "Only tpredicates are allowed for joins") in 
    List.map helper arr

let handle_predicates json : (Logical_plan.cpredicates * Logical_plan.tpredicates) = 
  match try_retrive "predicates" json with 
  | None -> raise (Error  "Predicates not specified")
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in 
    List.fold_left (fun acc elem -> 
      if Logical_plan.is_cpredicate elem then
         ((List.append (fst acc) [Logical_plan.cpredicate_from_string elem]), (snd acc)) 
      else ((fst acc), (List.append (snd acc) [Logical_plan.tpredicate_from_string elem]))  
    ) ([], []) arr 



let parse_to_logical_plan json : Logical_plan.logical_plan = print_json json;
  let m = handle_method json 
  and t = handle_tuples json 
  and fms = handle_froms json 
  and jns = handle_joins json
  and preds = handle_predicates json in 
  let lp = {Logical_plan.met=m;Logical_plan.tups=t;Logical_plan.fms=fms;Logical_plan.jns=jns;Logical_plan.cps=(fst preds);Logical_plan.tps=(snd preds);} in 
  Logs.info(fun m -> m "LP: %a" Logical_plan.pp_logical_plan lp); lp