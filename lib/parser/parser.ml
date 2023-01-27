module Json = Yojson.Basic
module JsonUtil = Yojson.Basic.Util

exception Error of string

(*
  This should be rewritten as monad with state   
*)

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



type alias =  string * string [@@deriving show] 
type property = string * string [@@deriving show]

type tpredicate = {
  x: property; 
  op: string;
  y: property;
} [@@deriving show] 

(* tpredicate TableName.property operator TableName.property *)
let is_tpredicate str = 
    let regx = Str.regexp "[a-zA-z1-9]+\\.[a-zA-z1-9]+ . [a-zA-z1-9]+\\.[a-zA-z1-9]+" in
  Str.string_match regx str 0

let tpredicate_from_string str = 
  let split_by_dot = Str.split (Str.regexp "\\.") in
  let splitted_pred = Str.split (Str.regexp " ") str in
  match splitted_pred with 
  | x :: op :: y :: _ -> 
    begin
    let a1 = split_by_dot x and a2 = split_by_dot y in 
      match a1, a2 with 
      | tname1 :: property1 :: _, tname2 :: property2 :: _ -> 
        begin 
        {x = (tname1,property1); op= op; y= (tname2, property2)}
        end
      | _ -> raise (Error  "52: Wrong predicate")
    end
  | _ -> raise (Error  "54: Wrong predicate")


type cpredicate = {
  x: property; 
  op: string;
  y: string;
} [@@deriving show]

let is_cpredicate str =  
    let regx = Str.regexp "[a-zA-z1-9]+\\.[a-zA-z1-9]+ . [a-zA-Z1-9]+" in
  Str.string_match regx str 0

(* a.tuple <operator> constant*)
let cpredicate_from_string str = 
  let split_by_dot = Str.split (Str.regexp "\\.") in
  match Str.split (Str.regexp " ") str with 
  | x :: op :: y :: _ -> 
    begin
    let a1 = split_by_dot x  in 
      match a1, y with 
      | tname1 :: property1 :: _, const -> 
        begin 
        {x = (tname1,property1); op= op; y= const }
        end
      | _ -> raise (Error  "79: Wrong predicate")
    end
  | _ -> raise (Error  "81: Wrong predicate")

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

(* Implement custom formatters *)

(* let is_table_alias str = 
  let rgx = Str.regexp "^[a-aA-Z]*\\([a-aA-Z]+\\)" in 
  Str.string_match rgx str 0 *)
  
let print_json (json :Json.t) = 
  Logs.info(fun m -> m "Parsed json: %a" Json.pp json)

(* Wraper todo docs *)
let try_retrive member json =
  let j = Json.Util.member member json in match j with 
  | `Null -> None
  | _ -> Some(j)

(* 
let members = [
  "method";
  "tuples";
  "froms";
  "joins";
  "predicates";
] 
*)

let remove_last_char str = 
  let n = String.length str in 
    if n = 0 then 
      raise (Error  "Can not remove char from empty string")
    else String.sub str 0 (n - 1) 


let handle_method json : method_ = 
  match try_retrive "method" json with
  | None -> raise (Error  "Method not specified")
  | Some(m) -> m |> JsonUtil.to_string


let handle_tuples json : tuples = 
  let is_tuple str = 
    let rgx = Str.regexp "^.+\\..+$" in 
    Str.string_match rgx str 0 in
  match try_retrive "tuples" json with 
  | None -> raise (Error  "Tuples not specified")
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


let handle_froms json : froms =
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
            (tname, remove_last_char a) in 
    List.map helper filtered_arr


let handle_joins json : (string * tpredicate) list =
  (*  joins: [alias<predicate>, ...] *)
  (* let is_join str =
    let rgx = Str.regexp "^[a-zA-Z]*<[a-zA-Z]+>" in 
    Str.string_match rgx str 0 in  *)
  match try_retrive "joins" json with 
  | None -> raise (Error  "Froms not specified")
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
              if is_tpredicate predicate then 
                (tname, (tpredicate_from_string predicate))
              else raise (Error "Only tpredicates are allowed for joins") in 
    List.map helper arr

let handle_predicates json : (cpredicates * tpredicates) = 
  match try_retrive "predicates" json with 
  | None -> raise (Error  "Froms not specified")
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in 
    List.fold_left (fun acc elem -> 
      if is_cpredicate elem then
         ((List.append (fst acc) [cpredicate_from_string elem]), (snd acc))
      else ((fst acc), (List.append (snd acc) [tpredicate_from_string elem]))  
    ) ([], []) arr 


(* TODO: Create method to create LOGICAL PLAN *)
let parse_to_logical_plan json = print_json json;
  let m = handle_method json 
  and t = handle_tuples json 
  and fms = handle_froms json 
  and jns = handle_joins json
  and preds = handle_predicates json in 
  let lp = {met=m;tups=t;fms=fms;jns=jns;cps=(fst preds);tps=(snd preds)} in 
  Logs.info(fun m -> m "LP: %a" pp_logical_plan lp)

  



