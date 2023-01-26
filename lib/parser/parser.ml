module Json = Yojson.Basic
module JsonUtil = Yojson.Basic.Util

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


(* TODO : ALL OF THIS SHIT WORKS ONLY FOR ONE ELEM *)

type alias =  string * string
type property = string * string

type tpredicate = {
  x: property; 
  op: string;
  y: property;
}

(* tpredicate TableName.property operator TableName.property *)
let is_tpredicate str = 
    let regx = Str.regexp "[a-zA-z1-9]+\\.[a-zA-z1-9]+ . [a-zA-z1-9]+\\.[a-zA-z1-9]+" in
  Str.string_match regx str 0

let tpredicate_from_string str = 
  let split_by_dot = Str.split (Str.regexp "\\.") in
  match Str.split (Str.regexp " ") str with 
  | x :: op :: y :: _ -> 
    begin
    let a1 = split_by_dot x and a2 = split_by_dot y in 
      match a1, a2 with 
      | tname1 :: property1 :: _, tname2 :: property2 :: _ -> 
        begin 
        {x = (tname1,property1); op= op; y= (tname2, property2)}
        end
      | _ -> failwith "Wrong predicate"
    end
  | _ -> failwith "Wrong predicate"


type cpredicate = {
  x: property; 
  op: string;
  y: string;
}

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
      | _ -> failwith "Wrong predicate"
    end
  | _ -> failwith "Wrong predicate"

type method_ = string
type tuples = (string * string) list
type froms = alias list
type joins = (string * tpredicate) list
type tpredicates = tpredicate list
type cpredicates = cpredicate list

(* type logicalPlan = {
  met: method_ ref;
  tups: tuples ref;
  fms: froms ref;
  jns: joins ref;
  tps : tpredicates ref;
  cps : cpredicates ref;
} *)

(* let is_table_alias str = 
  let rgx = Str.regexp "^[a-aA-Z]*\\([a-aA-Z]+\\)" in 
  Str.string_match rgx str 0 *)
  
let print_json (json :Json.t) = 
  Logs.info( fun m -> m "Parsed json: %a" Json.pp json)

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
      failwith "Can not remove char from empty string"
    else String.sub str 0 (n - 1) 


let handle_method json : method_ = 
  match try_retrive "method" json with
  | None -> failwith "Method not specified"
  | Some(m) -> m |> JsonUtil.to_string


let handle_tuples json : tuples = 
  let is_tuple str = 
    let rgx = Str.regexp "^.+\\..+$" in 
    Str.string_match rgx str 0 in
  match try_retrive "tuples" json with 
  | None -> failwith "Tuples not specified"
  | Some(arr_t) -> 
    (* todo make this better *)
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_t) in
      let filtered_arr = List.filter is_tuple arr in 
      let split_str = Str.split (Str.regexp "\\.") in
      let helper str = 
        let splitted_arr = split_str str in 
          if List.length splitted_arr < 2 then failwith "Wrong tuple definition"
          else ((List.nth splitted_arr 0), (List.nth splitted_arr 1)) in
        List.map helper filtered_arr


let handle_froms json : froms =
  let is_from str =
    let rgx = Str.regexp "^[a-aA-Z]*\\([a-aA-Z]+\\)" in 
    Str.string_match rgx str 0 in 
  match try_retrive "froms" json with 
  | None -> failwith "Froms not specified"
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in
    let filtered_arr = List.filter is_from arr in 
    (* This should have following form : ["table name"; "alias>"] *)
    let split_str = Str.split (Str.regexp "\\(") in
    let helper str = 
      let splitted = split_str str in 
        if List.length splitted < 2 then failwith "Wrong from definition"
        else 
          let tname = (List.nth splitted 0) and a = (List.nth splitted 1) in 
          let n = String.length a in if n = 0 then failwith "Wrong alias" else 
            (tname, remove_last_char a) in 
    List.map helper filtered_arr


let handle_joins json : (string * cpredicate) list * (string * tpredicate) list =
  (*  joins: [alias<predicate>, ...] *)
  let is_from str =
    let rgx = Str.regexp "^[a-aA-Z]*\\<[a-aA-Z]+\\>" in 
    Str.string_match rgx str 0 in 
  match try_retrive "froms" json with 
  | None -> failwith "Froms not specified"
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in
    let filtered_arr = List.filter is_from arr in 
    (* This should have following form : ["table name"; "alias>"] *)
    let split_str = Str.split (Str.regexp "\\(") in
    let helper str = 
      let splitted = split_str str in 
        if List.length splitted < 2 then failwith "Wrong join definition"
        else 
          let tname = (List.nth splitted 0) and p = (List.nth splitted 1) in 
          let n = String.length p in if n = 0 then failwith "Wrong predicate" else 
            let predicate = remove_last_char p in 
              if is_cpredicate predicate then 
                Either.left(tname, (cpredicate_from_string predicate))
              else Either.right(tname, (tpredicate_from_string predicate)) in
    let r = List.map helper filtered_arr in
     List.fold_left (fun acc elem -> 
        match elem with 
        | Either.Left(p) -> ((List.append (fst acc) [p]), (snd acc))
        | Either.Right(p) -> ((fst acc), (List.append (snd acc) [p]))
      ) ([], []) r


let handle_predicates json : (cpredicates * tpredicates) = 
  match try_retrive "froms" json with 
  | None -> failwith "Froms not specified"
  | Some(arr_f) -> 
    let arr = JsonUtil.filter_string (JsonUtil.to_list arr_f) in 
    List.fold_left (fun acc elem -> 
      if is_cpredicate elem then
         ((List.append (fst acc) [cpredicate_from_string elem]), (snd acc))
      else ((fst acc), (List.append (snd acc) [tpredicate_from_string elem]))  
    ) ([], []) arr 


let parse_to_logical_plan json = print_json json;
  let m = handle_method json 
  and _ = handle_tuples json 
  and _ = handle_froms json 
  and _ = handle_joins json
  and _ = handle_predicates json in 
  print_endline m;


