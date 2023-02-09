exception Error of string

type stats = {
  ncard: int;
  tcard: int;
  icard: int;
  nindx: int;
}


let get_property_by_name (xs: (string * int) list ) (prop: string) : int =
  let rec loop = function
  | (name, value) :: tl -> if name = prop then value else (loop tl)
  | [] -> -1 in 
  loop xs 


let print s = 
  print_string "NCARD: "; print_int s.ncard; print_newline (); 
  print_string "TCARD: "; print_int s.tcard; print_newline (); 
  print_string "ICARD: "; print_int s.icard; print_newline (); 
  print_string "NINDX: "; print_int s.nindx; print_newline ()

let get_stats (relation_name: string) (attribute: string) : stats  = 
  let file = ("statistics/" ^ relation_name ^ "/" ^ attribute ^ "/stats") in 
  let lines = BatFile.lines_of file in 
    let tmp =  (BatList.of_enum (BatEnum.map (fun s -> 
      let splitted = (Str.split (Str.regexp ";") s) in 
        ((String.trim (List.hd splitted)), (int_of_string ((String.trim (List.hd (List.tl splitted))))))  (* ugh refactor *)
    )  lines)) in 
    let ncard = get_property_by_name tmp "ncard"
    and tcard = get_property_by_name tmp "tcard" 
    and  icard = get_property_by_name tmp "icard" 
    and nindx = get_property_by_name tmp "nindx" in
    {ncard=ncard;tcard=tcard;icard=icard;nindx=nindx}

let get_ncard_only (relation_name: string) (attribute: string) =
  let file = ("statistics/" ^ relation_name ^ "/" ^ attribute ^ "/stats") in 
    let lines = BatFile.lines_of file in 
      let (_, v) = BatEnum.find (fun s -> (fst s) = "ncard") (BatEnum.map (fun s -> 
        let splitted = (Str.split (Str.regexp ";") s) in 
          ((String.trim (List.hd splitted)), (int_of_string ((String.trim (List.hd (List.tl splitted))))))  (* ugh refactor *)
      )  lines) in v