exception Error of string

type stats = {
  mutable ncard: int;
  mutable tcard: int;
  mutable icard: int;
  mutable nindx: int;
}

let stats_init_default : stats = 
  {
    ncard = 0;
    tcard = 0;
    icard = 0;
    nindx = 0;
  }

let print s = 
  print_string "NCARD: "; print_int s.ncard; print_newline (); 
  print_string "TCARD: "; print_int s.tcard; print_newline (); 
  print_string "ICARD: "; print_int s.icard; print_newline (); 
  print_string "NINDX: "; print_int s.nindx; print_newline ()

let get_stats (relation_name: string) (attribute: string) : stats  = 
  let file = ("statistics/" ^ relation_name ^ "/" ^ attribute ^ "/stats") in
  let lines = BatFile.lines_of file in 
    let tmp = BatEnum.map (fun s -> 
      let splitted = (Str.split (Str.regexp ";") s) in 
        ((String.trim (List.hd splitted)), (int_of_string ((String.trim (List.hd (List.tl splitted))))))  (* ugh refactor *)
    )  lines in 
    let ret = stats_init_default in 
    let _ = BatEnum.iter (fun s -> match s with 
          | ("ncard", value) -> ret.ncard <- value; ()
          | ("tcard", value) -> ret.tcard <- value; ()
          | ("icard", value) -> ret.icard <- value; ()
          | ("nindx", value) -> ret.nindx <- value; ()
          | _ -> raise (Error "Unkown field in stats")
        ) tmp in 
        ret

let get_ncard s = s.ncard;
    