module Ws = Dream
module Json = Yojson.Basic

let handle_json (json: Json.t) = 
  Parser.parse_to_logical_plan json

let handler client =
  let rec loop () = 
    match%lwt Ws.receive client with 
    | Some buf ->
      let json = Json.from_string buf in 
      let lp = handle_json json in 
      Join_opt.get_best_plan lp; loop ();
    | None -> Ws.close_websocket client in loop ()

let routes = [
  Ws.get "/"  (fun _ -> Ws.websocket handler)
]

let run_server () = Ws.run @@ Ws.logger @@ Ws.router routes 