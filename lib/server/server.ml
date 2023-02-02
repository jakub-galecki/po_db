module Ws = Dream
module Json = Yojson.Basic

let handle_json (json: Json.t) = 
  Parser.parse_to_logical_plan json

let handler client =
  let rec loop () = 
    match%lwt Ws.receive client with 
    | Some buf ->
      let json = Json.from_string buf in 
      handle_json json; loop ()
    | None -> Ws.close_websocket client in loop ()

let routes = [
  Ws.get "/"  (fun _ -> Ws.websocket handler)
]

let run_server () = Ws.run @@ Ws.logger @@ Ws.router routes 