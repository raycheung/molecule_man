open Opium.Std
open Cohttp

let parse json_t =
  let json = Ezjsonm.value json_t in
  let snitch = Ezjsonm.find json ["data"; "snitch"] in
  let event_type = Ezjsonm.(get_string (find json ["type"])) in
  let name = Ezjsonm.(get_string (find snitch ["name"])) in
  let status = Ezjsonm.(get_string (find snitch ["status"])) in
  let previous_status = Ezjsonm.(get_string (find snitch ["previous_status"])) in
  match (event_type, previous_status, status) with
  | ("snitch.reporting", "missing", "healthy") ->
      Some ("Service " ^ name ^ " resumed")
  | ("snitch.missing", "healthy", "missing") ->
      Some ("Service " ^ name ^ " missed")
  | _ -> None

let transform req =
  req |> App.json_of_body_exn |> Lwt.map (fun json_t ->
    match parse json_t with
    | Some s -> respond (`String s)
    | None -> respond (`String "Bad request") ~code:(Code.status_of_code 400))

let _ =
  App.empty
  |> post "/transform" transform
  |> App.run_command
