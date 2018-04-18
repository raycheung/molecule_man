open Opium.Std
open Lwt
open Cohttp_lwt_unix

let opsgenie_create_alert
    ?api_key:(api_key=Unix.getenv "OPSGENIE_API_KEY")
    ?description:(description="")
    message =
  let json = Ezjsonm.(dict [("message", (string message)); ("description", (string description))]) in
  let req_h = Cohttp.Header.init() in
  let req_h = Cohttp.Header.add_list req_h
              [("authorization", "GenieKey " ^ api_key);
               ("content-type", "application/json")] in
  let req_body = Cohttp_lwt.Body.of_string(Ezjsonm.to_string json) in
  Client.post (Uri.of_string "https://api.opsgenie.com/v2/alerts")
    ~body:req_body ~headers:req_h >>= fun (resp, body) ->
      let code = resp |> Response.status |> Cohttp.Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      body |> Cohttp_lwt.Body.to_string

let parse_event json_t =
  let json = Ezjsonm.value json_t in
  let snitch = Ezjsonm.find json ["data"; "snitch"] in
  let event_type = Ezjsonm.(get_string (find json ["type"])) in
  let name = Ezjsonm.(get_string (find snitch ["name"])) in
  let token = Ezjsonm.(get_string (find snitch ["token"])) in
  let status = Ezjsonm.(get_string (find snitch ["status"])) in
  let previous_status = Ezjsonm.(get_string (find snitch ["previous_status"])) in
  (event_type, previous_status, status, name, token)

let report_to_opsgenie req =
  req |> App.json_of_body_exn |> Lwt.map (fun json_t ->
    match parse_event json_t with
    | ("snitch.reporting", "missing", "healthy", name, _) ->
        respond (`String ("Service " ^ name ^ " resumed"))
    | ("snitch.missing", "healthy", "missing", name, token) -> begin
      let msg = name ^ " hasn't checked in" in
      let desc = "https://deadmanssnitch.com/snitches/" ^ token in
      let t = opsgenie_create_alert msg ~description:desc in
      let resp = Lwt_main.run t in
      respond (`String resp)
    end
    | _ -> respond (`String "Bad request") ~code:(Cohttp.Code.status_of_code 400))

let _ =
  App.empty
  |> post "/report_to_opsgenie" report_to_opsgenie
  |> App.run_command
