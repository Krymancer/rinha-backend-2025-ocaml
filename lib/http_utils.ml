open Lwt.Syntax
open Cohttp_lwt_unix

type http_status = 
  | OK 
  | Not_found 
  | Internal_server_error

type payment_summary_query = {
  from_time : string option;
  to_time : string option;
  local_only : bool;
}

let status_to_code = function
  | OK -> 200
  | Not_found -> 404
  | Internal_server_error -> 500

let send_response status ?body () =
  let code = status_to_code status in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  let body_str = match body with
    | Some json -> Yojson.Safe.to_string json
    | None -> ""
  in
  Server.respond_string ~status:(`Code code) ~headers ~body:body_str ()

let read_body req =
  let* body = Cohttp_lwt.Body.to_string (snd req) in
  try
    let json = Yojson.Safe.from_string body in
    Lwt.return_some json
  with _ ->
    Lwt.return_none

let read_query_params uri =
  let params = Uri.query uri in
  let get_param key = 
    match List.assoc_opt key params with
    | Some [value] -> Some value
    | _ -> None
  in
  let from_time = get_param "from" in
  let to_time = get_param "to" in
  let local_only = match get_param "localOnly" with
    | Some "true" -> true
    | _ -> false
  in
  { from_time; to_time; local_only }

let parse_timestamp str_opt =
  match str_opt with
  | None -> None
  | Some str ->
    try
      (* Simple timestamp parsing - you might want to use a proper date library *)
      let time_float = Float.of_string str in
      Some (Int64.of_float (time_float *. 1000.0))
    with _ -> None
