open Lwt.Syntax
open Types

let payment_processor_default_url = "http://payment-processor-default:8080"
let payment_processor_fallback_url = "http://payment-processor-fallback:8080"

(* Health check cache *)
type health_status = {
  failing : bool;
  min_response_time : int;
  last_check : float;
}

let default_health = ref { failing = false; min_response_time = 100; last_check = 0.0 }
let fallback_health = ref { failing = false; min_response_time = 100; last_check = 0.0 }

let check_health url =
  let uri = Uri.of_string (url ^ "/payments/service-health") in
  try
    let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
    let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    if status_code = 200 then (
      let* body_string = Cohttp_lwt.Body.to_string body in
      try
        let json = Yojson.Safe.from_string body_string in
        let open Yojson.Safe.Util in
        let failing = json |> member "failing" |> to_bool in
        let min_response_time = json |> member "minResponseTime" |> to_int in
        Lwt.return { failing; min_response_time; last_check = Unix.time () }
      with _ ->
        Lwt.return { failing = true; min_response_time = 100; last_check = Unix.time () }
    ) else
      Lwt.return { failing = true; min_response_time = 100; last_check = Unix.time () }
  with _ ->
    Lwt.return { failing = true; min_response_time = 100; last_check = Unix.time () }

let should_check_health last_check =
  let now = Unix.time () in
  now -. last_check >= 5.0

let get_health url health_ref =
  if should_check_health !health_ref.last_check then (
    let* new_health = check_health url in
    health_ref := new_health;
    Lwt.return new_health
  ) else
    Lwt.return !health_ref

let get_iso_time () =
  let now = Unix.time () in
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let make_payment_request url amount correlation_id _health_info =
  let uri = Uri.of_string (url ^ "/payments") in
  let body_json = `Assoc [
    ("requestedAt", `String (get_iso_time ()));
    ("amount", `Float amount);
    ("correlationId", `String correlation_id);
  ] in
  let body = Yojson.Safe.to_string body_json in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  
  (* No artificial waiting - use health info only for routing decisions *)
  
  (* Add retry logic for failed requests *)
  let rec retry_request retries_left =
    try
      let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body:(`String body) uri in
      let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let* _body_string = Cohttp_lwt.Body.to_string resp_body in
      Lwt.return status_code
    with 
    | _exn when retries_left > 0 -> 
      let* () = Lwt_unix.sleep 0.001 in (* Just 1ms retry delay *)
      retry_request (retries_left - 1)
    | _exn -> 
      Lwt.return 500
  in
  retry_request 1

let process_with_default (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  let iso_time = get_iso_time () in
  let* health = get_health payment_processor_default_url default_health in
  let* status_code = make_payment_request payment_processor_default_url queue_message.amount queue_message.correlation_id health in
  if status_code = 200 then
    let response = {
      requested_at = iso_time;
      amount = queue_message.amount;
      correlation_id = queue_message.correlation_id;
      payment_processor = Default;
    } in
    Lwt.return_some response
  else
    Lwt.return_none

let process_with_fallback (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  let iso_time = get_iso_time () in
  let* health = get_health payment_processor_fallback_url fallback_health in
  let* status_code = make_payment_request payment_processor_fallback_url queue_message.amount queue_message.correlation_id health in
  if status_code = 200 then
    let response = {
      requested_at = iso_time;
      amount = queue_message.amount;
      correlation_id = queue_message.correlation_id;
      payment_processor = Fallback;
    } in
    Lwt.return_some response
  else
    Lwt.return_none

let process_payment (queue_message : Types.queue_message) _is_fire_mode : Types.payment_data_response option Lwt.t =
  (* Check health of both processors *)
  let* default_health_info = get_health payment_processor_default_url default_health in
  let* fallback_health_info = get_health payment_processor_fallback_url fallback_health in
  
  (* Choose which processor to try first based on health status *)
  let (first_processor, second_processor) = 
    if default_health_info.failing && not fallback_health_info.failing then
      (* Default is failing, fallback is healthy - try fallback first *)
      (process_with_fallback, process_with_default)
    else
      (* Default case: try default first, then fallback *)
      (process_with_default, process_with_fallback)
  in
  
  (* Try first processor *)
  let* first_result = first_processor queue_message in
  match first_result with
  | Some result -> Lwt.return_some result
  | None ->
    (* First failed, try second processor *)
    second_processor queue_message
