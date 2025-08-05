open Lwt.Syntax
open Types

let payment_processor_default_url = "http://payment-processor-default:8080"
let payment_processor_fallback_url = "http://payment-processor-fallback:8080"

let get_iso_time () =
  let now = Sys.time () in
  let tm = Unix.gmtime now in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let make_payment_request url amount correlation_id =
  let uri = Uri.of_string (url ^ "/payments") in
  let body_json = `Assoc [
    ("requested_at", `String (get_iso_time ()));
    ("amount", `Float amount);
    ("correlation_id", `String correlation_id);
  ] in
  let body = Yojson.Safe.to_string body_json in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  try
    let* (resp, _) = Cohttp_lwt_unix.Client.post ~headers ~body:(`String body) uri in
    let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    Lwt.return status_code
  with _ -> Lwt.return 500

let process_with_default (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  let iso_time = get_iso_time () in
  let* status_code = make_payment_request payment_processor_default_url queue_message.amount queue_message.correlation_id in
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
  let* status_code = make_payment_request payment_processor_fallback_url queue_message.amount queue_message.correlation_id in
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

let req_count = ref 0

let process_payment (queue_message : Types.queue_message) (is_fire_mode : bool) : Types.payment_data_response option Lwt.t =
  let* default_result = process_with_default queue_message in
  match default_result with
  | Some result -> Lwt.return_some result
  | None ->
    incr req_count;
    if !req_count mod 10 = 0 && not is_fire_mode then
      process_with_fallback queue_message
    else
      Lwt.return_none
