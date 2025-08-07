open Lwt.Syntax
open Types

let payment_processor_default_url = "http://payment-processor-default:8080"
let payment_processor_fallback_url = "http://payment-processor-fallback:8080"

(* Simplified health status - only check when payments fail *)
type health_status = {
  is_healthy : bool;
  last_check : float;
}

let default_health = ref { is_healthy = true; last_check = 0.0 }
let fallback_health = ref { is_healthy = true; last_check = 0.0 }

let check_health_if_needed url health_ref =
  let now = Unix.time () in
  if now -. !health_ref.last_check >= 10.0 then (
    let uri = Uri.of_string (url ^ "/payments/service-health") in
    try
      let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
      let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let* _body_string = Cohttp_lwt.Body.to_string body in
      let is_healthy = (status_code = 200) in
      health_ref := { is_healthy; last_check = now };
      Lwt.return is_healthy
    with _ ->
      health_ref := { is_healthy = false; last_check = now };
      Lwt.return false
  ) else
    Lwt.return !health_ref.is_healthy

let mark_unhealthy health_ref =
  health_ref := { !health_ref with is_healthy = false }

let get_iso_time () =
  let now = Unix.time () in
  let seconds = floor now in
  let milliseconds = int_of_float ((now -. seconds) *. 1000.0) in
  let tm = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec milliseconds

let make_payment_request url amount correlation_id =
  let uri = Uri.of_string (url ^ "/payments") in
  let iso_time = get_iso_time () in
  let body = Printf.sprintf 
    "{\"requestedAt\":\"%s\",\"amount\":%.2f,\"correlationId\":\"%s\"}"
    iso_time amount correlation_id in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  
  (* Add timeout to prevent hanging requests *)
  let timeout_duration = 2.0 in (* 2 second timeout *)
  let request_promise = 
    try
      let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body:(`String body) uri in
      let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let* _body_string = Cohttp_lwt.Body.to_string resp_body in
      Lwt.return (status_code = 200)
    with 
    | _exn -> Lwt.return false
  in
  let timeout_promise = 
    let* () = Lwt_unix.sleep timeout_duration in
    Lwt.return false
  in
  Lwt.pick [request_promise; timeout_promise]

let process_with_default (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  let iso_time = get_iso_time () in
  let* success = make_payment_request payment_processor_default_url queue_message.amount queue_message.correlation_id in
  if success then (
    let response = {
      requested_at = iso_time;
      amount = queue_message.amount;
      correlation_id = queue_message.correlation_id;
      payment_processor = Default;
    } in
    Lwt.return_some response
  ) else (
    mark_unhealthy default_health;
    Lwt.return_none
  )

let process_with_fallback (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  let iso_time = get_iso_time () in
  let* success = make_payment_request payment_processor_fallback_url queue_message.amount queue_message.correlation_id in
  if success then (
    let response = {
      requested_at = iso_time;
      amount = queue_message.amount;
      correlation_id = queue_message.correlation_id;
      payment_processor = Fallback;
    } in
    Lwt.return_some response
  ) else (
    mark_unhealthy fallback_health;
    Lwt.return_none
  )

let process_payment (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  (* Check health status only occasionally, not on every payment *)
  let* default_healthy = check_health_if_needed payment_processor_default_url default_health in
  let* fallback_healthy = check_health_if_needed payment_processor_fallback_url fallback_health in
  
  (* Choose which processor to try first based on health status *)
  let (first_processor, second_processor) = 
    if not default_healthy && fallback_healthy then
      (* Default is unhealthy, fallback is healthy - try fallback first *)
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
