open Lwt.Syntax
open Types

let payment_processor_default_url = "http://payment-processor-default:8080"
let payment_processor_fallback_url = "http://payment-processor-fallback:8080"

(* Background health monitoring with cached snapshot *)
type health_snapshot = {
  healthy : bool;
  min_response_time : int; (* ms *)
  last_updated : float;
}

let default_health = ref { healthy = true; min_response_time = 0; last_updated = 0.0 }
let fallback_health = ref { healthy = true; min_response_time = 0; last_updated = 0.0 }

let fetch_health url =
  let uri = Uri.of_string (url ^ "/payments/service-health") in
  try
    let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
    let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let* body_str = Cohttp_lwt.Body.to_string body in
    if code = 200 then (
      try
        let json = Yojson.Safe.from_string body_str in
        let open Yojson.Safe.Util in
        let failing = json |> member "failing" |> to_bool in
        let min_rt = json |> member "minResponseTime" |> to_int in
        Lwt.return { healthy = (not failing); min_response_time = min_rt; last_updated = Unix.time () }
      with _ -> Lwt.return { healthy = false; min_response_time = max_int; last_updated = Unix.time () }
    ) else (
      Lwt.return { healthy = false; min_response_time = max_int; last_updated = Unix.time () }
    )
  with _ -> Lwt.return { healthy = false; min_response_time = max_int; last_updated = Unix.time () }

let rec health_loop url health_ref =
  let* snap = fetch_health url in
  health_ref := snap;
  (* Respect 1 call per 5s limit *)
  let* () = Lwt_unix.sleep 5.0 in
  health_loop url health_ref

let monitors_started = ref false

let ensure_health_monitors_started () =
  if not !monitors_started then (
    monitors_started := true;
    Lwt.async (fun () -> health_loop payment_processor_default_url default_health);
    Lwt.async (fun () -> health_loop payment_processor_fallback_url fallback_health)
  )

let get_iso_time () =
  let now = Unix.time () in
  (* Convert to milliseconds and round to match JavaScript behavior *)
  let millis = Int64.of_float (now *. 1000.0) in
  let seconds = Int64.div millis 1000L in
  let ms_part = Int64.rem millis 1000L in
  let tm = Unix.gmtime (Int64.to_float seconds) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03LdZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec ms_part

let make_payment_request url amount correlation_id ~timeout_s =
  let uri = Uri.of_string (url ^ "/payments") in
  let iso_time = get_iso_time () in
  let body = Printf.sprintf 
    "{\"requestedAt\":\"%s\",\"amount\":%.2f,\"correlationId\":\"%s\"}"
    iso_time amount correlation_id in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  
  let request_promise = 
    Lwt.catch
      (fun () ->
        let* (resp, resp_body) = Cohttp_lwt_unix.Client.post ~headers ~body:(`String body) uri in
        let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
        let* _response_body = Cohttp_lwt.Body.to_string resp_body in
        if status_code = 200 then Lwt.return (true, iso_time) else Lwt.return (false, iso_time))
      (function
        | Lwt.Canceled -> Lwt.return (false, iso_time)
        | _ -> Lwt.return (false, iso_time))
  in
  let timeout_promise =
    let* () = Lwt_unix.sleep timeout_s in
    Lwt.return (false, iso_time)
  in
  Lwt.catch
    (fun () -> Lwt.pick [request_promise; timeout_promise])
    (function
      | Lwt.Canceled -> Lwt.return (false, iso_time)
      | _ -> Lwt.return (false, iso_time))

let mk_response (processor:Types.payment_processor) (iso_time:string) (queue_message:Types.queue_message) : Types.payment_data_response =
  {
    requested_at = iso_time;
    amount = queue_message.amount;
    correlation_id = queue_message.correlation_id;
    payment_processor = processor;
  }

let try_processor (processor:Types.payment_processor) (url:string) (queue_message:Types.queue_message) ~(timeout_s:float) : Types.payment_data_response option Lwt.t =
  let* (ok, iso_time) = make_payment_request url queue_message.amount queue_message.correlation_id ~timeout_s in
  if ok then Lwt.return_some (mk_response processor iso_time queue_message) else Lwt.return_none

let process_payment (queue_message : Types.queue_message) : Types.payment_data_response option Lwt.t =
  (* Ensure background health monitors are running *)
  ensure_health_monitors_started ();

  (* Snapshot health *)
  let d = !default_health in
  let f = !fallback_health in

  (* Preferred order; default wins ties *)
  let ordered =
    if d.healthy && f.healthy then
      if d.min_response_time <= f.min_response_time then
        [ (Default, payment_processor_default_url);
          (Fallback, payment_processor_fallback_url) ]
      else
        [ (Fallback, payment_processor_fallback_url);
          (Default, payment_processor_default_url) ]
    else if d.healthy then
      [ (Default, payment_processor_default_url) ]
    else if f.healthy then
      [ (Fallback, payment_processor_fallback_url) ]
    else
      [ (Default, payment_processor_default_url);
        (Fallback, payment_processor_fallback_url) ]
  in

  let timeout_s = 1.5 in
  match ordered with
  | [] -> Lwt.return_none
  | [ (proc, url) ] ->
      try_processor proc url queue_message ~timeout_s
  | (p1_proc, p1_url) :: (p2_proc, p2_url) :: _ ->
      let hedge_delay = 0.2 in
      let resolved = ref false in
      let result_t, result_w = Lwt.wait () in

      let p1 = try_processor p1_proc p1_url queue_message ~timeout_s in
      Lwt.on_success p1 (function
        | Some _ as r when not !resolved ->
            resolved := true; Lwt.wakeup_later result_w r
        | _ -> ());

      let p2 =
        let* () = Lwt_unix.sleep hedge_delay in
        if !resolved then Lwt.return_none
        else try_processor p2_proc p2_url queue_message ~timeout_s
      in
      Lwt.on_success p2 (function
        | Some _ as r when not !resolved ->
            resolved := true; Lwt.wakeup_later result_w r
        | _ -> ());

      let none_when_both_finish =
        let* _ = Lwt.both p1 p2 in
        Lwt.return_none
      in
      Lwt.pick [ result_t; none_when_both_finish ]
