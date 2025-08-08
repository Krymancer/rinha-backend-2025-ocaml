open Lwt.Syntax

type payment_summary = {
  total_requests : int;
  total_amount : float;
}

type payment_summary_response = {
  default : payment_summary;
  fallback : payment_summary;
}

let process_state data from_timestamp to_timestamp =
  (* Fallback list-based logic, kept for compatibility in any remaining callers *)
  let summary = ref { total_requests = 0; total_amount = 0.0 } in
  List.iter (fun (entry : Storage.storage_entry) ->
    let is_out_of_range = 
      (match from_timestamp with
       | Some ts -> Int64.compare entry.requested_at ts < 0
       | None -> false) ||
      (match to_timestamp with
       | Some ts -> Int64.compare entry.requested_at ts > 0
       | None -> false)
    in
    if not is_out_of_range then (
      summary := { 
        total_requests = !summary.total_requests + 1;
        total_amount = !summary.total_amount +. Money.cents_to_float entry.amount
      }
    )
  ) data;
  !summary

let process_state_fold (storage: Storage.BitPackingStorage.t) from_ts to_ts =
  let total_requests = ref 0 in
  let total_amount = ref 0.0 in
  let within ts =
    let ge_from = match from_ts with None -> true | Some a -> Int64.compare ts a >= 0 in
    let le_to = match to_ts with None -> true | Some b -> Int64.compare ts b <= 0 in
    ge_from && le_to
  in
  for i = 0 to storage.length - 1 do
    let idx = i * 2 in
    let delta = storage.data.(idx) in
    let amount_cents = storage.data.(idx + 1) in
    let ts = Int64.add storage.start_timestamp (Int64.of_int delta) in
    if within ts then (
      incr total_requests;
      total_amount := !total_amount +. Money.cents_to_float amount_cents
    )
  done;
  { total_requests = !total_requests; total_amount = !total_amount }

let get_foreign_state foreign_state_host from_time to_time =
  try
    let query_params = [] in
    let query_params = match from_time with
      | Some time -> ("from", [time]) :: query_params
      | None -> query_params in
    let query_params = match to_time with
      | Some time -> ("to", [time]) :: query_params
      | None -> query_params in
    let query_params = ("localOnly", ["true"]) :: query_params in
    
    (* Make HTTP call through nginx using the specific foreign API route *)
    let path = "/" ^ foreign_state_host ^ "/payments-summary" in
    let uri = Uri.make ~scheme:"http" ~host:"nginx" ~port:9999 
      ~path:path
      ~query:query_params () in
    let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
    let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let* body_string = Cohttp_lwt.Body.to_string body in
    
    if status_code = 200 then (
      try
        let json = Yojson.Safe.from_string body_string in
        let open Yojson.Safe.Util in
        let default_obj = json |> member "default" in
        let fallback_obj = json |> member "fallback" in
        
        let default_summary = {
          total_requests = default_obj |> member "totalRequests" |> to_int;
          total_amount = default_obj |> member "totalAmount" |> to_float;
        } in
        let fallback_summary = {
          total_requests = fallback_obj |> member "totalRequests" |> to_int;
          total_amount = fallback_obj |> member "totalAmount" |> to_float;
        } in
        Lwt.return { default = default_summary; fallback = fallback_summary }
      with _ ->
        Lwt.return {
          default = { total_requests = 0; total_amount = 0.0 };
          fallback = { total_requests = 0; total_amount = 0.0 };
        }
    ) else (
      Lwt.return {
        default = { total_requests = 0; total_amount = 0.0 };
        fallback = { total_requests = 0; total_amount = 0.0 };
      }
    )
  with _ -> 
    Lwt.return {
      default = { total_requests = 0; total_amount = 0.0 };
      fallback = { total_requests = 0; total_amount = 0.0 };
    }

let parse_timestamp str_opt =
  match str_opt with
  | None -> None
  | Some str ->
    try
      (* Simple timestamp parsing *)
      let time_float = Float.of_string str in
      Some (Int64.of_float (time_float *. 1000.0))
    with _ -> None

let payment_summary_service state foreign_state_host from_time to_time local_only =
  let from_timestamp = parse_timestamp from_time in
  let to_timestamp = parse_timestamp to_time in
  
  let default_summary = process_state_fold state.Storage.default from_timestamp to_timestamp in
  let fallback_summary = process_state_fold state.Storage.fallback from_timestamp to_timestamp in
  
  if local_only then
    Lwt.return { default = default_summary; fallback = fallback_summary }
  else
    let* foreign_state = get_foreign_state foreign_state_host from_time to_time in
    let combined_default = {
      total_requests = default_summary.total_requests + foreign_state.default.total_requests;
      total_amount = default_summary.total_amount +. foreign_state.default.total_amount;
    } in
    let combined_fallback = {
      total_requests = fallback_summary.total_requests + foreign_state.fallback.total_requests;
      total_amount = fallback_summary.total_amount +. foreign_state.fallback.total_amount;
    } in
    Lwt.return { default = combined_default; fallback = combined_fallback }
