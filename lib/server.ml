open Lwt.Syntax
open Cohttp_lwt_unix

(* Safe timestamp parsing using Ptime for ISO 8601 dates *)
let parse_timestamp_safe str_opt =
  match str_opt with
  | None -> None
  | Some str ->
    try
      (* First try parsing as Unix timestamp (float) *)
      let time_float = Float.of_string str in
      Some (Int64.of_float (time_float *. 1000.0))
    with _ ->
      (* If that fails, try parsing as ISO 8601 date using Ptime *)
      match Ptime.of_rfc3339 str with
      | Ok (ptime, _, _) ->
        let timestamp_float = Ptime.to_float_s ptime in
        Some (Int64.of_float (timestamp_float *. 1000.0))
      | Error _ -> None

let app_state = Storage.create_state ()
let task_queue : Types.queue_message Queue.queue = Queue.create ()

let process_payments_worker () =
  let rec loop () =
    let* queue_message = Queue.dequeue task_queue in
    let* result = Payment_processor.process_payment queue_message Config.config.is_fire_mode in
    (match result with
     | Some payment_response ->
       let amount = Money.float_to_cents payment_response.amount in
       let timestamp = Int64.of_float (Unix.time () *. 1000.0) in
       (match payment_response.payment_processor with
        | Types.Default -> 
          Storage.BitPackingStorage.push app_state.default amount timestamp
        | Types.Fallback -> 
          Storage.BitPackingStorage.push app_state.fallback amount timestamp)
     | None -> ());
    loop ()
  in
  loop ()

let send_response status ?body () =
  let code = match status with
    | `OK -> 200
    | `Not_found -> 404
    | `Internal_server_error -> 500
  in
  let headers = Cohttp.Header.init_with "content-type" "application/json" in
  let body_str = match body with
    | Some json -> Yojson.Safe.to_string json
    | None -> ""
  in
  Server.respond_string ~status:(`Code code) ~headers ~body:body_str ()

let read_body (_req, body) =
  let* body_str = Cohttp_lwt.Body.to_string body in
  try
    let json = Yojson.Safe.from_string body_str in
    Lwt.return_some json
  with _ ->
    Lwt.return_none

let payments_controller (req, body) =
  let* body_opt = read_body (req, body) in
  match body_opt with
  | None -> send_response `Internal_server_error ()
  | Some json ->
    try
      let open Yojson.Safe.Util in
      let amount = json |> member "amount" |> to_float in
      let correlation_id = json |> member "correlationId" |> to_string in
      if amount <= 0.0 || correlation_id = "" then (
        send_response `Internal_server_error ()
      ) else (
        let queue_message = Types.{ amount; correlation_id } in
        let* () = Queue.enqueue task_queue queue_message in
        send_response `OK ()
      )
    with _ -> send_response `Internal_server_error ()

let payments_summary_controller (req, _body) =
  let uri = Cohttp.Request.uri req in
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
  
  if local_only then (
    let from_timestamp = parse_timestamp_safe from_time in
    let to_timestamp = parse_timestamp_safe to_time in
    let default_summary = Payment_summary.process_state 
      (Storage.BitPackingStorage.list app_state.default) from_timestamp to_timestamp in
    let fallback_summary = Payment_summary.process_state 
      (Storage.BitPackingStorage.list app_state.fallback) from_timestamp to_timestamp in
    let response = Payment_summary.{
      default = default_summary;
      fallback = fallback_summary;
    } in
    let json = `Assoc [
      ("default", `Assoc [
        ("total_requests", `Int response.default.total_requests);
        ("total_amount", `Float response.default.total_amount);
      ]);
      ("fallback", `Assoc [
        ("total_requests", `Int response.fallback.total_requests);
        ("total_amount", `Float response.fallback.total_amount);
      ]);
    ] in
    send_response `OK ~body:json ()
  ) else (
    (* Get local state *)
    let from_timestamp = parse_timestamp_safe from_time in
    let to_timestamp = parse_timestamp_safe to_time in
    let local_default_summary = Payment_summary.process_state 
      (Storage.BitPackingStorage.list app_state.default) from_timestamp to_timestamp in
    let local_fallback_summary = Payment_summary.process_state 
      (Storage.BitPackingStorage.list app_state.fallback) from_timestamp to_timestamp in
    
    (* Get foreign state *)
    let* foreign_state = Payment_summary.get_foreign_state Config.config.foreign_state from_time to_time in
    
    (* Combine local + foreign states *)
    let combined_default = {
      Payment_summary.total_requests = local_default_summary.total_requests + foreign_state.default.total_requests;
      total_amount = local_default_summary.total_amount +. foreign_state.default.total_amount;
    } in
    let combined_fallback = {
      Payment_summary.total_requests = local_fallback_summary.total_requests + foreign_state.fallback.total_requests;
      total_amount = local_fallback_summary.total_amount +. foreign_state.fallback.total_amount;
    } in
    
    let json = `Assoc [
      ("default", `Assoc [
        ("total_requests", `Int combined_default.total_requests);
        ("total_amount", `Float combined_default.total_amount);
      ]);
      ("fallback", `Assoc [
        ("total_requests", `Int combined_fallback.total_requests);
        ("total_amount", `Float combined_fallback.total_amount);
      ]);
    ] in
    send_response `OK ~body:json ()
  )

let purge_payments_controller (_req, _body) =
  Storage.BitPackingStorage.reset app_state.default;
  Storage.BitPackingStorage.reset app_state.fallback;
  send_response `OK ()

let callback _conn req body =
  let uri = Cohttp.Request.uri req in
  let meth = Cohttp.Request.meth req in
  let path = Uri.path uri in
  
  match (meth, path) with
  | (`POST, "/payments") -> payments_controller (req, body)
  | (`GET, path) when String.starts_with ~prefix:"/payments-summary" path -> 
    payments_summary_controller (req, body)
  | (`POST, "/purge-payments") -> purge_payments_controller (req, body)
  | _ -> send_response `Not_found ()

let read_http_request fd =
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec read_until_end acc =
    let* n = Lwt_unix.read fd buffer 0 buffer_size in
    if n = 0 then
      Lwt.return (String.concat "" (List.rev acc))
    else
      let chunk = Bytes.sub_string buffer 0 n in
      let combined = String.concat "" (List.rev (chunk :: acc)) in
      if String.contains combined '\r' && String.contains combined '\n' then
        Lwt.return combined
      else
        read_until_end (chunk :: acc)
  in
  read_until_end []

let parse_request request_str =
  let lines = String.split_on_char '\n' request_str in
  match lines with
  | [] -> None
  | first_line :: rest ->
    let parts = String.split_on_char ' ' first_line in
    (match parts with
     | [method_str; path; _] ->
       let method_type = match String.uppercase_ascii method_str with
         | "GET" -> `GET
         | "POST" -> `POST  
         | _ -> `Other
       in
       
       (* Extract body if present *)
       let body = 
         let full_content = String.concat "\n" rest in
         if String.contains full_content '\r' then
           let parts = String.split_on_char '\r' full_content in
           match List.rev parts with
           | body_part :: _ when String.length (String.trim body_part) > 0 ->
             Some (String.trim body_part)
           | _ -> None
         else None
       in
       
       Some (method_type, path, body)
     | _ -> None)

let send_http_response fd status_code ?(content_type="application/json") body =
  let status_text = match status_code with
    | 200 -> "OK"
    | 404 -> "Not Found"  
    | 500 -> "Internal Server Error"
    | _ -> "Unknown"
  in
  let body_str = match body with Some s -> s | None -> "" in
  let response = Printf.sprintf 
    "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    status_code status_text content_type (String.length body_str) body_str
  in
  let* _ = Lwt_unix.write_string fd response 0 (String.length response) in
  Lwt.return_unit

let parse_query_params path =
  if String.contains path '?' then
    let parts = String.split_on_char '?' path in
    match parts with
    | [_; query_str] ->
      String.split_on_char '&' query_str
      |> List.map (fun param ->
          match String.split_on_char '=' param with
          | [key; value] -> (key, value)
          | _ -> ("", ""))
      |> List.filter (fun (k, _) -> k <> "")
    | _ -> []
  else []

let get_param params key =
  try Some (List.assoc key params)
  with Not_found -> None

let handle_http_request fd request_str =
  match parse_request request_str with
  | None ->
    send_http_response fd 400 ~content_type:"text/plain" (Some "Bad Request")
    
  | Some (method_type, path, body_opt) ->
    (match (method_type, String.split_on_char '?' path |> List.hd) with
     
     (* POST /payments *)
     | (`POST, "/payments") ->
       (match body_opt with
        | Some body_str ->
          (try
             let json = Yojson.Safe.from_string body_str in
             let open Yojson.Safe.Util in
             let amount = json |> member "amount" |> to_float in
             let correlation_id = json |> member "correlation_id" |> to_string in
             
             if amount > 0.0 && correlation_id <> "" then (
               let queue_message = Types.{ amount; correlation_id } in
               let* () = Queue.enqueue task_queue queue_message in
               send_http_response fd 200 (Some "")
             ) else (
               send_http_response fd 500 (Some "{\"error\":\"Invalid request data\"}")
             )
           with 
           | Yojson.Safe.Util.Type_error (msg, _) ->
             send_http_response fd 500 (Some ("{\"error\":\"JSON error: " ^ msg ^ "\"}"))
           | _ ->
             send_http_response fd 500 (Some "{\"error\":\"JSON parsing error\"}"))
        | None ->
          send_http_response fd 500 (Some "{\"error\":\"No body provided\"}"))
          
     (* GET /payments-summary *)
     | (`GET, "/payments-summary") ->
       let params = parse_query_params path in
       let from_time = get_param params "from" in
       let to_time = get_param params "to" in
       let local_only = match get_param params "localOnly" with
         | Some "true" -> true
         | _ -> false
       in
       
       if local_only then (
         let from_timestamp = parse_timestamp_safe from_time in
         let to_timestamp = parse_timestamp_safe to_time in
         let default_summary = Payment_summary.process_state 
           (Storage.BitPackingStorage.list app_state.default) from_timestamp to_timestamp in
         let fallback_summary = Payment_summary.process_state 
           (Storage.BitPackingStorage.list app_state.fallback) from_timestamp to_timestamp in
         
         let json = `Assoc [
           ("default", `Assoc [
             ("totalRequests", `Int default_summary.total_requests);
             ("totalAmount", `Float default_summary.total_amount);
           ]);
           ("fallback", `Assoc [
             ("totalRequests", `Int fallback_summary.total_requests);
             ("totalAmount", `Float fallback_summary.total_amount);
           ]);
         ] in
         send_http_response fd 200 (Some (Yojson.Safe.to_string json))
       ) else (
         (* Get local state *)
         let from_timestamp = parse_timestamp_safe from_time in
         let to_timestamp = parse_timestamp_safe to_time in
         let local_default_summary = Payment_summary.process_state 
           (Storage.BitPackingStorage.list app_state.default) from_timestamp to_timestamp in
         let local_fallback_summary = Payment_summary.process_state 
           (Storage.BitPackingStorage.list app_state.fallback) from_timestamp to_timestamp in
         
         (* Get foreign state *)
         let* foreign_state = Payment_summary.get_foreign_state Config.config.foreign_state from_time to_time in
         
         (* Combine local + foreign states *)
         let combined_default = {
           Payment_summary.total_requests = local_default_summary.total_requests + foreign_state.default.total_requests;
           total_amount = local_default_summary.total_amount +. foreign_state.default.total_amount;
         } in
         let combined_fallback = {
           Payment_summary.total_requests = local_fallback_summary.total_requests + foreign_state.fallback.total_requests;
           total_amount = local_fallback_summary.total_amount +. foreign_state.fallback.total_amount;
         } in
         
         let json = `Assoc [
           ("default", `Assoc [
             ("totalRequests", `Int combined_default.total_requests);
             ("totalAmount", `Float combined_default.total_amount);
           ]);
           ("fallback", `Assoc [
             ("totalRequests", `Int combined_fallback.total_requests);
             ("totalAmount", `Float combined_fallback.total_amount);
           ]);
         ] in
         send_http_response fd 200 (Some (Yojson.Safe.to_string json))
       )

     | (`POST, "/purge-payments") ->
       Storage.BitPackingStorage.reset app_state.default;
       Storage.BitPackingStorage.reset app_state.fallback;
       send_http_response fd 200 (Some "")

     | _ ->
       send_http_response fd 404 (Some "{\"error\":\"Not Found\"}"))

let handle_client fd =
  let* request_str = read_http_request fd in
  let* () = 
    try handle_http_request fd request_str 
    with _ ->
      send_http_response fd 500 (Some "{\"error\":\"Internal server error\"}")
  in
  let* () = Lwt_unix.close fd in
  Lwt.return_unit

let start_server socket_path =
  (try Sys.remove socket_path with Sys_error _ -> ());

  let num_workers = Config.config.workers in (* Use environment variable *)
  for _ = 1 to num_workers do
    Lwt.async (fun () -> process_payments_worker ());
  done;
  
  (* Create Unix domain socket for nginx communication *)
  let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX socket_path in
  
  let* () = Lwt_unix.bind sock addr in
  Lwt_unix.listen sock 64; (* Increased back to 64 *)
  Unix.chmod socket_path 0o666;
  
  let rec accept_loop () =
    let* (client_fd, _client_addr) = Lwt_unix.accept sock in
    (* Handle each client asynchronously *)
    Lwt.async (fun () -> handle_client client_fd);
    accept_loop ()
  in
  accept_loop ()
