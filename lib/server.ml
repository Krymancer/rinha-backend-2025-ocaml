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
    let* result = Payment_processor.process_payment queue_message in
    (match result with
     | Some payment_response ->
       let amount = Money.float_to_cents payment_response.amount in
       (* Use the timestamp from payment processor response for consistency *)
       let timestamp = match Ptime.of_rfc3339 payment_response.requested_at with
         | Ok (ptime, _, _) -> Int64.of_float (Ptime.to_float_s ptime *. 1000.0)
         | Error _ ->
           (* If timestamp parsing fails, use current time to avoid losing the payment *)
           Int64.of_float (Unix.time () *. 1000.0)
       in
       (match payment_response.payment_processor with
        | Types.Default ->
          Storage.BitPackingStorage.push app_state.default amount timestamp
        | Types.Fallback ->
          Storage.BitPackingStorage.push app_state.fallback amount timestamp)
     | None -> ());
    loop ()
  in
  loop ()

(* Manual fast JSON parse left intact, but could be improved later *)
let fast_parse_payment body_str =
  let len = String.length body_str in
  if len < 30 then None else (* Minimum size check *)

  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in

  (* Find "amount" field *)
  let rec find_amount i =
    if i >= len - 8 then None else
    if String.sub body_str i 8 = "\"amount\"" then
      let rec find_colon j =
        if j >= len then None else
        if body_str.[j] = ':' then
          let rec skip_whitespace k =
            if k >= len then None else
            if is_whitespace body_str.[k] then skip_whitespace (k + 1)
            else Some k
          in
          skip_whitespace (j + 1)
        else find_colon (j + 1)
      in
      find_colon (i + 8)
    else find_amount (i + 1)
  in

  (* Find "correlation_id" field *)
  let rec find_correlation_id i =
    if i >= len - 16 then None else
    if String.sub body_str i 16 = "\"correlation_id\"" then
      let rec find_colon j =
        if j >= len then None else
        if body_str.[j] = ':' then
          let rec skip_whitespace k =
            if k >= len then None else
            if is_whitespace body_str.[k] then skip_whitespace (k + 1)
            else if body_str.[k] = '"' then Some (k + 1)
            else None
          in
          skip_whitespace (j + 1)
        else find_colon (j + 1)
      in
      find_colon (i + 16)
    else find_correlation_id (i + 1)
  in

  let amount_opt =
    match find_amount 0 with
    | None -> None
    | Some pos ->
      let rec find_end j acc =
        if j >= len then Float.of_string acc
        else match body_str.[j] with
        | '0'..'9' | '.' | '-' | '+' | 'e' | 'E' -> find_end (j + 1) (acc ^ String.make 1 body_str.[j])
        | _ -> Float.of_string acc
      in
      try Some (find_end pos "") with _ -> None
  in

  let correlation_id_opt =
    match find_correlation_id 0 with
    | None -> None
    | Some pos ->
      let rec find_end j acc =
        if j >= len then Some acc
        else if body_str.[j] = '"' then Some acc
        else find_end (j + 1) (acc ^ String.make 1 body_str.[j])
      in
      find_end pos ""
  in

  match amount_opt, correlation_id_opt with
  | Some amount, Some correlation_id when amount > 0.0 && correlation_id <> "" ->
    Some Types.{ amount; correlation_id }
  | _ -> None

(* Robust HTTP read: read headers until CRLFCRLF, then read body by Content-Length or chunked *)
let rec find_double_crlf s from_idx =
  try Some (String.index_from s from_idx '\r') with Not_found -> None
  |> function
  | None -> None
  | Some i ->
    if i + 3 < String.length s && String.sub s i 4 = "\r\n\r\n" then Some i else
    if i + 1 < String.length s then find_double_crlf s (i + 1) else None

let contains_sub (s:string) (sub:string) =
  let ls = String.length s and lsub = String.length sub in
  let rec loop i =
    if i + lsub > ls then false
    else if String.sub s i lsub = sub then true
    else loop (i + 1)
  in
  loop 0

let read_http_request fd =
  let buf_size = 4096 in
  let buf = Bytes.create buf_size in
  let rec read_headers acc =
    let* n = Lwt_unix.read fd buf 0 buf_size in
    if n = 0 then Lwt.return (String.concat "" (List.rev acc), "")
    else
      let chunk = Bytes.sub_string buf 0 n in
      let combined = String.concat "" (List.rev (chunk :: acc)) in
      match find_double_crlf combined 0 with
      | Some pos ->
        let headers = String.sub combined 0 (pos + 4) in
        let rest = String.sub combined (pos + 4) (String.length combined - (pos + 4)) in
        Lwt.return (headers, rest)
      | None -> read_headers (chunk :: acc)
  in
  let* (headers, rest) = read_headers [] in
  (* Parse headers robustly into (name,value) pairs *)
  let raw_lines = String.split_on_char '\n' headers |> List.map (fun s -> String.trim s) in
  let header_kv =
    raw_lines
    |> List.tl (* skip request line *)
    |> List.filter (fun l -> l <> "")
    |> List.filter_map (fun l ->
         match String.index_opt l ':' with
         | None -> None
         | Some idx ->
           let name = String.lowercase_ascii (String.trim (String.sub l 0 idx)) in
           let value =
             let v = String.sub l (idx + 1) (String.length l - (idx + 1)) in
             String.trim v
           in
           Some (name, value))
  in
  let content_length =
    match List.assoc_opt "content-length" header_kv with
    | Some v -> (try int_of_string v with _ -> -1)
    | None -> -1
  in
  let is_chunked =
    match List.assoc_opt "transfer-encoding" header_kv with
    | Some v -> contains_sub (String.lowercase_ascii v) "chunked"
    | None -> false
  in
  (* Read exactly N bytes from fd, in a loop *)
  let rec read_exact remaining acc =
    if remaining <= 0 then Lwt.return (String.concat "" (List.rev acc))
    else
      let to_read = min remaining buf_size in
      let extra = Bytes.create to_read in
      let* r = Lwt_unix.read fd extra 0 to_read in
      if r = 0 then Lwt.return (String.concat "" (List.rev acc))
      else read_exact (remaining - r) (Bytes.sub_string extra 0 r :: acc)
  in
  if content_length >= 0 && not is_chunked then
    (* Content-Length path *)
    if content_length <= String.length rest then
      Lwt.return (headers, String.sub rest 0 content_length)
    else
      let remaining = content_length - String.length rest in
      let* tail = read_exact remaining [] in
      Lwt.return (headers, rest ^ tail)
  else if is_chunked then (
    (* Chunked request body parsing *)
    let data = ref rest in
    let pos = ref 0 in
    let ensure n =
      if String.length !data - !pos >= n then Lwt.return_unit
      else
        let need = n - (String.length !data - !pos) in
        let tmp = Bytes.create (max need buf_size) in
        let* r = Lwt_unix.read fd tmp 0 (Bytes.length tmp) in
        if r = 0 then Lwt.return_unit
        else (
          data := !data ^ Bytes.sub_string tmp 0 r;
          Lwt.return_unit)
    in
    let rec read_line () =
      let rec find_crlf i =
        if i + 1 >= String.length !data then None
        else if !data.[i] = '\r' && !data.[i+1] = '\n' then Some i
        else find_crlf (i + 1)
      in
      match find_crlf !pos with
      | Some idx ->
        let line = String.sub !data !pos (idx - !pos) in
        pos := idx + 2; (* skip CRLF *)
        Lwt.return line
      | None ->
        let* () = ensure 2 in
        read_line ()
    in
    let hex_to_int s =
      let s =
        match String.index_opt s ';' with
        | Some i -> String.sub s 0 i
        | None -> s
      in
      try Scanf.sscanf s "%x" (fun v -> v) with _ -> 0
    in
    let body_buf = Buffer.create 1024 in
    let rec loop () =
      let* size_line = read_line () in
      let size = hex_to_int (String.trim size_line) in
      if size = 0 then (
        Lwt.return_unit
      ) else (
        let* () = ensure size in
        Buffer.add_substring body_buf !data !pos size;
        pos := !pos + size;
        (* consume CRLF after chunk data *)
        let* () = ensure 2 in
        pos := !pos + 2;
        loop ()
      )
    in
    let* () = loop () in
    Lwt.return (headers, Buffer.contents body_buf)
  ) else
    (* No length and not chunked: use whatever was already read after headers *)
    Lwt.return (headers, rest)

let send_all fd s =
  let rec loop off len =
    if len <= 0 then Lwt.return_unit
    else
      let* wrote = Lwt_unix.write_string fd s off len in
      loop (off + wrote) (len - wrote)
  in
  loop 0 (String.length s)

let send_http_response fd status_code ?(content_type="application/json") body =
  let status_text = match status_code with
    | 200 -> "OK"
    | 404 -> "Not Found"
    | 500 -> "Internal Server Error"
    | 400 -> "Bad Request"
    | _ -> "Unknown"
  in
  let body_str = match body with Some s -> s | None -> "" in
  let response = Printf.sprintf
    "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
    status_code status_text content_type (String.length body_str) body_str
  in
  send_all fd response

let parse_query_params path =
  if String.contains path '?' then
    let parts = String.split_on_char '?' path in
    match parts with
    | [_; query_str] ->
      String.split_on_char '&' query_str
      |> List.map (fun param ->
          match String.split_on_char '=' param with
          | [key; value] -> (key, value)
          | [key] -> (key, "")
          | _ -> ("", ""))
      |> List.filter (fun (k, _) -> k <> "")
    | _ -> []
  else []

let get_param params key =
  try Some (List.assoc key params)
  with Not_found -> None

let payments_summary_json (d:Payment_summary.payment_summary) (f:Payment_summary.payment_summary) =
  Printf.sprintf
    "{\"default\":{\"totalRequests\":%d,\"totalAmount\":%.2f},\"fallback\":{\"totalRequests\":%d,\"totalAmount\":%.2f}}"
    d.total_requests d.total_amount f.total_requests f.total_amount

let header_debug headers =
  let lines = String.split_on_char '\n' headers |> List.map String.trim in
  let cl =
    List.find_map (fun l ->
      let ll = String.lowercase_ascii l in
      if String.length ll >= 15 && String.sub ll 0 15 = "content-length:" then
        Some (String.trim (String.sub ll 15 (String.length ll - 15)))
      else None
    ) lines
  in
  let te =
    List.find_map (fun l ->
      let ll = String.lowercase_ascii l in
      if String.length ll >= 18 && String.sub ll 0 18 = "transfer-encoding:" then
        Some (String.trim (String.sub ll 18 (String.length ll - 18)))
      else None
    ) lines
  in
  (cl, te)

let handle_http_request fd request_line _headers body_opt =
  let path =
    match String.split_on_char ' ' request_line with
    | _meth :: path :: _ -> path
    | _ -> "/"
  in
  match String.split_on_char '?' path |> List.hd with
  | "/payments" ->
    (match body_opt with
     | Some body_str ->
       (match fast_parse_payment body_str with
        | Some queue_message ->
          let* () = Queue.enqueue task_queue queue_message in
          send_http_response fd 200 (Some "")
        | None ->
          send_http_response fd 500 (Some "{\"error\":\"Invalid request data\"}"))
     | None ->
       let (cl, te) = header_debug _headers in
       let hdr_sample =
         let hlen = String.length _headers in
         let take = if hlen > 200 then 200 else hlen in
         String.sub _headers 0 take |> String.escaped
       in
       let msg =
         Printf.sprintf
           "{\"error\":\"No body provided!!!!\",\"contentLengthHeader\":%s,\"transferEncoding\":%s,\"requestLine\":\"%s\",\"headersSample\":\"%s\"}"
           (match cl with Some v -> Printf.sprintf "\"%s\"" v | None -> "null")
           (match te with Some v -> Printf.sprintf "\"%s\"" v | None -> "null")
           (String.escaped request_line)
           hdr_sample
       in
       send_http_response fd 500 (Some msg))
  | "/payments-summary" ->
    let params = parse_query_params path in
    let from_time = get_param params "from" in
    let to_time = get_param params "to" in
    let local_only = match get_param params "localOnly" with Some "true" -> true | _ -> false in

    let* summary = Payment_summary.payment_summary_service app_state Config.config.foreign_state from_time to_time local_only in
    let body = payments_summary_json summary.default summary.fallback in
    send_http_response fd 200 (Some body)
  | "/purge-payments" ->
    Storage.BitPackingStorage.reset app_state.default;
    Storage.BitPackingStorage.reset app_state.fallback;
    send_http_response fd 200 (Some "")
  | _ -> send_http_response fd 404 (Some "{\"error\":\"Not Found\"}")

let handle_client fd =
  let* (headers, body) = read_http_request fd in
  let lines = String.split_on_char '\n' headers in
  let request_line = match lines with | h :: _ -> String.trim h | [] -> "" in
  let* () =
    try
      let body_opt = if String.length body > 0 then Some body else None in
      handle_http_request fd request_line headers body_opt
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
  Lwt_unix.listen sock 64; (* backlog *)
  Unix.chmod socket_path 0o666;

  let rec accept_loop () =
    let* (client_fd, _client_addr) = Lwt_unix.accept sock in
    Lwt.async (fun () -> handle_client client_fd);
    accept_loop ()
  in
  accept_loop ()
