open Lwt.Syntax

type 'a queue = {
  mutable items : 'a list;
  mutable length : int;
  mutex : Lwt_mutex.t;
  condition : unit Lwt_condition.t;
}

let create () = {
  items = [];
  length = 0;
  mutex = Lwt_mutex.create ();
  condition = Lwt_condition.create ();
}

let enqueue queue item =
  let* () = Lwt_mutex.lock queue.mutex in
  queue.items <- item :: queue.items;
  queue.length <- queue.length + 1;
  Lwt_condition.signal queue.condition ();
  Lwt_mutex.unlock queue.mutex;
  Lwt.return_unit

let dequeue queue =
  let* () = Lwt_mutex.lock queue.mutex in
  let rec wait_for_item () =
    match List.rev queue.items with
    | [] ->
      let* () = Lwt_condition.wait ~mutex:queue.mutex queue.condition in
      wait_for_item ()
    | item :: rest ->
      queue.items <- List.rev rest;
      queue.length <- queue.length - 1;
      Lwt.return item
  in
  let* item = wait_for_item () in
  Lwt_mutex.unlock queue.mutex;
  Lwt.return item
