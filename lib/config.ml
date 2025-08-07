type config = {
  socket_path : string;
  foreign_state : string;
  workers : int;
}

let load_config () =
  let socket_path = 
    try Sys.getenv "SOCKET_PATH" 
    with Not_found -> "/tmp/app.sock" 
  in
  let foreign_state = 
    try Sys.getenv "FOREIGN_STATE" 
    with Not_found -> "" 
  in
  let workers = 
    try int_of_string (Sys.getenv "WORKERS")
    with _ -> 1
  in
  { socket_path; foreign_state; workers }

let config = load_config ()
