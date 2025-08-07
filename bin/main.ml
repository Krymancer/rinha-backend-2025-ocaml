open Rinha_backend_2025_ocaml

let () =
  Printf.printf "Socket path: %s\n%!" Config.config.socket_path;
  Printf.printf "Foreign state: %s\n%!" Config.config.foreign_state;
  Printf.printf "Workers: %d\n%!" Config.config.workers;
  
  Lwt_main.run (Server.start_server Config.config.socket_path)
