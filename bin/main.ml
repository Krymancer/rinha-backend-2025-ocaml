open Rinha_backend_2025_ocaml

let () =
  Printf.printf "Socket path: %s\n%!" Config.config.socket_path;
  Printf.printf "Foreign state: %s\n%!" Config.config.foreign_state;
  Printf.printf "Fire mode: %b\n%!" Config.config.is_fire_mode;
  
  Lwt_main.run (Server.start_server Config.config.socket_path)
