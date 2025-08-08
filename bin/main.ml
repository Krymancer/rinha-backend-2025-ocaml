open Rinha_backend_2025_ocaml

let () =
  Printf.printf "Rinha de Backend 2025 - Ocaml";
  Lwt_main.run (Server.start_server Config.config.socket_path)
