(lang dune 3.19)

(name rinha-backend-2025-ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rinha-backend-2025-ocaml)
 (synopsis "Rinha de Backend 2025 - OCaml Implementation")
 (description
  "High-performance payment processing API for Rinha de Backend 2025")
 (depends
  ocaml
  dune
  lwt
  lwt_ppx
  cohttp-lwt-unix
  yojson
  ppx_yojson_conv
  ppx_deriving
  domainslib
  logs
  fmt)
 (tags
  ("payment-processing" "high-performance" "rinha-backend")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
