FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

USER root
RUN apt-get update && apt-get install -y \
  pkg-config \
  libev-dev \
  libssl-dev \
  libgmp-dev \
  m4

USER opam
WORKDIR /home/opam

COPY --chown=opam:opam . .

RUN opam update && \
  opam install -y dune lwt lwt_ppx cohttp-lwt-unix yojson ppx_yojson_conv ppx_deriving domainslib logs fmt uri && \
  eval $(opam env) && \
  dune build

FROM ubuntu:22.04 AS production

RUN apt-get update && apt-get install -y \
  libev4 \
  libgmp10

WORKDIR /app

COPY --from=builder /home/opam/_build/default/bin/main.exe ./rinha-backend

RUN chown -R www-data:www-data /tmp && \
  chmod +x ./rinha-backend

USER www-data

ENV OCAMLRUNPARAM=b
ENV SOCKET_PATH=/tmp/app.sock

CMD ["./rinha-backend"]
