FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

USER root
RUN apt-get update && apt-get install -y \
  netbase \
  pkg-config \
  libev-dev \
  libssl-dev \
  libgmp-dev \
  m4

USER opam
WORKDIR /home/opam

# Copy project files
COPY --chown=opam:opam . .

# Install dependencies and build
RUN opam install . --deps-only --with-test --with-doc && \
  eval $(opam env) && \
  dune build

FROM ubuntu:22.04 AS production

RUN apt-get update && apt-get install -y \
  libev4 \
  libgmp10 \
  netbase

WORKDIR /app

COPY --from=builder /home/opam/_build/default/bin/main.exe ./rinha-backend

RUN chown -R www-data:www-data /tmp && \
  chmod +x ./rinha-backend

USER www-data

ENV OCAMLRUNPARAM=b
ENV SOCKET_PATH=/tmp/app.sock

CMD ["./rinha-backend"]
