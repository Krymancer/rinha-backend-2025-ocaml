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

# Copy only dependency files first for better caching
COPY --chown=opam:opam dune-project .
COPY --chown=opam:opam rinha-backend-2025-ocaml.opam .

# Install dependencies (this layer will be cached unless deps change)
RUN opam install . --deps-only --with-test --with-doc

# Now copy source code (this invalidates cache only when source changes)
COPY --chown=opam:opam . .

# Build the project
RUN eval $(opam env) && \
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
