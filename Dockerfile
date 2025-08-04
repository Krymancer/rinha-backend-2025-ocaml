# Multi-stage build for OCaml application
FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

# Switch to root to install system dependencies
USER root
RUN apt-get update && apt-get install -y \
  pkg-config \
  libev-dev \
  libssl-dev \
  libgmp-dev \
  m4 \
  && rm -rf /var/lib/apt/lists/*

# Switch back to opam user
USER opam
WORKDIR /home/opam

# Copy project files
COPY --chown=opam:opam . .

# Install dependencies and build
RUN opam update && \
  opam install -y dune lwt lwt_ppx cohttp-lwt-unix yojson ppx_yojson_conv ppx_deriving domainslib logs fmt uri && \
  eval $(opam env) && \
  dune build

# Production stage
FROM ubuntu:22.04 AS production

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
  libev4 \
  libgmp10 \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy built executable
COPY --from=builder /home/opam/_build/default/bin/main.exe ./rinha-backend

# Set ownership and permissions
RUN chown -R www-data:www-data /tmp && \
  chmod +x ./rinha-backend

USER www-data

ENV OCAMLRUNPARAM=b
ENV SOCKET_PATH=/tmp/app.sock

CMD ["./rinha-backend"]
