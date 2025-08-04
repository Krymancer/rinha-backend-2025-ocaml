# Rinha de Backend 2025 - OCaml

## Visão Geral

Este projeto é uma implementação em OCaml de uma API de processamento de pagamentos de alta performance, desenvolvida para a competição Rinha de Backend 2025. O foco é maximizar throughput e eficiência de recursos, utilizando OCaml funcional e técnicas de otimização.

## Arquitetura

- **Cluster de APIs**: Dois containers (`api1` e `api2`) rodam a mesma aplicação, comunicando-se via Unix Socket com baixo overhead de rede.
- **Nginx**: Atua como load balancer, roteando requisições para os containers via sockets.
- **Lwt Concurrency**: Processamento assíncrono de pagamentos usando Lwt para concorrência cooperativa.
- **Bit Packing**: Armazenamento dos pagamentos em memória usando packing de bits para reduzir uso de memória e acelerar consultas.
- **Comunicação entre instâncias**: Cada instância consulta o "estado estrangeiro" da outra para sumarização global, via HTTP interno.

## Técnicas de Performance

- **Unix Sockets**: Comunicação local entre Nginx e APIs via socket reduz latência e overhead de TCP.
- **Bit Packing**: Pagamentos são armazenados como inteiros compactados, otimizando uso de memória e cache.
- **Lwt Async**: Uso do Lwt para processamento assíncrono sem threads pesadas.
- **OCaml Performance**: Compilação nativa e garbage collector otimizado do OCaml.
- **Functional Programming**: Imutabilidade e functional programming reduzem bugs e melhoram performance.

## Dependências OCaml

- `lwt`: Biblioteca de concorrência assíncrona
- `cohttp-lwt-unix`: Cliente/servidor HTTP assíncrono
- `yojson`: Parsing e serialização JSON
- `ppx_yojson_conv`: Derivação automática de conversores JSON
- `domainslib`: Pool de workers e paralelismo
- `logs`: Sistema de logging

## Endpoints

- `POST /payments`: Enfileira um novo pagamento.
- `GET /payments-summary`: Retorna sumário dos pagamentos (local e global).
- `POST /purge-payments`: Limpa o estado em memória.

## Estrutura do Projeto

- `lib/`
  - `config.ml`: Configurações de ambiente
  - `types.ml`: Tipos de dados e JSON
  - `money.ml`: Utilitários de conversão monetária
  - `storage.ml`: Armazenamento em memória com bit packing
  - `queue.ml`: Fila assíncrona thread-safe
  - `http_utils.ml`: Utilitários HTTP
  - `payment_processor.ml`: Cliente para processadores de pagamento
  - `payment_summary.ml`: Serviço de sumário de pagamentos
  - `server.ml`: Servidor HTTP principal
- `bin/`
  - `main.ml`: Ponto de entrada da aplicação

## Como rodar

### Desenvolvimento local
```bash
# Instalar dependências
opam install dune lwt lwt_ppx cohttp-lwt-unix yojson ppx_yojson_conv ppx_deriving domainslib logs fmt

# Compilar
dune build

# Executar
dune exec rinha-backend-2025-ocaml
```

### Produção com Docker
```bash
docker-compose up --build
```

## Vantagens da Implementação OCaml

1. **Performance**: OCaml compila para código nativo extremamente eficiente
2. **Memory Safety**: Sistema de tipos forte previne erros de memória
3. **Concorrência**: Lwt oferece concorrência de alta performance sem threads
4. **Functional Programming**: Imutabilidade reduz bugs de concorrência
5. **Garbage Collection**: GC otimizado para aplicações de alta performance
6. **Pattern Matching**: Tratamento de casos e erros mais expressivo

## Comparação com Node.js

| Aspecto | Node.js | OCaml |
|---------|---------|--------|
| Runtime | V8 JIT | Código nativo |
| Concorrência | Event loop + Workers | Lwt cooperativo |
| Memory Safety | JavaScript dinâmico | Sistema de tipos forte |
| Performance | Boa para I/O | Excelente para CPU + I/O |
| Ecosystem | NPM massivo | Opam menor mas focado |

Esta implementação OCaml demonstra como linguagens funcionais podem atingir alta performance em aplicações de backend, combinando safety com velocidade.
