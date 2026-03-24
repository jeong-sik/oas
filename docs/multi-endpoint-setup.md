# Multi-Endpoint LLM Setup

OAS supports multiple local llama-server instances with automatic discovery,
round-robin load balancing, and health-aware failover.

## Quick Start

```bash
# Option 1: Explicit endpoints
export LLM_ENDPOINTS="http://127.0.0.1:8085,http://127.0.0.1:8086,http://127.0.0.1:8087"

# Option 2: Auto-discovery (default when LLM_ENDPOINTS is unset)
# OAS probes ports 8085-8090 on 127.0.0.1 at startup
```

## Running Multiple llama-server Instances

```bash
# 3x 9B models on different ports
llama-server -m qwen3.5-9b.gguf --port 8085 --n-gpu-layers 99 &
llama-server -m qwen3.5-9b.gguf --port 8086 --n-gpu-layers 99 &
llama-server -m qwen3.5-9b.gguf --port 8087 --n-gpu-layers 99 &

# Or mixed models
llama-server -m qwen3.5-35b.gguf --port 8085 &
llama-server -m qwen3.5-9b.gguf  --port 8086 &
llama-server -m qwen3.5-9b.gguf  --port 8087 &
```

## Auto-Discovery

When `LLM_ENDPOINTS` is not set, `Provider_registry.refresh_llama_endpoints`
scans ports 8085-8090 via `GET /health`. Only healthy endpoints are registered.

MASC calls this at server startup. Standalone OAS users call it explicitly:

```ocaml
Eio.Switch.run @@ fun sw ->
let net = Eio.Stdenv.net env in
let endpoints = Provider_registry.refresh_llama_endpoints ~sw ~net () in
(* endpoints: ["http://127.0.0.1:8085"; "http://127.0.0.1:8086"] *)
```

## Load Balancing

`Provider_registry.next_llama_endpoint ()` returns endpoints in round-robin
order using lock-free `Atomic.fetch_and_add`. Safe for concurrent Eio fibers.

## API Reference

| Function | Description |
|----------|-------------|
| `refresh_llama_endpoints ~sw ~net ()` | Scan and update endpoint list |
| `next_llama_endpoint ()` | Get next endpoint (round-robin) |
| `active_llama_endpoints ()` | Snapshot of current endpoint list |

## Health Probes

Each endpoint is probed with 4 requests:
- `GET /health` — reachability
- `GET /v1/models` — loaded model names
- `GET /props` — total_slots, ctx_size
- `GET /slots` — per-slot busy/idle status

Only endpoints responding to `/health` are included in the active list.

## Failover

If all local endpoints are unhealthy, the cascade config falls back to
cloud providers (GLM, Gemini, etc.) as defined in `config/cascade.json`.
