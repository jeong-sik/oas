# OAS — OCaml Agent SDK

OCaml 5.x + Eio 기반 에이전트 SDK. v0.149.0.

## Architecture

```
lib/        →  agent_sdk       (Layer 1: Agent Runtime)
bin/        →  oas_cli, oas_runtime
test/       →  alcotest 기반 단위/통합 테스트
examples/   →  사용 예제
```

### Layer 1: Agent Runtime (`lib/`)

단일 에이전트 실행 엔진. `(include_subdirs unqualified)` — 서브디렉토리 자동 포함.

| 디렉토리 | 역할 |
|----------|------|
| `lib/agent/` | Agent 라이프사이클, 턴 실행, 도구 호출, handoff |
| `lib/pipeline/` | 6-stage 턴 파이프라인 |
| `lib/protocol/` | A2A, Agent Card, Agent Registry, MCP |
| `lib/*.ml` | API(multi-provider), Context, Hooks, Guardrails, Orchestrator 등 |

주요 모듈: `Agent`, `Types`, `Error`, `Provider`, `Context`, `Orchestrator`, `Hooks`, `Tool`

## Build

```bash
dune build --root .          # 빌드
make test                    # 전체 테스트
```

## Conventions

- OCaml 5.x, Eio 구조적 동시성
- `.mli` API 계약 우선
- `ppx_deriving_yojson`, `ppx_deriving.show` 사용
- `bisect_ppx` 커버리지 (75%+)
- 테스트: alcotest + qcheck (property-based)
- 파일 300줄 이상 → 분할 검토 (`agent.ml` → `agent/` 서브디렉토리)

## Provider Support

| Provider | Module | Endpoint |
|----------|--------|----------|
| Anthropic | `api_anthropic.ml` | Messages API |
| OpenAI-compatible | `api_openai.ml` | Chat Completions |

## LLM Discovery (`lib/llm_provider/discovery.ml`)

Probes local llama-server instances via OpenAI-compatible API:
- `GET /health` — reachability
- `GET /v1/models` — loaded models
- `GET /props` — total_slots, ctx_size
- `GET /slots` — per-slot busy/idle status

```ocaml
let statuses = Discovery.discover ~sw ~net
  ~endpoints:(Discovery.endpoints_from_env ())
```

Configure via `LLM_ENDPOINTS` env var (comma-separated, default `http://127.0.0.1:8085`).

## Provider Routing

OAS provides multi-provider routing via `Complete.complete_cascade`.
Cascade configuration (model lists, health filtering, failover) is
the responsibility of downstream consumers.

Supported providers: `llama`, `claude`, `gemini`, `glm`, `openrouter`, `custom:model@url`.

## Key Types

- `Types.agent_config` — 에이전트 설정 (model, system_prompt, max_turns 등)
- `Types.api_response` — LLM 응답 (content blocks, usage, stop_reason)
- `Error.sdk_error` — 타입 안전한 에러 (Agent, Config, Orchestration 등)

## Dependencies

`mcp_protocol`, `mcp_protocol_eio` — 별도 fork (`jeong-sik/mcp-protocol-sdk`).
`bisect_ppx` — OCaml 5.4 compat fork (`patricoferris/bisect_ppx`).
