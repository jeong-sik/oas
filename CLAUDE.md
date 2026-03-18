# OAS — OCaml Agent SDK

OCaml 5.x + Eio 기반 에이전트 SDK. v0.54.0.

## Architecture

```
lib_swarm/  →  agent_sdk_swarm (Layer 2: Swarm Engine)
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

### Layer 2: Swarm Engine (`lib_swarm/`)

별도 라이브러리 `agent_sdk_swarm`. `agent_sdk`에 의존.

| 파일 | 역할 |
|------|------|
| `swarm_types.ml` | agent_role, orchestration_mode, convergence_config, agent_entry, swarm_state, callbacks |
| `runner.ml` | 3가지 모드(Decentralized/Supervisor/Pipeline), 수렴 루프, Eio.Mutex 상태 보호 |

`agent_entry.run`은 closure 기반 — `Agent.t` abstract type 장벽을 우회. `make_entry`로 Agent.t를 wrapping.

## Build

```bash
dune build --root .          # 빌드
dune exec --root . test/test_swarm.exe   # swarm 테스트 (28개, 0.1s)
make test                    # 전체 테스트
```

## Conventions

- OCaml 5.x, Eio 구조적 동시성
- `.mli` API 계약 우선
- `ppx_deriving_yojson`, `ppx_deriving.show` 사용
- `bisect_ppx` 커버리지 (75%+)
- 테스트: alcotest + qcheck (property-based)
- 파일 300줄 이상 → 분할 검토 (`agent.ml` → `agent/` 서브디렉토리)

## Testing

Swarm 테스트에 실제 LLM 불필요. `agent_entry.run` closure에 mock function 주입.

```ocaml
(* Mock agent — LLM 호출 없이 즉시 응답 *)
let mock_run text ~sw:_ _prompt =
  Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
       content = [Text text]; usage = None }

(* 12-worker 하네스도 mock만으로 검증 *)
let config = { entries = List.init 12 (fun i ->
  { name = Printf.sprintf "w%d" i; run = mock_run "ok"; role = Execute });
  mode = Decentralized; ... }
```

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

## Key Types

- `Types.agent_config` — 에이전트 설정 (model, system_prompt, max_turns 등)
- `Types.api_response` — LLM 응답 (content blocks, usage, stop_reason)
- `Error.sdk_error` — 타입 안전한 에러 (Agent, Config, Orchestration 등)
- `Swarm_types.swarm_config` — 스웜 설정 (entries, mode, convergence)
- `Swarm_types.swarm_result` — 스웜 결과 (iterations, final_metric, converged)

## Dependencies

`mcp_protocol`, `mcp_protocol_eio` — 별도 fork (`jeong-sik/mcp-protocol-sdk`).
`bisect_ppx` — OCaml 5.4 compat fork (`patricoferris/bisect_ppx`).
