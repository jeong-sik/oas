# OAS — OCaml Agent SDK

OCaml 5.x + Eio 기반 에이전트 SDK. v0.60.0.

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

주요 모듈: `Agent`, `Types`, `Error`, `Provider`, `Context`, `Collaboration`, `Orchestrator`, `Hooks`, `Tool`

### Layer 2: Swarm Engine (`lib_swarm/`)

별도 라이브러리 `agent_sdk_swarm`. `agent_sdk`에 의존.

| 파일 | 역할 |
|------|------|
| `swarm_types.ml` | agent_role, orchestration_mode, convergence_config, agent_entry, swarm_state, callbacks, collaboration option |
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

## Cascade Config (`lib/llm_provider/cascade_config.ml`)

Named cascade profiles with JSON hot-reload and discovery-aware health filtering.
Consumers (MASC, standalone agents) define cascade profiles; OAS handles routing.

```ocaml
(* Execute a named cascade: load config, filter health, failover *)
Cascade_config.complete_named ~sw ~net ~clock
  ~name:"heartbeat_action"
  ~defaults:["llama:qwen3.5-35b"; "glm:auto"]
  ~messages ~temperature:0.3 ~max_tokens:500 ()

(* Or use individual pieces *)
let configs = Cascade_config.parse_model_strings ["llama:qwen3.5"; "glm:glm-4.5"] in
let healthy = Cascade_config.filter_healthy ~sw ~net configs in
(* ... use with Complete.complete_cascade *)
```

Supported providers: `llama`, `claude`, `gemini`, `glm`, `openrouter`, `custom:model@url`.

## Key Types

- `Types.agent_config` — 에이전트 설정 (model, system_prompt, max_turns 등)
- `Types.api_response` — LLM 응답 (content blocks, usage, stop_reason)
- `Error.sdk_error` — 타입 안전한 에러 (Agent, Config, Orchestration 등)
- `Collaboration.t` — 공유 협업 컨텍스트 (goal, phase, participants, artifacts, votes)
- `Swarm_types.swarm_config` — 스웜 설정 (entries, mode, convergence, collaboration)
- `Swarm_types.swarm_result` — 스웜 결과 (iterations, final_metric, converged)

## Collaboration (3-Type Session Model)

세션을 3개 독립 타입으로 분리. `Session.t`(개인) / `Collaboration.t`(공유) / `Orchestrator`(전략).

```
Session.t ──참조──> Collaboration.t <──사용── Orchestrator
(개인 런타임)       (공유 맥락)              (조율 전략)
```

### 사용 패턴

```ocaml
(* 새 코드: Collaboration.t 직접 사용 *)
let collab = Collaboration.create ~goal:"deploy v2" () in
let collab = Collaboration.add_participant collab
  { name = "alice"; role = Some "lead"; state = Working;
    joined_at = Some (Unix.gettimeofday ()); finished_at = None; summary = None } in
let collab = Collaboration.set_phase collab Active in

(* Runtime.session에서 변환 (기존 코드 호환) *)
let collab = Runtime_projection.collaboration_of_session session in
let session = Runtime_projection.update_session_from_collaboration session collab in

(* Swarm에 collaboration 전달 *)
let config = { entries; mode = Decentralized; ...; collaboration = Some collab } in
```

### 타입 관계

| 타입 | 필드 수 | 용도 |
|------|---------|------|
| `Session.t` | 7 | 에이전트 개인 런타임 (턴, 타임스탬프, 메타데이터) |
| `Collaboration.t` | 12 | 공유 협업 (goal, phase, participants, artifacts, votes) |
| `Runtime.session` | 18 | 와이어 프로토콜 (Session + Collaboration 필드 혼재, 마이그레이션 중) |

### Runtime.session과의 관계

`Runtime.session`은 협업 필드(goal, phase, participants 등)를 직접 가지고 있다.
`Runtime_projection.collaboration_of_session`으로 lossy projection 가능.
새 코드는 `Collaboration.t`를 사용하고, 기존 코드는 `Runtime.session`을 유지한다.
`Runtime.session`의 협업 필드에는 `(→ Collaboration.t)` 마이그레이션 주석이 있다.

### 외부 소비자 (MASC)

MASC는 `agent_sdk >= 0.57.0`에 의존하며, `team_context.collaboration_of_session`으로
team_session (47필드) → Collaboration.t (12필드) lossy projection을 수행한다.

## Dependencies

`mcp_protocol`, `mcp_protocol_eio` — 별도 fork (`jeong-sik/mcp-protocol-sdk`).
`bisect_ppx` — OCaml 5.4 compat fork (`patricoferris/bisect_ppx`).
