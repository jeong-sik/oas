# agent-sdk

OCaml 5.x + Eio 기반 agent runtime SDK. Anthropic Messages API와 OpenAI-compatible local LLM 엔드포인트를 지원하며, `query` / `Client` public API 뒤에 bundled subprocess runtime을 두는 구조를 제공한다.

- OCaml 패키지 이름: `agent_sdk`
- OCaml 모듈 이름: `Agent_sdk`
- 기본 local-first 기준선: `llama.cpp` + `qwen3.5`

## 아키텍처

```
Public SDK (`query` / `Client`)
           |
      Transport
           |
  Bundled `oas-runtime`
           |
 Runtime journal + projection
           |
 Provider  ->  API  ->  Agent  ->  Tool
```

| 모듈 | 역할 |
|------|------|
| `Types` | 메시지, 역할, stop_reason, content_block (Text/Thinking/Image/Document), SSE event 등 도메인 타입 |
| `Provider` | LLM 엔드포인트 추상화 (Local / Anthropic / OpenAICompat) |
| `Api` | HTTP 클라이언트 -- `create_message` (동기) + `create_message_stream` (SSE) |
| `Runtime` | 세션/이벤트/명령/리포트/증명 타입을 정의하는 런타임 프로토콜 |
| `Transport` | `oas-runtime` subprocess transport와 runtime binary 탐색 |
| `Client` | persistent runtime client (`connect`, `start_session`, `apply_command`, `status`, `finalize`) |
| `Agent` | 멀티턴 에이전트 루프 (tool_use 자동 처리) |
| `Tool` | 도구 정의 + JSON Schema 생성 + 실행 (Simple / WithContext) |
| `Retry` | 구조적 API 에러 분류 (7 variant) + exponential backoff with jitter |
| `Hooks` | 에이전트 lifecycle hooks (BeforeTurn, AfterTurn, PreToolUse, PostToolUse, OnStop) |
| `Context` | Cross-turn 공유 상태 (key-value store, Yojson.Safe.t 값) |
| `Guardrails` | Tool 필터링 (AllowList/DenyList/Custom) + per-turn 호출 제한 |
| `Handoff` | Sub-agent 위임 (`transfer_to_*` tool 패턴) |
| `Session` | 세션 lifecycle 및 메타데이터 (턴 카운트, 타임스탬프, 재개) |
| `Checkpoint` | 에이전트 상태 스냅샷 (메시지, 사용량, 설정 보존) |
| `Checkpoint_store` | 파일 기반 체크포인트 영속화 |
| `Builder` | Fluent API 방식 에이전트 구성 |
| `Orchestrator` | 멀티 에이전트 태스크 오케스트레이션 |
| `Otel_tracer` | OpenTelemetry 호환 트레이서 (OTLP JSON 내보내기) |
| `Tracing` | 관측성 인터페이스 (`TRACER` module type, `Null_tracer`, `Fmt_tracer`) |
| `Skill` | 재사용 가능 에이전트 능력 번들 (markdown 파싱) |
| `Subagent` | 격리된 컨텍스트의 자식 에이전트 스폰 |
| `Structured` | 타입 안전 구조화 출력 추출 (tool_use + tool_choice 패턴) |
| `Context_reducer` | 메시지 윈도잉 (keep_last, token_budget, custom 전략) |
| `Streaming` | SSE 이벤트 파싱 및 사용량 추적 |
| `Mcp` | MCP 클라이언트 (mcp-protocol-sdk 래핑, 서버 lifecycle 관리) |
| `Mcp_session` | MCP 세션 캡처/복원 (checkpoint/resume 연동) |
| `Event_bus` | 에이전트 lifecycle 이벤트 발행/구독 (Eio.Stream 기반) |
| `Error` | 2-level 구조적 에러 타입 (`sdk_error` = 7 도메인 inner type + `Internal`) |

## Provider 패턴

```ocaml
(* Local LLM (llama-server, vllm-mlx 등) *)
let local_cfg : Provider.config = {
  provider = Local { base_url = "http://127.0.0.1:8085" };
  model_id = "qwen3.5-35b";
  api_key_env = "LOCAL_LLM_KEY";
}

(* Anthropic *)
let anthropic_cfg : Provider.config = {
  provider = Anthropic;
  model_id = "claude-sonnet-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

(* OpenAI-compatible (OpenRouter 등) *)
let openrouter_cfg = Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" ()
```

`Provider.resolve`는 `(base_url * api_key * headers, error_msg) result`를 반환한다. 환경변수가 없으면 `Error`를 반환하며, silent fallback 없음.

## Canonical Local Path

- high-level `Client.default_options`와 `query`는 `provider = Some "local-qwen"` / `model = Some "qwen3.5"`를 기본값으로 둔다.
- 즉 기본 happy path는 `llama.cpp` 계열 OpenAI-compatible endpoint가 `127.0.0.1:8085`에서 `qwen3.5`를 서빙하는 환경이다.
- 문서 예제는 결정론 검증을 위해 `mock` provider도 사용하지만, 실제 운용 기준선은 local `llama.cpp`다.

## 사용법

### One-shot Query

```ocaml
open Agent_sdk

let () =
  match
    query
      ~options:
        {
          Client.default_options with
          session_root = Some "./.oas-runtime-demo";
          provider = Some "mock";
          agents =
            [
              ( "planner",
                {
                  Client.description = "planner";
                  prompt = "Break the problem into steps.";
                  tools = None;
                  model = None;
                } );
            ];
        }
      ~prompt:"Coordinate a small runtime task"
      ()
  with
  | Ok messages ->
      Printf.printf "Collected %d runtime messages\n" (List.length messages)
  | Error err ->
      Printf.eprintf "Query failed: %s\n" (Error.to_string err)
```

### Interactive Client

```ocaml
open Agent_sdk

let () =
  match
    Client.connect
      ~options:
        {
          Client.default_options with
          session_root = Some "./.oas-runtime-demo";
          provider = Some "mock";
          agents =
            [
              ( "reviewer",
                {
                  Client.description = "reviewer";
                  prompt = "Review the user request carefully.";
                  tools = None;
                  model = None;
                } );
            ];
        }
      ()
  with
  | Error err -> prerr_endline (Error.to_string err)
  | Ok client ->
      Fun.protect
        ~finally:(fun () -> Client.close client)
        (fun () ->
          match Client.query client "Review this runtime setup" with
          | Error err -> prerr_endline (Error.to_string err)
          | Ok () ->
              let server_info = Client.get_server_info client in
              let first_batch = Client.receive_response ~timeout:0.5 client in
              Printf.printf "Received %d messages after first turn\n"
                (List.length first_batch);
              ignore (Client.query client "Continue with one more refinement.");
              ignore (Client.wait_until_idle client);
              let second_batch = Client.receive_messages client in
              Printf.printf "Received %d messages after second turn\n"
                (List.length second_batch);
              ignore (Client.finalize client ());
              let final_batch = Client.receive_messages client in
              Printf.printf "Received %d final messages\n"
                (List.length final_batch);
              ignore server_info)
```

`include_partial_messages = true`를 주면 `Client.receive_messages()`에
`Partial_message { participant_name; delta }`가 함께 들어온다.

### Session Helpers

```ocaml
open Agent_sdk

let () =
  match Sessions.list_sessions ~session_root:"./.oas-runtime-demo" () with
  | Ok infos -> Printf.printf "Known sessions: %d\n" (List.length infos)
  | Error err -> prerr_endline (Error.to_string err)
```

### Resume a Session

```ocaml
open Agent_sdk

let reconnect session_id =
  Client.connect
    ~options:
      {
        Client.default_options with
        session_root = Some "./.oas-runtime-demo";
        resume_session = Some session_id;
      }
    ()
```

### Explicit Runtime Contract

```ocaml
open Agent_sdk

let review_skill =
  Skill.of_markdown
    "---\nname: reviewer\n---\nList concrete findings before summaries."

let contract =
  Contract.empty
  |> Contract.with_runtime_awareness
       "You are running inside an explicit runtime contract."
  |> Contract.with_trigger ~source:"room" ~reason:"direct mention"
       "direct_mention"
  |> Contract.add_instruction_layer ~label:"role"
       "Prefer short, grounded answers."

let agent =
  Builder.create ~net ~model:Types.Claude_sonnet_4_6
  |> Builder.with_system_prompt "Review the request carefully."
  |> Builder.with_contract contract
  |> Builder.with_skill review_skill
  |> Builder.with_tool_grants ["read_file"; "search"]
  |> Builder.build
```

### Advanced Runtime Access

```ocaml
open Agent_sdk

let request =
  Runtime.Start_session
    {
      session_id = Some "demo-session";
      goal = "Coordinate a small runtime task";
      participants = [ "planner"; "builder" ];
      provider = Some "mock";
      model = None;
      system_prompt = Some "Coordinate the team carefully.";
      max_turns = Some 3;
      workdir = None;
    }

let () =
  match runtime_query ~session_root:"./.oas-runtime-demo" request with
  | Ok (Runtime.Session_started_response session) ->
      Printf.printf "Started %s\n" session.session_id
  | Ok other ->
      Printf.eprintf "Unexpected response: %s\n" (Runtime.show_response other)
  | Error err ->
      Printf.eprintf "Runtime error: %s\n" (Error.to_string err)
```

### 기본 에이전트

```ocaml
open Agent_sdk

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let agent = Agent.create ~net
    ~config:{ Types.default_config with
      name = "assistant";
      system_prompt = Some "You are a helpful assistant.";
    }
    ~tools:[] () in
  match Agent.run ~sw agent "What is 2+2?" with
  | Ok response ->
    List.iter (function
      | Types.Text t -> print_endline t | _ -> ()) response.content
  | Error e -> prerr_endline ("Error: " ^ Error.to_string e)
```

### 스트리밍

```ocaml
let on_event = function
  | Types.ContentBlockDelta { delta = Types.TextDelta s; _ } ->
    print_string s; flush stdout
  | _ -> ()
in
match Api.create_message_stream ~sw ~net ~config ~messages ~on_event () with
| Ok response -> Printf.printf "\nDone: %s\n" response.id
| Error e -> prerr_endline (Error.to_string e)
```

### Hooks

```ocaml
let my_hooks = { Hooks.empty with
  pre_tool_use = Some (function
    | Hooks.PreToolUse { tool_name; _ } ->
      Printf.printf "Calling tool: %s\n" tool_name;
      Hooks.Continue
    | _ -> Hooks.Continue);
}
```

### Guardrails

```ocaml
let guardrails = {
  Guardrails.tool_filter = Guardrails.AllowList ["calculator"; "weather"];
  max_tool_calls_per_turn = Some 5;
}
```

### Multimodal (Image/Document)

```ocaml
let image_block = Types.Image {
  media_type = "image/png";
  data = base64_encoded_string;
  source_type = "base64";
}
let doc_block = Types.Document {
  media_type = "application/pdf";
  data = base64_encoded_pdf;
  source_type = "base64";
}
let msg = { Types.role = User; content = [Types.Text "Describe this:"; image_block] }
```

### Handoff (Sub-agent delegation)

```ocaml
let target = {
  Handoff.name = "researcher";
  description = "Research agent for web queries";
  config = { Types.default_config with name = "researcher" };
  tools = [search_tool];
}
let handoff_tool = Handoff.make_handoff_tool target
(* Agent runner intercepts "transfer_to_researcher" tool calls *)
```

## 도구 정의

```ocaml
(* Simple handler *)
let calc_tool = Tool.create
  ~name:"calculator"
  ~description:"Evaluate a math expression"
  ~parameters:[{
    Types.name = "expression"; description = "The math expression";
    param_type = String; required = true;
  }]
  (fun args ->
    let open Yojson.Safe.Util in
    match args |> member "expression" |> to_string_option with
    | Some expr -> Ok (Printf.sprintf "Result: %s" expr)
    | None -> Error "missing expression")

(* Context-aware handler *)
let stateful_tool = Tool.create_with_context
  ~name:"counter"
  ~description:"Increment and return counter"
  ~parameters:[]
  (fun ctx _input ->
    let n = match Context.get ctx "count" with
      | Some (`Int n) -> n + 1
      | _ -> 1
    in
    Context.set ctx "count" (`Int n);
    Ok (string_of_int n))
```

## 빌드

```bash
dune build @all

# runtime binary
_build/default/bin/oas_runtime.exe
```

## 테스트

```bash
# 단위 테스트
dune runtest

# runtime query/client vertical slice
dune exec ./test/test_runtime.exe

# 통합 테스트 (로컬 LLM 서버 필요)
LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
LLAMA_LIVE_TEST=1 dune exec ./test/test_streaming_e2e.exe
dune exec ./test/test_integration.exe
```

## stop_reason 처리

API 응답의 `stop_reason` 필드는 `Unknown of string` variant를 포함한다. 알 수 없는 값이 들어와도 silent 무시하지 않고 원본 문자열을 보존한다.

## 의존성

- `eio`, `eio_main` -- 구조적 동시성
- `cohttp-eio` -- HTTP 클라이언트
- `tls-eio`, `ca-certs` -- HTTPS/TLS
- `yojson` -- JSON 파싱
- `ppx_deriving`, `ppx_deriving_yojson` -- show, yojson 자동 생성
- `alcotest` -- 테스트 (dev)

## 제약 및 트레이드오프

- HTTP 응답 본문 상한: 10MB. 이보다 큰 응답은 에러 처리.
- SSE 스트리밍은 실시간 이벤트 콜백 방식. 재시도(retry)는 스트림 외부에서 래핑 필요.
- tool_use 루프에서 max_turns 초과 시 마지막 응답을 반환하고 종료.
- Eio 단일 도메인에서 동작. 멀티코어 병렬 실행은 미구현.
- Prompt caching은 streaming/non-streaming 모두에서 cache usage를 추적한다 (v0.4.0부터). SSE `message_start` 이벤트에서 `cache_creation_input_tokens`와 `cache_read_input_tokens`를 추출.

## 버전

0.10.0
