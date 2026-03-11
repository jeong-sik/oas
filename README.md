# agent-sdk

OCaml 5.x + Eio 기반 LLM Agent SDK. Anthropic Messages API와 OpenAI-compatible local LLM 엔드포인트를 지원한다.

- OCaml 패키지 이름: `agent_sdk`
- OCaml 모듈 이름: `Agent_sdk`

## 아키텍처

```
Provider  ->  API  ->  Agent  ->  Tool
(endpoint)   (HTTP)   (loop)    (functions)
               |
            Streaming (SSE)
```

| 모듈 | 역할 |
|------|------|
| `Types` | 메시지, 역할, stop_reason, content_block (Text/Thinking/Image/Document), SSE event 등 도메인 타입 |
| `Provider` | LLM 엔드포인트 추상화 (Local / Anthropic / OpenAICompat) |
| `Api` | HTTP 클라이언트 -- `create_message` (동기) + `create_message_stream` (SSE) |
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
| `Mcp_bridge` | Eio 네이티브 MCP 클라이언트 브릿지 |

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

## 사용법

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
  | Error e -> prerr_endline ("Error: " ^ e)
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
| Error e -> prerr_endline e
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
```

## 테스트

```bash
# 단위 테스트
dune runtest

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
- Prompt caching은 non-streaming에서만 cache usage를 추적한다. SSE streaming은 cache token을 0으로 보고.

## 버전

0.8.0
