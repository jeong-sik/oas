# agent-sdk

이 디렉토리가 현재 작업 환경에서의 로컬 SSOT다.

- 사람 기준 이름: `agent-sdk`
- local workspace path in this environment: `~/me/workspace/yousleepwhen/agent-sdk`
- OCaml 패키지 이름: `agent_sdk`
- OCaml 모듈 이름: `Agent_sdk`
- `agent-swarm` 코드는 별도 제품이 아니라 `~/me/workspace/yousleepwhen/masc-mcp` 내부 구현으로 이관한다

OCaml 5.x + Eio 기반 LLM Agent SDK. Anthropic Messages API와 OpenAI-compatible local LLM 엔드포인트를 지원한다.

## 아키텍처

```
Provider  →  API  →  Agent  →  Tool
(endpoint)  (HTTP)  (loop)   (functions)
```

| 모듈 | 역할 |
|------|------|
| `Types` | 메시지, 역할, stop_reason 등 도메인 타입 |
| `Provider` | LLM 엔드포인트 추상화 (Local / Anthropic) |
| `Api` | HTTP 클라이언트 — `create_message` 호출 |
| `Agent` | 멀티턴 에이전트 루프 (tool_use 자동 처리) |
| `Tool` | 도구 정의 + JSON Schema 생성 + 실행 |

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
  model_id = "claude-sonnet-4-20250514";
  api_key_env = "ANTHROPIC_API_KEY";
}
```

`Provider.resolve`는 `(base_url * api_key * headers, error_msg) result`를 반환한다. 환경변수가 없으면 `Error`를 반환하며, silent fallback 없음.

## 사용법

```ocaml
open Agent_sdk

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let provider : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:8085" };
    model_id = "qwen3.5-35b";
    api_key_env = "LOCAL_LLM_KEY";
  } in
  let agent = Agent.create
    ~provider
    ~system_prompt:"You are a helpful assistant."
    ~tools:[]
    ~max_turns:5 in
  match Agent.run ~sw ~net agent "What is 2+2?" with
  | Ok response -> Printf.printf "Response: %s\n" response
  | Error e -> Printf.eprintf "Error: %s\n" e
```

## 도구 정의

```ocaml
let calc_tool = Tool.create
  ~name:"calculator"
  ~description:"Evaluate a math expression"
  ~parameters:[
    ("expression", `String, "The math expression to evaluate", true);
  ]
  ~handler:(fun args ->
    match List.assoc_opt "expression" args with
    | Some expr -> Printf.sprintf "Result: %s" expr
    | None -> "Error: missing expression"
  )
```

## 빌드

```bash
# from the repo root
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

```ocaml
match Types.stop_reason_of_string "some_new_reason" with
| Types.Unknown s -> Printf.printf "Unknown stop reason: %s\n" s
| Types.EndTurn -> (* ... *)
| _ -> (* ... *)
```

## 의존성

- `eio`, `eio_main` — 구조적 동시성
- `cohttp-eio` — HTTP 클라이언트
- `yojson` — JSON 파싱
- `ppx_deriving` — show, eq 자동 생성
- `alcotest` — 테스트 (dev)

## 제약 및 트레이드오프

- HTTP 응답 본문 상한: 10MB. 이보다 큰 응답은 에러 처리.
- streaming 미지원. 전체 응답을 버퍼링한 뒤 파싱.
- tool_use 루프에서 max_turns 초과 시 마지막 응답을 반환하고 종료.

## 버전

0.2.0
