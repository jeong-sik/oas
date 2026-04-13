# RFC-OAS-004: Code-Snippet Tool Strategy (Experimental)

**Status**: Draft
**Date**: 2026-04-13
**Scope**: `lib/tool_selector.ml`, `lib/tool_middleware.ml`, `lib/agent/agent.ml`
**One sentence**: Smolagents가 주장하는 "code snippet으로 tool을 조합하면 JSON tool call 대비 LLM call 30% 감소" 가설을 OAS에서 재현 가능한 실험 프레임워크로 구현하고, trajectory eval로 재현 여부를 판정한다.

## Related Documents

- Smolagents (HuggingFace): code-based tool composition
- `lib/tool_selector.mli` — 현재 4 strategy (`All`, `TopK_bm25`, `TopK_llm`, `Categorical`)
- `lib/tool_middleware.ml` — tool execution pipeline, `heal_tool_call`
- `lib/agent/agent.ml` — turn loop, tool dispatch
- `lib/harness.ml` — trajectory eval, swiss_verdict
- `lib/eval_baseline.ml` — regression detection
- RFC-OAS-002 — OTel metric naming (eval metrics 표준화)

## Problem Statement

### 현재 상태

OAS의 tool 실행 모델은 JSON-based tool call이다:
1. LLM이 `tool_use` content block(JSON)을 생성
2. OAS가 해당 tool을 dispatch하고 결과를 `tool_result`로 반환
3. LLM이 결과를 보고 다음 행동 결정

이 모델에서 **N개의 tool을 순차적으로 사용하려면 최소 N번의 LLM turn이 필요하다**(concurrent tool call 제외). 각 turn마다 전체 context를 LLM에 전송하므로 token 소비가 선형 증가한다.

### Smolagents 가설

Smolagents는 LLM이 JSON 대신 **code snippet을 생성**하여 여러 tool call을 하나의 코드 블록으로 조합할 수 있다고 주장한다:

```python
# JSON tool call: 3 turns 필요
# Turn 1: search_files("auth")
# Turn 2: read_file(results[0])
# Turn 3: edit_file(results[0], patch)

# Code snippet: 1 turn
results = search_files("auth")
content = read_file(results[0])
edit_file(results[0], apply_patch(content, fix))
```

주장되는 이점: LLM call 30% 감소, context window 소비 감소.

### 검증이 필요한 이유

1. **재현 가능한 벤치마크가 없다**: Smolagents 공개 수치는 특정 task set에서의 결과. OAS agent의 실제 tool usage 패턴에서도 동일한 효과가 나는지 불명.
2. **안전성 우려**: LLM이 생성한 코드를 실행하는 것은 sandbox 없이 위험. blast radius가 크다.
3. **결정론 경계 붕괴**: code snippet은 본질적으로 비결정론적. determinism wrapper 내에서 실행해도 코드 자체의 side effect는 제어 불가.

## Design

### Principle: Experiment-First, Adopt-on-Evidence

이 RFC는 **실험 프레임워크**를 구현한다. 채택은 trajectory eval에서 30% LLM call 감소가 재현될 때만. 재현 실패 시 RFC를 종결(Close)한다.

### Part A: `CodeSnippet` Strategy Variant

`tool_selector.ml`에 5번째 strategy를 추가한다:

```ocaml
type strategy =
  | All
  | TopK_bm25 of { ... }
  | TopK_llm of { ... }
  | Categorical of { ... }
  | CodeSnippet of {
      allowed_tools: string list;
      (** Code snippet에서 호출 가능한 tool 이름 whitelist.
          빈 리스트면 모든 tool 허용 (위험). *)
      sandbox: sandbox_mode;
      (** 실행 격리 수준. *)
      max_snippet_lines: int;
      (** 코드 블록 최대 줄 수. 기본 30. *)
      experimental: bool;
      (** true면 OAS_EXPERIMENTAL_CODE_SNIPPET env var 필요.
          false면 항상 활성화 (채택 후에만). *)
    }

type sandbox_mode =
  | Deterministic_only
    (** Pure function만 허용. I/O tool 호출 시 즉시 abort.
        가장 안전하지만 활용 범위가 좁다. *)
  | Subprocess_sandbox of { timeout_s: float; memory_limit_mb: int }
    (** 별도 프로세스에서 실행. timeout + memory 제한.
        OAS가 직접 spawn하므로 MASC playground 불필요. *)
```

### Part B: Code Snippet Execution Engine

`code_snippet_engine.ml` 신규 모듈:

```ocaml
(** LLM이 생성한 code snippet을 파싱하고 실행하는 엔진. *)

type snippet = {
  code: string;
  tool_calls: string list;  (** 코드에서 참조하는 tool 이름 *)
}

type snippet_result =
  | Success of { output: string; tool_calls_made: int; tokens_saved_estimate: int }
  | Parse_error of string
  | Sandbox_violation of string
  | Timeout

val parse : raw:string -> (snippet, string) result
(** LLM 응답에서 code block을 추출하고 AST-level 검증.
    허용되지 않는 구문(import, exec, eval 등)은 reject. *)

val execute :
  sandbox:sandbox_mode ->
  tools:(string -> Yojson.Safe.t -> string) ->
  snippet ->
  snippet_result
(** Snippet을 실행. tools 함수는 tool_name → input → output 매핑.
    sandbox_mode에 따라 격리 수준 결정. *)
```

### Part C: DSL 설계 (JSON이 아닌 제한된 언어)

LLM에게 OCaml이나 Python을 생성하도록 요청하는 것은 위험하다. 대신 **제한된 DSL**을 정의한다:

```
// Tool Composition DSL (v1)
// 변수 바인딩, tool call, 조건분기만 허용

let results = search_files("auth", limit=5)
let first = results[0]
let content = read_file(first.path)
if contains(content, "deprecated") then
  report("found deprecated auth")
else
  noop()
```

DSL 제약:
- **허용**: `let` 바인딩, tool call, `if/then/else`, string literal, integer, array index
- **금지**: loop(`for`, `while`), function 정의, import, system call, 변수 재할당
- **타입**: string, int, bool, array, tool_result (opaque)
- **파서**: 직접 구현 (OCaml parser combinator). 외부 인터프리터 의존 없음.

이 제약으로 Turing-complete가 아니므로 무한 루프/자원 고갈 위험이 구조적으로 불가.

### Part D: Turn Loop 통합

`agent.ml` turn loop에 CodeSnippet 분기를 추가:

```
Turn Start
  → Tool Selection (strategy = CodeSnippet)
  → LLM에 DSL prompt 주입 ("다음 도구들을 조합하여 code snippet으로 응답하세요")
  → LLM 응답 파싱
    → JSON tool_use block이면: 기존 경로 (단일 tool dispatch)
    → Code snippet block이면: DSL 파싱 → Sandbox 실행 → 결과를 tool_result로 반환
  → 다음 turn (또는 종료)
```

### Part E: Trajectory Eval 프레임워크

```ocaml
(** code_snippet_eval.ml *)

type comparison = {
  task_name: string;
  json_mode: { turns: int; llm_calls: int; tokens: int; passed: bool };
  snippet_mode: { turns: int; llm_calls: int; tokens: int; passed: bool };
  call_reduction_pct: float;  (** (json - snippet) / json * 100 *)
}

val run_comparison :
  task:(Agent.t -> unit) ->
  tools:Tool.t list ->
  comparison
(** 동일 task를 JSON mode와 CodeSnippet mode로 실행하여 비교.
    두 모드 모두 harness verdict를 통과해야 유효한 비교. *)
```

**채택 기준**: 10+ task에서 평균 call_reduction_pct >= 25% AND snippet_mode.passed >= json_mode.passed.

## Implementation Phases

### Phase 1: DSL Parser + Sandbox (1 PR)
- `code_snippet_dsl.ml` — parser combinator 기반 DSL 파서
- `code_snippet_engine.ml` — `Deterministic_only` sandbox만 구현
- Unit test: 정상 snippet 파싱, 금지 구문 reject, sandbox violation

### Phase 2: Strategy Variant + Turn Loop 통합 (1 PR)
- `tool_selector.ml`에 `CodeSnippet` variant 추가
- `agent.ml` turn loop에 snippet 분기 추가
- `OAS_EXPERIMENTAL_CODE_SNIPPET=true` env var guard
- Integration test: mock tool로 snippet 실행 검증

### Phase 3: Trajectory Eval (1 PR)
- `code_snippet_eval.ml` — comparison 프레임워크
- 최소 10개 task set 정의 (기존 harness 기반)
- eval 실행 + 결과 보고서 생성

### Phase 4: 판정 (코드 아님)
- Phase 3 결과 분석
- 25%+ call reduction 재현 시 → experimental flag 제거, `auto` 함수에 통합 검토
- 재현 실패 시 → RFC 종결, Phase 1-3 코드는 `experimental/` 이동 또는 삭제

## Risks

| Risk | Mitigation |
|------|------------|
| DSL이 너무 제한적이어서 LLM이 활용하지 못함 | Phase 3 eval에서 드러남. DSL 확장은 별도 RFC |
| LLM이 DSL 대신 자연어/JSON으로 응답 | Fallback: JSON tool call 경로로 자동 전환 (기존 동작) |
| Sandbox escape | DSL이 Turing-incomplete이므로 구조적 불가. Subprocess sandbox는 timeout+memory 하드 리밋 |
| 실험 코드가 본 코드에 복잡도 추가 | env var guard + 별도 모듈 격리. 재현 실패 시 삭제 |
| Smolagents의 30% 수치가 특정 모델/task에 특화 | OAS 자체 task set으로 독립 평가. Smolagents 수치를 목표가 아닌 참조만으로 사용 |

## Scope Exclusion

- Python/OCaml 코드 직접 실행 (DSL만 허용)
- Loop 구문 (Turing-completeness 의도적 배제)
- MASC keeper에서의 CodeSnippet 사용 (MASC는 OAS consumer이므로 별도 결정)
- Subprocess sandbox의 Docker isolation (RFC-OAS-003C 범위)
- Production 채택 (Phase 4 판정 이전까지 experimental only)
