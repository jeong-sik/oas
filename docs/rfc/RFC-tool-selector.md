# RFC: Tool Selector -- 2-Stage Tool Routing for OAS

- **Status**: Draft
- **Author**: Vincent (jeong-sik)
- **Created**: 2026-04-04
- **OAS Version**: v0.100.7
- **Related**: Samchon Rank 3, Tool Selection Benchmark (2026-04-01/02)

---

## 1. Problem

Tool count가 20을 넘으면 LLM의 tool selection 정확도가 급격히 떨어진다.

### 1.1 실측 근거

MASC essential tool 21개에 대한 벤치마크 (2026-04-01/02):

| 조건 | Selection Accuracy | 비고 |
|------|-------------------|------|
| 21 tools, tool_choice=Any (OpenAI wire: `required`) | **42.9%** | 9B, 35B-A3B 동일 |
| 21 tools, tool_choice=Any + system prompt | 42.9% | prompt 개선 무효 |
| 21 tools, tool_choice=Any + few-shot | 42.9% | few-shot 무효 |
| 21 tools, tool_choice=Any + description 개선 | 42.9% | description 개선 무효 |
| **2 tools, 고유 이름** | **100%** | 모델 능력 자체는 충분 |
| 27B dense, 21 tools | 60.7% | 모델 크기 +17pp |

42.9% ceiling은 prompt engineering으로 돌파 불가. 2개로 줄이면 100%.
핵심 변수는 **tool 수**와 **이름 유사성** (heartbeat vs heartbeat_start, broadcast vs agents).

### 1.2 TF-IDF pre-filter 실험 (2026-04-02)

| 모델 | 단독 | TF-IDF top-3 + LLM | 개선 |
|------|------|-------------------|------|
| 9B | 42.9% | 66.7% | +24pp |
| 27B | 60.7% | 79.2% | +19pp |
| 35B-A3B | 42.9% | **83.3%** | **+40pp** |

TF-IDF + synonym top-3 -> LLM select -> self-healing retry = **100%**.
경로: 42.9% -> 75.0% (prefilter) -> 83.3% (synonym) -> 91.7% (test clarity) -> 100% (retry).

### 1.3 선행 연구

**Samchon/Agentica (Wrtn Technologies)**:
Selector-Caller-Describer 3단계 분리. Selector가 후보 function을 먼저 필터링하여
context를 줄이고, Caller가 축소된 후보에 대해 argument를 구성.
"Schema specs are the new prompts" -- schema 수준에서 불가능한 값을 물리적으로 제거.
6.75% -> 100% first-try success (self-healing 포함).

**ToolLLM (Qin et al., 2023)**:
Neural API retriever로 16,464개 real-world API에서 적합한 API를 선택.
ToolLLaMA가 ChatGPT 수준 성능 달성.

**Open-Source Tool Manipulation (2023)**:
In-context demonstration retriever + system prompt로 open-source LLM의
tool 성공률을 최대 90%까지 향상. GPT-4와 8개 ToolBench task 중 4개에서 동등.

공통 패턴: **retrieval 단계를 tool execution 앞에 삽입하여 후보를 줄인다**.

---

## 2. Proposal: 2-Stage Tool Routing

### 2.1 개요

```
Stage 1: Selector (lightweight)
  input:  user query + tool catalog
  output: top-K relevant tool names (3-5개)
  method: BM25 (deterministic) or LLM (non-deterministic)

Stage 2: Caller (existing Agent.run)
  input:  user query + top-K tool schemas (full JSON Schema)
  output: tool_use blocks with composed arguments
  method: LLM (existing pipeline, 변경 없음)
```

Describer (Samchon의 3번째 단계)는 scope 밖.
OAS는 이미 `Response_harness`로 응답 후처리를 수행한다.

### 2.2 왜 2-stage인가

1-stage (현재): 21개 tool schema 전체를 system prompt에 삽입 -> LLM이 선택 + 인자 구성을 동시에 수행 -> 42.9%.
2-stage: Selector가 3-5개로 줄임 -> LLM이 축소된 집합에서 인자 구성 -> 100% (실측).

이점:
- **정확도**: 42.9% -> 83-100% (모델 크기 무관)
- **토큰 절약**: 21개 full schema 대신 3-5개만 전송. tool description이 평균 100 token이면 (21-5)*100 = 1,600 token/turn 절약
- **모델 중립성**: BM25 selector는 모델 능력에 비의존. LLM selector도 이름만 선택하므로 경량

---

## 3. OAS API Design

### 3.1 Interface Sketch (OCaml .mli)

```ocaml
(** Tool selector: 2-stage tool routing for large tool catalogs.

    When an agent has 20+ tools, sending all schemas to the LLM
    degrades selection accuracy. Tool_selector narrows the candidate
    set before the LLM composes arguments.

    @since 0.100.0 *)

module Tool_selector : sig

  (** Selection strategy. *)
  type strategy =
    | All
      (** Current behavior: send all tools to LLM. No filtering.
          Use when tool count <= 15 or accuracy is acceptable. *)
    | TopK_bm25 of {
        k: int;               (** Number of tools to select (default 5) *)
        always_include: string list;
          (** Tool names always included regardless of score.
              Use for essential tools (e.g., "done", "handoff"). *)
      }
      (** BM25-based deterministic selection.
          Uses Tool_index internally. Fast, no LLM call.
          Suitable for keyword-matchable tool descriptions. *)
    | TopK_llm of {
        k: int;               (** Number of tools to select (default 5) *)
        always_include: string list;
        selector_config: Types.agent_config option;
          (** Optional separate config for the selector LLM call.
              If None, uses lightweight defaults (low max_tokens, temp=0). *)
      }
      (** LLM-based selection: a lightweight LLM call that receives
          tool names + one-line descriptions, returns top-K names.
          More accurate than BM25 for semantic similarity.
          Costs ~200 tokens per selection call. *)
    | Categorical of {
        groups: (string * string list) list;
          (** (group_name, tool_name list) pairs.
              e.g., [("git", ["git_commit"; "git_push"; "git_diff"])] *)
        classifier: [ `Bm25 | `Llm ];
          (** How to pick the relevant group(s). *)
        always_include: string list;
      }
      (** Group-based selection: classify query into group(s),
          then expose all tools in matching groups.
          Good when tools have clear domain boundaries. *)

  (** Select tools relevant to the current turn context.

      @param strategy How to select
      @param context The user's current query/message text
      @param tools Full tool catalog
      @return Filtered tool list (subset of input tools) *)
  val select :
    strategy:strategy ->
    context:string ->
    tools:Tool.t list ->
    Tool.t list

  (** Convenience: select names only (for logging/debugging). *)
  val select_names :
    strategy:strategy ->
    context:string ->
    tools:Tool.t list ->
    string list

  (** Default strategy based on tool count.
      <= 15 tools -> All
      > 15 tools  -> TopK_bm25 { k = 5; always_include = [] } *)
  val auto : tools:Tool.t list -> strategy

end
```

### 3.2 Pipeline 통합 지점

현재 OAS turn pipeline (6-stage, codebase terminology):

```
Stage 1: Input   (lifecycle, BeforeTurn, elicitation)
Stage 2: Parse   (BeforeTurnParams, context reduction, tool preparation)
Stage 3: Route   (provider selection, API call dispatch)
Stage 4: Collect (usage accumulation, AfterTurn, message append)
Stage 5: Execute (tool execution on StopToolUse)
Stage 6: Output  (stop reason -> turn_outcome)
```

Tool_selector는 **Stage 2 (Parse)** 안의 tool preparation 단계에 삽입된다:

```
Stage 2: Parse
  2a. resolve turn params
  2b. prepare_tools (guardrails, operator_policy, policy_channel 적용)
  2c. [NEW] Tool_selector.select (strategy, context, visible_tools)
  2d. tools_json = filtered tools -> JSON schema
```

구체적으로는 `Agent_turn.prepare_tools` 내부에서 `Tool_set.filter`로 visible tools를 계산한 뒤,
`tool_schemas`를 만들기 전에 선택된 tool list로 한 번 더 좁히는 방식이다:

```ocaml
(* Before: agent_turn.ml *)
let visible = Tool_set.filter effective_guardrails tools in
let tool_schemas = List.map Tool.schema_to_json (Tool_set.to_list visible) in

(* After: agent_turn.ml *)
let visible = Tool_set.filter effective_guardrails tools in
let selected = match tool_selector with
  | None -> Tool_set.to_list visible
  | Some strategy ->
    let context = extract_context_from_messages messages in
    Tool_selector.select ~strategy ~context
      ~tools:(Tool_set.to_list visible)
in
let tool_schemas = List.map Tool.schema_to_json selected in
```

> **NOTE**: `prepare_tools`의 현재 시그니처에는 `messages`가 없으므로,
> `prepare_turn`에서 context를 추출하여 `prepare_tools`에 전달하는 방식으로 구현한다.
> 즉 `prepare_tools` 시그니처에 `~context:string option` 파라미터를 추가한다.

### 3.3 Builder API

```ocaml
(** Set tool selection strategy for large tool catalogs.
    When tool count > 15, selector narrows candidates per turn
    before sending schemas to the LLM.

    Can be combined with Progressive_tools: progressive disclosure
    determines the available pool, then selector narrows further
    within that pool.

    @since 0.100.0 *)
val with_tool_selector : Tool_selector.strategy -> t -> t
```

### 3.4 Progressive_tools와의 관계

두 모듈은 다른 축에서 동작한다:

| 모듈 | 축 | 작동 방식 | 입력 |
|------|-----|----------|------|
| `Progressive_tools` | 시간 (turn) | turn 번호에 따라 pool 결정 | turn count |
| `Tool_selector` | 의미 (query) | query 유사도에 따라 subset 선택 | user message |

합성 순서: `Progressive_tools (turn-based pool)` -> `Tool_selector (query-based narrowing)`.

Progressive_tools가 turn 3에서 `[read_file, write_file, git_commit, search, ...]` 10개를 허용하면,
Tool_selector가 그 10개 중 query와 가장 관련 있는 3-5개만 LLM에 전달.

충돌 없음: Progressive_tools는 Guardrails.AllowList를 tool_filter_override에 설정하고,
Tool_selector는 그 이후 filtered 결과에서 추가 축소만 수행.

---

## 4. Strategy별 상세 설계

### 4.1 All (현행 유지)

아무 것도 하지 않는다. 현재 OAS 동작과 동일.
tool count <= 15이면 이 전략이 합리적이다.

### 4.2 TopK_bm25

기존 `Tool_index` 모듈을 재활용한다.

```ocaml
let select_bm25 ~k ~always_include ~context ~tools =
  let index = Tool_index.of_tools tools in
  let retrieved = Tool_index.retrieve index context in
  let top_names = List.filteri (fun i _ -> i < k) retrieved
    |> List.map fst in
  (* always_include를 먼저 넣고, top_names에서 중복 제거 후 추가 *)
  let seen = Hashtbl.create 16 in
  let result = ref [] in
  let add name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.replace seen name true;
      result := name :: !result
    end
  in
  List.iter add always_include;
  List.iter add top_names;
  let selected_names = List.rev !result in
  (* Preserve BM25 ranking order, not original catalog order *)
  let tool_by_name = Hashtbl.create (List.length tools) in
  List.iter (fun (t : Tool.t) ->
    Hashtbl.replace tool_by_name t.schema.name t
  ) tools;
  List.filter_map (fun name ->
    Hashtbl.find_opt tool_by_name name
  ) selected_names
```

장점: LLM 호출 없음, 결정론적, < 1ms.
단점: 의미적 유사도 부족 (keyword 기반).
실측: TF-IDF top-3 -> LLM에서 35B-A3B 83.3% 달성. BM25 단독으로도 recall@3 = 100%.

### 4.3 TopK_llm

경량 LLM 호출로 tool 이름을 선택한다.

```
System: You are a tool selector. Given a user query and a list of available
tools, select the {k} most relevant tools. Return ONLY a JSON array of
tool names, nothing else.

User: Query: "{context}"

Available tools:
1. masc_heartbeat - Send heartbeat to keep session alive
2. masc_broadcast - Broadcast message to all agents in room
3. masc_status - Get current room status and agent list
...

Response: ["masc_broadcast", "masc_status"]
```

장점: 의미적 유사도 처리. "모든 에이전트에게 알려줘" -> broadcast 매핑 가능.
단점: 추가 LLM 호출 1회 (~200 token, ~0.5-3s). Non-deterministic.
완화: temperature=0, max_tokens=100으로 결정론에 근접.

### 4.4 Categorical

Tool을 사전 정의 그룹으로 분류한다.

```ocaml
let groups = [
  ("task", ["add_task"; "claim"; "claim_next"; "done"; "transition"]);
  ("communication", ["broadcast"; "agents"; "who"]);
  ("plan", ["plan_get"; "plan_update"; "plan_set_task"]);
  ("session", ["heartbeat"; "heartbeat_start"; "status"; "check"]);
]
```

Query가 "task" 그룹에 매칭되면 해당 그룹 전체를 노출.
이미 `Tool_index`의 group co-retrieval이 이 패턴을 지원한다.

장점: 예측 가능하고 설명 가능.
단점: 그룹 정의가 수동. 그룹 간 경계가 모호한 tool 존재.

---

## 5. Determinism Boundary

```
         Deterministic                    Non-Deterministic
  ┌──────────────────────┐        ┌──────────────────────────┐
  │                      │        │                          │
  │  BM25 Selector       │        │  LLM Selector            │
  │  (Tool_index.retrieve)│       │  (lightweight LLM call)  │
  │                      │        │                          │
  │  Group Classifier    │        │  LLM Caller              │
  │  (Categorical/Bm25)  │        │  (existing Agent.run)    │
  │                      │        │                          │
  │  Guardrails Filter   │        │                          │
  │  (AllowList/DenyList)│        │                          │
  │                      │        │                          │
  └──────────────────────┘        └──────────────────────────┘
           │                                 │
           └─────────┬───────────────────────┘
                     ▼
          Deterministic Validation
  ┌──────────────────────────────────────────┐
  │                                          │
  │  Tool_middleware.validate_and_coerce     │
  │  Tool_input_validation                   │
  │  Response_harness                        │
  │                                          │
  └──────────────────────────────────────────┘
```

원칙 (RFC-0001 Det/NonDet Boundary 확장):
- **Selector 출력은 항상 deterministic validation을 거친다**: 선택된 tool 이름이 실제 tool catalog에 존재하는지 검증. 존재하지 않는 이름은 drop하되, dropped name을 debug log로 남긴다.
- **Validation 결과가 empty이면 fallback**: `always_include` tool로 fallback하고, 그것도 비어 있으면 `All` 전략으로 fallback한다.
- **BM25 selector는 완전 결정론적**: 동일 입력 -> 동일 출력. 테스트에서 정확한 assertion 가능.
- **LLM selector는 비결정론적이지만 bounded**: always_include가 최소 보장. 선택 실패 시 fallback to All.
- **Caller (기존 pipeline)의 비결정론적 행동은 변경하지 않는다**: Tool_middleware가 argument validation을 수행.

---

## 6. MASC Integration

### 6.1 원칙: OAS는 MASC를 모른다

OAS에 `masc_` 접두어나 MASC 특화 로직이 들어가면 안 된다 (feedback: oas-must-not-know-masc).
Tool_selector는 범용 API를 제공하고, MASC가 소비자로서 전략을 구성한다.

### 6.2 MASC 소비 패턴

```ocaml
(* MASC keeper가 agent를 빌드할 때 *)
let agent =
  Builder.create ()
  |> Builder.with_tools masc_tools  (* 21+ MASC tools *)
  |> Builder.with_tool_selector
       (Tool_selector.TopK_bm25 {
         k = 5;
         always_include = ["masc_heartbeat"; "masc_done"];
       })
  |> Builder.build_safe
```

또는 Categorical 전략으로 MASC tool 그룹을 정의:

```ocaml
let masc_groups = [
  ("lifecycle", ["masc_heartbeat"; "masc_heartbeat_start"; "masc_done"; "masc_leave"]);
  ("task", ["masc_add_task"; "masc_claim"; "masc_claim_next"; "masc_transition"]);
  ("communication", ["masc_broadcast"; "masc_agents"; "masc_who"]);
  ("plan", ["masc_plan_get"; "masc_plan_update"; "masc_plan_set_task"]);
  ("status", ["masc_status"; "masc_check"; "masc_tasks"]);
]

let agent =
  Builder.create ()
  |> Builder.with_tool_selector
       (Tool_selector.Categorical {
         groups = masc_groups;
         classifier = `Bm25;
         always_include = ["masc_heartbeat"];
       })
  |> Builder.build_safe
```

MASC 쪽에서 그룹을 정의하고 OAS에 전달하는 방향.
OAS는 그룹이 무엇을 의미하는지 모른다.

### 6.3 Cascade와의 관계

Tool_selector의 LLM 전략은 cascade를 통해 LLM을 호출할 수 있다.
하지만 selector LLM 호출과 main agent LLM 호출은 별개다:

```
Selector LLM call (optional, lightweight)
  -> cascade: "selector" profile (fast, cheap model)
  -> output: tool name list

Main Agent LLM call (existing)
  -> cascade: "primary" or agent's named cascade
  -> input: selected tool schemas only
  -> output: tool_use blocks
```

Selector용 cascade profile을 별도로 정의하면
main agent와 다른 모델/설정으로 selection을 수행할 수 있다.
이는 OAS cascade_config의 기존 기능으로 충분하다.

---

## 7. Benchmark Plan

### 7.1 측정 항목

| Metric | 정의 | 목표 |
|--------|------|------|
| **Selection Accuracy** | 정확한 tool 선택 비율 | >= 80% (단독), >= 95% (retry 포함) |
| **First-Try Success** | 선택 + 인자 구성 모두 정확 | >= 70% |
| **Selector Latency** | Selector 단계 소요 시간 | BM25 < 5ms, LLM < 3s |
| **Token Overhead** | Selector 단계 추가 토큰 | LLM < 300 tokens/call |
| **Recall@K** | Top-K에 정답 tool 포함 비율 | >= 95% |
| **Regression** | tool count <= 15에서 기존 대비 성능 | 0% degradation |

### 7.2 테스트 구성

기존 벤치마크 데이터셋을 재활용 (28 test cases: 15 direct, 8 ambiguous, 5 complex_params).

```
A: Baseline (All strategy, 현재 동작)
B: TopK_bm25 (k=5)
C: TopK_bm25 (k=3) + synonym
D: TopK_llm (k=5)
E: Categorical (MASC groups)
F: TopK_bm25 (k=5) + self-healing retry
```

### 7.3 모델 매트릭스

| 모델 | Active Params | 역할 |
|------|--------------|------|
| Qwen3.5-9B | 9B | Worst-case stress test |
| Qwen3.5-35B-A3B | 3B active | Primary MASC agent |
| Qwen3.5-27B | 27B | Mid-range reference |

"3B 모델도 통과하면 어떤 모델도 통과한다" (Samchon 원칙).
35B-A3B (3B active)로 통과하는 전략만 채택.

### 7.4 Harness 통합

기존 OAS `Harness` 모듈로 자동화:

```ocaml
let test_case = Harness_case.create
  ~name:"broadcast_selection"
  ~input:"모든 에이전트에게 빌드 실패 알려줘"
  ~expected_tool:"masc_broadcast"
  ~strategy:(TopK_bm25 { k = 5; always_include = [] })
  ()
```

---

## 8. Implementation Plan

### Phase 1: Core Module (추정 2-3일)

1. `lib/tool_selector.ml` + `lib/tool_selector.mli` 생성
2. `All`, `TopK_bm25` 전략 구현 (Tool_index 재활용)
3. 단위 테스트 (alcotest)
4. `Agent_turn.prepare_tools`에 선택적 통합

### Phase 2: Builder Integration (추정 1일)

1. `Builder.with_tool_selector` 추가
2. Pipeline stage 2에 selector 삽입
3. Progressive_tools와의 합성 테스트

### Phase 3: LLM Selector (추정 2-3일)

1. `TopK_llm` 전략 구현
2. Selector용 lightweight API call 경로
3. Fallback 로직 (LLM 실패 -> BM25 -> All)

### Phase 4: Benchmark (추정 2일)

1. 기존 28 test case로 A/B/C/D/E/F 비교
2. 3-model matrix 실행
3. 결과 기록 및 전략 선택 가이드 작성

### Phase 5: MASC Integration (추정 1일, MASC 쪽)

1. MASC keeper에서 tool_selector 전략 설정
2. Categorical groups 정의
3. 실제 keeper 세션에서 검증

---

## 9. Scope Boundary

### In Scope

- `Tool_selector` 모듈 (.ml + .mli)
- `All`, `TopK_bm25`, `TopK_llm`, `Categorical` 전략
- `Agent_turn.prepare_tools` 통합
- `Builder.with_tool_selector` API
- 벤치마크 테스트

### Out of Scope

- **Describer** (Samchon 3단계): OAS `Response_harness`가 이미 처리
- **Synonym dictionary**: Tool_index의 기존 tokenizer + description으로 충분. 필요 시 후속 RFC
- **Dynamic group learning**: 그룹은 소비자가 정적 정의. 자동 그룹 발견은 별도 연구
- **Cross-turn tool memory**: 이전 turn에서 사용한 tool을 기억하여 우선순위 부여. 별도 RFC
- **Provider-specific tool format**: OAS는 이미 provider 추상화. Selector는 Tool.t만 다룸

---

## 10. Risk and Mitigation

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| BM25 recall 부족 (의미적 유사 query) | Medium | Selector가 정답 tool 미포함 | always_include + fallback to All |
| LLM selector가 잘못된 이름 반환 | Medium | Invalid tool name | Deterministic validation: catalog에 없는 이름 drop |
| Selector 추가 latency (LLM) | Low | Turn 시작 ~2s 지연 | BM25 기본, LLM은 opt-in |
| Progressive_tools와 충돌 | Low | 예상 외 tool set | 합성 순서 고정: Progressive -> Selector |
| always_include tool이 너무 많으면 효과 감소 | Medium | K에 비해 고정 tool 비율 높음 | always_include < k/2 권장 가이드 |

---

## 11. Open Questions

1. **k의 적정 값**: 벤치마크에서 top-3으로 100% 달성. Top-5가 더 안전한가, top-3이 충분한가? -> Phase 4에서 결정
2. **Hybrid strategy**: BM25 top-10 -> LLM top-3? 2단 selector의 비용/이득 비율 -> Phase 3에서 실험
3. **Turn-adaptive k**: 첫 turn은 넓게 (k=7), 이후 좁게 (k=3)? Progressive_tools와 역할 중복 가능성
4. **Selector 결과 캐싱**: 같은 query에 대해 BM25 결과를 turn 간 캐시? -> Tool_index.build는 이미 인덱스를 유지하므로 retrieve만 캐시하면 됨

---

## 12. References

1. Samchon, "Function Calling Harness: From 6.75% to 100%", Qwen Meetup 2026. dev.to/samchon
2. MASC Tool Selection Benchmark, 2026-04-01/02. Internal benchmark notes referenced by this RFC.
3. Samchon Function Calling Harness Research. Internal research notes referenced by this RFC.
4. ToolLLM, Qin et al., 2023. arxiv.org/abs/2307.16789
5. Open-Source LLM Tool Manipulation, 2023. arxiv.org/abs/2305.16504
6. OAS Tool_index (BM25), v0.89.0. `lib/tool_index.ml`
7. OAS Progressive_tools, v0.43.0/v0.89.0. `lib/progressive_tools.ml`
8. RFC-0001 Det/NonDet Boundary. Internal RFC note referenced by this document.
