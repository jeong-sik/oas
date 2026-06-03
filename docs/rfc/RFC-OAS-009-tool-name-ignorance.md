# RFC-OAS-009 (v2): Sever Core→CDAL Dependencies

| | |
|---|---|
| Status | Draft (v2 — supersedes merged v1 in-place) |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-08 (v1) / 2026-05-08 (v2 redefinition) |
| Target | `agent_sdk` (oas) v0.193+ |
| Supersedes | RFC-OAS-009 v1 (머지 후 의도 재정의) |
| Related | RFC-OAS-011 (CDAL Migration to masc-mcp), RFC-OAS-012 (Tool Name Ignorance within CDAL) |

## 0. Revision History

| Version | Merged | Title | Status |
|---|---|---|---|
| v1 | PR #1478 (`7149c5a7`, 2026-05-08 12:41Z) | "Tool Name Ignorance (OAS는 consumer tool 이름을 모름)" — `default_tool_entries` 등 *내부 정리* 의도 | **Superseded** by v2. v1 본문은 git history에 보존 |
| v2 (this) | TBD | "Sever Core→CDAL Dependencies" — *경계 재정의*의 전제 작업 | Draft |

### 0.1 Why v2 redefinition

v1은 *OAS 내부의 `default_tool_entries` 표면 정리*에 머물렀다. PR #1478 Draft 검토 중 사용자가 다음 두 질문을 제기:

1. *"경계 제대로 나뉘는거 맞냐?"*
2. *"CDAL 이거는 어차피 masc 에서만 있음 되는거 아니냐"*

이 두 질문이 v1 의도의 한계를 노출했다. grep 검증 결과 (§1.1):

- OAS lib에 **두 패키지가 섞여있음**: `agent_sdk core` (README 약속) + `CDAL` ("PoC-1" 자기 표기, README/CLAUDE.md 미언급).
- CDAL 외부 consumer: **masc-mcp 단일** (검증).
- OAS core → CDAL 역방향 의존: **단 2 호출** (둘 다 동일 함수 `Mode_enforcer.builtin_descriptor`).

→ 사용자 의도: CDAL은 masc-mcp로 이주해야 옳다. v1의 `default_tool_entries` 정리는 *이주 후* 진행하는 것이 깨끗 (RFC-OAS-012). 본 v2 RFC는 *이주의 전제*: OAS core → CDAL 의존을 0건으로 만드는 작업.

## 1. Problem Statement

### 1.0 Boundary 그림 (verified)

```
OAS lib/ (157 top-level modules, 단일 dune library `agent_sdk`)
├─ agent_sdk core (README/CLAUDE.md 약속한 표면)
│   ├─ agent_sdk(.ml/.mli — façade), agent_tool, async_agent, subagent, agent_typed
│   ├─ api, api_anthropic, api_openai, api_zai, client, internal_client, provider_*
│   ├─ tool, tool_set, tool_index, tool_middleware, typed_tool, hooks
│   ├─ runtime, runtime_*, streaming, transport
│   ├─ harness, harness_*, eval, eval_*
│   ├─ pipeline/ (6-stage 턴 파이프라인 — CLAUDE.md)
│   ├─ protocol/ (A2A, Agent Card, MCP — CLAUDE.md)
│   ├─ agent/ (Agent 라이프사이클, 턴 실행, 도구 호출 — CLAUDE.md)
│   └─ base/ (Completion_contract_id, Tool, Hooks, Error, Util, Types)
│
└─ CDAL (Contract-Driven Agent Loop, "PoC-1" 자기 표기)
    ├─ Self-tagged (mli에 "CDAL PoC-1" 명시): cdal_proof, execution_mode,
    │     mode_resolver, proof_capture, proof_store, risk_class, risk_contract
    ├─ Implied (CDAL 모듈에 의존, 동일 layer): mode_enforcer, contract_runner,
    │     audit, autonomy_*, cognitive_event, completion_contract,
    │     guardrail_*, guardrails_async, verified_output, conformance,
    │     direct_evidence, effect_evidence, runtime_evidence, sessions_proof
    └─ ~ 30 modules total
```

### 1.0.1 OAS의 *공식 문서*는 CDAL을 모른다

- **README.md**: *"OCaml agent SDK on OCaml 5.x + Eio. Talks to Anthropic Messages API and OpenAI-compatible chat endpoints"*. CDAL/Contract/Proof/Audit/Mode 언급 0건.
- **dune-project synopsis**: *"Anthropic Agent SDK for OCaml (Eio Edition). A native OCaml implementation of the Anthropic Agent SDK using OCaml 5.x Eio"*. CDAL 언급 0건.
- **CLAUDE.md**: *"Layer 1: Agent Runtime — 단일 에이전트 실행 엔진"*. CDAL/Contract/Proof/Audit/Mode 언급 0건. `lib/` 디렉토리 표에 CDAL 카테고리 *없음*.

즉 *3개 공식 문서*가 OAS = "agent SDK + LLM client + agent loop"로 일관되게 정의. CDAL은 어디에도 OAS의 정식 책임으로 표기되지 않음. 그러나 *코드는* CDAL 30+ 모듈을 lib/에 거주시킴 — **문서·코드 incongruence**.

### 1.1 Verified facts (line-pinned, origin/main `fd28104b` 기준)

#### 1.1.1 OAS core → CDAL 역방향 의존 = **단 2 호출**

```
$ rg -n "Cdal_proof|Mode_enforcer|Risk_contract|Execution_mode|Proof_capture|Proof_store" \
       lib/agent/ lib/llm_provider/ lib/protocol/ lib/base/
```

결과:
- `lib/agent/agent_tools.ml:68` — `Mode_enforcer.builtin_descriptor tool.schema.name`
- `lib/protocol/mcp_schema.ml:63` — `let descriptor_for_builtin_tool = Mode_enforcer.builtin_descriptor`

이게 전부. 이 두 호출은 *동일 함수* (`Mode_enforcer.builtin_descriptor`)이고, 둘 다 *fallback 경로*에 있다 (Tool.descriptor가 없을 때만 builtin registry 조회).

#### 1.1.2 base는 CDAL 의존 0

```
$ rg -n "Cdal_proof|Mode_enforcer|Risk_contract|Execution_mode|Proof_capture|Audit\b|Autonomy_" lib/base/
```

결과: `lib/base/error.ml:40`, `lib/base/error.mli:39`에서 `Completion_contract_id.t` 사용. 이건 `Agent_sdk_base.Completion_contract_id` (base 자체의 type), CDAL 아님. CDAL 모듈 import: 0건.

#### 1.1.3 `Completion_contract` 모듈은 *core* (CDAL 아님)

검증:
- `lib/completion_contract.mli` 미존재 — *gold-standard CDAL 7개* (cdal_proof, execution_mode, mode_resolver, proof_capture, proof_store, risk_class, risk_contract)에 안 들어감.
- `lib/completion_contract.ml`: `type t = Completion_contract_id.t = ...` — base의 variant를 re-export + satisfaction predicate extension point (`required_tool_satisfaction`, 기본 `any_tool_call_satisfies`).
- CDAL import: 0건 (검증: `rg -n "Cdal_proof|Mode_enforcer|Risk_contract|Execution_mode|Proof_capture|Proof_store|Audit\b|Autonomy_" lib/completion_contract.ml`).
- 의존: `Types`, `Log`, `Tool`, `Yojson`, `Completion_contract_id` — 모두 base/core.

따라서 `agent/agent_types.ml`/`agent.mli`/`builder.ml`이 사용하는 `Completion_contract.required_tool_satisfaction`은 *core 내부 의존*. CDAL 의존 아님.

#### 1.1.4 pipeline은 core (Completion_contract만 사용)

`lib/pipeline/pipeline_common.ml`/`pipeline.ml`의 의존:
- `Completion_contract.of_tool_choice` / `validate_response` / `Require_tool_use` / `Allow_text_or_tool` / `Require_specific_tool` / `Require_no_tool_use` / `tool_use_names` / `resolve_tool_choice_contract`.
- 모두 `Completion_contract` 모듈 (§1.1.3에서 core로 분류됨).
- CDAL 모듈 호출: 0건.

#### 1.1.5 `lib/agent_sdk.ml`/`mli`는 *façade* (re-export only)

```
$ rg -n "module\s+(Cdal_proof|Mode_enforcer|...)" lib/agent_sdk.ml lib/agent_sdk.mli
```

결과: 16개 CDAL 모듈을 façade로 re-export (예: `module Mode_enforcer = Mode_enforcer`, `module Cdal_proof = Cdal_proof`, `module Risk_contract = Risk_contract`, `module Audit = Audit`, …). 이건 *코드 의존*이 아니라 *공개 표면 노출*. β3 진행 시 이 16 라인을 *삭제 또는 sublibrary 경유 re-export로 변경*하면 끝.

#### 1.1.6 CDAL 외부 consumer: masc-mcp 단일

masc-mcp의 OAS CDAL 표면 사용 (검증: `rg -n "Mode_enforcer\.|Cdal_proof\.|Risk_contract\.|Risk_class\.|Execution_mode\." ~/me/workspace/yousleepwhen/masc-mcp/lib/`):

masc-mcp가 *사용하는* CDAL 표면:
- `Mode_enforcer.violation_kind` / `violation` / serialization 함수 (type re-export at `violation_record.ml`/`mli`)
- `Cdal_proof.t` / `schema_version_current` / `of_json` / `to_json` / `run_id` / `result_status` / `artifact_ref`
- `Risk_contract.t` / `of_yojson` / `contract_id` (`cdal_loader.ml`)
- `Execution_mode.t` / `Execute` / `Draft` / `Diagnose` / `of_yojson`

masc-mcp가 *호출하지 않는* CDAL API: `Mode_enforcer.classify_tool` / `all_read_only` / `all_workspace_only` / `builtin_descriptor` / `create` / `hooks` / `Contract_runner.*` / `Mode_resolver.*` / `Proof_capture.*` / `Audit.*` / `Autonomy_*.*` / `Effect_evidence.*` / `Direct_evidence.*` / `Verified_output.*` / `Conformance.*` / `Cognitive_event.*` / `Guardrail_*`.

즉 masc-mcp는 *CDAL의 type/serialization*만 사용. 실행 함수(create/hooks/runner)는 미호출 — masc-mcp 자체에 자기 governance가 있음 (`autonomous/autonomous_executor.ml:19` `let classify_tool = ...`).

#### 1.1.7 CDAL 안 leaf 순서 (RFC-OAS-011 batch 순서 결정용)

```
for m in cdal_proof execution_mode risk_class risk_contract mode_resolver proof_capture proof_store mode_enforcer contract_runner audit autonomy_exec autonomy_diff_guard cognitive_event verified_output guardrail_llm conformance direct_evidence effect_evidence; do
  count=$(rg -l "Cdal_proof|Mode_enforcer|...|Effect_evidence" lib/$m.ml lib/$m.mli | wc -l)
  echo "$m: imports $count CDAL files"
done
```

결과:
- **Pure leaves (0 CDAL imports)**: `execution_mode`, `effect_evidence`, `guardrail_llm`
- **Low-deps (1)**: `verified_output`, `conformance`
- **Mid+High-deps (2+)**: 나머지 (cdal_proof, risk_class, risk_contract, mode_resolver, proof_capture, proof_store, mode_enforcer, contract_runner, audit, autonomy_exec, autonomy_diff_guard, cognitive_event, direct_evidence)

→ RFC-OAS-011은 leaf-first 5-batch migration.

### 1.2 무엇이 망가져 있나

1. **Layering 위반**: `agent_tools` (core, agent loop의 일부) → `Mode_enforcer.builtin_descriptor` (CDAL) — 의존 그래프가 하층 → 상층으로 흐름.
2. **공식 문서 약속 위반**: README/dune-project/CLAUDE.md는 OAS = "Anthropic Agent SDK + LLM client + agent loop"로 일관 정의. CDAL은 약속 표면에 *없음*에도 lib/에 거주.
3. **이주 차단**: RFC-OAS-011 (CDAL → masc-mcp 이주)이 진행되려면 core → CDAL 의존이 0이어야 함. 본 RFC는 그 전제.
4. **호출이 fallback에 위치**: 두 호출 모두 *Tool.descriptor가 없을 때*의 fallback 경로. 즉 *primary path*는 이미 깨끗 (`contract_runner.ml:96-110`이 `Tool.descriptor.mutation_class` 직접 사용). fallback만 끊으면 됨.

### 1.3 무엇이 *문제 아닌가* (Out of scope)

- **CDAL 모듈 자체의 이주**: RFC-OAS-011 (별도).
- **`default_tool_entries` 정리 / `classify_tool` 글로벌 제거 / `builtin_descriptor` 자체 제거**: RFC-OAS-012 (CDAL 이주 *후*).
- **`agent_sdk.ml` façade의 16개 re-export 라인 정리**: RFC-OAS-011 §3.
- **base/Completion_contract/pipeline의 의존**: §1.1.2~4에서 core로 확정 — 변경 불필요.

## 2. Proposal

### 2.1 변경 1: `lib/agent/agent_tools.ml:68`의 builtin_descriptor fallback 제거

#### Before

```ocaml
let concurrency_class_of_tool tool =
  match Tool.descriptor tool with
  | Some descriptor -> concurrency_class_from_descriptor descriptor
  | None ->
    (* Fallback: check builtin descriptor registry before defaulting *)
    (match Mode_enforcer.builtin_descriptor tool.schema.name with
     | Some descriptor -> concurrency_class_from_descriptor descriptor
     | None -> Tool.Sequential_workspace)
;;
```

#### After

```ocaml
let concurrency_class_of_tool tool =
  match Tool.descriptor tool with
  | Some descriptor -> concurrency_class_from_descriptor descriptor
  | None -> Tool.Sequential_workspace  (* fail-closed, no CDAL fallback *)
;;
```

#### 동작 변화

| 입력 | Before | After |
|---|---|---|
| Tool with `descriptor` | `concurrency_class_from_descriptor` | 동일 (변경 없음) |
| Tool without descriptor, name in CDAL `default_tool_entries` | descriptor에서 추출 | `Sequential_workspace` (fail-closed) |
| Tool without descriptor, name unknown | `Sequential_workspace` | 동일 (변경 없음) |

Tool.descriptor를 채우지 않은 *legacy consumer*가 있다면 그 동시성 분류가 가장 보수적인 `Sequential_workspace`로 떨어짐. 안전 측면에서 더 엄격해지는 변경 (over-serialize 위험만 있음, race condition 위험은 없음).

### 2.2 변경 2: `lib/protocol/mcp_schema.ml:63`의 `descriptor_for_builtin_tool` 제거

#### Before

```ocaml
let descriptor_for_builtin_tool = Mode_enforcer.builtin_descriptor

let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  let params = json_schema_to_params mcp_tool.input_schema in
  Tool.create
    ?descriptor:(descriptor_for_builtin_tool mcp_tool.name)
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~parameters:params
    call_fn
;;
```

#### After

```ocaml
(* descriptor_for_builtin_tool removed: MCP→SDK Tool conversion no longer
   consults the CDAL builtin registry. Consumer is responsible for supplying
   descriptor through MCP tool annotation or a post-conversion enrichment hook. *)

let mcp_tool_to_sdk_tool ~call_fn mcp_tool =
  let params = json_schema_to_params mcp_tool.input_schema in
  Tool.create
    ~name:mcp_tool.name
    ~description:mcp_tool.description
    ~parameters:params
    call_fn
;;
```

#### 동작 변화

MCP server가 노출한 tool을 SDK Tool.t로 변환할 때, descriptor가 *None*으로 시작. consumer가 변환 직후 `Tool.with_descriptor` 또는 별도 enrichment 단계에서 채워야 함. RFC-OAS-012가 이 자리를 *consumer-side annotation* (MCP `_meta` field 또는 명시적 register)으로 채움.

### 2.3 inline test 정리

`lib/protocol/mcp_schema.ml:192-193`의 `let%test "descriptor_for_builtin_tool task_create is mutation"` test는 함수 제거와 함께 사라짐. 다른 builtin 이름이 박힌 inline test는 일괄 grep 후 PR-C 안에서 정리.

## 3. Backward Compatibility

### 3.1 OAS 내부 호출자 영향

- `concurrency_class_of_tool`의 호출자 (`agent_tools.ml:88` `schedule_tool_use`): *Tool.descriptor 채워진 경우 영향 0*.
- `mcp_tool_to_sdk_tool`의 호출자: PR-C 작성 시 `rg -n "mcp_tool_to_sdk_tool" lib/ test/`로 검증.

### 3.2 외부 consumer (masc-mcp) 영향

- masc-mcp는 `Mode_enforcer.builtin_descriptor`를 직접 호출하지 않음 (§1.1.6 verified).
- masc-mcp가 사용하는 표면 (violation type, Cdal_proof, Risk_contract, Execution_mode)은 본 RFC에서 변경 없음.
- 영향: **0건**.

### 3.3 Wire/JSON schema 호환

- 본 RFC는 *함수 시그니처/구현*만 변경. type/JSON schema는 불변.
- masc-mcp manifest 파일과의 호환성: 변경 없음.

## 4. Migration Plan

| PR | Branch | 내용 | 의존 |
|---|---|---|---|
| **A** (this) | `feature/rfc-oas-009-v2-sever-cdal-deps` | RFC-OAS-009 v2 본문 + RFC-OAS-011 + RFC-OAS-012 docs | 독립 |
| **B** | `feature/rfc-oas-009-pr-b-agent-tools` | `agent_tools.ml:68`의 builtin_descriptor fallback 제거. inline test 추가 (Tool descriptor 없음 → Sequential_workspace) | A 머지 후 |
| **C** | `feature/rfc-oas-009-pr-c-mcp-schema` | `mcp_schema.ml:63`의 `descriptor_for_builtin_tool` 제거. mcp_tool_to_sdk_tool descriptor=None로. inline test 정리 | B 머지 후 |
| **D** | `feature/rfc-oas-009-pr-d-verify-zero-deps` | 검증 PR: `rg -n "Cdal_proof|Mode_enforcer|Risk_contract|Risk_class|Execution_mode|Proof_capture|Proof_store" lib/agent/ lib/llm_provider/ lib/protocol/ lib/base/`가 0건 반환을 *CI lint*로 강제 | C 머지 후 |

각 PR은 Draft + `human-approved-ready` 라벨 게이트 (메모리 `feedback_user_rejects_cron_pr_loop`, `feedback_masc_mcp_draft_guard_blocks_agent_ready` 준수). cron+자동 PR 패턴 거부.

## 5. Risks (4건)

| # | 위험 | 완화 |
|---|---|---|
| 1 | Tool.descriptor를 채우지 않은 *외부 consumer*가 빈 descriptor 반환에 의존 | §2.1 동작 변화 표 참조: 가장 보수적 분류 (`Sequential_workspace`)로 fail-closed. 안전 측면에서 더 엄격 |
| 2 | `mcp_tool_to_sdk_tool`의 호출자가 descriptor=None Tool을 그대로 hook chain에 보내면 `mode_enforcer.classify_tool` 글로벌이 `External_effect` fallback 처리 (현재 동작 그대로 유지) | RFC-OAS-012가 `classify_tool` 글로벌 자체를 정리. 본 RFC 범위 외 |
| 3 | inline test 제거가 다른 dune build/CI에 묶여있을 수 있음 | PR-C 작성 시 `rg -n "descriptor_for_builtin_tool" lib/ test/`로 grep 의무. 0건 확인 후 제거 |
| 4 | RFC-OAS-009 v1이 main에 머지된 상태에서 v2로 본문 대체 — git history에 의도 변경이 모호하게 보일 수 있음 | §0 Revision History를 본 RFC 본문 첫 섹션에 명시. v1 PR (#1478)에 v2 redefinition 코멘트 추가 |

## 6. References

- RFC-OAS-008: Typed Tool Identification
- RFC-OAS-009 v1 (`docs/rfc/RFC-OAS-009-tool-name-ignorance.md`, merged `7149c5a7`): superseded by this v2
- RFC-OAS-011 (this PR): CDAL Migration to masc-mcp — *follow-up* to this RFC
- RFC-OAS-012 (this PR): Tool Name Ignorance within CDAL — RFC-OAS-009 v1의 *원의도*가 이주 후 흡수되는 곳
- README.md (OAS): "Anthropic Agent SDK for OCaml (Eio Edition)" — CDAL은 약속에 없음
- CLAUDE.md (OAS): "Layer 1: Agent Runtime — 단일 에이전트 실행 엔진" — CDAL 카테고리 없음
- `lib/cdal_proof.mli` line 4: "Part of the Contract-Driven Agent Loop (CDAL) PoC-1"
- `lib/contract_runner.ml:96-110` (positive evidence — 깨끗한 `Tool.descriptor` 경로)
- 메모리 `feedback_rfc_section_1_4_caller_context_unverified` (2026-05-05): line-pinned 검증 룰
- 메모리 `feedback_user_rejects_cron_pr_loop` (2026-05-07): Draft + `human-approved-ready` 라벨 게이트
- 메모리 `feedback_masc_mcp_draft_guard_blocks_agent_ready` (2026-05-05): agent ready_for_review 자동 거부 — Draft 유지
