# RFC-OAS-013: Keeper Tool Schema Disclosure Activation

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-11 |
| Target | `agent_sdk` (oas) v0.194+ — API merged via PR #1508 / `masc_mcp` v0.20+ — keeper activation |
| Supersedes | None |
| Depends-on | PR #1508 (merged f48ccec3) — introduces `Tool.disclosure_level` and `Builder.with_disclosure_level` |
| Related | RFC-OAS-004 (Code Snippet Tool Strategy), Tool_selector (`lib/tool_selector.mli`, 2-stage routing) |

## 0. Summary

OAS PR #1508은 `Tool.disclosure_level = Full_schema | Minimal_index | Hybrid` 인프라를 default `Full_schema`로 머지했다. 본 RFC는 masc-mcp keeper에서 *언제, 어떻게, 얼마나 안전하게* `Hybrid`를 활성화할지를 정한다.

**핵심 가설**: 39+ keeper tool × 평균 ~2.5KB schema = 매 turn 약 97.5KB(≈25–30k tokens) tool schema 직렬화. `Tool_selector.TopK_llm`로 narrowing된 top-K(~5–10개)만 `Full_schema`로 보내고 나머지를 `Minimal_index`(name+description만)로 보내면 schema bucket -60~70% 절감 가능.

**핵심 위험**: `Minimal_index`로 보내는 tool에 대해 모델이 `tool_use` 인자를 정확히 채울 수 없음 → tool_call 실패율 증가. 카나리 + fallback 로직으로 차단.

## 1. Problem Statement

### 1.1 측정된 사실

- masc-mcp keeper의 base system prompt는 9-section 조합 (`lib/keeper/keeper_prompt.ml:154-285`).
- 등록된 keeper tool 39개, 평균 ~2.5KB schema → 매 turn 약 97.5KB serialization.
- A6000 ×1, 64k context × 4 keeper 병렬 시나리오에서 초기 instruction이 25-30k tokens 점유하면 대화 budget이 30-40k로 압축됨 — 1-2 hop tool chain 이상 어려움.

### 1.2 2-stage routing의 절반 닫힘 상태

- masc-mcp `lib/keeper/keeper_run_tools.ml:721,729,740` — `Tool_selector.TopK_llm` + `select_names`로 *이름 기반 narrowing* 이미 수행.
- OAS `lib/agent/agent_turn.ml:159` — selected top-K가 LLM에 갈 때 schema는 *full*. **이름 단계는 좁히는데 schema 단계는 그대로**.
- 본 RFC는 schema 단계도 Hermes 류 lazy-disclosure로 닫는다.

### 1.3 PR #1508 이후 잔존 책임

PR #1508은 *인프라*만 추가. default `Full_schema`로 머지 시 wire byte-identical → 측정 가능한 효과 0. 본 RFC가 *없으면* PR #1508은 dormant code로 남는다 (MEMORY `feedback_lint_string_classifier_is_workaround_not_fundamental` 경고 패턴).

## 2. Proposal

### 2.1 활성화 형태

masc-mcp `lib/keeper/keeper_run_tools.ml`에서 keeper 생성 시:

```ocaml
let selected_names = Tool_selector.select_names ~strategy ~context ~tools in
let disclosure = Tool.Hybrid { full_names = selected_names } in
Builder.with_disclosure_level disclosure builder
```

`Tool_selector`가 이미 top-K를 결정하므로 `full_names`에 그 결과를 그대로 위임. Selector와 Disclosure가 **동일한 데이터(top-K names)** 를 공유 — 두 단계가 한 점에서 결정됨.

### 2.2 Activation Plan (3-phase canary)

| Phase | 대상 | 기간 | Gate |
|---|---|---|---|
| **P0** | keeper 1명 (`imseonghan` 권장 — TLA spec 있고 transition 단순) | 7일 | `tool_call_error_rate` ≤ baseline + 2pp |
| **P1** | keeper 5명 (다양한 persona) | 7일 | `display_total_tokens` schema bucket -30% 이상 + `tool_call_error_rate` ≤ baseline + 2pp |
| **P2** | 전체 keeper | rolling | regression alert 없음 |

P0/P1 종료 후 데이터로 P2 진입 결정. 한 phase 실패 시 즉시 `with_disclosure_level Full_schema` (또는 호출 자체 제거)로 롤백.

### 2.3 Fallback Design (별도 OAS PR로 분리)

`Minimal_index` tool에 대한 `tool_use` 실패를 감지하면 다음 turn에서 해당 keeper만 `Full_schema` 강등.

```ocaml
(* lib/agent/agent_turn.ml 신규 logic *)
let next_disclosure ~prev_disclosure ~last_tool_errors =
  match prev_disclosure, last_tool_errors with
  | Hybrid _, errors when has_schema_shape_error errors -> Full_schema
  | level, _ -> level
```

`has_schema_shape_error`는 `Llm_provider`의 tool argument validation error를 식별. 본 RFC 머지 *전*에 OAS에 fallback PR(별도) 먼저 머지 — 카나리 안전망.

### 2.4 결정성 보장

`Hybrid { full_names = [...] }`의 `full_names`는 매 turn 결정적으로 구성됨 (Tool_selector 결과 = 결정적 함수). prefix cache hit이 유지되도록 list 순서도 안정 정렬 필수 (`List.sort compare`).

## 3. Measurement

`Keeper_agent_prompt_metrics.build_ctx_composition_metrics` (`lib/keeper/keeper_agent_prompt_metrics.ml:222-282`) 재사용.

| 메트릭 | 기준 | Goal (P0) | Goal (P1) |
|---|---|---|---|
| `display_total_tokens` (schema bucket) | baseline 직전 7일 평균 | -30% | -50% |
| `tool_call_error_rate` (tool_use 인자 파싱 실패) | baseline | ≤ +2pp | ≤ +1pp |
| `keeper_turn_success_rate` | baseline | ≥ -1pp | ≥ baseline |
| `prefix_cache_hit_ratio` | baseline | ≥ baseline | ≥ baseline (sorting 결정성으로) |

측정 수집: 기존 Prometheus exporter + Grafana board. 새 메트릭 없음.

## 4. Risks & Mitigations

| Risk | Severity | Mitigation |
|---|---|---|
| 모델이 minimal schema로 args 생성 실패 | HIGH | (a) Fallback PR 선행 머지. (b) P0 카나리 1명 + tool_call_error_rate gate. (c) keeper별 toggle. |
| `Tool_selector`가 잘못 narrowing해서 진짜 필요한 tool이 `Minimal`로 강등 | MEDIUM | top-K 크기를 보수적으로 시작 (10+). `Tool_selector.strategy`는 본 RFC 변경 없음. |
| Prefix cache invalidation | MEDIUM | `full_names`를 매 turn `List.sort compare` 적용해 결정성 확보. |
| Telemetry 측정 오류 / 잘못된 baseline | MEDIUM | P0 기간 baseline은 활성화 *직전* 7일 — 같은 keeper, 같은 worker pool. |
| RFC 머지만 하고 활성화 PR 지연되면 dormant 시간 길어짐 | LOW | RFC 머지와 활성화 PR을 *같은 sprint* 안에 묶는다. RFC body에 "P0 시작 expected date" 명시. |

## 5. Rollback

| Phase | Rollback 절차 |
|---|---|
| P0 | masc-mcp keeper config에서 `disclosure_level` 호출 1줄 제거 → 즉시 Full_schema 복원. PR revert 옵션도 가능. |
| P1 | P0와 동일. 다중 keeper면 plant-wide config flag 도입 고려 (별도 RFC). |
| P2 | 전체 비활성화 시 OAS는 무영향(default Full). |

## 6. Out of Scope

| 차후 작업 | 이유 |
|---|---|
| `Y`: `input_schema` required-only 압축 | `disclosure_level`과 직교한 별도 절감 축. P2 이후 별도 RFC. |
| Final prompt hard cap | masc-mcp `keeper_run_prompt.ml:153` gate, 결정성 보장 별도 축. |
| Section-wise enforcement gate | `keeper_agent_prompt_metrics`가 측정만 함 → enforce 추가는 별도 RFC. |
| Tool_selector strategy 변경 | 본 RFC는 strategy 무변경. selection 알고리즘 자체 개선은 별도. |
| BPE 토큰 정밀 측정 | 본 RFC는 byte size proxy 사용. tokenizer 정밀 측정은 측정 인프라 별도 추가 시. |

## 7. Activation Sequence (작업 순서)

1. **OAS fallback PR** — `next_disclosure` 로직 + `has_schema_shape_error` 추가. `Llm_provider` argument-validation error 식별 경로 신규. ~80-150 LOC + 테스트.
2. **본 RFC 머지** — masc-mcp `pr-rfc-check.sh` 트리거 통과 조건.
3. **masc-mcp 활성화 PR (P0)** — keeper 1명 한정. RFC-OAS-013 인용. `lib/keeper/keeper_run_tools.ml` 수정.
4. **P0 telemetry 1주 → P1 확대 PR**.
5. **P1 telemetry 1주 → P2 전체 적용**.

## 8. Open Questions

- (Q1) `Tool_selector` strategy 미설정 keeper (default = `All`)는 어떻게? — P0 대상은 `TopK_llm` 설정된 keeper로 제한. 미설정 keeper는 P2까지 default Full 유지.
- (Q2) MCP tool과 inline tool 혼합 시 disclosure 적용 일관성? — 양쪽 모두 `Tool.t`로 들어오므로 동일 처리. CLI runtime-MCP tool은 본 PR 영향 밖.
- (Q3) `full_names`에 selected에 없는 이름이 들어가면? — PR #1508 구현은 `List.mem` 단순 매치, 단순 무시 (성능 무영향). 별도 검증 불요.
