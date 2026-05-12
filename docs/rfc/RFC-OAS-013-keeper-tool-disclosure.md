# RFC-OAS-013: Keeper Tool Schema Disclosure Activation

| | |
|---|---|
| Status | Amended (2026-05-12) |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-11 |
| Amended | 2026-05-12 — v1 §2.1 wiring 의사코드가 OAS 인프라와 mismatch 발견 → v2 static Hybrid로 정정 |
| Target | `agent_sdk` (oas) >= v0.193.6 / `masc_mcp` keeper activation |
| Supersedes | None |
| Depends-on | OAS PR #1508 (merged f48ccec3) `Tool.disclosure_level`; OAS PR #1511 (merged 7ed9c052) `Disclosure_resolver` |
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

PR #1508은 *인프라*만 추가. default `Full_schema`로 머지 시 wire byte-identical → 측정 가능한 효과 0. 본 RFC가 *없으면* PR #1508은 dormant code로 남는다 (인프라는 추가됐지만 활성화 RFC가 없으면 근본 효과 없이 우회용 코드로 남는다는 경고 패턴).

## 2. Proposal

### 2.1 활성화 형태

**Amend 2026-05-12 (v2)**: 초안 v1의 wiring 의사코드는 *현재 머지된 OAS 인프라*와 안 맞음 (post-merge 정직 평가에서 발견). v1의 형태:

```ocaml
(* v1 — 작동 불가 *)
let selected_names = Tool_selector.select_names ~strategy ~context ~tools in
Builder.with_disclosure_level (Tool.Hybrid { full_names = selected_names }) builder
```

이 형태는 **두 가지 mismatch**:
1. `Builder.with_disclosure_level`는 agent 생성 시 *1회* 설정 → agent lifetime 동안 *동일 값*.
2. `Tool_selector.select_names`는 *매 turn의 query context 기반* 동적 결과.

→ static builder 자리에 dynamic selector 출력을 박으면 *첫 turn의 query*만 반영되고 두 번째 turn부터 stale.

OAS의 `Disclosure_resolver.resolve` signature는:
```ocaml
resolver : Types.tool_result list -> Tool.disclosure_level option
```
*last_results만* 받음. selector 결과나 query messages는 못 봄 → v1 의사코드의 *매 turn dynamic Hybrid* 를 OAS 인프라만으로 구현할 수 없음.

#### v2 활성화 형태 (P0 범위)

masc-mcp `lib/worker_oas.ml`의 keeper builder 파이프라인에서:

```ocaml
(* P0: imseonghan keeper에만 static Hybrid 적용. *)
|> (fun b ->
     if meta.name = "imseonghan"
     then
       let core_names = Keeper_run_tools.core_tool_names meta in
       Agent_sdk.Builder.with_disclosure_level
         (Agent_sdk.Tool.Hybrid
            { full_names = List.sort compare core_names })
         b
     else b)
|> (fun b ->
     if meta.name = "imseonghan"
     then
       let demote_on_error (results : Agent_sdk.Types.tool_result list) =
         if List.exists Result.is_error results
         then Some Agent_sdk.Tool.Full_schema
         else None
       in
       Agent_sdk.Builder.with_disclosure_resolver demote_on_error b
     else b)
```

핵심 결정:
- **Static Hybrid**: `full_names`는 **keeper의 `always_include` core tools 만**. selector top-K 추적 *안 함*. selector는 *시각적으로* visible_tools를 좁히는 역할, disclosure는 *그 안에서* schema 깊이를 다시 좁힘 — 두 단계는 독립.
- **List.sort compare**: prefix cache hit 유지를 위해 결정적 순서 강제.
- **Resolver = demote-on-error**: 직전 turn에 *어떤* tool error라도 있으면 다음 turn은 Full_schema. 정밀 분류(`error_class = Deterministic` 등)는 v3 후속.

#### Selector top-K 동적 forwarding 은 v3 (Out of Scope, §6)

매 turn selector top-K → Hybrid.full_names 전달이 더 정밀한 절감이지만, OAS `Disclosure_resolver.resolve` signature 확장(`~messages` 또는 `~tool_selector_result` 인자 추가) 필요. 별도 OAS PR + RFC §6 항목으로 분리.

### 2.2 Activation Plan (3-phase canary)

| Phase | 대상 | 기간 | Gate |
|---|---|---|---|
| **P0** | keeper 1명 (`imseonghan` 권장 — TLA spec 있고 transition 단순) | 7일 | `tool_call_error_rate` ≤ baseline + 2pp |
| **P1** | keeper 5명 (다양한 persona) | 7일 | `display_total_tokens` schema bucket -30% 이상 + `tool_call_error_rate` ≤ baseline + 2pp |
| **P2** | 전체 keeper | rolling | regression alert 없음 |

P0/P1 종료 후 데이터로 P2 진입 결정. 한 phase 실패 시 즉시 `with_disclosure_level Full_schema` (또는 호출 자체 제거)로 롤백.

### 2.3 Fallback Design

**Amend 2026-05-12 (v2)**: OAS PR #1511(머지)로 `Disclosure_resolver` mechanism이 들어옴. v1 의사코드의 `next_disclosure ~prev_disclosure ~last_tool_errors`는 *policy를 OAS에 박는* 형태였는데, 머지된 mechanism은 *policy를 caller(masc-mcp)에 위임*:

```ocaml
(* OAS 측 — mechanism only *)
val Disclosure_resolver.resolve
  :  resolver:(Types.tool_result list -> Tool.disclosure_level option) option
  -> static:Tool.disclosure_level option
  -> last_results:Types.tool_result list
  -> Tool.disclosure_level option

(* masc-mcp 측 — policy 결정 (P0 v2 형태) *)
let demote_on_error (results : Types.tool_result list) =
  if List.exists Result.is_error results
  then Some Tool.Full_schema
  else None
;;
Builder.with_disclosure_resolver demote_on_error builder
```

**Signature 한계**: resolver는 `last_results`만 받음 → 직전 turn에 *어떤* tool result가 있었는지만 봄. selector 결과, messages 전체, 현재 turn의 query는 못 봄. 그래서 § 2.1 v2가 *selector top-K dynamic forwarding*을 P0 범위에서 제외하고 v3 (Out of Scope §6)으로 미룸.

**fallback 정밀도**: v2 P0의 `demote_on_error`는 `Result.is_error` 기반 — *모든 tool error*가 demote 트리거. 정밀한 schema-shape error 분류 (`Tool_input_validation.Invalid` 만 트리거)는 OAS resolver signature 확장과 함께 v3.

### 2.4 결정성 보장

`Hybrid { full_names = [...] }`의 `full_names`는 **keeper config 또는 keeper meta에서 도출**한 core tool 이름 (v2: static). v1의 "Tool_selector 결과를 결정적 함수로 사용"은 *별도 turn마다 다른 결과*라 prefix cache 안정성과 충돌. v2는 *agent lifetime 전체 동일* full_names → prefix cache 안정성 자동 확보.

순서 결정성을 위해 `List.sort compare full_names` 강제 — v1과 동일 원칙.

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
| RFC 머지만 하고 활성화 PR 지연되면 dormant 시간 길어짐 | LOW | RFC 머지와 활성화 PR을 *같은 sprint* 안에 묶는다. |

## 5. Rollback

| Phase | Rollback 절차 |
|---|---|
| P0 | masc-mcp keeper config에서 `disclosure_level` 호출 1줄 제거 → 즉시 Full_schema 복원. PR revert 옵션도 가능. |
| P1 | P0와 동일. 다중 keeper면 plant-wide config flag 도입 고려 (별도 RFC). |
| P2 | 전체 비활성화 시 OAS는 무영향(default Full). |

## 6. Out of Scope

| 차후 작업 | 이유 |
|---|---|
| **v3: Disclosure_resolver signature 확장** (Amend 2026-05-12) | 현재 resolver는 `last_results : tool_result list`만 받음. selector top-K, messages, 현재 query를 못 봐서 §2.1 v1의 *매 turn dynamic Hybrid* 구현 불가. OAS 측 `~messages` 또는 `~tool_selector_result` 인자 추가 PR → 본 RFC §2.1 v3 wiring으로 확대. |
| **v3: 정밀 fallback 분류** (Amend 2026-05-12) | v2의 `demote_on_error`는 `Result.is_error` 기반(*모든* tool error 트리거). `Tool_input_validation.Invalid`만 트리거하는 정밀 분류는 OAS `tool_error.error_class` 활성화 + resolver signature 확장 함께. |
| `input_schema` required-only 압축 | `disclosure_level`과 직교한 별도 절감 축. P2 이후 별도 RFC. |
| Final prompt hard cap | masc-mcp `keeper_run_prompt.ml:153` gate, 결정성 보장 별도 축. |
| Section-wise enforcement gate | `keeper_agent_prompt_metrics`가 측정만 함 → enforce 추가는 별도 RFC. |
| Tool_selector strategy 변경 | 본 RFC는 strategy 무변경. selection 알고리즘 자체 개선은 별도. |
| BPE 토큰 정밀 측정 | 본 RFC는 byte size proxy 사용. tokenizer 정밀 측정은 측정 인프라 별도 추가 시. |

## 7. Activation Sequence (작업 순서)

**Amend 2026-05-12 — actual sequence**:

1. ✅ **OAS PR #1508** (merged f48ccec3) — `Tool.disclosure_level` infrastructure.
2. ✅ **OAS PR #1511** (merged 7ed9c052) — `Disclosure_resolver` mechanism (RFC §2.3 fallback의 머지된 형태).
3. ✅ **OAS PR #1510 — 본 RFC v1** (merged bf68fa55) — masc-mcp `pr-rfc-check.sh` 트리거 통과 조건.
4. ✅ **masc-mcp PR #14676** (merged) — `agent_sdk` lock bump 0.184 → 0.193.4 (catch-up).
5. 🟡 **본 amend PR** — §2.1 v1 → v2 정정 (정직성 회복).
6. ⏭ **masc-mcp 활성화 PR (P0)** — imseonghan keeper에 §2.1 v2 wiring 적용. `lib/worker_oas.ml` 분기. `masc_mcp.opam` + `dune-project` constraint를 `>= 0.193.6`로 좁힘 (Disclosure_resolver는 OAS PR #1511에서 0.193.5로 릴리스됨; 현재 SDK는 0.193.6 — constraint widening + 실제 호출을 같은 PR — N-of-M 회피).
7. ⏭ **P0 telemetry 1주 → P1 확대 PR**.
8. ⏭ **P1 telemetry 1주 → P2 전체 적용**.
9. ⏭ **v3 OAS PR — Disclosure_resolver signature 확장** — `~messages` 또는 `~tool_selector_result` 인자 추가. §2.1 v3 wiring(매 turn dynamic Hybrid) 가능해짐.
10. ⏭ **v3 활성화 PR** — selector top-K → Hybrid.full_names 매 turn forward.

## 8. Open Questions

- (Q1) `Tool_selector` strategy 미설정 keeper는 v2 wiring에서? — v2는 selector와 독립. `full_names = core_tool_names`만 사용 → selector strategy 무관. **v2에서 Q1은 무의미해짐**.
- (Q2) MCP tool과 inline tool 혼합 시 disclosure 적용 일관성? — 양쪽 모두 `Tool.t`로 들어오므로 동일 처리. CLI runtime-MCP tool은 본 RFC 영향 밖.
- (Q3) `full_names`에 keeper tool set에 없는 이름이 들어가면? — OAS PR #1508 구현은 `List.mem` 단순 매치, mismatch 시 silent ignore (해당 이름은 그 turn에 그냥 무효). v2에서는 `core_tool_names`가 meta에서 도출되므로 mismatch 가능성 낮음.
- (Q4 v2 신규) **`core_tool_names`의 SSOT는?** — `Keeper_run_tools.core_tool_names meta`로 가정했지만 실제 함수가 없으면 활성화 PR이 helper 추가. keeper TOML config의 `always_include` 필드가 1차 후보, 코드에서 derive하는 helper가 2차.
- (Q5 v2 신규) **v3 이전 P0 절감 추정?** — v2 static Hybrid는 core tools(보통 5-8개)만 Full, 나머지 30+ 개 Minimal → schema bucket -50~70% 예상. v3 dynamic top-K Hybrid는 절감 폭 비슷하지만 *적합도*가 더 높음 (turn의 실제 query에 맞는 tool만 Full). v2 P0의 1차 목표는 *모델이 minimal로 args 채울 수 있는가* 시그널 — token saving은 부차적.
