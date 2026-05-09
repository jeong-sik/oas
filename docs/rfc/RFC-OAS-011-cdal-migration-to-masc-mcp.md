# RFC-OAS-011: CDAL Migration to masc-mcp

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-08 |
| Target | `agent_sdk` (oas) v0.193+ → CDAL 30+ 모듈 제거 / `masc_mcp` v0.20+ → `masc_mcp.cdal` sublibrary 신설 |
| Supersedes | None |
| Depends-on | RFC-OAS-009 v2 (Sever Core→CDAL Dependencies) — *완료 후* 본 RFC 진행 |
| Related | RFC-OAS-012 (Tool Name Ignorance within CDAL) |

## 0. Summary

CDAL (Contract-Driven Agent Loop) 30+ 모듈을 OAS lib에서 *제거*하고 masc-mcp의 새 sublibrary `masc_mcp.cdal`로 이주한다.

**근거**:
- OAS의 README/dune-project synopsis/CLAUDE.md 어디에도 CDAL이 OAS의 정식 책임으로 명시되지 않음 (RFC-OAS-009 §1.0.1 verified).
- CDAL 외부 consumer는 masc-mcp 단일 (RFC-OAS-009 §1.1.6 verified).
- CDAL 자체 mli 7개가 *"PoC-1"*로 자기 표기 — *experimental*임을 인정.
- RFC-OAS-009 v2 완료 후 OAS core → CDAL 역방향 의존 0건이 됨 — 이주 가능 조건 충족.

**결과**:
- OAS lib는 README가 약속한 "Anthropic Agent SDK + LLM client + agent loop" 표면만 보유.
- CDAL은 자기 *유일한 consumer* 안에 거주 — multi-agent governance가 multi-agent coordinator의 일부라는 *자연스러운 위치*.
- 새 generic agent SDK consumer (CDAL 불필요)는 OAS만 의존.

## 1. Problem Statement

### 1.0 OAS lib는 *약속한 것보다 많은 것*을 갖고 있다

#### 약속 (3개 공식 문서)
- README.md: *"OCaml agent SDK on OCaml 5.x + Eio. Talks to Anthropic Messages API and OpenAI-compatible chat endpoints"*
- dune-project synopsis: *"Anthropic Agent SDK for OCaml (Eio Edition)"*
- CLAUDE.md: *"Layer 1: Agent Runtime — 단일 에이전트 실행 엔진"*

#### 실제
- 157 top-level modules
- 그중 ~30 모듈이 *Contract-Driven Agent Loop (CDAL) PoC-1* — governance/proof/audit framework
- 7 mli가 *"Part of the Contract-Driven Agent Loop (CDAL) PoC-1"*으로 자기 표기

### 1.1 CDAL 모듈 인벤토리 (verified, line-pinned)

#### 1.1.1 Self-tagged CDAL (mli "PoC-1" 표기, 7개)

```
$ rg -l "Part of the Contract-Driven Agent Loop|CDAL PoC" lib/
```

결과:
- `lib/cdal_proof.mli`
- `lib/execution_mode.mli`
- `lib/mode_resolver.mli`
- `lib/proof_capture.mli`
- `lib/proof_store.mli`
- `lib/risk_class.mli`
- `lib/risk_contract.mli`

이 7개는 *gold-standard CDAL*. 자기 선언.

#### 1.1.2 Implied CDAL (gold-standard에 의존, 동일 layer로 간주)

| 모듈 | gold-standard 의존 | OAS-내 caller (lib/ + bin/) | 비고 |
|---|---|---|---|
| `mode_enforcer` | `Execution_mode`, `Risk_contract`, `Effect_evidence` | core가 호출 (RFC-OAS-009 §1.1.1) — RFC-OAS-009 v2 PR-B/C에서 끊음 | 이주 대상 (CDAL) |
| `contract_runner` | `Risk_contract`, `Execution_mode`, `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `audit` | `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `autonomy_exec` | `Audit`, `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `autonomy_diff_guard` | `Audit` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `autonomy_trace_analyzer` | 0 (검증) | **lib/ + bin/ caller 0**, test 2 (`test_autonomy_smoke`, `test_autonomy_trace_unit`) | **dead-in-lib (test-only)** — 이주 대상 (CDAL test와 함께) |
| `cognitive_event` | `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `completion_contract` | base only — *core* (RFC-OAS-009 §1.1.3) | 9 (`pipeline_common`, `pipeline`, `agent_sdk` façade, `agent_types`, `builder`) | **이주 대상 아님 (core)** |
| `guardrail_llm` | 0 (검증) | 2 (`agent_sdk` façade .ml/.mli) | **이주 대상 아님 (core)** — façade 직접 노출, mli에 PoC 표기 없음 |
| `guardrail_tripwire` | 0 (검증) | 2 (`agent_sdk` façade .ml/.mli) | **이주 대상 아님 (core)** — façade 직접 노출 |
| `guardrails_async` | 0 (검증, `open Types`) | 10 (`pipeline`, `agent_types`, `builder`, `agent_sdk` façade …) | **이주 대상 아님 (core)** — pipeline 통합 |
| `verified_output` | `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `conformance` | `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `direct_evidence` | `Cdal_proof`, `Runtime_evidence` | masc-mcp 호출 0 | 이주 대상 (CDAL) |
| `effect_evidence` | 0 (검증, `open Result_syntax`) | 3 (`mode_enforcer.ml/.mli`, `proof_capture`) — 모두 CDAL gold-standard | **이주 대상 (CDAL)** — caller 모두 CDAL |
| `runtime_evidence` | 0 (검증, `open Runtime`) | 2 — `runtime_server_worker` (core, 16 호출) + `direct_evidence` (CDAL, 11 호출) | **이주 대상 아님 (core stratum)** — core가 16곳 사용. CDAL이 import하는 정상 방향 |
| `sessions_proof` | `Cdal_proof` | masc-mcp 호출 0 | 이주 대상 (CDAL) |

→ **§1.1.2 검증 결과 (2026-05-09, origin/main `9aabd00f`)**: 분류 검토 표시였던 7개 모듈을 grep으로 layer 확정. 결과:
- **이주 대상 (CDAL) 추가**: `autonomy_trace_analyzer` (test-only, dead-in-lib), `effect_evidence`
- **이주 제외 (core) 확정**: `guardrail_llm`, `guardrail_tripwire`, `guardrails_async`, `runtime_evidence`, `completion_contract`(기존)
- 측정 명령:
  ```bash
  $ rg -n "Cdal_proof|Mode_enforcer|Risk_contract|Execution_mode|Risk_class|Proof_capture|Proof_store|Mode_resolver|Audit\.|Cognitive_event|Conformance|Verified_output|Direct_evidence|Sessions_proof|Effect_evidence|Autonomy_|Runtime_evidence" lib/<m>.{ml,mli}
  $ rg -l "\b<Module>\b" lib/ bin/ test/
  ```

→ §1.1.2 검증 의무는 G0로 *해소*. PR-A는 §1.1.3 batch 정의에 따라 곧장 작업 가능.

→ 본 검증으로 §1.1.3 B5 ("분류 검토 통과 모듈") 항목은 사실상 **0개 추가** (분류 검토 통과 7개 중 1개만 CDAL = `effect_evidence`이고, 그건 §1.1.3에서 이미 B1 leaf로 명시됨; `autonomy_trace_analyzer`는 dead-in-lib로 별도 처리 가능).

#### 1.1.3 Migration batch 순서 (leaf-first, 5 batches)

CDAL 모듈끼리의 의존 그래프 (RFC-OAS-009 §1.1.7 verified, §1.1.2 검증 결과 반영):

| Batch | 모듈 | leaf-status |
|---|---|---|
| **B1 (pure leaves)** | `execution_mode`, `effect_evidence`, `autonomy_trace_analyzer` (+ test 2개) | 0 CDAL imports |
| **B2 (low-deps)** | `risk_class`, `verified_output`, `conformance` | 1 CDAL import |
| **B3 (mid-deps)** | `risk_contract`, `cdal_proof`, `mode_resolver`, `cognitive_event` | 2+ CDAL imports |
| **B4 (high-deps)** | `proof_capture`, `proof_store`, `mode_enforcer`, `contract_runner`, `direct_evidence` | gold-standard 다수 의존 |
| **B5 (top-deps)** | `audit`, `autonomy_exec`, `autonomy_diff_guard`, `sessions_proof` | 모두 합체 |

각 batch는 *별도 PR*. 빌드 clean + dune runtest 회귀 0이 batch 통과 조건.

**B1 변경 사항 (G0 결과 반영)**:
- `guardrail_llm`은 *core 잔류* 확정 (façade `agent_sdk.{ml,mli}` 직접 노출). B1에서 제외.
- `autonomy_trace_analyzer`는 dead-in-lib (test-only) → B1에 포함하되 `test_autonomy_smoke.ml`, `test_autonomy_trace_unit.ml`도 함께 이주.
- `effect_evidence`는 §1.1.2 검증으로 CDAL 확정 (caller 모두 CDAL gold-standard).

**Core 잔류 확정 모듈 (이주 제외)**:
- `completion_contract` (RFC-OAS-009 §1.1.3 + G0 caller 9건 확인)
- `guardrail_llm` (G0)
- `guardrail_tripwire` (G0)
- `guardrails_async` (G0)
- `runtime_evidence` (G0 — `runtime_server_worker` 16 호출, core stratum)

### 1.2 masc-mcp 측 사용 표면 (이주 후 inline 가능 여부 평가)

masc-mcp가 *사용하는* CDAL 표면 (RFC-OAS-009 §1.1.6):
- `Mode_enforcer.violation_kind` / `violation` / serialization (`violation_record.ml`/`mli`)
- `Cdal_proof.t` / `schema_version_current` / `of_json` / `to_json` / `run_id` / `result_status` / `artifact_ref`
- `Risk_contract.t` / `of_yojson` / `contract_id`
- `Execution_mode.t` / variants / `of_yojson`

이주 후 masc-mcp가 *직접 정의*: `Masc_mcp.Cdal.Mode_enforcer.violation`, `Masc_mcp.Cdal.Cdal_proof.t`, … 동일 type, 동일 serialization. 호출 코드만 prefix 변경.

이주 후 masc-mcp가 *호출하지 않는* CDAL API: `classify_tool` / `all_read_only` / `all_workspace_only` / `builtin_descriptor` / `create` / `hooks` / `Contract_runner.*` / `Mode_resolver.*` / `Proof_capture.*` / `Audit.*` / `Autonomy_*.*` / `Effect_evidence.*` / `Direct_evidence.*` / `Verified_output.*` / `Conformance.*` / `Cognitive_event.*` — 즉 *대부분의 CDAL*은 masc-mcp 자체 호출자도 없음. *self-contained governance framework*가 *외부 호출 0*인 상태로 lib에 거주.

→ 이주 후 *masc-mcp가 자기 호출 시작*할지, *deprecate*할지는 RFC-OAS-013+에서 결정 (본 RFC 범위 외).

## 2. Proposal

### 2.1 Target home: `masc-mcp/lib/cdal/` + dune sublibrary

#### 새 dune library

`~/me/workspace/yousleepwhen/masc-mcp/lib/cdal/dune`:

```dune
(library
 (name masc_mcp_cdal)
 (public_name masc_mcp.cdal)
 (libraries
   masc_types fs_compat time_compat masc_log
   ; Note: NO dependency on agent_sdk's CDAL re-exports.
   ;       Only depends on OAS's *core* (Tool, Hooks, base/types).
   agent_sdk.base
   yojson
   ppx_deriving_yojson.runtime)
 (preprocess
  (pps ppx_deriving_yojson ppx_deriving.show ppx_inline_test ppx_let)))
```

masc-mcp의 main library가 `masc_mcp.cdal`을 의존하도록 `lib/dune`에 추가:

```dune
(library
 (name masc_mcp)
 (libraries
   ...
   masc_mcp.cdal     ; NEW
   agent_sdk
   agent_sdk.base
   ...))
```

### 2.2 Module path 변환

| Before (OAS) | After (masc-mcp) |
|---|---|
| `Agent_sdk.Cdal_proof` | `Masc_mcp_cdal.Cdal_proof` (또는 `Masc_mcp.Cdal.Cdal_proof`) |
| `Agent_sdk.Mode_enforcer` | `Masc_mcp_cdal.Mode_enforcer` |
| `Agent_sdk.Risk_contract` | `Masc_mcp_cdal.Risk_contract` |
| `Agent_sdk.Execution_mode` | `Masc_mcp_cdal.Execution_mode` |
| ... (30+ 모듈) | ... |

### 2.3 Backward-compatibility shim (한시 유지)

OAS 0.193 (이주 PR 머지 시점) 부터 0.194 (shim 제거 시점)까지 OAS lib에 *deprecated re-export* 유지:

`lib/agent_sdk.ml`에 추가:
```ocaml
[@@@deprecated "RFC-OAS-011: CDAL moved to masc_mcp.cdal. Re-export will be removed in v0.194"]
module Cdal_proof = Cdal_proof
module Mode_enforcer = Mode_enforcer
(* ...30+ 모듈 동일 패턴... *)
```

이 shim의 OAS lib 안 *원본 모듈*은 새 빈 stub (compile-only):

`lib/cdal_proof.ml`:
```ocaml
(* DEPRECATED: This module has migrated to masc_mcp.cdal (RFC-OAS-011).
   The implementation here is a compile-only re-export from masc_mcp.cdal.
   Will be removed in agent_sdk v0.194. *)
include Masc_mcp_cdal.Cdal_proof
```

→ **단, 이 패턴은 *역방향 의존*을 만들어 cycle 위험**: OAS lib가 masc_mcp.cdal에 의존하면 masc-mcp ↔ OAS 양방향 cycle. **Forbidden**. 따라서 shim은 *원본 코드를 OAS lib에 한시 유지*하고 masc-mcp에 *원본을 복사*하는 dual-source 방식으로 갈 수밖에 없음.

#### Shim 설계 옵션

| 옵션 | 면적 | 위험 |
|---|---|---|
| **A. Dual-source (원본을 OAS와 masc-mcp 양쪽에 한시 유지)** | OAS lib 변경 0 (이주 PR이 OAS 측에서는 *제거*만 수행). masc-mcp가 자기 카피 도입 | dual-source drift 위험 (한쪽만 수정되는 사고) → 이주 기간을 *짧게* (≤ 7일) 잡고 빠르게 OAS 측 제거 |
| **B. No shim, hard switch** | OAS 0.193: CDAL 30+ 모듈 일괄 제거. masc-mcp 0.20: `masc_mcp.cdal` 신설 + agent_sdk pin 동시 bump | masc-mcp가 build break를 피하려면 *동시 머지* 필요. 머지 순서 race 위험 |
| **C. opam package 분리 (`agent_sdk.cdal_legacy`)** | 별도 opam package로 한시 유지. masc-mcp 0.20+가 `agent_sdk.cdal_legacy`를 명시 의존 | opam package 추가 발급 + 추후 yank — 무거움 |

**권장: B (Hard switch)**. 이유:
- shim은 *drift 위험*을 키움 (메모리 `feedback_telemetry_as_fix_workaround` 정신).
- 원본은 *masc-mcp가 자기 단독 consumer*임이 verified — 머지 순서만 잘 잡으면 hard switch가 깨끗.
- 머지 순서: masc-mcp PR (cdal sublibrary 신설 + agent_sdk pin **현재 0.192 그대로**) → OAS PR (CDAL 제거, agent_sdk 0.193) → masc-mcp PR (agent_sdk pin → 0.193). 즉 *masc-mcp self-contained 시작*을 먼저 만들고, OAS 측 제거를 그 후에 하는 *zero-downtime* 시퀀스.

### 2.4 OAS 측 제거 PR (B5 머지 후)

`lib/agent_sdk.ml`/`mli`의 16 CDAL 모듈 re-export 라인 *전부 삭제*. 대응 모듈 파일 30+개 *전부 삭제*. dune `(libraries ...)`에서 CDAL이 의존하던 항목들 (예: `ppx_deriving_yojson.runtime`은 base에서도 사용하므로 유지) 정리.

OAS 0.193.0 release notes에 *breaking change*: `Agent_sdk.{Cdal_proof, Mode_enforcer, Risk_contract, ...}` 모듈 제거. 이주 가이드: `masc_mcp.cdal`로 이전 또는 자체 governance 구현.

## 3. Cross-Repo Wiring

### 3.1 OAS 측 작업

| PR | 내용 |
|---|---|
| **P-pre** | RFC-OAS-009 v2 PR-A/B/C/D 완료 (전제: core → CDAL 의존 0) |
| **P1** | RFC-OAS-011 docs (this) — `docs/rfc/RFC-OAS-011-cdal-migration-to-masc-mcp.md` |
| **P-final** | OAS lib에서 CDAL 30+ 모듈 + façade re-export 제거. agent_sdk 0.193.0 release |

### 3.2 masc-mcp 측 작업

| PR | 내용 |
|---|---|
| **M1** | `lib/cdal/` 디렉토리 + `lib/cdal/dune` 신설 (sublibrary `masc_mcp.cdal`). 빈 placeholder만 |
| **M2** | OAS의 CDAL 모듈 30+개를 `lib/cdal/` 안에 복사. 각 파일의 `Agent_sdk.{X}` 참조를 `Agent_sdk_base.{X}` 또는 sublibrary 내부 참조로 갱신 |
| **M3** | masc-mcp 본체의 `Agent_sdk.{Cdal_proof, Mode_enforcer, ...}` 호출을 `Masc_mcp_cdal.{X}`로 변환. dune `(libraries)`에 `masc_mcp.cdal` 추가 |
| **M4** | (OAS P-final 머지 후) opam pin bump → `agent_sdk 0.193.0`. dune build clean 검증 |

### 3.3 머지 시퀀스 (zero-downtime)

```
1. OAS P-pre (RFC-OAS-009 v2 PR-A/B/C/D) — 전제
2. masc-mcp M1 + M2 + M3 — masc-mcp가 self-contained CDAL 보유 (agent_sdk pin = 0.192 그대로)
   ↳ 이 시점에 masc-mcp는 OAS의 CDAL과 *자기 카피* 둘 다 컴파일에 보유 (자기 카피만 실제 호출)
3. OAS P-final — OAS lib에서 CDAL 제거. agent_sdk 0.193.0
4. masc-mcp M4 — opam pin → 0.193.0. 자동으로 OAS의 CDAL 부재 확인
```

이 시퀀스는 어느 단계에서도 *production-blocking* 상태가 없음:
- 단계 2 직후: masc-mcp는 자기 카피 + OAS 카피 둘 다 가짐, 자기 카피만 호출 → 동작 영향 0
- 단계 3 직후: OAS의 CDAL 사라짐. masc-mcp는 자기 카피만 사용 → 동작 영향 0
- 단계 4: opam pin bump만 — OAS의 *core 변경*이 자동 반영

## 4. Schema/JSON 호환성

### 4.1 Cdal_proof JSON wire format

본 RFC는 *모듈 path*만 변경. JSON wire format은 불변 (RFC-OAS-012가 capability_snapshot 시그니처를 변경할 때 schema version bump).

masc-mcp의 디스크 manifest (`*.cdal-proof.json` 등): wire 호환 유지.

### 4.2 OPAM package boundary

- `agent_sdk` (OAS) v0.193.0: CDAL 30+ 모듈 *제거* — breaking change.
- `masc_mcp` v0.20.0: `masc_mcp.cdal` sublibrary *추가*.
- 외부 consumer (jeong-sik 본인의 다른 repo, 미래 generic agent SDK consumer): `Agent_sdk.{Cdal_proof, ...}` 호출 → 컴파일 에러. 이주 가이드 release notes에 명시.

검증 (현재 외부 consumer):
- jeong-sik의 다른 repo (kirin, wkbl, ocaml-webrtc, grpc-direct, anthropic-proxy-rs, masc-mcp, oas) 중 OAS의 CDAL 호출하는 곳: **masc-mcp만** (RFC-OAS-009 §1.1.6 verified).
- 외부 사용자: README의 *"개인 프로젝트입니다. 프로덕션 SLA, 외부 지원, 호환성 보증 없음. 사용 시 자기 책임."* 명시 — breaking change 정당화.

## 5. PR Sequence (12 PRs across both repos)

### OAS

| PR | Branch | 내용 | 의존 |
|---|---|---|---|
| **OAS-A** | `feature/rfc-oas-009-v2-sever-cdal-deps` | RFC-OAS-009 v2 + RFC-OAS-011 + RFC-OAS-012 docs (현재 PR) | 독립 |
| **OAS-B** | `feature/rfc-oas-009-pr-b-agent-tools` | agent_tools.ml builtin_descriptor fallback 제거 | OAS-A 머지 후 |
| **OAS-C** | `feature/rfc-oas-009-pr-c-mcp-schema` | mcp_schema.ml descriptor_for_builtin_tool 제거 | OAS-B 머지 후 |
| **OAS-D** | `feature/rfc-oas-009-pr-d-verify-zero-deps` | core → CDAL 0 의존 CI lint | OAS-C 머지 후 |
| **OAS-E** | `feature/rfc-oas-011-pr-e-remove-cdal` | OAS lib에서 CDAL 30+ 모듈 + façade re-export 제거. agent_sdk 0.193.0 | masc-mcp M3 머지 후 |

### masc-mcp

| PR | Branch | 내용 | 의존 |
|---|---|---|---|
| **MM-1** | `feature/rfc-oas-011-cdal-sublibrary-skel` | `lib/cdal/` 디렉토리 + `lib/cdal/dune` skel | OAS-D 머지 후 |
| **MM-2-leaves** | `feature/rfc-oas-011-cdal-batch-1-leaves` | B1 batch 이주 (execution_mode, effect_evidence, guardrail_llm) | MM-1 머지 후 |
| **MM-2-low** | `feature/rfc-oas-011-cdal-batch-2-low` | B2 batch (risk_class, verified_output, conformance) | MM-2-leaves 머지 후 |
| **MM-2-mid** | `feature/rfc-oas-011-cdal-batch-3-mid` | B3 batch (risk_contract, cdal_proof, mode_resolver, cognitive_event) | MM-2-low 머지 후 |
| **MM-2-high** | `feature/rfc-oas-011-cdal-batch-4-high` | B4 batch (proof_capture, proof_store, mode_enforcer, contract_runner, direct_evidence) | MM-2-mid 머지 후 |
| **MM-2-top** | `feature/rfc-oas-011-cdal-batch-5-top` | B5 batch (audit, autonomy_*, sessions_proof + 분류 검토 모듈) | MM-2-high 머지 후 |
| **MM-3-rewire** | `feature/rfc-oas-011-rewire-callers` | masc-mcp 본체의 `Agent_sdk.{Cdal_proof,...}` → `Masc_mcp_cdal.{X}` | MM-2-top 머지 후 |
| **MM-4-pin-bump** | `feature/rfc-oas-011-pin-bump` | opam pin → agent_sdk 0.193.0 | OAS-E 머지 후 |

각 PR Draft + `human-approved-ready` 라벨 게이트.

## 6. Risks (5건)

| # | 위험 | 완화 |
|---|---|---|
| 1 | 분류 검토 미정 모듈 (autonomy_trace_analyzer, guardrail_*, runtime_evidence, effect_evidence)이 core/CDAL 경계 모호 | OAS-D 머지 직후 1차 grep으로 확정. core면 이주 제외, CDAL이면 batch 안 추가 |
| 2 | 30+ 모듈 동시 이주의 회귀 위험 | leaf-first 5-batch 분할. 각 batch마다 dune build clean + dune runtest 통과 강제 |
| 3 | masc-mcp의 자기 governance(`Autonomous_executor.classify_tool`)와 이주된 CDAL 사이 *중복/모순* | RFC-OAS-013+에서 통합 검토. 본 RFC는 *위치 이동*만, 통합/제거는 별도 |
| 4 | opam pin race (OAS-E 머지 직후 masc-mcp 빌드 실패 짧은 윈도우) | MM-4-pin-bump를 OAS-E 머지 *직후* 자동화 또는 즉시 수동 머지 |
| 5 | OAS façade re-export 의존자 (검증되지 않은 외부 사용자) | README의 "사용 시 자기 책임" 약관. 0.193.0 release notes에 이주 가이드 명시 |

## 7. References

- RFC-OAS-009 v2 (this PR): Sever Core→CDAL Dependencies — *전제*
- RFC-OAS-012 (this PR): Tool Name Ignorance within CDAL — *후속*
- README.md (OAS): "OCaml agent SDK on OCaml 5.x + Eio. Talks to Anthropic Messages API and OpenAI-compatible chat endpoints"
- dune-project (OAS): "Anthropic Agent SDK for OCaml (Eio Edition)"
- CLAUDE.md (OAS): "Layer 1: Agent Runtime — 단일 에이전트 실행 엔진"
- `lib/cdal_proof.mli` line 4: "Part of the Contract-Driven Agent Loop (CDAL) PoC-1"
- `lib/contract_runner.ml:96-110` (positive evidence — `Tool.descriptor.mutation_class` 경로)
- 메모리 `feedback_user_rejects_cron_pr_loop` (2026-05-07): Draft + `human-approved-ready` 라벨 게이트
- 메모리 `feedback_split_brain_rfc_0022_pr_2_pr3_overlap` (2026-05-05): same-author parallel agents 동일 axis 동시 PR 회피 — 본 RFC는 *명시적 시퀀스 게이트*로 차단
