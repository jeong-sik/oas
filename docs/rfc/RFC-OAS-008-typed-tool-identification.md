# RFC-OAS-008: Typed Tool Identification

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude/Kimi analysis) |
| Created | 2026-05-08 |
| Target | `agent_sdk` (oas) v0.193+ / `masc-mcp` v0.20+ consumer |
| Supersedes | None |
| Related | RFC-OAS-004 (code-snippet tool), RFC-OAS-005 (tool-result relocation), RFC-tool-selector |

## 0. Summary

Tool 식별의 단일 진실 공급원이 *문자열*이라는 점을 *닫힌 Variant*로 옮긴다. 변환 범위는 **Phase 1만** — `mode_enforcer.tool_registry`, `agent_tools.find_tool_by_name`, 그리고 양쪽 모두에서 키로 쓰이는 `Tool.schema.name` 비교 지점이다. PPX 자동 생성, 도구 통합(61→35), 스키마 DSL은 별도 RFC(`RFC-OAS-009+`)로 분리한다.

본 RFC의 의도는 *작은 면적, 검증 가능한 회귀 0, 향후 단계의 토대*이다. 사용자 메모리 `feedback_user_rejects_cron_pr_loop.md`(2026-05-07)와 `workaround_rejection_bar`에 따라 cron+자동 PR 패턴을 거부하고 stacked-PR + Draft + `human-approved-ready` 라벨 게이트를 따른다.

## 1. Problem Statement

### 1.1 검증된 사실 (line-pinned)

`lib/mode_enforcer.ml`:
- L85: `let default_tool_entries : (string * tool_effect_class) list = ...` — 50+ 도구 이름이 string 리터럴.
- L150: `let tool_registry : (string, tool_effect_class) Hashtbl.t = ...` — runtime mutable, lowercase string key.
- L193, L331: `match Hashtbl.find_opt tool_registry key with` — 두 곳에서 string lookup, miss 시 fallback이 `External_effect`로 broadly permissive.

`lib/agent/agent_tools.ml`:
- L73: `let find_tool_by_name tools name = List.find_opt (fun t -> t.schema.name = name) tools` — O(n) string equality.
- L84: `match find_tool_by_name tools name with` — 단일 caller, agent turn dispatch 경로.

`lib/llm_provider/types.ml`:
- L38: `type param_type = String | Integer | ...` — 이미 Variant.
- L75: `type tool_param = { name : string; ... }` — 파라미터 이름은 string 유지(타당, 사용자 정의).
- L139: `type tool_schema = { name : string; description : string; parameters : tool_param list }` — *도구* 이름이 string. 이 string이 위 두 파일에서 키로 쓰임.

### 1.2 무엇이 망가지나

1. **Compile-time 검증 부재**: 새 도구 추가 시 `mode_enforcer.default_tool_entries`에 등록을 누락해도 컴파일러가 잡지 못한다. 런타임에 `External_effect` fallback으로 silent permissive 처리.
2. **Hot-path O(n)**: agent turn마다 `find_tool_by_name`이 `List.find_opt`로 선형 탐색. 도구 30+개 환경에서 turn당 평균 ~15 비교.
3. **Drift 유발**: `tool_registry` lowercase 정규화 vs `Tool.schema.name` raw 비교가 분리되어, 같은 이름이 두 경로에서 다르게 매칭될 수 있다 (대소문자/lowercase 정규화 차이).
4. **Workaround 토양**: 다음 PR이 "unknown tool인데 동작은 시켜야겠다"는 욕구로 catch-all `_ -> External_effect`를 *합리적 선례*로 학습한다 — 이는 `workaround_rejection_bar` 시그니처 #1 (Unknown→Permissive Default).

### 1.3 무엇이 *문제 아닌가* (out of scope)

- `tool_param.name` (L75): 파라미터 이름은 user-defined schema, JSON key로 직접 노출되어야 함. 유지.
- `Tool.schema.description`: 자유 서술. 유지.
- `tool_schemas/*.ml`의 hand-written Yojson AST: PPX로 자동화 가능하지만 *별도 RFC*. 본 RFC에서 손대지 않음.
- 도구 통합(`read_file` ↔ `masc_code_read` 등): 의미론 결정 필요, *별도 RFC*.

## 2. Proposal

### 2.1 새 모듈 `lib/base/tool_id.ml`

```ocaml
(** 도구 식별자. 빌트인은 닫힌 Variant, 사용자/외부는 [User of string]. *)
type t =
  (* read-only *)
  | Read | Glob | Grep | Search | List_dir | Read_file
  | Find_symbol | Notebook_read | Read_console_messages
  | Read_network_requests | Get_page_text | Read_page
  | Task_list | Task_get | Task_output
  (* local-mutation *)
  | Write | Edit | Create_text_file | Replace_content
  | Rename_symbol | Notebook_edit
  | Task_create | Task_update | Task_stop
  | Team_create | Team_delete
  (* external-effect *)
  | Ask_user_question | Web_fetch | Web_search
  | Navigate | Computer | Find | Form_input
  | Javascript_tool | Tabs_create_mcp | Upload_image
  (* shell-dynamic *)
  | Bash | Execute_shell_command
  (* MCP / unknown *)
  | Mcp of { server : string; tool : string }
  | User of string  (* user-supplied tool name, registered at runtime *)
[@@deriving show, eq]

val to_string : t -> string
val of_string : string -> t
(** [of_string] never fails — unknown name → [User name] (or [Mcp _] when prefixed [mcp__]). *)

val effect_class : t -> Mode_enforcer.tool_effect_class
(** Total function: every constructor maps to a class at compile time.
    [User _] → [External_effect] (conservative). [Mcp _] → [External_effect]. *)
```

핵심 결정:
- **Total function**: `effect_class : t -> tool_effect_class`. catch-all `_` 사용 금지. 새 빌트인 추가 시 컴파일러가 누락 강제.
- **`User of string` escape hatch**: 외부 사용자가 등록한 도구는 식별자 검증 불가능. 보수적으로 `External_effect`. 기존 `register_tool_class` API는 `User name` 키로 보존(런타임 등록 호환).
- **`Mcp` 분리**: 현재 `mode_enforcer.ml:331` 인근의 `mcp__` prefix 검사를 타입화. prefix substring 매칭 제거.

### 2.2 `mode_enforcer.ml` 변환

```ocaml
(* Before *)
let tool_registry : (string, tool_effect_class) Hashtbl.t = ...
let classify_tool name = Hashtbl.find_opt tool_registry (String.lowercase_ascii name)

(* After *)
let classify_tool_id (id : Tool_id.t) : tool_effect_class = Tool_id.effect_class id
let classify_tool name = classify_tool_id (Tool_id.of_string name)  (* 호환 shim *)
```

`tool_registry` Hashtbl 자체는 **유지**하되 `User of string` 도구만 담는 좁은 역할로 축소. 빌트인은 `Tool_id.t`의 patternematic match가 SSOT.

### 2.3 `agent_tools.find_tool_by_name` 변환

```ocaml
(* Before *)
let find_tool_by_name tools name =
  List.find_opt (fun (t : Tool.t) -> t.schema.name = name) tools

(* After *)
type tool_index = {
  by_id : (Tool_id.t, Tool.t) Hashtbl.t;
  by_user_name : (string, Tool.t) Hashtbl.t;  (* User _ 도구 *)
}

val build_index : Tool.t list -> tool_index  (* O(n), 한 번 *)
val find_in_index : tool_index -> string -> Tool.t option  (* O(1) *)
```

`Tool.t`에 `id : Tool_id.t` 필드를 추가하지 *않는다* — `Tool.schema.name` 그대로 두고, index 구축 시 `Tool_id.of_string name`으로 한 번 변환. 기존 `Tool.create` API 호환.

### 2.4 회귀 0 보장 전략

1. **호환 shim 유지**: `Mode_enforcer.classify_tool : string -> tool_effect_class option`은 그대로 유지. 내부 구현만 `Tool_id` 경유.
2. **테스트 전략**:
   - 신규: `test/test_tool_id.ml` — 모든 `default_tool_entries` 항목이 `Tool_id.of_string |> effect_class`로 동일 결과 산출 (parity test).
   - 신규: `test/test_agent_tools_index.ml` — 100개 도구 list에서 `find_tool_by_name` vs `find_in_index` 결과 동일 + benchmark.
   - 기존: `test/mode_enforcer/*` — 변경 없이 통과.

### 2.5 무엇을 하지 않는가 (Non-Goals)

- ❌ PPX `[@@deriving tool]` 작성 (별도 RFC).
- ❌ `tool_schemas/*.ml` 14모듈 통합 (별도 RFC).
- ❌ JSON Schema 자동 생성 (별도 RFC).
- ❌ GADT 기반 `(_,_,_,_) command` Shell IR (별도 RFC).
- ❌ 도구 통합 61→35 (별도 RFC).
- ❌ Telemetry `Hashtbl keyed by string` 변환 (회귀 위험 큰 변경, 별도 PR).

## 3. Stacked PR Plan

| PR | Branch | Base | 내용 | 수락 조건 |
|---|---|---|---|---|
| **PR-1** | `feature/rfc-oas-008-typed-tool-id` | `main` | 본 RFC 문서만 | reviewer 1+ 합의, `human-approved-ready` 라벨 |
| **PR-2** | `feature/rfc-oas-008-tool-id-module` | PR-1 | `lib/base/tool_id.ml` + `tool_id.mli` 신규, 단독 사용처 0 | 컴파일/테스트 green, parity test 추가 |
| **PR-3** | `feature/rfc-oas-008-mode-enforcer-typed` | PR-2 | `mode_enforcer.ml` 내부 구현 변환, 외부 API 동일 | 기존 mode_enforcer 테스트 변경 없이 통과 + 신규 parity test |
| **PR-4** | `feature/rfc-oas-008-agent-tools-index` | PR-3 | `agent_tools.find_tool_by_name` → `tool_index` | 기존 agent_tools 테스트 통과 + benchmark assertion (≤ 기존의 1.2배) |
| **PR-5** | `feature/rfc-oas-008-cleanup` | PR-4 | shim deprecation comment, doc 업데이트 | 사용처 grep 0 confirmed |

각 PR은:
- **Draft 유지** — agent push only, ready 전환 금지.
- `human-approved-ready` 라벨 대기.
- 머지 순서: PR-1 → PR-2 → PR-3 → PR-4 → PR-5. 충돌 시 rebase, force-push 금지.

## 4. Risks and Mitigations

| Risk | Mitigation |
|---|---|
| 빌트인 enum이 너무 길어져 가독성 저하 | 4개 카테고리 주석 분리. `effect_class` total match는 compiler-enforced이므로 추가 비용 없음. |
| `User of string`이 사실상 escape hatch로 남아 가치 절반 | 메트릭 추가: `User _` 도구 비율을 OTel gauge로 노출. >30% 시 RFC-OAS-009에서 reduce 정책. |
| `Mcp { server; tool }`로 분리한 prefix 검사가 새 사례를 놓침 | `of_string`에서 `mcp__<server>__<tool>` 정규식 + parity test로 기존 `mode_enforcer.ml:331` 분기와 동일 결과 강제. |
| 다른 in-flight 브랜치(`axis3-provider-tool-matrix-oas`, `codex/strict-required-tool-contract`, `feat/missing-tool-use-harness`, `fix/anti-pattern-tool-choice`)와 충돌 | 각 브랜치 author에 사전 commit 알림. PR-2 머지 직전 `git rebase main` 강제. |
| Workaround 시그니처 #1 (Unknown→Permissive) 위반 우려 | `User _` 기본값을 `External_effect`로 명시 + RFC-OAS-008 §2.1에 *의도적 보수 정책*으로 문서화. counter `tool_id_unknown_total` 추가. |

## 5. Open Questions

1. `tool_id.mli`를 `lib/base/`에 두는 것이 맞나, 아니면 `lib/llm_provider/`(현재 `tool_schema` 위치)인가?
   - **잠정 결정**: `lib/base/`. `tool_schema`는 LLM 와이어 포맷, `tool_id`는 도메인 식별자. 의존성 방향: `mode_enforcer` → `tool_id` → `base`(없음).
2. masc-mcp 측 `tool_schemas_*.ml` 14모듈에서 도구 이름 string과 `Tool_id.t`를 어떻게 동기화?
   - **잠정 결정**: PR-3 시점에 masc-mcp에 `Tool_id.of_string` 호출 추가. 별도 follow-up RFC-0042(masc-mcp)로 PPX 자동 생성 검토.
3. `effect_class : User _ -> External_effect` 보수 정책이 사용자 신뢰 도구를 과도하게 격리하지 않나?
   - **잠정 결정**: `register_tool_class` 런타임 API로 override 허용 유지. RFC-OAS-008은 *기본값*만 결정.

## 6. References

- 진단 보고서: `Kimi_Agent_에이전트 컨텍스트 흐름/{repo_analysis,tools_system_improvement_plan,bash_exec_gadt_migration}.md` (2026-05-08).
- 사용자 메모리:
  - `feedback_user_rejects_cron_pr_loop.md` (2026-05-07): cron+자동 PR 거부, single-shot Draft.
  - `feedback_masc_mcp_draft_guard_blocks_agent_ready.md` (2026-05-05): agent ready 전환 금지.
  - `feedback_rfc_section_1_4_caller_context_unverified.md` (2026-05-05): RFC §1 file:line은 caller-context grep 검증 필수 → 본 RFC §1.1에 line-pinned 인용 적용.
  - `software-development.md` 워크어라운드 거부 시그니처 #1 (Unknown→Permissive Default).
- 인접 in-flight 브랜치 (충돌 가능): `axis3-provider-tool-matrix-oas`, `codex/strict-required-tool-contract`, `feat/missing-tool-use-harness`, `fix/anti-pattern-tool-choice`.

## 7. Decision Log

- 2026-05-08: Draft 작성. 사용자 stacked-PR 작전 선택(B).
