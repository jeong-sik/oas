# RFC-OAS-009: Tool Name Ignorance (OAS는 consumer의 tool 이름을 모른다)

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-08 |
| Target | `agent_sdk` (oas) v0.193+ |
| Supersedes | None |
| Related | RFC-OAS-008 (typed tool identification) — 본 RFC가 그 한계를 보강 |

## 0. Summary

OAS는 일반 Agent SDK다. 어떤 consumer (masc-mcp / Claude Code agent harness / Serena MCP / claude-in-chrome MCP / computer-use)의 *tool 이름*도 OAS lib 코드에 박혀있어선 안 된다. Tool effect class는 *consumer가 자기 `Tool.descriptor.mutation_class`에 박고* OAS는 *런타임에 읽기만* 한다 (Inversion of Control).

본 RFC는 RFC-OAS-008(typed tool identification)이 *식별의 정확성*은 풀었으나 *layering 위반*은 풀지 않은 점을 보강한다. 적용 범위는 **OAS 내부의 4건 위반 제거**: `mode_enforcer.default_tool_entries`(46개 외부 builtin string 하드코딩), `mode_enforcer.classify_tool` 글로벌 함수(호출처 0건의 죽은 API), RFC-OAS-008 PR-2의 `Tool_id` builtin variants, `mcp_schema.ml`의 builtin 이름 inline test.

cross-repo 영향 0 — masc-mcp는 자체 `classify_tool` 보유 + OAS의 violation 타입/serialization만 import (검증: `lib/violation_record.ml`, `autonomous/autonomous_executor.ml:19`).

## 1. Problem Statement

### 1.0 Retroactive moment

RFC-OAS-008 PR-2 (#1475) 머지 직후, 사용자가 `Tool_id` Variant에 `Team_create | Team_delete`가 들어있는 것을 보고 다음 질문을 제기:

> "야 Team 기능은 없는데 왜 한거야?"

이 질문이 RFC-OAS-008의 한계를 노출한다. PR-2는 string→Variant 매핑의 *충실한 미러링*을 했을 뿐, *애초에 OAS가 이 46개 이름을 알아야 하는가*에는 답하지 않았다. Team_create/Team_delete는 Claude Code agent harness의 tool 이름이고, OAS lib는 Claude Code의 존재를 몰라야 한다.

### 1.1 검증된 사실 (line-pinned, origin/main 3c67d1e5 기준)

`lib/mode_enforcer.ml`:
- L85+ `default_tool_entries : (string * tool_effect_class) list` — 46개 외부 builtin 이름 하드코딩.
  - Serena MCP (11): `find_symbol`, `find_file`, `get_symbols_overview`, `find_referencing_symbols`, `search_for_pattern`, `create_text_file`, `replace_content`, `rename_symbol`, `insert_after_symbol`, `insert_before_symbol`, `replace_symbol_body`
  - Claude Code core (13): `read`, `glob`, `grep`, `search`, `list_dir`, `read_file`, `write`, `edit`, `bash`, `ask_user_question`, `web_fetch`, `web_search`, `notebook_read`
  - Claude Code Task/Team (8): `task_list`, `task_get`, `task_output`, `task_create`, `task_update`, `task_stop`, **`team_create`**, **`team_delete`**
  - claude-in-chrome MCP (12): `read_console_messages`, `read_network_requests`, `get_page_text`, `read_page`, `tabs_context_mcp`, `navigate`, `computer`, `find`, `form_input`, `javascript_tool`, `tabs_create_mcp`, `upload_image`
  - 기타 (2): `notebook_edit`, `execute_shell_command`
- `default_tool_entries` 외부 의존: **0건** (검증: `rg -n "default_tool_entries" .` returned 0 matches outside `lib/mode_enforcer.ml`).

`lib/mode_enforcer.mli`:
- L84 `val classify_tool : string -> tool_effect_class` — 글로벌 함수. 호출처 검증:
  - OAS 내부 호출: 0건
  - masc-mcp 호출: 0건 (masc-mcp는 자체 `Autonomous_executor.classify_tool` 보유)
  - 즉 **dead public API**.

`lib/base/tool_id.ml` (RFC-OAS-008 PR-2, `8f413f8a`):
- 46개 builtin Variant 케이스가 `default_tool_entries`를 그대로 미러. 외부 의존 0건이므로 비파괴적 제거 가능.

`lib/protocol/mcp_schema.ml`:
- L192-193 `let%test "descriptor_for_builtin_tool task_create is mutation"` — inline test가 builtin 이름을 lib 내부에 박는다. layering 위반의 *마지막 흔적*.

### 1.2 OAS가 *이미* 갖고 있는 깨끗한 경로 (positive evidence)

`lib/contract_runner.ml:96-110`:
```ocaml
let tool_classifications =
  Agent.tools agent
  |> Tool_set.to_list
  |> List.filter_map (fun (t : Tool.t) ->
    match t.descriptor with
    | Some d ->
      Option.bind d.Tool.mutation_class Mode_enforcer.mutation_class_of_string
      |> Option.map (fun cls -> t.schema.name, cls)
    | None -> None)
in
let enforcer_state =
  Mode_enforcer.create
    ~contract
    ~effective_mode:mode_decision.effective_mode
    ~tool_classifications
    ()
```

이 경로가 *옳은 패턴*이다. 각 Tool이 자기 `descriptor.mutation_class`를 가지고 다니고, runtime이 그것만 읽어 분류 리스트를 빌드한다. consumer가 자기 도구를 등록할 때 effect class를 함께 박는다 — OAS는 string도 Variant도 *모른다*.

`Mode_enforcer.create`의 `?tool_classifications` 옵셔널 파라미터는 이미 *Inversion-of-Control 진입점*. RFC-OAS-009는 사실상 "기본값(default_tool_entries)을 비우고 깨끗한 경로만 살린다"는 작은 작업이다.

### 1.3 무엇이 망가지나

1. **Layering 위반**: OAS는 Claude Code/Serena/claude-in-chrome/Team 개념을 *몰라야* 하는 라이브러리인데 lib 코드에 그 이름들이 박혀 있음. CLAUDE.md의 *AI 코드 생성 안티패턴 #3 (Boundary Violation)* 정확한 위반.
2. **Dead public API**: `classify_tool : string -> tool_effect_class`는 호출처가 없는데도 `.mli`에 노출되어 *외부 약속처럼 보임*. 향후 consumer가 잘못된 IoC 진입점으로 삼을 수 있음.
3. **Workaround 토양**: 46개 builtin이 lib 내부에 있으면, 새 도구 추가 시 *자연스럽게* 47번째를 박는 PR이 등장한다. 이는 `workaround_rejection_bar` 시그니처 #2 (String 분류기 보강)의 정확한 양분.
4. **이중 분류 경로**: `default_tool_entries`(글로벌 하드코딩)와 `Tool.descriptor.mutation_class`(consumer-supplied) 두 경로가 공존. 같은 도구 이름이 두 경로에서 다르게 분류될 위험.

### 1.4 무엇이 *문제 아닌가* (out of scope)

- **`?tool_classifications` 옵셔널 → required 전환**: 외부 consumer 깰 수 있어 별도 RFC. RFC-OAS-009는 *기본값을 비우는 것*까지만.
- **`Tool.descriptor.mutation_class` 자체의 Variant화**: 현재 `string option` 타입. 별도 RFC.
- **PPX 자동 생성**: RFC-OAS-008 §1.3의 out-of-scope 그대로 유지.
- **masc-mcp의 자체 `classify_tool` 통합**: cross-repo 의사결정. 별도 RFC.

## 2. Proposal

### 2.1 Tool effect class는 *Tool과 함께 다닌다*

**원칙**: OAS lib 코드는 어떤 consumer의 tool 이름도 알지 못한다. 분류는 다음 경로로만 흐른다:

```
consumer (masc-mcp / Claude Code / Serena / ...)
   |
   | (1) Tool.create ~name ~descriptor:{ mutation_class = "Read_only"; ... }
   v
Agent.tools (consumer-supplied tool set)
   |
   | (2) Mode_enforcer.create ~tool_classifications:[<from Tool.descriptor>]
   v
Mode_enforcer.state (no global, no hardcoded names)
```

OAS lib는 (2)에서 `tool_classifications`로 들어온 분류만 사용한다. (1)에서 미지정한 도구는 fallback `External_effect` (fail-closed) — 이미 구현됨, 변경 없음.

### 2.2 4건 위반 제거 매트릭스

| 위치 | 변경 |
|---|---|
| `mode_enforcer.ml` `default_tool_entries` | `[]` 빈 리스트로. 46개 외부 이름 제거 |
| `mode_enforcer.ml` `tool_registry` 초기 시드 | `default_tool_entries` 의존 제거. 빈 Hashtbl로 시작 |
| `mode_enforcer.mli` `val classify_tool` | `[@@deprecated]` 마크 → 다음 minor에서 완전 제거 |
| `mode_enforcer.ml` `classify_tool` 구현 | `tool_registry` lookup 유지하되 Hashtbl이 비어있으므로 항상 fallback |
| `tool_id.ml` (RFC-OAS-008 PR-2 main 잔존) | `type t = Mcp of {server; tool} \| User of string`만 남김. 46 builtin 제거 |
| `mcp_schema.ml:192-193` builtin inline test | 제거. consumer-side로 이동 (해당 inline test 의존자 grep 후) |

### 2.3 Backward compatibility

- `Mode_enforcer.create` 시그니처 **변경 없음** (`?tool_classifications` 옵셔널 그대로).
- `Mode_enforcer.classify_tool` 시그니처 변경 없음 (deprecated mark만, 구현은 살아있음). 다음 minor에서 제거.
- `Tool_id.t` 타입 시그니처 변경 (PR-2 머지 직후라 외부 호출처 0건. 검증: `rg -n "Tool_id\." lib/ test/` returned 0).
- masc-mcp 영향 0 (자체 `classify_tool` + OAS violation type/serialization만 import).

## 3. PR 시리즈 (5단계)

| PR | 내용 | 단일 파일? | 위험 |
|---|---|---|---|
| **A** (본 PR) | `docs/rfc/RFC-OAS-009-tool-name-ignorance.md` 추가 (구현 0줄) | yes | 0 |
| **B** | `mode_enforcer.ml` `default_tool_entries → []`. parity test 제거. `classify_tool` 글로벌 함수에 `[@@deprecated "RFC-OAS-009: use Mode_enforcer.create ~tool_classifications"]` | yes | 낮음 (callers 0) |
| **C** | `tool_id.ml` `type t = Mcp of {server;tool} \| User of string`만. PR-B 이후 builtin variants가 unused identifier로 잔존하던 것을 제거 | yes | 중간 (PR-2 후속) |
| **D** | `mcp_schema.ml` builtin inline test 정리. 다른 builtin 이름 inline test 전수 grep 후 제거 | 1-2 파일 | 낮음 |
| **E** | `Mode_enforcer.classify_tool` 글로벌 함수 *완전 제거* (`.mli`에서 삭제). PR-3 (#1476) close. RFC-OAS-009 마무리 | 단일 파일 | 낮음 |

각 PR은 Draft + `human-approved-ready` 라벨 게이트. cron+자동 PR 패턴 거부 (메모리 `feedback_user_rejects_cron_pr_loop.md` 준수).

## 4. Migration

### 4.1 PR-3 (#1476) 처리

PR-3 (`feature/rfc-oas-008-mode-enforcer-typed`)은 `default_tool_entries → Tool_id` 매핑이다. 본 RFC가 `default_tool_entries`를 비우므로 매핑 대상이 사라짐. PR-A 머지 후 close. close 코멘트에서 본 RFC를 reference로 인용.

### 4.2 RFC-OAS-008과의 관계

RFC-OAS-008은 *closed*되지 않는다. PR-2 (Tool_id 모듈 자체)는 `Mcp/User` 형태로 살아남는다 — 외부 tool 식별을 *typed Mcp* 형태로 잡는 가치는 유지. 다만 **builtin variants는 RFC-OAS-008의 잘못된 mirror**였음을 본 RFC §1.0에 명시.

## 5. Risks (3건)

| # | 위험 | 완화 |
|---|---|---|
| 1 | PR-2 main 잔존 → builtin variants가 unused identifier로 PR-C 머지까지 잠시 남음 | OCaml `unused-constructor` warning만 발생, 동작 영향 0. PR-B 본문에 명시 |
| 2 | `mcp_schema.ml` inline test가 다른 OAS lint/CI에 묶여있을 수 있음 | PR-D 작성 시 `rg -n "descriptor_for_builtin_tool" lib/ test/` 1회 grep 의무 |
| 3 | `?tool_classifications`을 미공급한 외부 consumer가 빈 분류로 fail-closed에 걸림 | 의도된 동작. consumer가 자기 도구 분류를 register하지 않으면 `External_effect` fallback (fail-closed가 OAS의 기본 안전 정책) |

## 6. References

- RFC-OAS-008: Typed Tool Identification (Phase 1)
- CLAUDE.md `AI 코드 생성 안티패턴` §3 Boundary Violation
- CLAUDE.md `워크어라운드 거부 기준` 시그니처 #2 String 분류기 보강
- 메모리 `feedback_user_rejects_cron_pr_loop` (2026-05-07): 본 RFC도 stacked Draft PR + 라벨 게이트 준수
- 메모리 `feedback_rfc_section_1_4_caller_context_unverified` (2026-05-05): 본 RFC §1.1의 line-pinned 검증은 본 메모리 룰 적용
- `lib/contract_runner.ml:96-110` (positive evidence — 깨끗한 경로)
