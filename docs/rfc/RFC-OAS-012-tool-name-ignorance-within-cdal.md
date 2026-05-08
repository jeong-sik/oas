# RFC-OAS-012: Tool Name Ignorance within CDAL (post-migration)

| | |
|---|---|
| Status | Draft (deferred — depends on RFC-OAS-011 completion) |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-08 |
| Target | `masc_mcp.cdal` v0.20+ (이주 후) |
| Supersedes | RFC-OAS-009 v1 의도 (default_tool_entries 정리) — 이주 후 흡수 |
| Depends-on | RFC-OAS-011 (CDAL Migration to masc-mcp) — *완료 후* 본 RFC 진행 |

## 0. Summary

CDAL이 `masc_mcp.cdal` sublibrary로 이주한 후, *CDAL 내부의 layering 위반*을 정리한다.

- `Mode_enforcer.default_tool_entries`(46개 외부 builtin 이름 하드코딩) → 빈 리스트
- `Mode_enforcer.classify_tool`(글로벌, 호출처 0건의 dead API) → 제거
- `Mode_enforcer.builtin_descriptor`(글로벌, RFC-OAS-009 v2 PR-B/C 후 호출처 0건) → 제거
- `Cdal_proof.capability_snapshot.tools : string list` → `(string * tool_effect_class) list` 시그니처 변경 + JSON schema version bump

이 작업은 RFC-OAS-009 v1이 *원래* 다루려 했던 내용. 그러나 CDAL이 OAS lib에 거주할 때는 *cross-repo 영향*과 *layering boundary 미정리* 때문에 깨끗하게 끝낼 수 없었다. RFC-OAS-011 완료 후, CDAL은 *자기 유일한 consumer 안*에 거주하므로 본 RFC가 *masc-mcp 내부 작업*으로 줄어든다.

## 1. Problem Statement (post-migration)

### 1.0 RFC-OAS-009 v1의 잔여 의도

v1은 다음 4건의 layering 위반을 식별했다 (현재는 RFC-OAS-011 완료 후 모두 `masc_mcp.cdal/` 안):

1. `Mode_enforcer.default_tool_entries` — 46개 외부 builtin 이름 하드코딩 (Claude Code/Serena/claude-in-chrome/Team)
2. `Mode_enforcer.classify_tool : string -> _` — 글로벌 함수, 호출처 0건
3. (이전 위반) `Tool_id`의 builtin variants — RFC-OAS-008 PR-2의 mirror. RFC-OAS-009 v1 제안에서 trim 대상이었으나 *이주 후*에도 잔존
4. `mcp_schema.ml`의 builtin 이름 inline test — RFC-OAS-009 v2 PR-C에서 이미 제거

### 1.1 추가 위반 (RFC-OAS-009 v2 §1에서 발견되지 않은 것)

`Cdal_proof.capability_snapshot.tools : string list` — 이름만 갖고 있고 *분류 정보 없음*. 그래서 `mode_resolver.capability_cap`이 `Mode_enforcer.all_read_only`/`all_workspace_only` 글로벌 lookup에 의존. 즉 capability_snapshot 자체가 *분류 정보를 직접 들고 다니지 않는* 구조 — 이게 글로벌 분류기의 *진짜 root cause*.

올바른 구조: `capability_snapshot.tools : (string * tool_effect_class) list`. 그러면 `mode_resolver`가 lookup 없이 직접 분류.

### 1.2 OAS 측 영향 (이주 후)

- 본 RFC가 *masc-mcp 내부 작업*이 된 이유: RFC-OAS-011 완료 후 CDAL이 OAS lib에 없음.
- OAS 측 변경: 0건.
- Cdal_proof JSON schema 변경 → masc-mcp manifest의 *디스크 호환성*만 영향.

### 1.3 무엇이 *문제 아닌가*

- masc-mcp의 자체 `classify_tool` (`autonomous/autonomous_executor.ml:19`)와의 통합: RFC-OAS-013+.
- `Tool.descriptor.mutation_class` 자체의 Variant화 (현재 `string option`): 별도 RFC.
- PPX 자동 생성: RFC-OAS-008 §1.3과 일관 — out of scope.

## 2. Proposal

### 2.1 `Mode_enforcer.default_tool_entries` → `[]`

`masc_mcp/lib/cdal/mode_enforcer.ml:85+` (이주 후 위치):

```ocaml
let default_tool_entries : (string * tool_effect_class) list = []
```

### 2.2 `Mode_enforcer.classify_tool` 제거

`masc_mcp/lib/cdal/mode_enforcer.mli`에서 `val classify_tool` 라인 제거.
`mode_enforcer.ml`의 구현 제거. `all_read_only`/`all_workspace_only`도 같이 제거 (호출처 = `mode_resolver.capability_cap`만, §2.3에서 시그니처 변경).

### 2.3 `Cdal_proof.capability_snapshot.tools` 시그니처 변경

#### Before

```ocaml
type capability_snapshot =
  { tools : string list
  ; mcp_servers : string list
  ; max_turns : int
  ; max_tokens : int option
  ; thinking_enabled : bool option
  }
[@@deriving yojson, show]
```

#### After

```ocaml
type capability_snapshot =
  { tools : (string * Mode_enforcer.tool_effect_class) list
  ; mcp_servers : string list
  ; max_turns : int
  ; max_tokens : int option
  ; thinking_enabled : bool option
  }
[@@deriving yojson, show]

(* Backward-compat helper for legacy manifests *)
val capability_snapshot_v1_to_v2 :
  string list -> (string * Mode_enforcer.tool_effect_class) list
```

#### JSON schema 변화

Before:
```json
{ "tools": ["read", "write", "bash"] }
```

After:
```json
{ "tools": [
    ["read", "Read_only"],
    ["write", "Local_mutation"],
    ["bash", "Shell_dynamic"]
  ]
}
```

`Cdal_proof.schema_version_current` bump (예: 1 → 2).

### 2.4 `mode_resolver.capability_cap` 직접 분류

#### Before

```ocaml
let capability_cap (capabilities : Cdal_proof.capability_snapshot) =
  if Mode_enforcer.all_read_only capabilities.tools
  then Execution_mode.Diagnose
  else if Mode_enforcer.all_workspace_only capabilities.tools
  then Execution_mode.Draft
  else Execution_mode.Execute
;;
```

#### After

```ocaml
let capability_cap (capabilities : Cdal_proof.capability_snapshot) =
  let all_in cls =
    List.for_all (fun (_, c) -> Mode_enforcer.tool_effect_class_le c cls) capabilities.tools
  in
  if all_in Read_only
  then Execution_mode.Diagnose
  else if all_in Local_mutation
  then Execution_mode.Draft
  else Execution_mode.Execute
;;
```

### 2.5 `Mode_enforcer.builtin_descriptor` 제거

RFC-OAS-009 v2 PR-C 머지 후 호출처 0건. `mode_enforcer.mli`에서 `val builtin_descriptor` 제거. ml에서 구현 제거. `default_tool_entries` Hashtbl seed 로직도 함께 제거 (이미 빈 리스트).

### 2.6 `Tool_id` (RFC-OAS-008 PR-2 main 잔존) trim

`masc_mcp/lib/cdal/tool_id.ml` (이주 후 위치):

```ocaml
type t =
  | Mcp of { server : string; tool : string }
  | User of string
[@@deriving show, eq]

val to_string : t -> string
val of_string : string -> t
```

46개 builtin variant 제거. `of_string`은 `mcp__` prefix 시 `Mcp _`, 그 외 `User _`로 단순화.

## 3. Manifest Migration

### 3.1 디스크 manifest

- masc-mcp의 production manifest (`*.cdal-proof.json`)는 *schema_version=1*이 자동 detect되어 `capability_snapshot_v1_to_v2`로 자동 변환 후 read.
- write-time은 항상 v2 schema. 즉 *이주 후 첫 write*부터 새 schema 사용.
- 30일 후 (또는 다음 minor): v1 read 지원 제거.

### 3.2 검증

PR-D에서 mock manifest fixtures (v1 + v2)로 alcotest. `capability_snapshot_v1_to_v2`의 default classification은 *fail-closed* (`External_effect`).

## 4. PR Sequence

| PR | Branch (masc-mcp) | 내용 |
|---|---|---|
| **A** (this) | OAS의 `feature/rfc-oas-009-v2-sever-cdal-deps` | RFC-OAS-012 docs only (RFC-OAS-009 v2 + RFC-OAS-011와 같은 PR) |
| **B** | `feature/rfc-oas-012-empty-default-tool-entries` | `Mode_enforcer.default_tool_entries → []`. `classify_tool`/`all_read_only`/`all_workspace_only` deprecated mark |
| **C** | `feature/rfc-oas-012-builtin-descriptor-removal` | `builtin_descriptor` + `default_tool_entries` Hashtbl seed 제거 |
| **D** | `feature/rfc-oas-012-capability-snapshot-typed` | `capability_snapshot.tools` 시그니처 변경 + schema_version bump + v1→v2 helper |
| **E** | `feature/rfc-oas-012-mode-resolver-direct` | `mode_resolver.capability_cap` 직접 분류 |
| **F** | `feature/rfc-oas-012-tool-id-trim` | `Tool_id` `Mcp \| User`만 |
| **G** | `feature/rfc-oas-012-final-cleanup` | `classify_tool`/`all_read_only`/`all_workspace_only` 완전 제거 |

각 PR Draft + `human-approved-ready` 라벨 게이트.

## 5. Risks (3건)

| # | 위험 | 완화 |
|---|---|---|
| 1 | masc-mcp의 *production* manifest (디스크에 있는 `*.cdal-proof.json`) 호환성 | `capability_snapshot_v1_to_v2` helper로 자동 변환. 30일 grace |
| 2 | `Mode_enforcer.tool_effect_class_le` partial order가 모든 케이스 cover하는지 | Read_only < Local_mutation < {External_effect, Shell_dynamic}. inline test로 exhaustive |
| 3 | `mode_resolver.capability_cap`의 시멘틱이 *기본 비어있는 capability*를 어떻게 처리 | 빈 capability tools → `all_in Read_only`가 trivially true → `Diagnose`. 가장 보수적. 안전 |

## 6. References

- RFC-OAS-009 v1 (merged `7149c5a7`): 본 RFC가 *원의도*를 흡수
- RFC-OAS-009 v2 (this PR): Sever Core→CDAL Dependencies — *전제*
- RFC-OAS-011 (this PR): CDAL Migration to masc-mcp — *전제*
- `lib/cdal_proof.mli` line 30: `capability_snapshot.tools : string list` (현재)
- `lib/mode_resolver.ml:7-15` `capability_cap` (현재)
- 메모리 `feedback_user_rejects_cron_pr_loop` (2026-05-07): Draft + `human-approved-ready` 라벨 게이트
- 메모리 `feedback_telemetry_as_fix_workaround` (2026-05-08): 본 RFC는 *symptom 정리*가 아니라 *root cause* (구조적 의존 정리)
