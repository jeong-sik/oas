# RFC-OAS-017: Coordinator-Shape Leak in the Public SDK Surface

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-05-12 |
| Target | `agent_sdk` (oas) |
| Supersedes | None |
| Related | `scripts/check-sdk-independence.sh` (the vocabulary gate this RFC says is insufficient on its own) |

## 0. Summary

`scripts/check-sdk-independence.sh` enforces *vocabulary* independence — no `masc`/`keeper`/`board`/`room` in `lib/`/`bin/`/`README.md` — and currently passes. But the SDK still ships a **coordinator-shaped protocol** under non-coordinator vocabulary, via the public `Agent_sdk` facade:

- `lib/runtime.mli` exposes `participant_state`, `worker_id`, `collaboration_channel = Presence_channel | Activity_channel | System_channel`, and a `Waiting_on_workers` phase.
- ~13 `.mli` doc comments in `lib/` name "downstream coordinator" as a first-class consumer; two cross-repo PR/RFC numbers (`#13894`, `RFC-OAS-011`) leak into transport `.mli` files.
- `lib/collaboration.ml` declares bare types `Claim_registry | Turn_queue | Blackboard` and ships **without** an `.mli` — internals are unredacted.

The net effect: an independent OCaml agent application using `Agent_sdk` inherits a coordination contract it didn't ask for. A second, non-MASC coordinator implementation cannot reuse the SDK without conforming to MASC's participant / channel / worker model.

This RFC enumerates the leak, classifies each leak as **structural** (load-bearing for the SDK's design) or **historical** (cruft from when OAS was a MASC subpackage), and proposes a two-track fix: (A) demote historical leaks via wording changes + `.mli` redactions; (B) push the structural coordinator types out of the public facade behind an opt-in `Agent_sdk.Coordinator_plugin` interface that hosts implement to bridge MASC (or anything else) to OAS.

This RFC writes **no code**. Phases B-1..B-3 each become their own implementation PR.

## 1. Verified inventory (`origin/main` at `8d8402f6`)

### 1.1 Structural coordinator types exposed via `Agent_sdk`

`lib/agent_sdk.{ml,mli}` re-exports the runtime module family:

```
agent_sdk.mli:82  module Runtime = Runtime
agent_sdk.mli:83  module Runtime_projection = Runtime_projection
agent_sdk.ml:109  module Runtime = Runtime
agent_sdk.ml:110  module Runtime_projection = Runtime_projection
agent_sdk.ml:111  module Runtime_sync = Runtime_sync
agent_sdk.ml:113  module Runtime_client = Runtime_client
agent_sdk.ml:164  module Runtime_store = Runtime_store
agent_sdk.ml:165  module Runtime_server_types = Runtime_server_types
agent_sdk.ml:166  module Runtime_server_resolve = Runtime_server_resolve
agent_sdk.ml:167  module Runtime_health = Runtime_health
```

`lib/runtime.mli` defines (verified by `sed -n`):

```ocaml
type participant_state =
  | Planned | Starting | Live | Idle | Done
  | Failed_participant | Detached                       (* line 23 *)

type collaboration_channel =
  | Presence_channel | Activity_channel | System_channel (* line 33 *)

(* in a record with channel : collaboration_channel
                       phase : ... | Waiting_on_workers | ...
                       worker_id : string option        (* line 128 *)
                       state : participant_state        (* line 137 *)
*)
```

Any consumer who depends on `agent_sdk` and types `Agent_sdk.Runtime.participant_state` gets the full vocabulary. Renaming the constructors (e.g. `Live → Active`) is a breaking change. Re-purposing the SDK for a coordinator that has a different lifecycle vocabulary requires translating to this set first.

### 1.2 `lib/collaboration.ml` with no `.mli`

```
lib/collaboration.ml:19    | Claim_registry
lib/collaboration.ml:20    | Turn_queue
lib/collaboration.ml:21    | Blackboard
lib/collaboration.ml:25    | Claim_registry -> "claim_registry"
lib/collaboration.ml:26    | Turn_queue -> "turn_queue"
```

`ls lib/collaboration.*` returns only `lib/collaboration.ml` (no `.mli`). Without an interface file, every top-level definition in this 100-line module is implicitly public. Names like `Claim_registry` / `Turn_queue` / `Blackboard` are MASC-pattern vocabulary in everything-but-name.

### 1.3 "Downstream coordinator" docstring sprawl

`rg -l 'downstream coordinator' lib/` returns **13 files** including:

| File:line | Quoted excerpt |
|---|---|
| `lib/sessions.mli:5-6` | `{!Sessions_proof} has migrated to the downstream coordinator` |
| `lib/sessions.ml:6` | `the downstream coordinator (RFC-OAS-011 OAS-E PR-6).` |
| `lib/base/tool_id.mli:10` | `CDAL migrates to the downstream coordinator (RFC-OAS-011).` |
| `lib/vcs_graph_snapshot.mli:4` | `commands, infer downstream coordinator semantics, or carry renderer-specific fields.` |
| `lib/context/budget_strategy.mli:87` | `a single value that downstream coordinators can consume without ...` |
| `lib/llm_provider/transport_claude_code.mli:62` | `(see downstream coordinator #13894 for original RFC-0022 ...` |
| `lib/llm_provider/transport_codex_cli.mli:59` | `(see downstream coordinator #13894 for original RFC-0022 ...` |
| `lib/llm_provider/transport_kimi_cli.mli:58-59` | same `#13894` reference |
| `lib/llm_provider/transport_gemini_cli.mli:61` | same `#13894` reference |
| `lib/llm_provider/constants.ml:15` | `HTTP status codes that downstream coordinators may use ...` |
| `lib/llm_provider/constants.ml:27` | `downstream coordinators.` |
| `lib/llm_provider/request_priority.mli` | (similar) |
| `lib/telemetry_sca_registry.ml:9` | `Consumer-side audit lives in the downstream coordinator.` |

Two of these (`transport_claude_code.mli`, `transport_codex_cli.mli`) cite **cross-repo PR number `#13894`** in their public docstrings — a hard, dated leak. `tool_id.mli` cites `RFC-OAS-011` ("CDAL migrates to the downstream coordinator"), which is an OAS-side RFC that references the migration target.

### 1.4 Independence-gate hardcoded carve-out (closed by sibling PR)

`scripts/check-sdk-independence.sh:103` previously had a dead `grep -v 'lib/telemetry_sca_registry.ml:'` carve-out that suppressed nothing. The sibling PR `chore/sdk-independence-cleanup` (PR #1537) removes it. Noted here so future readers don't re-derive the same "the gate has an exception window" assumption.

## 2. Classification

For each leak, the question is: **does the SDK design require this type/concept to be visible to consumers, or is it cruft from when OAS lived inside `masc-mcp`?**

### 2.1 Structural (load-bearing) — keep, but redocument

| Type | Why structural | Action |
|---|---|---|
| `Runtime` module family (`Runtime`, `Runtime_projection`, `Runtime_sync`, `Runtime_client`, `Runtime_store`, `Runtime_server_*`, `Runtime_health`) | A2A protocol implementation lives here; A2A is a public SDK feature, not a coordinator concern. | Keep public. Doc-clarify: A2A != coordinator. |
| HTTP status codes `Constants.Http.{retryable_codes, cascadable_codes}` | Retry / cascade logic is public SDK behavior. Codes are stable IETF numbers, not coordinator vocabulary. | Reword the comment: drop "downstream coordinators may use" → state the codes' meaning directly. |

### 2.2 Mixed (some legitimate, some historical) — narrow

| Type | Issue | Action |
|---|---|---|
| `lib/runtime.mli` `participant_state`, `collaboration_channel`, `worker_id`, `Waiting_on_workers` | A2A doesn't need "Claim_registry"/"Turn_queue" vocabulary; the participant/channel set was modeled to match MASC's lifecycle. | Move coordinator-specific *constructors* out of the public surface; keep A2A-required ones (`Live`, `Done`, `Failed_participant`) and rename ambiguous ones (`Detached` → `Disconnected`?). Open a follow-up RFC for the exact split. |
| `lib/collaboration.ml` `Claim_registry`, `Turn_queue`, `Blackboard` (no `.mli`) | These are MASC-domain coordinator components. The fact that `.mli` is missing means *everything* in this module is public by accident. | Add `lib/collaboration.mli` redacting all coordinator-specific types; if A2A doesn't need them, hide entirely (`include_subdirs no` from public + drop from `agent_sdk.ml` re-exports). |

### 2.3 Historical (pure cruft) — delete or reword

| Site | Action |
|---|---|
| `sessions.mli:5-6`, `sessions.ml:6` `"{!Sessions_proof} has migrated to the downstream coordinator (RFC-OAS-011 ...)"` | Reword to "Sessions_proof has been removed; see RFC-OAS-011 for the design context." or just delete — the migration is done. |
| `base/tool_id.mli:10` `"CDAL migrates to the downstream coordinator (RFC-OAS-011)."` | Same — past-tense or delete. |
| `transport_claude_code.mli:62`, `transport_codex_cli.mli:59`, `transport_kimi_cli.mli:58-59`, `transport_gemini_cli.mli:61` `"see downstream coordinator #13894 for original RFC-0022 ..."` | Replace cross-repo `#13894` references with a self-contained explanation; cross-repo PR numbers do not belong in a public `.mli`. |
| `vcs_graph_snapshot.mli:4` `"infer downstream coordinator semantics"` | Reword to "infer renderer / consumer semantics" — generic. |
| `context/budget_strategy.mli:87` `"downstream coordinators can consume"` | Reword to "downstream consumers can use" — generic. |
| `telemetry_sca_registry.ml:9` `"Consumer-side audit lives in the downstream coordinator."` | Reword to "Consumer-side audit lives downstream of this registry." |
| `request_priority.mli` similar | Reword. |

## 3. Why "vocabulary gate only" is insufficient

`scripts/check-sdk-independence.sh` is a substring blocklist. It's the right tool for catching the *coarse* leak (someone typing `masc_keeper_blah` in `lib/`), but it cannot catch a **concept** under different names. Renaming `keeper_state` → `participant_state` makes the gate green while leaving the coordination contract unchanged.

That said, the gate is *necessary* — it stops accidental new mentions. The proposal here is to add a **semantic layer** on top of the lexical one: a small "facade audit" script that:

- enumerates the public module surface (`Agent_sdk.*`),
- flags any type whose constructor set matches a coordinator pattern (a TBD heuristic — initially manual, optionally a regex over `.mli`),
- and fails if `lib/collaboration.mli` or `lib/runtime.mli` doesn't carry a `(** PUBLIC: ... *)` rationale annotation for each exposed type.

Concrete shape of the audit and the rationale annotations is in scope for the implementation PR (Phase B-1 below), not for this RFC.

## 4. Proposed plan

### Phase A — historical cruft (low-risk, small PRs)

- **A-1**: reword the 13 docstrings/comments listed in §2.3. Drop cross-repo PR numbers from public `.mli` files. Single PR, no code change. ETA: 1 PR-day.
- **A-2**: add `lib/collaboration.mli` redacting coordinator-specific types (`Claim_registry`, `Turn_queue`, `Blackboard`) — make them private to the implementation or remove them entirely if A2A doesn't need them. ETA: 1 PR-day after grepping callers.

### Phase B — structural narrowing (RFC-sized each)

- **B-1**: facade audit script + rationale annotations on every `Agent_sdk.*` re-export. Pin which modules are "load-bearing public" vs. "historically public, candidate for redaction." Acceptance: `bash scripts/check-facade-rationale.sh` exits 0 and the README documents the rationale categories.
- **B-2**: split `lib/runtime.mli` into "A2A-required public" (kept) and "coordinator-shaped" (moved behind an opt-in interface or removed). Requires deciding what A2A actually needs from the runtime; coordination-only types move to a new module `Agent_sdk.Coordinator_plugin` that hosts implement.
- **B-3**: same exercise for `Runtime_server_*` family — these names suggest A2A server-side implementation; verify whether all 7 modules really need public exposure.

### Out of scope

- Renaming the modules. `Runtime` is fine if its content is genuinely A2A-required after B-2.
- Touching the four already-closed sweep issues (#553/#555/#557/#558) — they're verifiably fixed in code.
- The `mcp_protocol` optional-dep refactor — separate RFC (RFC-OAS-016).

## 5. Acceptance criteria

- Phase A complete: `rg '\bMASC\b|\bdownstream coordinator\b' lib/` returns only `examples/`, `docs/`, `CHANGELOG.md` (the gate-excluded directories). `rg '#13894\|#1[0-9]{4}' lib/llm_provider/transport_*.mli` returns nothing — no cross-repo PR numbers in public docstrings.
- Phase B-1 complete: every `Agent_sdk.*` re-export has a `(** PUBLIC: <rationale> *)` annotation in the `.mli`; the facade-audit script enforces it.
- Phase B-2/3 complete: an independent OCaml agent app can `open Agent_sdk` without seeing coordinator vocabulary (`Claim_registry`, `Turn_queue`, `Blackboard`, etc.). A consumer building a non-MASC coordinator can implement `Agent_sdk.Coordinator_plugin` without inheriting MASC's lifecycle constructors.

## 6. Risk register

| Risk | Mitigation |
|---|---|
| Reading `runtime.mli` types as A2A-required when they're actually coordinator-only (or vice versa) | Phase B-1 explicitly forces the rationale to be written down before B-2 narrows anything. If a type can't be defended in its `(** PUBLIC: ... *)` annotation, it's a candidate for redaction. |
| Phase A wording changes break OCaml doc-search tooling that links to specific `.mli` paragraphs | Unlikely — these are prose changes, not API changes. Keep the section structure of each `.mli` to minimize churn. |
| `lib/collaboration.ml` types are used externally despite no `.mli` | Phase A-2 first runs `rg 'Collaboration\.(Claim_registry\|Turn_queue\|Blackboard)' . --type ml` outside `lib/collaboration.ml` to confirm zero callers; if there are any, scope the redaction to keep them addressable. |

## 7. Non-goals

- Replacing `Agent_sdk.Runtime` with a different name. Names are fine; the question is what's *behind* them.
- A full coordinator-plugin interface design — that goes into B-2's own RFC.
- Restoring the dead `grep -v` carve-out in `check-sdk-independence.sh` (PR #1537 deletes it; this RFC takes that as a baseline).

---

*Phase A is implementation-ready and should be the first PR. B-1 (rationale annotations) should sequence before B-2/B-3 so the narrowing decisions are documented up front.*
