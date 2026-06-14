# RFC-OAS-027: Stop-Hook Loop Re-Entry

| Field | Value |
|---|---|
| Status | Draft |
| Author | vincent (drafted by agent) |
| Created | 2026-06-14 |
| Target | `agent_sdk` (oas) |
| Related | RFC-OAS-025 (forced-tool-use enforcement boundary, mirror), RFC-OAS-017 (coordinator-shape leak), RFC-OAS-019 (stream-lifecycle aggregation), `docs/design/runtime-continuation-boundaries.md` (#2080) |

## 0. Summary

When the model ends a turn declaratively — stop reason
`EndTurn | MaxTokens | StopSequence | Refusal | PauseTurn | Compaction |
ContextWindowExceeded` — the turn pipeline invokes the `on_stop` hook but
**discards its decision**:

```ocaml
let _stop =
  invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_stop"
    agent.options.hooks.on_stop
    (Hooks.OnStop { reason = response.stop_reason; response })
in
Ok (Complete response)
```

(`lib/pipeline/pipeline.ml:534-542`, origin/main `3b1a994d`)

The hook runs and is traced, but whatever it returns the turn always completes.
A host therefore cannot reject a declared stop and re-enter the agent loop
in-band. The only re-entry path that exists — `Nudge` on the `on_idle` branch —
fires on the `StopToolUse` arm, not the declarative-stop arm.

This RFC proposes wiring the discarded `on_stop` decision so a host hook can
re-enter the loop with feedback, reusing the existing `Nudge of string` decision
and the existing `ToolsExecuted` re-entry outcome. The completion **predicate**
(what counts as "not done") stays in the host. OAS provides only the typed
mechanism. This is the mirror of RFC-OAS-025: there OAS removed a coordinator
policy it should not enforce; here OAS exposes a mechanism so the coordinator can
enforce its own policy in-band.

## 1. Current state (file:line, origin/main `3b1a994d`)

The agent turn is a 3-type outcome consumed by the run loop:

```ocaml
type turn_outcome =
  | Complete of Types.api_response   (* terminates the loop *)
  | ToolsExecuted                    (* re-enters the loop *)
  | IdleSkipped
```

(`lib/pipeline/pipeline.ml:57-60`)

The run loop maps these and iterates on `ToolsExecuted`, terminates on
`Complete`:

```ocaml
| Ok (`Complete response) -> (* log, return response *)
| Ok `ToolsExecuted -> (* yield, *) loop ~is_first_turn:false
```

(`lib/agent/agent.ml:229-248`; the mapping is `lib/agent/agent.ml:57-58`). The
loop is bounded on every iteration by `check_loop_guard` — `max_turns`, idle
detection, and token budget (`lib/agent/agent.ml:199`).

Hook decisions are a single shared sum with a typed, fail-closed per-stage
legality registry:

```ocaml
type hook_decision =
  | Continue | Skip | Override of string | ApprovalRequired
  | AdjustParams of turn_params | ElicitInput of elicitation_request
  | Nudge of string
```

(`lib/base/hooks.mli:152-164`). `Nudge of string` is documented as "inject
message as User-role into the conversation, continue execution"
(`lib/base/hooks.mli:164`). The legality registry is exhaustive over stages and
fail-closed:

```ocaml
| "on_stop" -> [ K_Continue ]   (* lib/base/hooks.ml:375 *)
| "on_idle" -> [ K_Continue; K_Skip; K_Nudge ]   (* lib/base/hooks.ml:376 *)
(* "Fail-closed: any decision not explicitly listed is rejected." :366 *)
```

The `on_idle` `Nudge` handler stashes the message and re-enters via
`ToolsExecuted` (`lib/pipeline/pipeline.ml:373-380`); the stashed text is then
appended as a user-role message alongside the tool results
(`lib/pipeline/pipeline.ml:430-452`, `make_message ~role:User [ Text text ]` at
`:452`).

## 2. Problem: the declared-stop branch is observation-only

1. **The decision is computed then dropped.** `on_stop` returns a
   `hook_decision`, the call is traced, and `let _stop` discards it
   (`pipeline.ml:534`). The hook is effectively observation-only despite the
   non-`unit` return type — a silent no-op for any decision other than the
   implicit "complete".

2. **No re-entry decision is even expressible.**
   `legal_decisions_for_stage "on_stop" = [K_Continue]` (`hooks.ml:375`). Even if
   the discard were removed, the fail-closed registry permits only `Continue`, so
   there is no legal way to ask the loop to continue from `on_stop`.

3. **Asymmetry between the two stop branches.** The `StopToolUse` arm honors
   `on_idle` `Nudge` (re-enter) and `Skip` (stop). The declarative-stop arm
   honors neither. A model that ends a turn with a text-only "done" (`EndTurn`)
   cannot be told "not done" in-band; the host must start a fresh run, losing
   turn-count, conversation, and loop-guard continuity.

The motivating consumer is a host completion-gate — for example MASC keepers that
declare done without the evidence their coordinator requires (MASC #21074, 14
fake-done cases). OAS does **not** encode that predicate (see §7); it is cited
only to show the gap has a real consumer.

## 3. Non-goals

- **Defining "complete".** What counts as done is host policy. RFC-OAS-025
  deliberately removed completion-contract shape from OAS; this RFC does not
  re-introduce it.
- **An on_stop-specific re-entry cap or cooldown.** `check_loop_guard`
  (`max_turns` / idle / token budget, `agent.ml:199`) already bounds re-entry on
  every iteration. A separate cap would be redundant symptom-suppression.
- **Changing `StopToolUse` / `on_idle` semantics.**
- **`Block`-as-hard-fail** (terminate the run with an error). That is a distinct
  concern from re-entry and is left to a follow-up if a host needs it.

## 4. Options

### Option A — Reuse `Nudge`; wire the discard; extend legality (recommended)

- `lib/base/hooks.ml:375`: `"on_stop" -> [ K_Continue; K_Nudge ]`.
- `lib/pipeline/pipeline.ml:534-542`: replace the `let _stop … in Ok (Complete
  response)` discard with an exhaustive match on the decision:
  - `Continue` (and, fail-closed, any decision the legality check rejects) →
    `Ok (Complete response)` (stop — current behavior).
  - `Nudge msg` → append `msg` as a user-role message to the conversation and
    return `Ok ToolsExecuted` (re-enter; bounded by `check_loop_guard`).

  The match must **enumerate** the `hook_decision` variants rather than use a bare
  `_ -> Ok (Complete response)` catch-all, so a future decision variant intended
  to re-enter is caught by the compiler instead of silently completing (OAS
  exhaustive-match convention; the project's AI-codegen anti-pattern #4 forbids
  `_ ->` catch-alls in this kind of dispatch). The fail-closed legality registry
  still rejects illegal decisions; the enumerated match makes the
  stop-vs-re-enter routing explicit per variant.

- Pro: no new public variant; the shared `hook_decision` union and every other
  consumer's exhaustive match are unchanged. The fail-closed legality registry is
  the safety net. The re-entry outcome (`ToolsExecuted`) and its bound already
  exist. Reuses the existing `Nudge` "inject user-role message, continue"
  meaning.
- Con: `Nudge` becomes legal for both `on_idle` and `on_stop`; the trigger
  context (idle vs declared-stop) is disambiguated only by which hook fired, not
  by the decision variant. The on_stop injection is **not** a pure copy of the
  idle path: the idle path appends its message alongside the tool results
  (`pipeline.ml:430-452`), but the declarative-stop arm has no tool-results
  message, so it must append a standalone user message.

### Option B — Add dedicated `Continue_with` / `Block` variants

Add `Continue_with of string` (re-enter with feedback) to `hook_decision`,
`hook_decision_kind`, `classify_decision`, `decision_kind_to_string`, and the
`on_stop` legality list.

- Pro: explicit naming at the stop boundary — `Continue_with` reads more clearly
  than `Nudge` for "reject the stop."
- Con: widens the shared `hook_decision` union; every exhaustive match across all
  hook consumers grows to cover variants only `on_stop` uses; larger public API
  surface. `Block` collapses to the same loop behavior as `Continue_with`
  (re-enter with text) unless it carries hard-fail semantics, which is a separate
  concern (§3).

### Option C — Dedicated `stop_decision` type (parse, don't validate)

Give `on_stop` a distinct result type, e.g. `stop_decision = Allow_stop |
Reject_stop_with of string`, instead of the shared `hook_decision`. Only the two
meaningful outcomes are representable; the `on_stop` row of the legality table
becomes unnecessary.

- Pro: strongest typing — illegal `on_stop` decisions are unrepresentable; no
  fail-closed table needed for this hook.
- Con: breaks the uniform `hook : hook_event -> hook_decision` signature
  (`hooks.mli:181`) and the `compose_hook` composition that assumes one decision
  type (definition `hooks.ml:475`, applied to the `on_stop` field at `:496`).
  Largest refactor and API churn for one hook.

## 5. Recommendation

**Option A.** It is minimal, reuses the existing mechanism faithfully, keeps the
fail-closed legality registry as the guard, and the re-entry outcome and its
bound already exist. The naming cost (`Nudge` at `on_stop`) is documented in the
decision's doc comment and disambiguated by hook identity. If explicit
stop-boundary naming later justifies widening the shared union, B is the
incremental follow-up; C only if the uniform-hook signature is revisited
project-wide.

## 6. Blast radius (Option A)

| Area | Change |
|---|---|
| `lib/base/hooks.ml` | `legal_decisions_for_stage`: `"on_stop" -> [ K_Continue; K_Nudge ]` (1 line). Update the legal-decision-matrix doc-comment table (`:349-364`): the `on_stop` row gains `Y` under `Nudge`. |
| `lib/base/hooks.mli` | The `Nudge` doc comment (`:164`) is currently scoped to `OnIdle`/`BeforeTurn`; extend it to state `Nudge` is also honored on `on_stop` (inject user-role feedback, re-enter the loop). |
| `lib/pipeline/pipeline.ml` | Replace the `let _stop` discard (`:534-542`) with a match that **enumerates** the `hook_decision` variants (no bare `_ ->` catch-all). `Nudge msg` appends a standalone user-role message — mirror `make_message ~role:User [ Text msg ]` at `:452` — and returns `ToolsExecuted`; `Continue` (and every other variant, all rejected by the fail-closed registry) → `Complete response`. |
| `test/` | (a) `on_stop` `Continue` / no hook → `Complete` (unchanged). (b) `on_stop` `Nudge` → one re-entry; injected message present as a user-role message. (c) a hook that always `Nudge`s terminates at `max_turns` (no unbounded loop). (d) an illegal `on_stop` decision (e.g. `Skip`) is fail-closed (rejected; turn completes; logged). |
| `CHANGELOG.md` / `lib/sdk_version.ml` | Additive behavior entry; minor version bump. |

`run_loop`, `check_loop_guard`, `turn_outcome`, and the `ToolsExecuted` mapping
are **unchanged** — the re-entry plumbing already exists.

## 7. Boundary: OAS owns the mechanism, the host owns the predicate

| Layer | Responsibility |
|---|---|
| **OAS** | The typed mechanism: `Nudge` legal at `on_stop`, the wiring that re-enters the loop, and the existing loop bound. Provider- and host-agnostic. No import of host types. |
| **Host (e.g. MASC)** | The predicate: the `on_stop` hook implementation decides whether a declared stop is acceptable (for example, is the required evidence present?) and returns `Nudge feedback` when it is not. The predicate and its shape (e.g. `completion_contract` / `reviewable_evidence_ref`) stay in the host (`keeper_hooks_oas.ml`), never in OAS. |

This keeps the line RFC-OAS-025 drew: OAS does not own coordinator-shaped policy.
RFC-OAS-025 *removed* such policy from OAS; RFC-OAS-027 exposes a mechanism so the
coordinator can apply its own policy in-band rather than restarting the run.
`check-sdk-independence.sh` (the CI gate that forbids host imports in `lib/`)
remains the enforcement.

## 8. Relationship to #2080 runtime-continuation boundaries

`docs/design/runtime-continuation-boundaries.md` / `lib/runtime_continuation.mli`
(#2080) types when **inbound host input** may be applied to a busy turn (queue /
apply / reject / interrupt at named boundaries). RFC-OAS-027 is a different axis:
whether a **declared stop** is honored or rejected by the host's own hook. They
are orthogonal but share the same split — OAS owns the typed policy/mechanism, the
host owns the queue/predicate — and they compose at the end of a turn:

- `runtime_continuation`'s `After_final_answer` boundary ("apply as next turn
  input") is where a host-*queued* user input lands.
- An `on_stop` `Nudge` re-entry is the host *rejecting* the final answer with its
  own feedback, not host-queued user input.

They must not be conflated: one is externally-supplied input arriving mid/`post`
turn; the other is the host's completion verdict on the turn just produced. The
RFC adds no dependency between the two modules.

## 9. Migration / compatibility

- Backward compatible. Hosts that return `Continue` from `on_stop`, or register no
  `on_stop` hook, see no behavior change.
- No public type is removed. `Nudge` already exists; the only surface change is
  that it becomes legal on `on_stop` (a widening of the legality registry, not a
  breaking change).
- Minor version bump per `lib/sdk_version.ml`.

## 10. Acceptance

- `on_stop` `Continue` / no hook → turn completes (unchanged).
- `on_stop` `Nudge msg` → loop re-enters; `msg` present as a user-role message;
  re-entry bounded by `max_turns`.
- An illegal `on_stop` decision → fail-closed (rejected; turn completes; logged).
- `scripts/dune-local.sh build lib` + `@runtest` + `check-sdk-independence.sh`
  (no host import) + `@fmt` green.
- `CHANGELOG.md` entry + `sdk_version` bump.
