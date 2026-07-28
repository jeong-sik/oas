# RFC-OAS-039: Suspend a tool call for input instead of terminating it

| | |
|---|---|
| Status | Draft |
| Author | vincent (with Claude analysis) |
| Created | 2026-07-28 |
| Target | `agent_sdk` (oas), consumer `masc` |
| Related | [[RFC-OAS-025]] (forced tool-use enforcement boundary), masc RFC-0356 (server-side gate replay — the substitute this RFC removes the need for) |

## 0. Summary

A caller gate at `Pre_tool_use` has two legal decisions: `Continue` and `Block`. A gate that needs authorization from outside the process — a human approval, an external policy service — has no third option, so it must answer the call now. The only non-failure answer is `Tool_succeeded`. A deferral therefore **terminates** the tool call with a success the model believes, and the authorization, when it later arrives, has no call to return to.

This RFC admits `ElicitInput` at `Pre_tool_use` and adds `Suspended_for_input` to `batch_completion`, so the turn stops with the `ToolUse` unanswered and resumes through the `Agent.provide_input` machinery the SDK already ships.

## 1. Problem (evidence)

### 1.1 The gate decides one stage after suspension stops being legal

`lib/base/hooks.ml:302-306`, fail-closed:

```ocaml
| Before_turn   -> [ K_Continue; K_ElicitInput; K_Nudge ]
| Pre_tool_use  -> [ K_Continue; K_Block ]
```

`ElicitInput` is the suspend primitive. It is legal only before a turn begins — before the model has chosen a tool or an input. A caller gate that authorizes *this command with this input* can only decide once both exist, which is `Pre_tool_use`.

`lib/agent/agent_tools.ml:786-803` treats the arrival of `ElicitInput` there as a validation escape:

```ocaml
| (Hooks.AdjustParams _ | Hooks.ElicitInput _ | Hooks.Nudge _) as decision ->
  … detail:(Printf.sprintf "illegal decision %s escaped hook validation" …)
```

### 1.2 The tool contract is total and two-valued, so a deferral has no image

```ocaml
(* lib/llm_provider/types.mli:75-79 *)
type tool_result_outcome =
  | Tool_succeeded
  | Tool_failed of tool_failure_provenance
```

`Tool.execute` is total. `ToolResult` carries no `_meta`. A consumer that wants to report "this call has not run yet" has nowhere to put it, and `Tool_failed` is factually wrong — nothing failed.

### 1.3 The one carrier that exists is discarded one frame later

`lib/agent/agent_tools.ml:417`:

```ocaml
| Ok { content; _meta = _ } -> content, Tool_succeeded
```

A caller may attach a disposition marker to `_meta`. It is dropped here, unconditionally, and the outcome is `Tool_succeeded` regardless. The downstream consumer `masc` constructs exactly such a marker (`masc.tool_disposition = "deferred"`, `lib/tool_bridge.ml:126`) whose own interface states the projection "cannot represent `Deferred`" (`lib/tool_types/tool_result.mli:52`). Both statements are true; the marker is written and then deleted.

### 1.4 The resulting transcript is well-formed and wrong

After a deferral: `Tool_succeeded`, `is_error:false`, `ToolUse` paired with `ToolResult`, checkpoint internally consistent. No consistency check, provider admission check, or corruption detector can distinguish it from a call that ran. The defect is invisible at every boundary that is inspected.

### 1.5 Observed consequence in the downstream consumer

Measured 2026-07-28 against a live `masc` gate store (458 approval deliveries):

| | |
|---|---|
| decisions | `approve` 458, `deny` 0 |
| grant consumed | 76 (16.6%) |
| grant never consumed | 382 (83.4%) |
| `network_read` consumed | 1 of 280 (0.4%) |

The 280 `network_read` requests carry one byte-identical payload, so approval matching is not the failure — 279 authorizations were granted against a call that had already been answered.

## 2. Non-goals

- **Adding a third state to the provider wire.** `is_error` is derived as a `bool` in the shared serializer (`lib/llm_provider/api_common.ml:326`) for every backend, and no `Capabilities` field describes deferred or pending tool results. No currently supported provider can carry a third outcome. Suspension is chosen because it puts nothing new on the wire: the provider is not called while suspended, and every request it does receive contains complete `tool_use`/`tool_result` pairs.
- **Changing `tool_result_outcome`.** It stays `Tool_succeeded | Tool_failed`. A suspended call produces no `ToolResult` at all.
- **The crash-window orphan.** A checkpoint written at `After_assistant_collected` contains a `ToolUse` with no `ToolResult` because the process stopped mid-cycle. That is a durability failure and is out of scope. This RFC creates a second, legitimate source of unanswered `ToolUse`, so §3.4 states how a host distinguishes them; conflating the two would make a type-algebra defect look like a durability defect.
- **Auto-resume.** The SDK does not decide when input arrives. The host calls `Agent.provide_input`.

## 3. Design

### 3.1 Admit the decision where the gate decides

```ocaml
| Pre_tool_use -> [ K_Continue; K_Block; K_ElicitInput ]
```

`AdjustParams` and `Nudge` stay illegal at this stage; the validator stays fail-closed.

### 3.2 A batch completion that is neither continue nor terminal

```ocaml
(* lib/agent/agent_tool_execution_types.ml:9 *)
type batch_completion =
  | Continue_after_batch
  | Terminal_completed of Tool_contract.Invocation.t
  | Terminal_failed of { … }
  | Suspended_for_input of
      { invocation : Tool_contract.Invocation.t
      ; request : Hooks.elicitation_request
      }
```

At `agent_tools.ml:786`, `ElicitInput request` produces `completed_result = None` and this completion. No `ToolResult` is manufactured, so nothing is appended for that `ToolUse`.

The variant is additive and changes no behaviour for the three existing ones. On locating the sites, the compiler helps unevenly and the difference matters:

- `lib/` — `ci.yml:287-290` runs `dune build @install --force` with `OCAMLPARAM: "_,warn-error=+a"`, and `@install` covers the installable library. A non-exhaustive match under `lib/` is a hard CI failure.
- `test/` — not built by `@install`. `dune build @all` and `dune runtest` (`ci.yml:51`, `:59`) carry no `OCAMLPARAM`. The repo has no root `dune` `(env …)` stanza and no `warn-error` in any dune file, so a missing arm in `test/` degrades to warning 8 and CI stays green.

Test-side arms therefore have to be found by hand, not by the compiler. Enumerated: `test/test_tool_execution.ml` (3 matches, at the `Continue_after_batch` / `Terminal_*` arms) and `test/test_pipeline.ml` (turn-outcome matches). `test_pipeline.ml:34` and the `Terminal_failed` occurrences in `agent_execution_runner.{ml,mli}` and `agent.{ml,mli}` belong to `terminal_outcome`, a different type, and are unaffected.

### 3.3 Resume is the durable-invocation path, not `provide_input`

`Agent.provide_input` cannot be reused here. Its implementation is:

```ocaml
(* lib/agent/agent_elicitation.ml:57-64 *)
let apply_response ?metadata agent (req : Error.input_required) response =
  match message_of_response ?metadata ~question:req.question response with
  | None -> false
  | Some message ->
    update_state agent (fun state ->
      { state with messages = Util.snoc state.messages message });
    true
```

It appends one **user message** and reads only `req.question`; it does not know a `tool_use_id`. Applied after a tool suspension it would produce

```
assistant: [ ToolUse ]
user:      [ Text "…" ]     (* no ToolResult *)
```

which no supported provider accepts. `ElicitInput` means the same thing at both stages — stop and ask the host — but the resume differs, because at `Before_turn` no `ToolUse` exists yet.

The resume that does work is already in the tree, as the crash-recovery path:

```ocaml
(* lib/agent/agent_tools.ml:734-744 *)
| Ok (Some durable_invocation) ->
  (* An existing durable occurrence proves the pre-tool gate already
     admitted this exact call. Replaying it must not invoke the gate or any
     lifecycle observer again. If no attempt was committed, [execute_phased]
     starts the effect once; an in-flight attempt fails closed. *)
  execute_durable durable_invocation
```

So suspension opens the invocation without executing it (`Execution_agent_scope.open_invocation`, which is separable from `execute_durable` — today they are merely adjacent inside the `Continue` branch at `agent_tools.ml:817-832`). That leaves an open, addressed, zero-attempt invocation. On resume the turn re-runs from its checkpoint, `find_invocation` returns `Some`, the gate is not re-consulted, and the effect starts exactly once against the original `tool_use_id`. `Execution_tool_settlement` supplies the idempotency: a second approval cannot double-execute, and an in-flight attempt fails closed.

Consequence: **suspension requires `?execution_store`** (`lib/agent/agent.mli:307,320`). Without a provider attempt there is nowhere to record the call, and a suspension with no resume path loses it outright — worse than answering it. `ElicitInput` at `Pre_tool_use` therefore fails closed when no execution store is armed, rather than suspending into a dead end.

### 3.4 Refusal needs its own entry point

Because resume deliberately skips the gate, a refusal must not travel the resume path — it would execute the command it denied. A gate that can only approve is not a gate, so this RFC also requires an entry point that settles a suspended invocation as refused: it appends a `ToolResult` for the original `tool_use_id` carrying the refusal text (the shape `blocked_tool_result` already produces at `agent_tools.ml:768`) and settles the journal invocation without starting the effect. `Hooks.elicitation_response` already carries the distinction (`Answer of Yojson.Safe.t | Declined`, `lib/base/hooks.ml:143-145`).

### 3.5 Distinguishing suspended from orphaned

A host that persists transcripts must not treat a suspended pair as corruption. The journal is what separates them: a suspended pair has an open, addressed invocation with zero committed attempts, and `Execution_agent_scope.provider_invocations_settled` answers "is this attempt settled?" directly rather than by inspecting message shape. An orphaned pair — the process stopped mid-cycle — has no such record.

## 4. Verification

CI runs `dune runtest` (`.github/workflows/ci.yml:59`), so the tests below gate the change.

1. `Pre_tool_use` returning `ElicitInput` yields `Suspended_for_input`, and the turn's message list contains the `ToolUse` with **no** `ToolResult` for that `tool_use_id`. Fails today: the decision is rejected as an escaped hook decision and the run reports `Hook_execution_failed`.
2. The suspended turn raises `Error.InputRequired` carrying the request. Fails today for the same reason.
3. After `Agent.provide_input`, the tool executes once and exactly one `ToolResult` is appended for the original `tool_use_id`. Fails today: no resume path reaches a suspended tool call.
4. Negative control: `Pre_tool_use` returning `AdjustParams` or `Nudge` still fails validation. Guards against widening the stage table beyond the one decision.
5. No provider request is issued while suspended, and the request issued after resume contains matched `tool_use`/`tool_result` pairs. This is the cross-provider claim in §2; assert it on the serialized request, not on backend-specific code.

## 5. Migration

`agent_sdk` is consumed by `masc`, which pins an exact SHA (`scripts/oas-agent-sdk-pin.sh`, currently equal to `oas` `main` head). The variant addition is source-breaking for exhaustive matches; `masc` has no match on `batch_completion` today, so the pin bump is mechanical. Adoption in `masc` (arming `?execution_store`, raising `ElicitInput` from its gate, calling `provide_input` on approval) is a separate change and is not required for this RFC to land.
