# RFC-OAS-025: Forced-Tool-Use Enforcement Boundary

| Field | Value |
|---|---|
| Status | Draft |
| Author | vincent (drafted by agent) |
| Created | 2026-06-04 |
| Target | `agent_sdk` (oas) |
| Related | RFC-OAS-017 (coordinator-shape leak), RFC-OAS-021 (approval-required missing-callback policy), RFC-OAS-013 (keeper tool disclosure) |

## 0. Summary

OAS validates, on the response side, that a model honored the caller's
`tool_choice` (the `Require_tool_use` / `Require_specific_tool` completion
contracts) and, on violation, retries the turn to coerce a tool call. The two
major providers already **guarantee** this server-side, so for compliant
providers the validation is redundant and the retry path is effectively dead;
the only case it guards is a **non-compliant local backend** that ignores
`tool_choice`. This RFC proposes moving forced-tool *enforcement* out of the SDK
(the SDK passes `tool_choice` through to the provider and reports, but does not
coerce), leaving any local-model coercion to the consumer/coordinator.

This is the same boundary principle as RFC-OAS-017: OAS should not own
coordinator-shaped policy.

## 1. Current state (file:line, origin/main `1b19c3f4`)

`tool_choice` maps to a completion contract (`lib/completion_contract.ml:63`):

```ocaml
let requested_of_tool_choice choice =
  match choice with
  | Some Any -> Require_tool_use
  | Some (Tool name) -> Require_specific_tool name
  | Some None_ -> Require_no_tool_use
  | Some Auto | None -> Allow_text_or_tool
```

`validate_response` (`lib/completion_contract.ml:185`) then checks the *response*
against the contract; for `Require_tool_use` a text-only response on a
non-resumable stop reason is an `Error`. The turn pipeline turns that into a
`CompletionContractViolation` and, when a `tool_retry_policy` is set, re-runs the
turn with a "you must call a tool" feedback message
(`lib/pipeline/pipeline.ml` `handle_missing_required_tool_use`, ~6 contract
sites at lines 65, 185, 218, 243, 249, 274). The contract type is public via
`agent_sdk.mli` (`module Completion_contract_id`) and is carried by
`Error.CompletionContractViolation` (`lib/base/error.ml:77`).

## 2. Problem: enforcement is in the wrong layer ("거꾸로")

`tool_choice` is a **request-side directive to the provider**. Both major
providers enforce it server-side:

- **OpenAI** `tool_choice: "required"`: "you can count on a tool being provided
  with every call" — the response is guaranteed to contain a tool call.
- **Anthropic** `tool_choice: {type: "any"}`: the API prefills the assistant
  message to force a tool, so the model **cannot** emit a natural-language
  (text-only) response.

(Evidence: OpenAI developer docs/cookbook on `tool_choice: "required"`; Anthropic
tool-use docs / cookbook `tool_choice.ipynb`, June 2026.)

Therefore OAS re-validating the response against `tool_choice` re-checks a
guarantee the provider already makes. The consequences:

- For OpenAI/Anthropic the `Require_tool_use` error branch and its retry are
  **dead** (the provider never produces the text-only response that would
  trigger them).
- The only live case is a **non-compliant local backend** (llama.cpp / Ollama /
  small models that ignore `tool_choice`). There, OAS burns turns retrying to
  coerce a tool call.
- Coercing a non-compliant backend is a **policy** decision — when to retry, how
  many times, whether to force at all. By RFC-OAS-017's principle (OAS does not
  own coordinator-shaped concerns) and consistent with OAS already delegating
  cross-provider failover / circuit-breaking to downstream consumers, this
  belongs to the consumer/coordinator, not the SDK.

The accept/reject logic itself is locally correct; what is inverted is **which
layer enforces** — the SDK re-enforces what the provider guarantees.

## 3. Non-goals

- Removing the ability to *send* `tool_choice` to the provider. The request-side
  parameter stays; providers keep enforcing it natively.
- Changing `Allow_text_or_tool` (the default) or `Require_no_tool_use` semantics
  beyond what each option below states.

## 4. Options

### Option A — Remove forced-tool response enforcement (recommended)

Drop the response-side validation **and** retry for `Require_tool_use` /
`Require_specific_tool`. `tool_choice` is still placed in the provider request.
The SDK no longer raises `CompletionContractViolation` for "no tool when a tool
was required", and the pipeline no longer retries to coerce one.

- Pro: removes redundant/dead logic for compliant providers; takes the SDK out of
  the tool-forcing-policy business (RFC-OAS-017 aligned); eliminates the
  turn-burning retry the user flagged.
- Con: a non-compliant local backend that ignores `tool_choice` will now return
  text and the SDK will accept it; the consumer must detect/handle that. Public
  API surface (`Completion_contract_id`) shrinks (breaking for any external
  matcher).

### Option B — Report-only (keep validation, drop retry)

Keep the contract + validation so a violation is still surfaced (as an error or
event), but remove the pipeline retry-coercion. The SDK reports "model did not
call a required tool"; the consumer decides whether to retry.

- Pro: smaller blast radius; preserves the diagnostic; directly removes the
  turn-waste.
- Con: keeps the redundant validation for compliant providers; the public
  contract type stays.

### Option C — Explicit policy toggle

Add `forced_tool_enforcement : Enforce | Report_only | Off` to `Agent_options`,
default `Enforce` (compat).

- Pro: no breaking change; opt-in; profiles choose.
- Con: keeps all the code paths; adds config surface rather than removing a
  concern. Does not resolve the layering objection — it just makes it optional.

## 5. Recommendation

**Option A.** The research shows the enforcement is redundant for compliant
providers and the only live case (local non-compliance) is a coordinator
concern. Option A is the one that actually resolves the "거꾸로" layering. If a
softer landing is wanted, ship Option B first (remove the turn-burning retry) and
follow with A.

## 6. Blast radius (Option A)

| Area | Change |
|---|---|
| `lib/base/completion_contract_id.ml` + `.mli` | Remove `Require_tool_use` / `Require_specific_tool` variants (or the whole forced-tool family); decide `Some Any` / `Some (Tool _)` → `Allow_text_or_tool`. |
| `lib/completion_contract.ml` | Drop the removed arms in `requested_of_tool_choice`, `validate_response`, `of_tool_choice`; drop ~10 inline tests for those variants. |
| `lib/pipeline/pipeline.ml` | Remove the ~6 forced-tool sites (`contract_requires_tool`, the violation/retry in `handle_missing_required_tool_use`, prompt strings). |
| `lib/base/error.ml` / `lib/error_domain.ml` | `CompletionContractViolation` no longer reachable for the removed contracts; keep or narrow the error variant. |
| `agent_sdk.mli` | Public `Completion_contract_id` surface shrinks (breaking). |
| `test/` | ~20 sites across `test_agent_pipeline`, `test_completion_contract_violation`, `test_error`, `test_error_domain`. |

`tool_choice` request-building (provider backends) is **unchanged** — providers
still receive and enforce it.

## 7. Migration / compatibility

- Removing public variants is a breaking change for external matchers on
  `Completion_contract_id.t`. If that matters, gate via Option C first, or land A
  in a major version bump per `lib/sdk_version.ml`.
- Consumers relying on OAS to coerce a tool from a non-compliant local model must
  add their own check (e.g. inspect `response` for a tool_use block and retry at
  the coordinator).

## 8. Acceptance

- `tool_choice` is still sent to the provider request (verified by a backend
  request-shape test).
- No `CompletionContractViolation` is raised for "no tool when required" (Option
  A/B), and the pipeline does not retry to coerce a tool.
- `dune build lib` + `@runtest` + `check-sdk-independence.sh` + `@fmt` green.
- Public surface change recorded in `CHANGELOG.md` (+ version bump if breaking).
