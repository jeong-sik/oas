# RFC-OAS-030: GLM glm-coding multi-turn perseveration + typed replay convergence

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (root-cause: multi-agent live audit, 2026-06-30) |
| Created | 2026-06-30 |
| Target | `agent_sdk` (oas) — `lib/llm_provider/reasoning_dialect.ml`, `lib/llm_provider/backend_openai_request.ml`, `lib/agent/agent.ml`, `lib/agent/agent_turn.ml` |
| Builds on | RFC-OAS-029 (Tools/Thinking/Reasoning/Multi-turn standard) §S3.1, §5 keystone |
| Boundary | OAS owns the canonical agent loop and the typed provider dialect. MASC consumes the canonical typed output only. OAS MUST NOT depend on MASC. |

## 0. Summary

A `glm-coding.glm-5-turbo` keeper (`sangsu` and siblings) presents as an infinite loop: every dispatch the model emits thinking + tool calls but no final text, the runtime executes the tools and re-dispatches, and the operator sees the dashboard render `Tool-only turn ended without a final reply.` repeatedly.

A three-axis live audit (OAS request serialization, OAS response/stream parse, MASC→OAS keeper loop) on 2026-06-30 reached a corrected root cause that overturns the first hypothesis:

- **The first hypothesis ("OAS strips GLM reasoning, breaking continuity") is wrong for the live config.** With `thinking-support = true` and `preserve-thinking = true` (set for `glm-5-turbo` in `runtime.toml`), `glm_should_replay_reasoning` is true, `clear_thinking=false` is emitted, and prior-turn `reasoning_content` is replayed. Reasoning replay functionally works.
- **The loop is perseveration, not reasoning loss.** The multi-turn tool loop lives in OAS (`lib/agent/agent.ml`) and, at the time of this audit, was bounded per dispatch by the then-default `max_turns=10` plus `body_timeout`, with no convergence/force-final mechanism. Tool-only turns count as progress; the idle guard only catches byte-identical repeats (`Exact` fingerprint), so a model that varies its tool calls runs to the turn ceiling every dispatch and terminates with empty text (`No_visible_reply`), then is re-dispatched.
- **Preserved-thinking re-priming compounds it.** Replaying the full accumulated reasoning every re-feed re-primes a reasoning model on its own tool-using chain. Correct per Z.AI's Preserved-Thinking contract, but a contributing driver of "thinks every turn, never answers."

This RFC separates the two concerns and specifies fixes for each, both inside OAS:

1. **S3.1 typed-replay prerequisite (this PR, code).** GLM's reasoning-replay decision is routed through the typed `Reasoning_dialect.replay_policy` instead of a serialize-time `is_glm_request`/`glm_should_replay_reasoning` branch. Behavior-preserving; closes RFC-OAS-029 `D1-glm-replay-hardcoded-heuristic` (P1) on the live `backend_openai_request` path.
2. **Perseveration convergence (proposed, separate PR).** An OAS-canonical, LLM-bounded convergence signal so a multi-turn tool loop concludes with a final answer instead of expiring on the turn ceiling — without a blunt cap.

## 1. Evidence

| Axis | Finding | Location |
|---|---|---|
| OAS output/stream | Hardened and correct: reasoning → typed `Thinking` block; tool_calls keyed by stable id, ambiguous → `SSEParseFailed`; tool-only turn never fabricates an empty `Text`; `finish_reason` reconciled against assembled content. Not the cause. | `complete_stream_acc.ml`, `streaming.ml`, `stop_reason_wire.ml` |
| OAS input/replay | GLM dialect resolves to the default `No_replay` (dead value); actual replay decided by `glm_should_replay_reasoning` + `is_zai_glm_config` string classifier, bypassing the typed `replay_policy`. | `reasoning_dialect.ml`, `provider_config.ml:423-449`, `backend_openai_request.ml:171-204` |
| OAS input/replay (live) | `thinking-support=true` + `preserve-thinking=true` for `glm-5-turbo` ⇒ `glm_should_replay_reasoning=true` ⇒ `clear_thinking=false` + reasoning replayed. Replay works; it is not stripped. | `runtime.toml`, `provider_config.ml:414-440` |
| MASC→OAS loop | Loop is in OAS `agent.ml:252-304`; at the audit point it was bounded by the then-default `max_turns=10` + `body_timeout`. Tool-only turns are "progress"; idle guard uses `Exact` fingerprint; no force-final/convergence; `No_visible_reply` has no salvage retry. | `agent.ml`, `agent_turn.ml:64-73,494`, `keeper_tool_response.ml:38-44` |

Version note: the live keeper runs the pinned `agent_sdk` (≤ v0.208.7); the fix targets the working tree (v0.208.8+). The replay logic is equivalent across both for the keeper config.

### 1a. Live probe — `glm-coding` / `glm-5-turbo` (2026-06-30)

Direct probes against `https://api.z.ai/api/coding/paas/v4/chat/completions` (model `glm-5-turbo`, `thinking={type:enabled, clear_thinking:false}`) confirm the I/O contract OAS implements:

| Probe | Request | Observed | OAS mapping confirmed |
|---|---|---|---|
| 1 (tool turn) | user + 1 tool | `finish_reason=tool_calls`; `reasoning_content` present (115 chars); `content=""`; one `tool_calls` with stable `id` and complete-string `arguments` `{"city":"Seoul"}` | OUTPUT parse: reasoning→`Thinking`, empty content→no `Text` block, stable-id keying. Matches. |
| 2 (replay) | user + assistant(`content:""` + replayed `reasoning_content` + `tool_calls`) + tool result, `clear_thinking:false` | Server **accepts** the replayed assistant shape (no 400); `finish_reason=stop`; final text answer; `tool_calls=0` (converges) | INPUT serialize: the exact `dialect_messages_of_message` wire shape (empty-string tool content + `reasoning_content` + `tool_calls`) is accepted and the model converges. Matches. |

Conclusion: OAS's GLM multi-turn Thinking+Tools+Reasoning I/O is correct against the live server. A resolvable two-turn tool exchange converges to `stop` naturally — so the observed keeper loop is not an I/O defect but a task-blocked scenario (the keeper's task was environment-blocked) plus the missing convergence guard of §3.

Evidence/currency: official Z.AI docs captured 2026-06-30 (thinking-mode guide + chat-completion API reference, the §5 authority); confidence High.

## 2. Fix 1 — typed reasoning replay (this PR)

RFC-OAS-029 S3.1 requires that "does this provider replay reasoning?" is answered only by `should_replay_reasoning` via `replay_policy`, and that the serializer not branch on `config.kind=Glm`/`is_glm_request`.

Change:
- `reasoning_dialect.ml` `for_provider_config`: for a ZAI-GLM config (`Provider_config.is_zai_glm_config`), resolve the clear_thinking-conditional replay to a typed `replay_policy` (`Preserve_always` when `glm_should_replay_reasoning`, else `No_replay`) at the single dialect boundary. The GLM capability profile previously left `replay_policy` at the dead `No_replay` default.
- `backend_openai_request.ml` `build_request_assoc`: select the message serializer uniformly via `Backend_openai_serialize.dialect_messages_of_message ~assistant_tool_content_format dialect`. Remove the `Glm when glm_should_replay_reasoning` and `OpenAI_compat when zai_glm_preserve_thinking_request` branches and the now-unused `zai_glm_preserve_thinking_request`.
- `backend_openai_request.ml` `capabilities_of_config`: a ZAI-GLM `OpenAI_compat` config with no catalog row resolves to `glm_capabilities` (so the empty-string tool-only content shape is preserved for the uniform serializer).

Behavior-preserving by construction: the replay gate is the same `glm_should_replay_reasoning`, relocated from serialize-time to dialect-resolution; the GLM tool-only content shape (`Assistant_tool_content_empty_string`) and reasoning-details suppression (`output_wire = No_output_control`) are unchanged.

Not in this PR (RFC-OAS-029 §5 sequencing, follow-ups): unifying the second thinking builder in `api_openai.ml` (`D1-dup`, S2.1); promoting `is_zai_glm_config`/`is_glm_model_id` to a typed `endpoint_mode`+kind so the remaining classifier uses disappear (`D6`, S1.1); routing GLM streaming through `dialect.streaming` instead of the hardcoded `Delta_field` (`D2`, S6.3).

## 3. Fix 2 — perseveration convergence (proposed, separate PR)

The loop expires on `max_turns` and renders `No_visible_reply` because nothing decides "enough information has been gathered; produce the answer." A blunt cap or a heuristic similarity threshold is rejected: caps turn into Pause/Stop (operator goal: Pause only when truly broken), and "are we still making progress?" is a judgment that belongs behind an LLM boundary, not a string/number heuristic.

Proposed OAS-canonical mechanism (design, not yet implemented):

- **Convergence signal as a typed turn outcome.** After a run of consecutive tool-only turns (no `Text` block produced), the agent loop requests one final turn with `tool_choice` disabled (force a textual answer) rather than expiring silently. This is a *redirect*, not a cap: the keeper keeps acting, it just must surface a final answer for the current request before continuing to gather.
- **LLM-bounded, not heuristic.** Whether the loop should converge is decided by the model itself (the forced-final turn) or an LLM judge, never by a similarity score on tool arguments. The `Exact`-fingerprint idle guard stays as a cheap exact-repeat backstop only.
- **No silent expiry.** `No_visible_reply` after the forced-final turn is surfaced as a typed outcome with the gathered tool context attached, not a blank render.

Resolved (was an open question):
- **Z.AI Preserved-Thinking replay scope — `Preserve_always` is correct.** The chat-completion API reference states `clear_thinking` default `true` "removes `reasoning_content` from prior turns" and `false` "retains `reasoning_content` from prior turns"; the thinking-mode guide requires "All consecutive reasoning_content blocks must exactly match the original sequence" (all preserved history, not just the active sequence). So GLM's replay is correctly clear_thinking-conditional, and when on it preserves the full history (`Preserve_always`), not `Drop_without_tool_preserve_with_tool`. Live probe 2 confirms the full-history replay shape is accepted and converges. The Fix 1 dialect resolution therefore uses `Preserve_always`.
- Note: byte-exact replay is contractually required ("Do not reorder or edit"). GLM returns a single `reasoning_content` string, which OAS parses into one `Thinking` block and replays verbatim (sanitize is identity for clean UTF-8, no multi-block concat), so byte-exactness holds for GLM in practice. The sanitize/concat path remains a latent strictness risk for multi-block or non-UTF-8 reasoning (RFC-OAS-029 D1-replay note, low).

Open question:
- Whether the forced-final convergence turn belongs in the OAS agent loop policy (canonical) or is exposed as a typed capability MASC drives. Boundary preference: the loop is OAS-owned, so the mechanism is OAS-canonical and MASC observes the typed outcome.

## 4. Tests

- This PR (non-vacuous, reverts red): `backend_glm.ml` — a GLM Preserved-Thinking config replays a prior `Thinking` block as `reasoning_content`; the default config does not. Reverting the `for_provider_config` resolution leaves GLM at `No_replay` and turns the preserved case red.
- Follow-up (Fix 2): a multi-turn fixture where the model emits N tool-only turns then converges on a forced-final turn; assert a `Visible_reply` outcome rather than `No_visible_reply` expiry.

## 5. Relationships
- **RFC-OAS-029** §S3.1 (replay is typed, one source), §5 keystone (GLM typed dialect reshape) — Fix 1 is the live-path step of that keystone.
- **CLAUDE.md workaround rejection** — Fix 2 must not be a cap/cooldown/dedup; the convergence decision is LLM-bounded.
