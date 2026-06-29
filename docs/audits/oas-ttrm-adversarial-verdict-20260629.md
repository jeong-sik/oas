# OAS Tools/Thinking/Reasoning/Multi-turn — Adversarial Standard & Verdict (2026-06-29)

> Adversarial standards audit of the OAS (OCaml Agent SDK) request/response surface across four dimensions, each cross-checked against current provider documentation and re-verified against source. This verdict feeds **RFC-OAS-029** (the standards SSOT, PR #2232). The boundary rule **"OAS does not know MASC"** was checked per dimension and holds in all four: OAS contains no MASC references, reasoning telemetry is parse-only (never gating budget/cost/turn), and the one soft boundary leak found (Tools alias module) is called out explicitly below.

> **Provenance & confidence.** Produced by a 13-agent adversarial workflow (run `wf_cf7f662b-7a6`, 2026-06-29): per dimension a source-extraction pass, a latest-provider-doc cross-check (web search), and an adversarial skeptic verdict, then synthesis. The skeptic stage re-verified every doc-check claim against the per-backend wire builders and **refuted several overstated mismatches** (e.g. the "Gemini `thought_signature` is dropped" and "Anthropic adaptive returns 400" claims are FALSE — the backends already handle them). Residual-risk sections flag the deltas that could **not** be confirmed against a dated official doc this pass (Kimi `thinking.keep`, MiniMax `reasoning_details`, OpenAI `none` effort, Anthropic `"max"` vs `"xhigh"` top tier); per the currency policy these must be re-checked against live docs before any code change, or they risk "fixing" a non-bug. Code locations (`file:line`) reflect main at audit time and may drift. The single P0 (Multi-turn malformed tool-args coercion) was independently re-verified against `complete_stream_acc.ml` on main before action.

---

## 1. Executive Verdict Table

| Dimension | Verdict | Top required fix (priority) |
|-----------|---------|------------------------------|
| **Tools** | PARTIAL | Remove/declare the hardcoded built-in alias string classifier with orphan targets `ReadFile`/`SearchFiles`/`Execute` (`agent_tool_name_alias.ml:131-134`) — a MASC→OAS boundary leak. **(P1)** |
| **Thinking** | PARTIAL | Split the overloaded `thinking_type` string into a typed kind tag + `signature:string option`; stop fabricating `signature:"thinking"`/`""` (`types.mli:112`, `api_common.ml:57-60`, `complete_stream_acc.ml:172-177`). **(P1)** |
| **Reasoning** | PARTIAL | Consolidate the budget→effort threshold ladder into one SSOT; backends re-hardcode `2_048`/`8_192`/`32_768` instead of reusing the named constants (`backend_anthropic.ml:48-52`, `backend_gemini.ml:45-49`). **(P1)** |
| **Multi-turn** | PARTIAL | A complete (non-truncated) tool_use block with malformed JSON args must fail closed, not coerce to empty args and dispatch (`complete_stream_acc.ml:214-218`). **(P0)** |

**Net:** no FAIL, no clean PASS. The closed-sum / parse-don't-validate / fail-closed discipline holds across all four dimensions; the adversarial cross-checks systematically overstated severity by reading descriptor metadata (`reasoning_dialect`, `constants`) instead of the per-backend wire builders. Each dimension carries real, code-confirmed bar violations that block PASS.

---

## 2. Per-Dimension Analysis

### 2.1 Tools — PARTIAL

**Established standard (what OAS guarantees).** Tool calls run a parse-don't-validate pipeline on closed sum types. Provider responses parse into `content_block` whose `ToolUse`/`ToolResult` variants are matched exhaustively everywhere (`agent_tools.ml:921-937`, `tool_use_recovery.ml:208-219`, `backend_tool_call_harness.ml:99-107`) — no `_` catch-all, so a new variant forces review. Dispatch resolves names through `find_in_index` (exact `by_name` → case-insensitive `Tool_id.of_string`, fail-closed for `User _` ids → alias). `Tool_id.t` is a closed sum; unknown names degrade to `User _` rather than raising. Concurrency is type-driven (`concurrency_class`, `mutation_class` closed sums with `of_yojson` rejecting unknown strings; missing descriptors fail closed to `Sequential_workspace`). Unknown tool names become typed failures (`Validation_error`/`Non_retryable_tool_error`), not permissive no-ops. JSON-Schema validation surfaces an unknown `type` keyword as a violation. Malformed-call recovery promotes Text→ToolUse only when the name is in `valid_tool_names`. Per-provider capability is modeled: `supports_tool_choice:bool` exists and GLM is explicitly gated false with the "tools supported, forced tool_choice unsupported" pattern (`capabilities.ml:426`); a dedicated `backend_openai_responses.ml` exists and `custom_tool_call` is recognized in streaming.

**Latest-doc deltas that matter.**
- **Gemini 3.1/3.5** requires the encrypted `thought_signature` round-tripped on the assistant function-call part (400 `INVALID_ARGUMENT` if stripped). **Verified FALSE as a defect:** OAS round-trips it through a typed `RedactedThinking` carrier keyed by `tool_use_id` (`backend_gemini.ml:441`, `streaming.ml:979`, tabled at `:124`, replayed at `:180`); `thinkingLevel` handled at `:62`.
- **GPT-5.5 Responses API** adds `custom` tools (freeform/Lark-CFG text, not JSON object) and an `allowed_tools` `tool_choice` mode. OAS has `backend_openai_responses.ml` and recognizes `custom_tool_call` in streaming (`streaming.ml:663,863`) but has no closed tool-kind variant for a freeform-text contract — `tool_input_validation` marks non-object input `Invalid`. **Currency gap.**
- **Kimi K2.6** restricts `tool_choice` to `auto`/`none`. The cross-check's "no per-provider tool-forcing capability" claim is **wrong** (`supports_tool_choice` exists), but the binary bool cannot express "auto/none allowed, required/named rejected."
- **Claude Opus 4.8** programmatic tool calling adds `allowed_callers` and code-execution-originated tool results that bypass normal `tool_result` blocks — not representable in OAS's descriptor/content-block model. Forward-looking gap.

**Verdict & required fixes.** The flagship HIGH mismatch (Gemini signature "structurally dropped") is refuted; the pipeline holds. PASS is blocked by a confirmed defect: `agent_tool_name_alias.ml:131-134` hardcodes a string classifier (`"Read"→"ReadFile"`, `"Bash"→"Execute"`) with name-based field guessing over `[query;regex;name;needle]` and a hand-rolled shell tokenizer. The canonical targets are **orphan strings** — registered nowhere in `lib/` (grep confirms only `test_event_bus.ml` creates them ad hoc) — yet the `.mli` markets them as "generic, provider-agnostic." For any consumer not using those exact names, the branch remaps to a non-existent canonical → dispatch miss → typed failure. This bakes a specific downstream (MASC-style) naming convention into OAS core: a soft MASC→OAS boundary leak.

| Fix | Priority | Location |
|-----|----------|----------|
| Remove hardcoded built-in alias branches OR make them a declared, documented closed-sum table backed by registered `Tool_id` identifiers; require consumers to register via `register_alias`. Orphan targets `ReadFile`/`SearchFiles`/`Execute` encode one consumer's convention. | **P1** | `agent_tool_name_alias.ml:131-134` |
| Replace name-based semantic field guessing + hand-rolled shell tokenizer with schema-driven normalization (declared tool parameter schema) or explicit reject. Approximate shell lexing mis-tokenizes quoted/multi-space commands and passes them through unchanged. | **P1** | `agent_tool_name_alias.ml:39,94,101,45-110` |
| Add a closed tool-kind variant (`json_object \| custom_freeform`) so a GPT-5.5 freeform string payload is a valid contract, not coerced/rejected. Streaming already recognizes `custom_tool_call`. | **P1** | `tool_input_validation.ml`, Tool descriptor |
| Extract Gather_act_verify magic turn thresholds (`turn<=2`, `turn<=5`) to named constants/strategy config. | **P2** | `progressive_tools.ml:43-46` |
| Surface a coercion record when `string->int/bool/number` rewrites occur; emit a signal (not `Valid`-unchanged) for `params=[]` / `Null`-as-empty-object so no-schema acceptance is observable. | **P2** | `tool_input_validation.ml:43-86` |
| Refine `supports_tool_choice` from `bool` to a closed sum (`None \| Auto_only \| Forced`) for Kimi K2.6; add opt-in `allowed_callers` for Claude 4.8 programmatic calling. | **P2** | `capabilities.ml`, Tool descriptor |

**Residual risk.** The alias module silently remaps provider tool names to orphan canonicals for any external consumer not using OAS's assumed naming — a latent footgun masquerading as "generic" convenience. It degrades to a typed failure (not a crash), which is why this is PARTIAL not FAIL, but every future agent reading the codebase treats this consumer-specific hardcoding as a sanctioned pattern (workaround-accumulation spiral). Secondary: the closed-sum model cannot yet represent freeform/custom inputs (GPT-5.5) or programmatic-caller results (Claude 4.8) — a currency gap that widens as more providers ship non-JSON-object tool contracts.

---

### 2.2 Thinking — PARTIAL

**Established standard.** Per-provider thinking is a typed `Reasoning_dialect.t`, built from the `Capabilities` catalog (`of_capabilities`) or a provider branch (`for_provider_config`). The request wire toggle is a closed sum `toggle_wire` (No_toggle/Thinking_object/Chat_template_kwargs/Chat_template_token/Reasoning_effort/Enable_thinking/Anthropic_thinking/Gemini_thinking_config); the OpenAI builder matches exhaustively on `caps.thinking_control_format` (`backend_openai_request.ml:305`), so a new format breaks compilation. Cross-turn replay is gated by a `replay_policy` closed sum via `should_replay_reasoning ~assistant_had_tool_call`. Streaming uses `ThinkingDelta`/`ThinkingSignatureDelta` into separate buffers, finalizing typed `Thinking`/`RedactedThinking`; stream block kinds are a closed sum with explicit `Unknown_block`. Redacted thinking is a distinct carrier never surfaced as assistant text.

**Latest-doc deltas that matter (and which cross-check claims are FALSE).**
- **Anthropic adaptive (Opus 4.8/4.7):** doc says manual `thinking:{type:enabled,budget_tokens}` → 400; only `type:adaptive` accepted. **Cross-check FALSE:** `capabilities.ml:229-236` maps these models to `Anthropic_adaptive_only`; `backend_anthropic.ml:78-80` emits `thinking:{type:"adaptive"}`. No 400.
- **Anthropic `output_config.effort`:** **Cross-check FALSE:** `backend_anthropic.ml:91-113,198-199` already emits the separate `output_config.effort` field (incl. `"max"`); `budget_tokens` is gated to `Anthropic_manual_budget` only.
- **Gemini `thinking_level` (3.x), 400 if both level+budget:** **Cross-check FALSE:** `backend_gemini.ml:52-73` emits `thinkingLevel` for Gemini 3+ and `thinkingBudget` only for 2.5 — mutually exclusive, never both.

The cross-check systematically read `reasoning_dialect`/`constants` descriptor metadata instead of the backends that emit the wire, inflating severity. Remaining unverified-but-plausible deltas to re-check against live docs: Kimi `thinking.keep` provider-side flag; MiniMax M3 inline `<think>`/`reasoning_split`/`reasoning_details` (plural); OpenAI `none` as first-class effort distinct from "disabled."

**Verdict & required fixes.** Three top "returns 400" mismatches refuted → not FAIL. PASS blocked by genuine bar violations:

| Fix | Priority | Location |
|-----|----------|----------|
| Split the overloaded `thinking_type` string into a typed block-source/kind tag + separate `signature:string option`. Omit the `"signature"` JSON field entirely when `None`; stop defaulting the absent-signature case to `"thinking"`/`""`. Removes conflation + fabricated-signature silent failure. | **P1** | `types.mli:109-112`, `api_common.ml:57-60,139`, `complete_stream_acc.ml:172-177`, `streaming.ml:151-155` |
| Collapse the two budget→effort heuristics into one typed SSOT — `backend_anthropic.ml:48-53 effort_of_budget` must reuse/extend `Reasoning_effort.of_budget` rather than bare magic numbers + raw strings; represent the `max` tier as a typed enum value. | **P1** | `backend_anthropic.ml:48-53`, `reasoning_effort.ml:26-34` |
| Retire the self-labeled WORKAROUND duplicate accumulator in `lib/streaming.ml` onto `Complete_stream_acc` so thinking/signature finalize has one code path. | **P2** | `streaming.ml:104-110,151-155` |
| Establish one SSOT for "is thinking on when `enable_thinking=None`" shared by Ollama and the dialect/sampling logic (they currently disagree: off vs on). | **P2** | `backend_ollama.ml:42` vs `reasoning_dialect.ml:176,317` |
| Derive (or delete) the inline Anthropic/Gemini dialect in `for_provider_config` from the Capabilities catalog, so the descriptor layer cannot drift from the backend builders. | **P2** | `reasoning_dialect.ml:237-250` |
| Extend `Reasoning_effort.t` with typed `none` and `max` members so `normalize_effort` stops string-special-casing them. | **P2** | `reasoning_dialect.ml:284-292`, `reasoning_effort.ml:3` |

**Residual risk.** Provider doc currency was not independently verifiable this pass, and the supplied cross-check proved unreliable (flagged three already-correct backend emissions). Its remaining claims (Kimi `thinking.keep`, MiniMax `reasoning_details`, OpenAI `none`) must be re-checked against live docs before any code change, or they risk "fixing" non-bugs. The one genuine latent correctness risk: the fabricated-signature branch (`complete_stream_acc.ml:175` / `streaming.ml:153`) becomes a live HTTP 400 source if a non-Anthropic-sourced thinking block is ever replayed to Anthropic on a tool-use turn — at which point fix #1 escalates to **P0**.

---

### 2.3 Reasoning — PARTIAL

**Established standard.** Reasoning is a typed closed sum (`Reasoning_effort.t` = Minimal/Low/Medium/High/XHigh) plus a per-provider `Reasoning_dialect.t` whose sub-fields (toggle_wire, effort_alias_policy, replay_policy, visibility, sampling_policy, streaming) are exhaustive variants matched without catch-all. Wire effort derives deterministically: `enable_thinking + thinking_budget → effort_of_thinking_config_value → Reasoning_effort.of_budget`, then per-provider aliasing (e.g. DeepSeek `Thinking_object` collapses Low/Med/High→`high`, XHigh→`max`). "Reasoning" vs "thinking" is distinguished by `toggle_wire`/`visibility` variants, not ad-hoc strings. `reasoning_tokens` is parse-only telemetry (aggregated, never gated); when a provider returns only reasoning text it is estimated (`len/4`) but explicitly flagged `reasoning_tokens_estimated=true` — a non-silent estimation boundary.

**Latest-doc deltas that matter (and refuted cross-check claims).**
- **Cross-check P0 REFUTED:** "OAS sends `budget_tokens` to claude-opus-4-8 → 400; cannot represent adaptive; effort in wrong place." `capabilities.ml:228-237` classifies opus-4-8/4-7 as `Anthropic_adaptive_only`; `backend_anthropic.ml:74-114` emits `thinking:{type:"adaptive"}` + `output_config:{effort}`, with `budget_tokens` only for `Anthropic_manual_budget`.
- **Gemini cross-check REFUTED:** `backend_gemini.ml:52-75` emits `thinkingLevel` XOR `thinkingBudget` gated by `Capabilities.gemini_thinking_control_of_id`, with a dated evidence comment (checked 2026-06-29).
- **DeepSeek V4 / GLM-5.2:** both accept only `high`/`max` (GLM default `max`). OAS's `Deepseek_high_or_max` alias is confirmed correct; GLM wiring (which `effort_alias_policy` variant) needs verification.
- **OpenAI GPT-5.5:** `none` is now a first-class effort distinct from disabling; `minimal` not accepted by GPT-5.5; `xhigh` is now officially documented (OAS's previously-flagged `xhigh` is vindicated).

**Verdict & required fixes.** No confirmed correctness break; boundary respected (no MASC reference; reasoning telemetry never gates). The typed closed-sum discipline is genuinely strong. PASS blocked by two real bar violations + lower-severity leaks:

| Fix | Priority | Location |
|-----|----------|----------|
| Consolidate the budget→effort threshold ladder into one SSOT — backends must consume the named constants (`low_budget_max_tokens`/`medium_budget_max_tokens`, plus a new `xhigh_budget_max_tokens=32768`) instead of re-hardcoding `2_048`/`8_192`/`32_768`. Eliminates 3 unlinked copies. | **P1** | `backend_anthropic.ml:48-52`, `backend_gemini.ml:45-49`, `reasoning_effort.ml:26-33` |
| Route Anthropic `effort_of_budget` through typed `Reasoning_effort.t` + an `effort_alias_policy` variant (mirroring `Deepseek_high_or_max`) rather than hand-rolled `low/medium/high/max` strings; attach a dated provider-doc evidence comment. Verify whether claude-opus-4-8 accepts `"max"` vs `"xhigh"` before shipping. | **P1** | `backend_anthropic.ml:48-52` |
| Make telemetry `reasoning_effort` symmetric — `reasoning_effort_of_config` returns `None` for all but Ollama even though OpenAI/GLM/DeepSeek emit a normalized effort on the wire; feed the actually-sent effort into telemetry for all providers. | **P2** | `provider_config.ml:380-388`, `backend_openai_request.ml:329-343` |
| Replace `Some "none"` string sentinel and the dead/test-only `normalize_effort` string classifier (leaks `"max"` as a free string) with typed `None`/`Disabled`; extract the duplicated `Medium` fallback literal to a named constant and reconsider reject-vs-warn on invalid `OAS_DEFAULT_REASONING_EFFORT`. | **P2** | `provider_config.ml:276-289`, `reasoning_dialect.ml:284-293` |

**Residual risk.** The Anthropic top-tier token `"max"` (`backend_anthropic.ml:51`) is unverified against any dated doc, unlike the Gemini path. If the correct opus-4-8 token is `"xhigh"`, then for adaptive-only models with `thinking_budget > 32768`, OAS emits `effort:"max"` and could get a 400 — narrowed from "every Anthropic request" (false) to "only top-tier effort on adaptive models" (plausible, unverified). Live effort vocabularies (Anthropic/OpenAI/GLM/Kimi/MiniMax) require fetching current docs. Secondary: model routing uses `String.starts_with` on `model_id` (`capabilities.ml:209-247`) — acceptable since model IDs are an open provider-controlled namespace, not an OAS-owned closed sum, but a new id silently falls through to `Anthropic_manual_budget` (budget_tokens), which would 400 if that future model also rejects budget_tokens.

---

### 2.4 Multi-turn — PARTIAL

**Established standard.** Each turn runs a 6-stage pipeline (Input/Parse/Route/Collect/Execute/Output), Execute dispatched only on `StopToolUse`. History is assembled immutably: append the full assistant content list in block order via `Util.snoc`, then a `role:Tool` results message, then any idle-nudge as a SEPARATE `role:User` message AFTER tool results (so `strip_orphaned_tool_results` never treats the nudge as an orphan boundary). Stop-reason reconciliation has a single SSOT in `Stop_reason_wire` enforcing `StopToolUse ⇒ has-tool-block` ("trust content over label"); a `tool_calls` finish with no tool block fails closed to `Unknown "tool_calls"`, and the driver rejects `Unknown` via typed `Error.Agent (UnrecognizedStopReason)` instead of looping. Execute takes a `Nonempty.t` (empty-tool case = compile error). Streaming blocks accumulate per index, finalized in sorted index order; thinking/signatures/redacted-thinking preserved across turns. Fail-closed guards dominate: phantom completion blocked when no stop_reason arrives; unknown block kinds → `Stream_parse_failed`; truncated turns drop partial tool calls. `stop_reason`, `wire_finish`, `block_kind` are closed sums with explicit `Unknown`/`Other` string variants.

**Latest-doc deltas that matter.**
- **GPT-5.5 Responses API** adds a per-assistant-item `phase` field (`commentary`/`final_answer`) that must be replayed unchanged; omitting it causes early stopping. OAS's block-order replay has no phase carrier.
- **DeepSeek V4** adds `finish_reason='insufficient_system_resource'` (transient/retryable backpressure). OAS would funnel it into `Other`/`Unknown` and hard-reject (`UnrecognizedStopReason`), turning a retryable signal into a fatal abort — the "reject unknown finish" policy is too coarse for infra-status codes.
- **Provider-divergent reasoning persistence:** Gemma 4 / gpt-oss require REMOVING prior CoT; DeepSeek V4 retains across tool turns only; Qwen3.7 gates behind `preserve_thinking` (off by default); Kimi K2.7 REQUIRES reasoning on the assistant tool-call message. OAS's uniform "always snoc the full content list incl. thinking" is wrong for several providers. (Gemini caveat: OAS has a native `backend_gemini.ml`, so the "OpenAI-compat drops the signature" premise may not apply — verify first.)

**Verdict & required fixes.** Core machinery is sound and fail-closed; no boundary violation, no hard crash → not FAIL. PASS blocked by code-confirmed defects:

| Fix | Priority | Location |
|-----|----------|----------|
| A complete (non-truncated) tool_use block with malformed JSON args must NOT silently become `Assoc []` and dispatch. Fail closed: return `Stream_parse_failed` or a typed parse failure so a tool never executes with silently-emptied arguments. Inconsistent with the sibling tool_result path (line 231) which uses typed `try_parse_json` returning `None`. | **P0** | `complete_stream_acc.ml:214-218` |
| Unify the unknown-model context-window fallback into ONE named SSOT instead of `128_000` (pipeline) vs `200_000` (builder) — `provider.ml:332-334` admits this disagreement is intentional. Pick a single conservative floor and route both compaction watermark and reducer budget through it; prefer surfacing "capability unknown" over a generous window (CTX-overrun risk, the #7083 case). | **P1** | `pipeline.ml:636` vs `builder.ml:256`, `provider.ml:332-342` |
| Replace the uniform "always preserve thinking every turn" policy with a per-provider, tool-conditional reasoning-retention policy at the provider boundary (Anthropic: round-trip by signature incl. omitted-display blocks; gpt-oss/Gemma: drop prior CoT). | **P1** | history assembly `pipeline.ml:259` |
| Missing `tool_id`/`tool_name` should not coerce to `""` (permissive empty identity into dispatch). Reject or carry as typed unknown. | **P2** | `complete_stream_acc.ml:204-213` |
| Stop overloading `block_tool_ids` Hashtbl to mean both real tool-use ids and the redacted_thinking carrier; add a dedicated carrier field. | **P2** | `complete_stream_acc.ml:22,179` |
| Extract magic numbers to named constants: `compact_attempts < 2` and the `0.5` watermark-remap constants. | **P2** | `pipeline.ml:1035,806-811` |
| Confirm against current docs whether infra-status finish codes (DeepSeek `insufficient_system_resource`) and post-cutoff carriers (GPT `phase`, Gemini per-call `thought_signature`, Opus `thinking.display=omitted`) are real; if so, distinguish retryable-backpressure finishes from terminal `Unknown` and add missing carriers. | **P2** | `pipeline.ml:577-626` |

**Residual risk.** The latest-doc cross-check (its own confidence 0.6) rests on post-cutoff models that must be confirmed against official docs per the currency policy before treating any as P0. Other latent risks: (a) the MaxTokens/`terminal_incomplete` branch (`complete_stream_acc.ml:182-202`) drops ALL tool_use blocks index-blind, discarding tool calls that completed before truncation — defensible fail-closed but over-broad data loss with no per-block completeness check; (b) OpenAI chat-completions interleaving is a synthesized heuristic order (reasoning<text<tools) that collapses a true text→tool→text wire order — an accepted wire-format limitation, not faithful replay.

---

## 3. Consolidated P0/P1 Fix Backlog (priority-ordered)

| # | Pri | Dimension | Fix | Location |
|---|-----|-----------|-----|----------|
| 1 | **P0** | Multi-turn | Complete tool_use block with malformed JSON args must fail closed, not coerce to empty args and dispatch. | `complete_stream_acc.ml:214-218` |
| 2 | **P1** | Tools | Remove/declare the hardcoded built-in alias string classifier; orphan targets `ReadFile`/`SearchFiles`/`Execute` are a MASC→OAS boundary leak. | `agent_tool_name_alias.ml:131-134` |
| 3 | **P1** | Tools | Replace name-based field guessing + hand-rolled shell tokenizer with schema-driven normalization or explicit reject. | `agent_tool_name_alias.ml:39,94,101,45-110` |
| 4 | **P1** | Tools | Add closed tool-kind variant (`json_object \| custom_freeform`) for GPT-5.5 freeform tools. | `tool_input_validation.ml`, Tool descriptor |
| 5 | **P1** | Thinking | Split `thinking_type` into typed kind tag + `signature:string option`; omit `"signature"` when `None`; stop fabricating `"thinking"`/`""`. | `types.mli:109-112`, `api_common.ml:57-60,139`, `complete_stream_acc.ml:172-177`, `streaming.ml:151-155` |
| 6 | **P1** | Thinking / Reasoning | Collapse the budget→effort heuristic into one typed SSOT; route Anthropic `effort_of_budget` through `Reasoning_effort.of_budget` + an alias-policy variant; represent `max` as a typed value. | `backend_anthropic.ml:48-53`, `reasoning_effort.ml:26-34` |
| 7 | **P1** | Reasoning | Consolidate the budget→effort threshold ladder; backends consume named constants (add `xhigh_budget_max_tokens=32768`) instead of re-hardcoding `2_048`/`8_192`/`32_768`. | `backend_anthropic.ml:48-52`, `backend_gemini.ml:45-49`, `reasoning_effort.ml:26-33` |
| 8 | **P1** | Multi-turn | Unify the unknown-model context-window fallback (`128_000` vs `200_000`) into one conservative SSOT; prefer "capability unknown" over a generous window. | `pipeline.ml:636`, `builder.ml:256`, `provider.ml:332-342` |
| 9 | **P1** | Multi-turn | Per-provider, tool-conditional reasoning-retention policy at the provider boundary (replace uniform "always preserve"). | `pipeline.ml:259` |

> **Note on overlap:** items #6 and #7 are the same root SSOT defect (the budget→effort ladder) surfaced from both the Thinking and Reasoning audits; treat as one remediation closing `backend_anthropic.ml:48-53` + `backend_gemini.ml:45-49` against `reasoning_effort.ml`.

**P2 backlog (deferred, all dimensions):** Tools — progressive-tools magic thresholds, coercion record, `supports_tool_choice` closed sum, `allowed_callers`. Thinking — retire WORKAROUND duplicate accumulator, Ollama `None` SSOT, derive inline dialect from catalog, extend `Reasoning_effort.t` with `none`/`max`. Reasoning — symmetric telemetry effort, remove `Some "none"` sentinel + dead string classifier, named `Medium` fallback. Multi-turn — reject empty tool identity, dedicated redacted-thinking carrier, extract magic numbers, distinguish retryable infra-status finish codes.

---

## 4. RFC Linkage & Boundary Check

- **Standards SSOT:** This verdict feeds **RFC-OAS-029** (PR #2232). All four dimensions resolve to **PARTIAL** — the established closed-sum / parse-don't-validate / fail-closed standard is ratified, with the P0/P1 backlog above to be absorbed as RFC-OAS-029 follow-up items (consistent with the workaround-rejection bar: defects like the orphan alias classifier and the duplicated budget→effort ladder are absorbed by RFC rather than patched as one-off site fixes).
- **Boundary rule "OAS does not know MASC" — CHECKED, holds with one called-out exception:**
  - **Tools:** no MASC reference in dispatch/recovery, but `agent_tool_name_alias.ml:131-134` is a **soft boundary leak** — it bakes a MASC-style downstream tool-naming convention (`ReadFile`/`SearchFiles`/`Execute`, orphan in `lib/`) into OAS core. Fix #2 closes this leak. This is the single boundary finding across all four dimensions and the reason the Tools verdict is not PASS.
  - **Thinking / Reasoning:** no MASC reference; `reasoning_tokens`/`reasoning_effort` are parse-only telemetry, aggregated and **never gating** budget, cost, or turn decisions.
  - **Multi-turn:** OAS does not reference MASC; the pipeline operates purely on provider wire types and OAS-owned closed sums.
