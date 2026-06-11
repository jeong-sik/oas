# Per-Model Tool-Calling Gap Analysis & Design — masc + oas

> Status: analysis deliverable ("연구 분석 후 진행"). No code changed. PRs proposed below.
> Repos: oas `/Users/dancer/me/workspace/yousleepwhen/oas`, masc `/Users/dancer/me/workspace/yousleepwhen/masc`.

---

## A. Evidence Record

**Timestamp:** `<FILL: ISO8601 at write>` · **Author:** research+code agent · **Method:** primary-source code reads (file:line) cross-checked against June-2026 official-spec research bundle + fleet `runtime.toml`.

### Fleet ground truth (resolves the research's "two-layer" ambiguity)

Source: `/Users/dancer/me/.masc/config/runtime.toml`. **Every provider is OpenAI-compatible (`provider_d-http`) except Anthropic (CLI).**

| Fleet model (api-name) | Provider / transport | OAS route matched | Spec endpoint that applies |
|---|---|---|---|
| `deepseek-v4-flash` (**default**, line 53) | `ollama_cloud` `provider_d-http` (110) | `Provider_g_v4_flash` (489) | **Ollama-Cloud OpenAI-compat**, NOT DeepSeek-native |
| `deepseek-v4-pro` | `ollama_cloud` | `Provider_g_v4_pro` (491) | Ollama-Cloud OpenAI-compat |
| `qwen36-35b-a3b-mtp` | `runpod_mtp` llama-server (138-141) | `Qwen_3` (`qwen3` prefix, 556) | **llama-server (`--jinja`)** |
| `qwen3.5:397b` | `ollama_cloud` | `Qwen_3` (556) | Ollama-Cloud OpenAI-compat |
| GLM (`glm-*`) | `glm-coding` (Z.AI) `provider_d-http` | `Glm_*` (518-555) + `is_zai_glm_request` | Z.AI OpenAI-compat (coding/paas/v4) |
| Anthropic | `cli_tool_a` `provider_d-cli` (62) | `Agent_llm_a_*` | Anthropic Messages |

**Out-of-fleet (do not optimize against, flag as future):** DeepSeek-native `api.deepseek.com`, DashScope cloud, Kimi/Moonshot direct. The fleet never serves these today.

### Per-model spec confidence (from research bundle; 확인필요 NOT papered over)

| Model family | Overall conf | High-conf anchors | 확인필요 / low-conf (DO NOT assume) |
|---|---|---|---|
| Anthropic Claude | **high** | tool_choice object-only; `disable_parallel_tool_use` nested; thinking-block+signature replay; adaptive-only on Opus 4.8/4.7 | — |
| OpenAI baseline | high | `parallel_tool_calls` top-level default true; nested tool_choice object; no `reasoning_content` in Chat Completions | reasoning_effort value-set is model/version-specific |
| Ollama-Cloud | high | `tool_choice` UNCHECKED in compat table (not honored); native streams tool_calls complete in done chunk | tool_choice silent-drop-vs-error (doc-unconfirmed) |
| Qwen3.x | medium | `enable_thinking` is `chat_template_kwargs` not top-level; vLLM<0.9.0 enable_thinking=False+tools incompatible; llama.cpp needs `--jinja` | self-hosted tool_choice default; qwen3_xml vs qwen3_coder parser name |
| GLM (Z.AI) | medium | tool_choice "only supports auto"; thinking is `{type:enabled/disabled}` object; `tool_stream=true` required for streamed args (4.6+) | **`parallel_tool_calls` 확인필요 — no official source**; unsupported tool_choice error-vs-coerce undocumented |
| DeepSeek-V4 | medium | thinking+tools coexist on V4; `reasoning_content` (native) vs `reasoning` (Ollama) field mismatch; multi-turn reasoning replay or HTTP 400 | **tool_choice NOT honored on Ollama-Cloud layer**; `parallel_tool_calls` disable-knob 확인필요; no verbatim streamed example in DeepSeek docs |
| Kimi K2 | medium | `tool_choice=required` explicitly unsupported; thinking object only (no reasoning_effort); reasoning_content replay-or-error | `parallel_tool_calls` disable-knob undocumented; no official dedicated coding SKU id |

---

## B. Root anti-pattern: one diagnosis, two surfaces

**OAS keys capability flags on `model_id` prefix, but two of the audited axes — `thinking_control_format` and `supports_tool_choice` — are ENDPOINT-dependent, not model-dependent.** A model served across multiple wire endpoints (e.g. DeepSeek-V4 on Ollama-Cloud vs DeepSeek-native; Qwen on llama-server vs Ollama-Cloud vs DashScope) gets the wrong fields emitted for at least one endpoint. The model-id table cannot encode "which wire am I on."

This is the **per-model mirror of the masc dual-registry root anti-pattern**: two surfaces describing the same logical thing, drifting apart with no compiler/boot guard forcing them to agree.

- **masc dual-registry** (`keeper_report_state`): schema + typed vocab + handler + `core_always_tools` all present, but **no descriptor** → descriptor-driven dispatch returns `unknown_tool` on every call. One surface (discovery/policy) says "callable"; the other (descriptor dispatch) cannot route. No boot guard catches it (`tool_registration_check.ml:42-43` is a neutered placeholder).
- **oas split capability resolution**: `backend_openai_request.ml` resolves `caps` (drives thinking/top_k/min_p/seed/max_tokens) via `capabilities_of_config` (kind-fallback, :90-102) but resolves `supports_tool_choice` via a SEPARATE `Capabilities.for_model_id` lookup defaulting to **true on unknown** (:274-281). Two resolution paths for one record. Combined with the model-id-vs-endpoint mismatch, the same model can emit a thinking field its endpoint drops while a tool_choice field it never honors gets sent.

**Both are the same bug class: a typed SSOT that isn't single, with no exhaustiveness/boot check forcing the surfaces to agree.**

---

## C. Per-model gap table (spec vs code) — ACTUAL fleet

### C.1 DeepSeek-V4 (fleet DEFAULT, `ollama_cloud.deepseek-v4-flash`) — HIGH severity

Route `Provider_g_v4_flash`/`_pro` (capabilities.ml:633-666) sets `thinking_control_format = Thinking_object`.

- **GAP-1 (high): wrong thinking wire-format for the serving endpoint.** `Thinking_object` emits `{"thinking":{"type":"enabled"},"reasoning_effort":<effort>}` (backend_openai_request.ml:211-221). But the fleet serves DeepSeek-V4 via **Ollama-Cloud OpenAI-compat**, whose request-transform maps `reasoning_effort` → internal `Think` and exposes reasoning as field `reasoning` — it has **no mapping for a top-level `thinking` object** (research: Ollama compat drops unmapped fields silently). The `thinking` object is silently dropped; only the co-emitted `reasoning_effort` is honored. The correct fleet format is `Reasoning_effort` (same as `ollama_capabilities`, line 263), not `Thinking_object`.
- **GAP-2 (high): disabling thinking silently fails on the default model.** The disabled branch emits only `{"thinking":{"type":"disabled"}}` with **NO `reasoning_effort:"none"`** (backend_openai_request.ml:222). On Ollama-Cloud the `thinking` object is dropped → the model keeps thinking. Contrast the `Reasoning_effort` path which emits `reasoning_effort:"none"` on disable (line 256). Net: an operator who disables thinking on the fleet default gets reasoning anyway.
- **Parse side is correct (info):** `backend_openai_parse.ml:298-301` and `streaming.ml:285-288` already read both `reasoning_content` (DeepSeek-native) and `reasoning` (Ollama) — defensively covers the field-name mismatch. Keep.
- **확인필요:** `parallel_tool_calls` disable-knob has no official DeepSeek/Ollama source. `supports_parallel_tool_calls` on the route is unset (default false) — benign for emission, but do not claim it can disable.

### C.2 Qwen3.x (`qwen36-35b-a3b-mtp` via llama-server; `qwen3.5:397b` via Ollama-Cloud) — HIGH severity

Route `Qwen_3` (capabilities.ml:923-936). **The record does not set `thinking_control_format`** → defaults to `No_thinking_control` (confirmed: field absent in the record literal; default from line 108).

- **GAP-3 (high): no thinking control field emitted at all.** With `No_thinking_control`, backend_openai_request.ml:253 emits nothing (and the `is_zai_glm_request` sub-branch at :241 does not match Qwen). Spec (high-conf): Qwen3 self-hosted requires `chat_template_kwargs.enable_thinking` (llama-server / vLLM). So the fleet's primary Qwen model cannot have thinking toggled — it always runs in its default (thinking-on) mode, burning tokens on reasoning with no operator control. Correct format = `Chat_template_kwargs`.
- **GAP-4 (medium): three Qwen-family paths disagree on thinking format.** For ONE logical family OAS has three different formats: `dashscope_capabilities` = `Enable_thinking` (line 271); `DashScope_3` route = `Chat_template_kwargs` (line 619); `Qwen_3` route = `No_thinking_control` (923-936). The `DashScope_3` route is the only one that matches the self-hosted spec, yet the fleet's Qwen models hit `Qwen_3`, not `DashScope_3`.
- **GAP-5 (medium): `supports_tool_choice = true` on a self-hosted endpoint where the default is unconfirmed.** `Qwen_3` declares `supports_tool_choice=true` (line 929). Research: self-hosted vLLM/llama.cpp tool_choice default is doc-unconfirmed; llama-server needs `--jinja` for tool_choice to be honored at all. Emitting `tool_choice` is likely accepted but the *forcing semantics* are unverified for the runpod_mtp deployment. Lower-risk than thinking because masc only sends `Auto`.

### C.3 GLM (Z.AI coding plan) — mostly correct, collapse candidates

Routes `Glm_*` (518-555); all set `supports_tool_choice=false`. `is_zai_glm_request` (backend_openai_request.ml:104-107) drives the GLM thinking object via the `No_thinking_control` sub-branch (:241-252). Live serialization is `Backend_glm.build_request` → delegates to `Backend_openai.build_request` then post-processes (`tool_stream`, strip `chat_template_kwargs`, GLM thinking) — backend_glm.ml:99.

- **CORRECT (info):** tool_choice auto-only is honored — `supports_tool_choice=false` makes backend_openai_request.ml:274-287 drop any forced tool_choice. Matches Z.AI doc ("only supports auto"), spec high-conf. The code comment (capabilities.ml:285-292) cites the source and date.
- **CORRECT (info):** `tool_stream=true` gated on `stream && config.tool_stream` (backend_glm.ml:116) matches Z.AI's "tool_stream required for streamed args, 4.6+".
- **GAP-6 (low): `parallel_tool_calls` emission unverified for GLM.** backend_openai_request.ml:295-300 emits `parallel_tool_calls:false` whenever `disable_parallel_tool_use && tools_present`, for ALL openai-compat kinds incl. GLM. Research: GLM `parallel_tool_calls` is **확인필요 — no official Z.AI source**. Emitting it is likely a silent no-op (not a 400), so low severity, but it is an unverified field. Do not claim it disables parallelism on GLM.
- **Collapse candidate (verify-by-golden-diff):** `Glm_5_turbo` (784-795) and `Glm_full_text` (873-884) appear field-identical (200K/128K, tools, no tool_choice, reasoning, json, streaming). Most other GLM variants differ numerically (4.5-air 128K/96K, 4-flash 4096, vision variants add image) → keep.

### C.4 Ollama-Cloud (gpt-oss, deepseek, qwen via `ollama_cloud`) — correct

- **CORRECT (info):** `ollama_capabilities.supports_tool_choice=false` (line 260) matches the compat table (tool_choice UNCHECKED). `thinking_control_format=Reasoning_effort` (263) matches Ollama compat. The native `/api/chat` backend (`backend_ollama.ml`) defaults `think=false` and reads `message.thinking`. This is the reference for what the DeepSeek-V4 route SHOULD do (see GAP-1/2).

### C.5 Anthropic Claude (CLI fleet) — reference; latent SDK gaps

`backend_anthropic.ml` + `anthropic_capabilities` (133-164). Snapshot test (test_snapshot_provider_serialization.ml:187) confirms `disable_parallel_tool_use` is correctly nested inside the tool_choice object — matches spec #1 shape rule.

- **GAP-7 (medium, latent): adaptive-only thinking on Opus 4.8/4.7 not modeled.** `Agent_llm_a_opus_4` (564-569) uses `anthropic_capabilities` with `supports_reasoning_budget=true`. Spec high-conf: Opus 4.8/4.7 are adaptive-only — manual `{type:enabled,budget_tokens:N}` returns 400; `budget_tokens`/`temperature`/`top_p`/`top_k` are removed. If OAS emits `budget_tokens` for an Opus-4.8 config it 400s. Needs a per-model adaptive flag. (Verify backend_anthropic emission against `config.thinking_budget` before fixing.)
- **GAP-8 (medium, latent): thinking + forced tool_choice incompatibility not enforced.** Spec: with thinking enabled, only `tool_choice` auto/none work; any/tool → 400. OAS has no guard coupling these.

### C.6 OpenAI-compat baseline (`openai` provider) — correct

`openai_compat_chat_*` (195-222). `parallel_tool_calls` top-level, nested tool_choice object, `reasoning_effort` string. Matches baseline. Note `Provider_d_4o`/`_4_1`/`_5` routes look like a keep (distinct context/output ceilings + computer_use on 5).

### C.7 Kimi K2 (out-of-fleet, latent)

- **GAP-9 (medium, latent): `kimi_capabilities.supports_tool_choice=true` (line 176) but spec says Kimi rejects `tool_choice=required`.** `Any → "required"` (backend_openai.ml:63). If any consumer sent `Any`/`Tool` to Kimi it would error. **Confirmed: no consumer sends `Any`/`Tool` to Kimi** — masc sends only `Auto` (runtime_agent_context.ml:192), and no other `with_tool_choice Any/Tool` call site exists in oas/masc outside tests. So this is latent at the SDK layer, not triggered by the fleet. Fix should coerce `required`→`auto` for Kimi or set a sub-flag.

---

## D. Target architecture

### D.1 Endpoint-aware capability resolution (single typed SSOT)

The fix for GAP-1..5 is structural, not per-flag patching. `thinking_control_format` and `supports_tool_choice` must be resolved against **(model_family × wire_endpoint)**, not model_id alone.

- Introduce an explicit `wire_endpoint` discriminator (e.g. `Ollama_cloud_compat | Llama_server | Zai_compat | Deepseek_native | Dashscope_*`) derived from `Provider_config.kind` + `base_url`, and make `thinking_control_format`/`supports_tool_choice` a function of `(family, endpoint)`. The fleet today is almost entirely `provider_d-http` so the endpoint is usually inferable from `base_url`/`kind`.
- **Concrete immediate fix** (smaller, fleet-correct): change `Provider_g_v4_*` routes to `thinking_control_format = Reasoning_effort` (they are only ever served via Ollama-Cloud today); set `Qwen_3` to `Chat_template_kwargs`. This makes the model-id table emit the right field for the as-deployed endpoint. The full endpoint-discriminator refactor is the durable version.
- **Unify the two capability-resolution paths** in backend_openai_request.ml: `supports_tool_choice` should read from the same `caps` binding as thinking, not a parallel `for_model_id` lookup with a different unknown-default. Keep the documented "unknown → assume tool_choice supported" intent as an explicit field on the resolved record, not a second lookup.

### D.2 Capabilities.ml as the typed SSOT

`capabilities.ml` already is the typed SSOT (exhaustive `static_model_route` variant + `capabilities_of_static_model_route`). Preserve that. The refactor is: (a) add the endpoint axis, (b) factor shared GLM records while **keeping variant names** (model identity matters for routing/logging — collapsing names would lose telemetry granularity), (c) delete genuinely-dead presets only with fan-in proof.

### D.3 Golden request-body harness as the keep/collapse discriminator

`test/test_snapshot_provider_serialization.ml` exists but is insufficient as a per-model proof:

- It uses synthetic `oas-snapshot-fixture-model` (matches no capability prefix) + `supports_tool_choice_override:true`. So `glm_any_expected` (line 263) pins `tool_choice:"required"` emitted — the **opposite** of production GLM (caps `supports_tool_choice=false` → dropped). The harness pins the overridden path and masks capability-gated behavior.
- **Extend it:** add per-fleet-model fixtures keyed on REAL model ids (`deepseek-v4-flash`, `qwen36-35b-a3b-mtp`, `glm-4.7`, `qwen3.5:397b`) with **no `supports_tool_choice_override`**, asserting the full request body (thinking field name, tool_choice present/absent, parallel_tool_calls, stream/tool_stream). This is the discriminator: record-identical + byte-identical body → collapse/factor-shared-base; differs in emitted fields → keep. It is also the regression fence proving the GAP fixes.

### D.4 Where masc must stop duplicating provider decisions

**Pushback on the task framing:** the evidence does NOT show masc maintaining an independent capability table. masc `provider_tool_support.ml` **derives** every capability from OAS (`capabilities_for_provider_config`, :108) and layers only (a) runtime-MCP delivery-lane + identity-header overlay, (b) CLI normalization (`normalize_cli_caps_when`, :92-94) which is **inert** because `is_cli_agent_provider` returns false unconditionally (:76-79). `supports_inline_tool_choice = caps.supports_tools && caps.supports_tool_choice` (:175) is a pure projection, not a second table. **No split-brain table found.**

- The only real cruft is the inert CLI-normalization override — `normalize_cli_caps_when` + `is_cli_agent_provider` are dead-but-referenced; flag as compiler-guided-removal candidate, not a deletion target.
- masc sets `tool_choice = Auto` unconditionally when tools present (runtime_agent_context.ml:192). This is **benign** — `Auto` is universally accepted (incl. GLM auto-only). Do not "fix" it.
- masc legitimately owns: delivery mode (inline vs runtime-MCP vs drop, runtime_transport.ml:225-232) and identity-header bridging — concerns OAS does not model. Keep.

---

## E. keeper_report_state production bug (separate fast masc hotfix)

**Decision: REGISTER (restore descriptor), not remove.** `state_report_result_json` (keeper_tool_task_runtime.ml:348-389) does real work — persists a forward-looking progress snapshot consumed by 5 readers across cycle boundaries (keeper_world_observation_continuity.ml:51, keeper_tool_memory_runtime.ml:461, keeper_turn.ml:274, keeper_post_turn.ml:461, memory.ml:115). Fleet logs show ~40 calls/day returning `unknown_tool` AND accruing toward the failure circuit-breaker (Runtime_failure, dispatch_runtime.ml:241-248).

- **Root fix (not symptom-suppression):** add the one missing `task_descriptor "report_state" "keeper_report_state" ... ~readonly:false` in keeper_tool_descriptor.ml (~:1021-1060), routing identically to the 8 sibling task tools. The header comment says "9 tools" but only 8 entries exist — the off-by-one is the fingerprint.
- **Close the class:** restore a boot-time guard (replace the neutered `tool_registration_check.ml:42-43` placeholder) asserting every `all_keeper_tool_schemas` name has a descriptor `internal_name`. Without it, any future keeper schema added without a paired descriptor is silently undispatchable.
- This is urgent and independent of the oas per-model work; ship it first as a small PR.

---

## F. Open questions (확인필요 / needs live probe or user input)

1. **DeepSeek disable-thinking on Ollama-Cloud (live probe):** confirm that `{"thinking":{"type":"disabled"}}` is dropped and reasoning continues, vs `reasoning_effort:"none"` actually disabling. Drives GAP-2 severity.
2. **DeepSeek-native path (out-of-fleet):** if the fleet ever adds `api.deepseek.com`, thinking=`thinking` object + `reasoning_content` replay-or-400 applies — different from the Ollama-Cloud path. Endpoint discriminator must distinguish.
3. **`parallel_tool_calls` disable-knob on DeepSeek / GLM / Kimi:** all 확인필요, no official source. Is OAS emitting `parallel_tool_calls:false` a silent no-op or a 400 on these? Live probe.
4. **Ollama `tool_choice` silent-drop vs error:** doc-unconfirmed. OAS sidesteps by never sending it (correct), but the contract assumption should be probed.
5. **`dashscope_capabilities` (Enable_thinking) reachability:** the fleet has no DashScope provider; this preset is reachable only via `kind=DashScope` fallback. Is it dead-in-fleet, and should it be deleted or kept for future DashScope onboarding? User input.
6. **Qwen llama-server `--jinja` + reasoning-parser version:** is runpod_mtp's llama-server started with `--jinja` and `--reasoning-parser qwen3` (vLLM≥0.9.0 equivalent)? GAP-3 fix (`Chat_template_kwargs`) assumes `--jinja` is on.
7. **Anthropic Opus 4.8/4.7 adaptive-only emission:** verify whether backend_anthropic emits `budget_tokens` for an Opus-4.8 config (GAP-7) before fixing — needs reading backend_anthropic build_request.
8. **GLM `Glm_5_turbo` vs `Glm_full_text` collapse:** confirm byte-identical via golden-diff before factoring.
