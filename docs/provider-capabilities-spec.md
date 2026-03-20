# Provider Capabilities Specification

Defines what an agent SDK needs to know about an LLM provider/model
to make correct runtime decisions.

Based on analysis of 20+ models across 10 providers (March 2026).

## Current State (v0.71.0)

```ocaml
type capabilities = {
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_reasoning: bool;
  supports_response_format_json: bool;
  supports_multimodal_inputs: bool;
  supports_native_streaming: bool;
  supports_top_k: bool;
  supports_min_p: bool;
}
```

8 boolean fields. No numeric limits. `supports_reasoning` conflates
4 distinct mechanisms. `supports_multimodal_inputs` conflates 3 modalities.

## Proposed: 3-Tier Capability Model

### Tier 1: Limits (numeric, agent crashes without these)

| Field | Type | Why |
|-------|------|-----|
| `max_context_tokens` | `int option` | context_reducer needs a ceiling. Range: 128K-10M across models. None = unknown. |
| `max_output_tokens` | `int option` | agent_config.max_tokens must not exceed this. Range: 8K-128K. Silent truncation if exceeded. |

Discovery fills these for local models (`ctx_size` from `/props`).
Cloud providers need hardcoded lookup tables keyed by model_id.

### Tier 2: Feature flags (boolean, affects agent behavior)

| Field | Current | Change | Rationale |
|-------|---------|--------|-----------|
| `supports_tools` | exists | keep | |
| `supports_tool_choice` | exists | keep | |
| `supports_parallel_tool_calls` | missing | add | DeepSeek R1 has no tool calling at all. Cohere supports it. OAS has `disable_parallel_tool_use` but no capability check. |
| `supports_system_prompt` | missing | add | Most models support it, but some fine-tuned/distilled variants do not. Agent must fold system into first user message. |
| `supports_reasoning` | exists | **split into 4** | See Thinking Taxonomy below. |
| `supports_extended_thinking` | missing | add | budget_tokens control. Claude, Gemini, GPT-5.4. |
| `supports_think_tool` | missing | add | Anthropic-only. Mid-reasoning pause between tool calls. |
| `supports_adaptive_reasoning` | missing | add | Model auto-selects depth. Opus 4.6, Gemini 3 dynamic. |
| `supports_reasoning_budget` | missing | add | budget_tokens (Anthropic), reasoning_effort (OpenAI), thinking_level (Gemini). |
| `supports_structured_output` | missing | add | JSON schema 100% guarantee (OpenAI/Gemini) vs JSON mode (DeepSeek) vs via tool_use (Anthropic). |
| `supports_response_format_json` | exists | keep (subset of structured_output) | |
| `supports_caching` | missing | add | Prompt caching: Anthropic (90% savings), OpenAI, Gemini, DeepSeek. |
| `supports_computer_use` | missing | add | Claude, GPT-5.4 native. New tool category. |
| `supports_code_execution` | missing | add | Gemini built-in, OpenAI code_interpreter. Server-side sandbox. |
| `supports_native_streaming` | exists | keep | |
| `supports_multimodal_inputs` | exists | **split into 3** | See Multimodal Taxonomy below. |
| `supports_image_input` | missing | add | Most models except DeepSeek, Cohere. |
| `supports_audio_input` | missing | add | Gemini only (native audio tokens). |
| `supports_video_input` | missing | add | Gemini, Qwen 3.5 large. |
| `supports_top_k` | exists | keep | |
| `supports_min_p` | exists | keep | |

### Tier 3: Protocol (how to talk to the provider)

Already covered by `request_kind` and `Provider.provider` types.
Consider adding:

| Field | Rationale |
|-------|-----------|
| `Openai_responses` request_kind | OpenAI Responses API: server-side state, built-in tools, remote MCP. Distinct from chat completions. |
| `supports_remote_mcp` | OpenAI Responses API passes MCP servers as API parameters. |

## Thinking Taxonomy

`supports_reasoning: bool` is insufficient. 2025-2026 models have 4 distinct mechanisms:

| Mechanism | When | Control | Models |
|-----------|------|---------|--------|
| Extended thinking | Before response | `budget_tokens` | Claude, Gemini, GPT-5.4 |
| Think tool | Between tool calls | Declared as tool | Claude only |
| Adaptive reasoning | Automatic | None (model decides) | Opus 4.6, Gemini 3 |
| Interleaved thinking | Mixed with tool calls | Beta header | Claude only |

Proposal: keep `supports_reasoning` as the union (any of the above),
add specific flags for SDK features that depend on the distinction.

Minimum viable split:
- `supports_extended_thinking` — enables `thinking_budget` in agent config
- `supports_reasoning_budget` — enables cost-aware thinking control

## Multimodal Taxonomy

`supports_multimodal_inputs: bool` conflates 3 modalities with different
availability:

| Modality | Models |
|----------|--------|
| Image | Claude, GPT, Gemini, Qwen 3.5 large, Llama 4, Mistral, Grok |
| Audio | Gemini only (native tokens) |
| Video | Gemini, Qwen 3.5 large only |

Minimum viable split: `supports_image_input`, keep `supports_multimodal_inputs`
as the union. Audio/video can be added later.

## Model Capability Matrix (reference)

| Model | ctx | out | tools | parallel | thinking | structured | vision | cache |
|-------|-----|-----|-------|----------|----------|------------|--------|-------|
| Claude Opus 4.6 | 1M | 128K | Y | Y | adaptive | via tool | Y | Y |
| Claude Sonnet 4.6 | 1M | 64K | Y | Y | extended | via tool | Y | Y |
| GPT-5.4 | 1.05M | 128K | Y | Y | Y | schema | Y | Y |
| Gemini 3.1 Pro | 1M | 65K | Y | Y | dynamic | schema | Y+A+V | Y |
| Gemini 3 Flash | 1M | 64K | Y | Y | dynamic | schema | Y+A+V | Y |
| Qwen 3.5 (397B) | 262K | ? | Y | Y | Y | Y | Y+V | cloud |
| Qwen 3.5-35B | 262K | ? | Y | Y | Y | Y | N | N |
| Llama 4 Scout | 10M | ? | Y | ? | N | platform | Y | N |
| DeepSeek V3.2 | 128K | 8K | Y | ? | Y | JSON only | N | Y |
| DeepSeek R1 | 128K | 8K | **N** | N | CoT | N | N | Y |
| Mistral Large 3 | 260K | ? | Y | Y | N | Y | Y | Y |
| Mistral Small 4 | 256K | ? | Y | Y | Y | Y | Y | Y |
| Cohere Command A | 256K | 32K | Y | Y | N | Y | N | ? |
| Grok 4 | 2M | ? | Y | Y | Y | Y | limited | Y |
| GLM-5 | 200K | 128K | Y | ? | Y | Y | ? | ? |

## Implementation Plan

### Phase 1: Numeric limits + critical booleans
- Add `max_context_tokens`, `max_output_tokens` to capabilities
- Add `supports_parallel_tool_calls`, `supports_system_prompt`
- Add lookup table for known models (keyed by model_id prefix)
- Wire Discovery.ctx_size into capabilities

### Phase 2: Thinking split + structured output
- Split `supports_reasoning` → keep as union + add `supports_extended_thinking`
- Add `supports_structured_output`, `supports_caching`
- Add `supports_reasoning_budget`

### Phase 3: New modalities
- Add `supports_computer_use`, `supports_code_execution`
- Split multimodal → `supports_image_input` (keep union)

### Phase 4: Protocol evolution
- Add `Openai_responses` request_kind
- Add `supports_remote_mcp`

## Sources

- Anthropic Claude docs (platform.claude.com)
- OpenAI GPT-5.4 docs (developers.openai.com)
- Google Gemini docs (ai.google.dev)
- Qwen 3.5 blog (qwen.ai)
- Meta Llama 4 (llama.com)
- DeepSeek API docs (api-docs.deepseek.com)
- Mistral docs (docs.mistral.ai)
- Cohere docs (docs.cohere.com)
- xAI Grok docs (docs.x.ai)
- MCP specification 2025-11-25
- arxiv: SimpleTool (2603.00030), SelfBudgeter (2505.11274), Agent Skills (2602.12430)

Analysis date: 2026-03-20.
