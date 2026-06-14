# Provider Reasoning Dialects

Date: 2026-06-14

## Purpose

OAS owns provider and transport behavior. MASC and other agent runtimes should
consume typed OAS facts instead of matching model names or provider-specific
strings when they decide how to display, replay, pause, or interrupt reasoning.

`Llm_provider.Reasoning_dialect` is the typed surface for those facts. It is
derived from the existing capability catalog and provider defaults, so it does
not introduce a second model registry.

## Current Dialects

| Capability wire format | Dialect meaning | Replay policy |
| --- | --- | --- |
| `Thinking_object` | DeepSeek-style top-level `thinking` object, optional `reasoning_effort`, side-channel `reasoning_content` | Drop reasoning between plain user turns, preserve reasoning after assistant tool calls |
| `Thinking_object_only` | Top-level `thinking` object without effort | No mandatory replay yet |
| `Chat_template_kwargs` | Self-hosted chat-template kwargs such as Qwen `enable_thinking` / `preserve_thinking` | No mandatory replay yet |
| `Chat_template_token` | Template token injection such as Gemma `<\|think\|>` | No mandatory replay; parse visible thought channel from generated text |
| `Reasoning_effort` | OpenAI-compatible `reasoning_effort` field | No mandatory replay yet |
| `Enable_thinking` | DashScope-style top-level `enable_thinking` | No mandatory replay yet |
| `Anthropic_thinking` | Claude Messages API `thinking` blocks | Preserve thinking blocks in history; Claude filters relevant blocks |
| `Gemini_thinking_config` | Gemini native `generationConfig.thinkingConfig` and thought parts/signatures | Preserve tool-call-linked thought signatures |

## Evidence

- DeepSeek official docs: thinking defaults to enabled, `low` and `medium`
  efforts map to `high`, `xhigh` maps to `max`, sampling parameters are ignored
  in thinking mode, and `reasoning_content` must be replayed after tool-call
  turns. Source: <https://api-docs.deepseek.com/guides/thinking_mode>,
  checked 2026-06-14.
- Gemma official docs: Gemma 4 thinking is enabled through chat-template
  control (`enable_thinking=True` in the processor), generated output contains
  a thought channel plus answer content, and parsing requires keeping special
  tokens. Source: <https://ai.google.dev/gemma/docs/capabilities/thinking?hl=ko>,
  checked 2026-06-14.
- Claude official docs: extended thinking uses `thinking` blocks, and during
  tool use those blocks must be passed back unchanged; Claude filters relevant
  blocks. Source:
  <https://platform.claude.com/docs/en/build-with-claude/extended-thinking>,
  checked 2026-06-14.
- OpenAI official docs: reasoning models expose reasoning controls and prior
  reasoning state can be preserved with `previous_response_id` or by manually
  passing reasoning items forward. Source:
  <https://developers.openai.com/api/docs/guides/reasoning>, checked
  2026-06-14.
- Gemini official docs: Gemini exposes `thinkingConfig`, `thinkingBudget`, and
  optional thought summaries marked on response parts. Source:
  <https://ai.google.dev/gemini-api/docs/thinking>, checked 2026-06-14.
- DashScope/Qwen official docs: OpenAI-compatible Qwen thinking uses
  `enable_thinking`, optional `thinking_budget`, side-channel
  `reasoning_content`, and `preserve_thinking` for carrying historical
  assistant reasoning forward. Source:
  <https://help.aliyun.com/zh/model-studio/deep-thinking>, checked 2026-06-14.

## Boundary

This module does not schedule tool calls, pause keepers, or decide whether a
user interruption should preempt a running turn. It only exposes provider
semantics. Agent runtimes should use these facts to build their own control
loops without copying provider-specific rules into MASC.
