# Changelog

All notable changes to `agent_sdk` are documented in this file.

## [0.3.1] - 2026-03-10

### Added
- Prompt caching: `cache_system_prompt` config option wraps system prompt with `cache_control` ephemeral
- `api_usage` record type with `cache_creation_input_tokens` and `cache_read_input_tokens` fields
- `usage_stats` accumulates cache token counts across turns

### Changed
- **BREAKING**: `api_response.usage` type changed from `(int * int) option` to `api_usage option`
- **BREAKING**: `add_usage` accepts `api_usage` record instead of two ints
- `usage_stats` cache fields renamed to `total_cache_creation_input_tokens` / `total_cache_read_input_tokens` for consistency
- `create_agent` convenience function now accepts `?cache_system_prompt`

### Known Limitations
- Streaming mode (`create_message_stream`) reports cache tokens as 0. SSE event types still use `(int * int) option` for usage. Full streaming cache support planned for v0.4.0.

## [0.3.0] - 2026-03-05

### Added
- Fleet Orchestration: `Fleet` module with member selection, parallel execution (#747)
- `Handoff` module for multi-agent tool delegation (#747)
- `Context` module for shared key-value state across agents (#747)
- `Guardrails` module with tool filtering (AllowList, DenyList, Custom) and turn limits (#747)
- `Streaming` module with SSE event parsing and usage tracking (#747)
- API retry with exponential backoff and configurable max attempts (#749)

### Changed
- Provider config extended with `max_retries` field (#749)

## [0.2.0] - 2026-03-01

### Added
- Provider abstraction: Anthropic, Local (OpenAI-compatible), OpenAICompat (#730)
- Hook system: `before_api_call`, `after_api_call`, `before_tool_use`, `after_tool_use` (#730)
- Agent loop with tool execution and multi-turn conversation (#730)

## [0.1.0] - 2026-02-11

### Added
- Initial release: Types, API client, basic agent loop
