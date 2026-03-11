# Changelog

All notable changes to `agent_sdk` are documented in this file.

## [0.8.0] - 2026-03-11

### Added
- `Agent.clone`: deep-copy agent state with fresh or copied context, shared net/tools/options
- `Context.copy`: shallow-copy context hashtable
- `Structured.extract_stream`: SSE streaming for structured output extraction with schema validation
- `Event_bus`: typed publish/subscribe for agent lifecycle events (Eio.Stream per subscriber)
  - 7 event types: AgentStarted, AgentCompleted, ToolCalled, ToolCompleted, TurnStarted, TurnCompleted, Custom
  - 3 built-in filters: `filter_agent`, `filter_tools_only`, `accept_all`
  - Integrated into Agent.options, Orchestrator.config, Builder.with_event_bus
- `Mcp_session`: persistent MCP session capture/restore for checkpoint/resume cycles
  - `capture`/`capture_all`: serialize server specs and discovered tool schemas
  - `reconnect_all`: re-establish MCP connections from saved info
- `Checkpoint` v1 to v2 migration: added `mcp_sessions` field with backward compatibility
- `Mcp.managed`: added `spec` field to preserve original server_spec for reconnection

### Changed
- Agent: `run_turn`, `run_turn_stream`, `find_and_execute_tool` publish events to event_bus
- Orchestrator: `run_task` publishes AgentStarted/AgentCompleted events
- Version bump: 0.7.1 -> 0.8.0 (27 modules, 584 tests)

## [0.7.1] - 2026-03-11

### Fixed
- `Otel_tracer`: all mutable span operations (`start_span`, `end_span`, `add_event`, `add_attrs`, `flush`, `reset`) protected with `Stdlib.Mutex`
- `Otel_tracer.start_span`: ID generation moved outside critical section to reduce lock contention

### Changed
- README: architecture table expanded to 25 modules, version synced to 0.7.1
- `Mcp.initialize`, `Mcp_bridge.initialize`: client_version updated to 0.7.1
- CHANGELOG: added `[0.7.0]` section for previously undocumented changes

### Removed
- `Random.self_init()` calls in `Session` and `Otel_tracer` (unnecessary on OCaml 5.x domain-local PRNG)

## [0.6.0] - 2026-03-11

### Added
- `Agent.resume`: restore agent from a `Checkpoint.t` with messages, usage, turn count, model, and config
- `Session.resume_from`: create a new session linked to a checkpoint's session_id via `resumed_from`
- MCP server lifecycle management: `Mcp.server_spec`, `Mcp.managed`, `Mcp.connect_and_load`, `Mcp.connect_all`, `Mcp.close_all`

### Changed
- MCP client: replaced self-contained implementation with `mcp-protocol-sdk` v0.10.0 wrapper (PR #29)
- `Mcp_bridge`: added Eio-native MCP client bridge module (PR #24)
- Version bump: 0.5.0 -> 0.6.0

## [0.5.0] - 2026-03-10

### Added
- `Tracing` module: observability via `TRACER` module type with `Null_tracer` (zero-allocation no-op) and `Fmt_tracer` (stderr output)
- `with_span` exception-safe RAII pattern for span lifecycle management
- Agent API calls and tool executions wrapped with tracing spans
- Human-in-the-Loop: `ApprovalRequired` hook decision variant with `approval_callback` type
- `approval_decision` type: `Approve`, `Reject of string`, `Edit of Yojson.Safe.t`
- `Context_reducer` module: message windowing with turn-boundary grouping
- `keep_last` strategy: retain last N turn groups
- `token_budget` strategy: approximate token-based windowing (4-char heuristic)
- `custom` strategy: user-provided `message list -> message list` function
- `group_into_turns`: respects ToolUse/ToolResult pairing constraint
- `find_and_execute_tool` helper: eliminates code duplication in tool execution

### Changed
- `Agent.t` record: added `tracer`, `approval`, `context_reducer` fields (all optional with defaults)
- `hook_decision` type: added `ApprovalRequired` variant (non-breaking: exhaustive match warning only)
- Context reducer applies as a view before API calls; full history preserved in agent state

### Migration Guide
- `hook_decision` match expressions will emit warning 8 for missing `ApprovalRequired` case. Add the case or use wildcard.
- New `Agent.create` optional params: `?tracer`, `?approval`, `?context_reducer` (all default to no-op/None)

## [0.4.0] - 2026-03-10

### Added
- `Structured` module: typed structured output extraction via tool_use + tool_choice=Tool pattern
- Token budget tracking: `max_input_tokens` and `max_total_tokens` fields in `agent_config`
- Token budget enforcement in agent run loop (per-turn check before API call)
- Property-based tests using QCheck (model/role/param_type round-trip, usage commutativity)
- Test coverage for structured output extraction and token budget logic

### Changed
- **BREAKING**: `MessageStart` and `MessageDelta` SSE event usage type changed from `(int * int) option` to `api_usage option`
- Streaming SSE parser now extracts `cache_creation_input_tokens` and `cache_read_input_tokens` from `message_start` events
- `create_message_stream` accumulates cache tokens in usage stats

### Migration Guide
- `MessageStart { usage = Some (inp, out) }` → `MessageStart { usage = Some { input_tokens; output_tokens; cache_creation_input_tokens; cache_read_input_tokens } }`
- `MessageDelta { usage = Some (inp, out) }` → same pattern
- New `agent_config` fields have `None` defaults (backward compatible for config construction)

## [0.3.2] - 2026-03-10

### Added
- `Session` module for conversation persistence (save/load JSON)
- `Skill` module for reusable agent capability bundles
- `Subagent` module for spawning child agents with isolated context
- Test hardening: 106+ unit tests across 15 test files
- `replace_tool_result` bug fix in `Agent` module

### Changed
- README synced to v0.3.1 feature set (prompt caching status corrected)

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
