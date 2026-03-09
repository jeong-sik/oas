# Changelog

All notable changes to `agent_sdk` are documented here.

## [0.4.0] - 2026-03-09

### Added
- `Session` module for lifecycle metadata, resumable state, and hook-visible session IDs
- `Skill` module for markdown frontmatter parsing, prompt rendering, and supporting-file resolution
- `Subagent` module for typed subagent specs and conversion into handoff targets
- `Types.tool_kind` plus `Tool.create ?kind` / `Tool.create_with_context ?kind`
- permission-aware `Guardrails` with `permission_mode`, allow/ask/deny lists, additional directories, and output truncation
- new hook events for session lifecycle, permission requests, and subagent lifecycle
- `Agent.run_with_subagents` convenience API

### Changed
- `Agent.create` now accepts optional `session`
- `Handoff.handoff_target` can carry hooks, guardrails, and sessions into delegated sub-agents
- README and shipped surface are now aligned around the actual public modules and streaming support

## [0.3.0] - 2026-03-05

### Added
- `Handoff` module for multi-agent tool delegation
- `Context` module for shared key-value state across agents
- `Guardrails` module with tool visibility filters and turn limits
- SSE streaming support in `Api` with usage tracking
- API retry with exponential backoff and configurable max attempts

## [0.2.0] - 2026-03-01

### Added
- Provider abstraction: Anthropic, Local, OpenAI-compatible
- Basic hook system for turn and tool lifecycle
- Agent loop with tool execution and multi-turn conversation

## [0.1.0] - 2026-02-11

### Added
- Initial release: `Types`, `Api`, `Agent`, `Tool`
