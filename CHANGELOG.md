# Changelog

All notable changes to `agent_sdk` are documented in this file.

## [0.18.0] - 2026-03-14

### Added
- Harness-level `Conformance` module:
  - `Conformance.check`
  - `Conformance.report`
  - `Conformance.run`
- Deterministic conformance checks over `Sessions.get_proof_bundle` covering:
  - raw trace shape consistency
  - validated worker count consistency
  - latest worker / latest validated worker / latest failed worker consistency
  - trace capability consistency
  - validated worker raw-capability guarantees
- `oas_conformance_demo` executable for generating a machine-readable conformance report from a mock runtime session.

### Changed
- `0.18.0` treats `proof_bundle` as the canonical verifier input for session-level harness conformance.
- Consumers can now validate a session with a single `Conformance.run` call instead of re-assembling their own pass/fail logic around proof bundles.

## [0.17.0] - 2026-03-14

### Added
- Session-level worker-run evidence getters for summary-first consumers:
  - `Sessions.get_worker_runs`
  - `Sessions.get_latest_worker_run`
  - `Sessions.get_latest_completed_worker_run`
  - `Sessions.get_latest_failed_worker_run`
- Worker-run summaries now expose:
  - `worker_run_id`
  - `agent_name`
  - `trace_capability`
  - `validated`
  - `tool_names`
  - `final_text`
  - `stop_reason`
  - `error`
  - `started_at`
  - `finished_at`
  - `policy_snapshot`
- Raw trace validation now includes consumer-friendly verdict details:
  - `paired_tool_result_count`
  - `has_file_write`
  - `verification_pass_after_file_write`
  - `final_text`
  - `tool_names`
  - `stop_reason`
  - `failure_reason`

### Changed
- `Sessions.get_proof_bundle` now carries worker-run oriented summary fields:
  - `worker_runs`
  - `latest_worker_run`
  - `latest_validated_worker_run`
  - `latest_failed_worker_run`
  - `validated_worker_runs`
  - `raw_trace_run_count`
  - `validated_worker_run_count`
  - `trace_capabilities`
- Runtime participants persist `provider` and `model` into session state so worker evidence can be read without reconstructing it from raw records.
- Runtime mock workers now emit a minimal raw trace so dashboard-style consumers can use the same summary/validation surfaces as direct-agent runs.

## [0.16.0] - 2026-03-14

### Added
- Consumer-safe evidence hardening on top of `0.15.0`:
  - `Sessions.get_latest_raw_trace_run`
  - `Sessions.get_raw_trace_summaries`
  - `Sessions.get_raw_trace_validations`
- Proof bundle now carries validated summary shape:
  - `latest_raw_trace_run`
  - `raw_trace_summaries`
  - `raw_trace_validations`
  - `capabilities`

### Changed
- `Raw_trace` can now be created directly under a session-scoped `raw-traces/` directory with `create_for_session`.
- `Sessions.get_proof_bundle` is now sufficient as a summary-first evidence entrypoint for consumers that should avoid raw path parsing by default.
- `Builder.with_raw_trace` and top-level `create_agent ?raw_trace` remain available for direct-agent consumers.

## [0.15.0] - 2026-03-14

### Added
- Session-scoped raw trace evidence surface:
  - `Raw_trace.create_for_session`
  - `Raw_trace.read_runs`
  - `Raw_trace.summarize_run`
  - `Raw_trace.validate_run`
- `Sessions` raw trace getters:
  - `Sessions.get_raw_trace_runs`
  - `Sessions.get_raw_trace_run`
  - `Sessions.get_raw_trace_records`
  - `Sessions.get_raw_trace_summary`
  - `Sessions.validate_raw_trace_run`
- `Builder.with_raw_trace` and optional `raw_trace` support in top-level `create_agent`.

### Changed
- `Sessions.get_proof_bundle` now includes `structured_telemetry` and discovered `raw_trace_runs`.
- Session-scoped direct agent runs can now be discovered and validated without parsing `Checkpoint.messages`.
- Runtime store now reserves a stable `raw-traces/` directory under each session root.

## [0.14.0] - 2026-03-14

### Added
- Append-only agent-level raw trace capability for direct `Agent` runs:
  - `Raw_trace.create`
  - `Raw_trace.read_all`
  - `Raw_trace.read_run`
  - `Agent.last_raw_trace_run`
- `Agent.options.raw_trace` for attaching a JSONL raw trace sink to `run` / `run_stream` / resumed agents.
- Raw trace record types:
  - `run_started`
  - `assistant_block`
  - `tool_execution_started`
  - `tool_execution_finished`
  - `run_finished`

### Changed
- Direct `Agent.run_stream` tool loops can now emit immutable audit traces without changing `Checkpoint`, which remains the latest-state resume mechanism.
- `Builder` now carries the new `Agent.options.raw_trace` field with a default of `None`.

## [0.13.0] - 2026-03-14

### Added
- Structured telemetry schema alongside the legacy string-based telemetry surface:
  - `Sessions.structured_event_count`
  - `Sessions.structured_telemetry_step`
  - `Sessions.structured_telemetry`
  - `Sessions.get_telemetry_structured`
- Telemetry artifacts now include normalized `event_name` counts and per-step structured fields such as actor, role, provider, model, checkpoint label, and outcome.

### Changed
- Telemetry JSON keeps the legacy `kind` and raw `event_counts` output, but now also emits normalized event-name fields for stable downstream verification.
- Runtime tests now validate structured telemetry metadata rather than relying only on `kind` string substring checks.

## [0.12.0] - 2026-03-14

### Added
- Official session proof-bundle read APIs:
  - `Sessions.get_telemetry`
  - `Sessions.get_evidence`
  - `Sessions.get_proof_bundle`
- Typed proof-bundle surface in `Sessions` for telemetry step counts, evidence files, and combined session/report/proof retrieval.

### Changed
- Runtime proof and evidence artifacts can now be consumed through the SDK without relying on raw artifact-name lookups.
- Runtime tests now verify the public proof-bundle getters rather than parsing evidence JSON ad hoc.

## [0.11.0] - 2026-03-14

### Added
- Provider model-spec and capability registry:
  - `Provider.capabilities`
  - `Provider.model_spec`
  - `Provider.capabilities_for_model`
  - `Provider.capabilities_for_config`
  - `Provider.model_spec_of_config`
- Scoped cross-turn state in `Context`:
  - scopes: `App`, `User`, `Session`, `Temp`, `Custom`
  - helpers: `delete`, `snapshot`, `scoped_key`, `get_scoped`, `set_scoped`, `delete_scoped`, `keys_in_scope`, `diff`
- Artifact service and session artifact access:
  - `Artifact_service.list`
  - `Artifact_service.get_text`
  - `Sessions.list_artifacts`
  - `Sessions.get_artifact_text`
- MCP expansion beyond tools:
  - `Mcp.list_resources`
  - `Mcp.read_resource`
  - `Mcp.list_prompts`
  - `Mcp.get_prompt`
- Trace-driven evaluation:
  - `Trace_eval.summarize`
  - `Trace_eval.evaluate`
  - `Trace_eval.evaluate_flushed`
- Runtime evidence bundle generation:
  - telemetry JSON artifact
  - telemetry markdown artifact
  - evidence JSON artifact with persisted file hashes
- `oas_proof_demo` executable for reproducible non-test proof bundles

### Changed
- OpenAI-compatible request shaping is now capability-gated instead of relying on scattered provider/model conditionals.
- Qwen-only request fields (`top_k`, `min_p`, `chat_template_kwargs.enable_thinking`) are omitted for generic OpenAI-compatible providers.
- Checkpoints now persist agent context and restore it through `Agent.resume` and `Session.resume_from`.
- Runtime artifact metadata is now first-class:
  - `artifact_id`
  - `mime_type`
  - `size_bytes`
- Runtime proof checks are stronger and deterministic:
  - `terminal_event`
  - `seq_contiguous`
  - `artifact_ids_unique`

### Fixed
- Runtime finalize now persists telemetry/evidence artifacts even after the session has entered a terminal phase.
- MCP output truncation is budget-aware for tool and resource text responses.
- Session-level proof/demo evidence now includes artifact manifests with file sizes and MD5 digests.

## [0.10.0] - 2026-03-13

### Added
- Harness-first runtime layer with bundled `oas-runtime` subprocess, file-backed session journal, report/proof generation, and typed runtime protocol
- High-level `query` / `Client` surface on top of the runtime harness, with low-level `runtime_query` / `Runtime_client` escape hatches
- Session helpers for listing, reading, renaming, and tagging persisted runtime sessions
- Control-protocol callback round-trip for permission and hook requests
- `Contract` module for explicit runtime awareness, trigger context, tool grants, MCP allowlists, and skill bundles
- Builder helpers for contract-aware assembly: `with_contract`, `with_skill`, `with_skills`, `with_tool_grants`, `with_mcp_tool_allowlist`
- Long-lived interactive client semantics:
  - partial message surfacing (`Partial_message`)
  - progressive receive (`receive_messages`, `receive_response`, `wait_for_messages`)
  - resume/attach via `session_id` / `resume_session`
  - persisted session settings across reconnects

### Changed
- Default high-level local-first path is now `provider = Some "local-qwen"` and `model = Some "qwen3.5"` for `llama.cpp`-style local runtimes
- Runtime transport now uses a background reader thread to handle response, control, and event envelopes
- `set_permission_mode` and `set_model` now persist through runtime session updates instead of mutating SDK-local state only
- Builder now compiles explicit contracts into composed system prompts, filtered local tools, filtered MCP tool surfaces, and reserved context metadata

### Fixed
- Runtime protocol version mismatch is detected during initialize handshake
- Blank protocol lines are ignored on both SDK and runtime sides instead of surfacing as JSON parse failures
- Runtime tests now reflect asynchronous worker completion semantics rather than assuming synchronous spawn completion

## [0.9.1] - 2026-03-11

### Changed (breaking)
- `Types.tool_choice_of_json`: `(_, string) result` → `(_, Error.sdk_error) result`
- `Provider.resolve`: `(_, string) result` → `(_, Error.sdk_error) result`
- `.mli`: `module Retry : sig ... end` and `module Error : sig ... end` → module aliases for type equality

### Fixed
- `Checkpoint.of_json`: removed redundant `Result.map_error` bridge for `tool_choice_of_json` (now returns `sdk_error` directly)
- `Api.create_message`, `Streaming.create_message_stream`: pass through `Provider.resolve` error directly instead of re-wrapping

### Migration
- All SDK functions now return `(_, Error.sdk_error) result` — structured error migration is complete
- `Structured.schema.parse` retains `('a, string) result` (user-provided parser; wrapped to `sdk_error` at boundary)

## [0.9.0] - 2026-03-11

### Added
- `Error` module: 2-level structured error type hierarchy (`sdk_error`) replacing `(_, string) result` across the SDK
  - 7 domain-specific inner types: `api_error`, `agent_error`, `mcp_error`, `config_error`, `serialization_error`, `io_error`, `orchestration_error`
  - `agent_error.TokenBudgetExceeded` with `{ kind; used; limit }` for structured budget checks
  - `Error.to_string` for human-readable messages, `Error.is_retryable` for retry decisions
  - `Error.api_error` is a type alias for `Retry.api_error` (zero-cost reuse)

### Changed (breaking)
- `api.ml` split into `api_common.ml`, `api_anthropic.ml`, `api_openai.ml`, `api_ollama.ml` — public API (`Api.create_message`) unchanged
- `agent.ml` split: tool execution extracted to `agent_tools.ml`, handoff helpers to `agent_handoff.ml` — public API unchanged
- `Mcp_bridge` module removed from public API — use `Mcp.connect` and `Mcp.to_tools` instead
- 33 function signatures changed from `(_, string) result` to `(_, Error.sdk_error) result` across 14 modules
- `Api.create_message`: no longer flattens `Retry.api_error` to string; returns `Error (Api err)` preserving the structured error
- `Agent.check_token_budget`: returns `Error.sdk_error option` instead of `string option`
- `Streaming.create_message_stream`: returns `Error.sdk_error` with `Config (UnsupportedProvider _)` for non-Anthropic providers
- `Orchestrator.task_result.result`: error type changed from `string` to `Error.sdk_error`
- `Event_bus.AgentCompleted.result`: error type changed from `string` to `Error.sdk_error`
- `Checkpoint_store.create`: `Eio.Fs.dir_ty Eio.Path.t -> t` changed to `-> (t, Error.sdk_error) result`
- `Checkpoint_store.list`: `t -> string list` changed to `-> (string list, Error.sdk_error) result`

### Fixed
- Removed string prefix matching anti-pattern in `agent.ml` (was guessing error types from message text)
- `Api.create_message` no longer discards structured error information from retry layer
- `Checkpoint_store.create`: now returns result instead of silently ignoring `mkdirs` failure
- `Checkpoint_store.list`: now returns result instead of silently returning `[]` on `read_dir` failure
- `Retry.classify_error`: narrowed `with _ ->` to `Yojson.Json_error | Type_error` so non-JSON exceptions propagate

### Migration
- Tool handler interfaces (`Tool.t`) retain `(string, string) result` (user-provided handlers, not SDK errors)
- Use `Error.to_string` where string representation is needed

### Internal
- Test coverage baseline: 63.72% (1491/2340 points, bisect_ppx)

## [0.8.3] - 2026-03-11

### Changed
- `Mcp.t`: removed `mutable tools` field — `list_tools` is now pure, `to_tools` takes explicit `mcp_tool list` argument

### Changed (breaking)
- `Mcp.to_tools`: signature changed from `t -> Tool.t list` to `t -> mcp_tool list -> Tool.t list`

### Added (tests)
- `test_mcp_session.ml`: server_spec roundtrip, JSON serialization with env, reconnect_all empty case
- `test_otel.ml`: in-progress span JSON, flush/reset state, concurrent span creation

## [0.8.1] - 2026-03-11

### Fixed
- `Event_bus`: replaced raw `lock`/`unlock` with `Eio.Mutex.use_rw ~protect:true` and `use_ro` for exception safety
- `Mcp_session.reconnect_all`: return type now `Mcp.managed list * (info * string) list` to preserve error messages from failed connections
- `Checkpoint.of_json`: malformed `mcp_sessions` (non-array, non-null) now returns `Error` instead of silently defaulting to `[]`
- `test_event_bus.ml`: added missing `Eio_main.run` wrappers to 4 tests that used Eio primitives without a domain context

### Changed (breaking)
- `Mcp_session.reconnect_all`: return type changed from `Mcp.managed list * info list` to `Mcp.managed list * (info * string) list` — callers matching on the second element need to destructure the `(info, error_msg)` pair
- `Event_bus.filter_agent`: `Custom` events now pass through all agent-scoped filters (previously silently dropped because `Custom` had no `agent_name` field)

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
