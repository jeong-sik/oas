# Changelog

All notable changes to `agent_sdk` are documented in this file.

Historical note: release notes for `0.100.3`, `0.100.5`, and `0.100.6` were
backfilled on 2026-04-04 from existing git tags. The dates below reflect the
original tag dates. `0.100.4` was never tagged or released.

## [0.135.0] - 2026-04-14

### Added
- `Agent.save_journal t path` — thin wrapper over
  {!Durable_event.save_to_file}. Returns `Error "no journal"` when the
  agent has no journal attached (#894).
- `Builder.with_auto_dump_journal ~path` — installs an
  `on_run_complete` callback that dumps the journal on every run
  completion. Creates a fresh journal if one is not attached so the
  dump is never empty (#894).

## [0.134.0] - 2026-04-14

### Added
- `Durable_event` JSONL persistence primitives:
  `save_to_file` (atomic tmp+rename) and `load_from_file`
  (missing → empty journal, malformed → line-numbered error) (#892).

### Changed
- Version bump to 0.134.0 after integrating the full Durable_event
  journal stack from 0.133.0.

## [0.133.0] - 2026-04-14

### Added
- `Durable_event` integration across agent runtime (#890, #891):
  - `agent_types.options.journal` field + `Builder.with_journal`.
  - `pipeline.ml` appends `Turn_started`, `State_transition`,
    `Llm_request`, `Llm_response`, `Error_occurred`,
    `Checkpoint_saved` (proactive + emergency).
  - `agent_tools.ml` appends `Tool_called` / `Tool_completed` with
    idempotency keys suitable for replay deduplication.
- `Durable_event.create ?on_append` fan-out callback (#891).
- `Journal_bridge.make ~bus` projects every journal event onto
  `Event_bus.Custom ("durable:<kind>", payload)` — Event_bus
  subscribers observe the full journal stream without payload
  schema changes (#891).

## [0.132.0] - 2026-04-13

### Added
- `Diag` module in `llm_provider` — pluggable structured diagnostic logging
  with level filtering and consumer-replaceable sink. Replaces 27 raw
  `Printf.eprintf` calls across 6 files.

## [0.131.0] - 2026-04-13

### Removed
- `Provider.local_qwen`, `Provider.local_mlx` deprecated aliases.
- `vote` type, `vote_request`, `Vote` command, `Vote_recorded` event
  (dead code: projection ignored votes, field never populated).
- `"local-qwen"` provider resolver string alias.
- `session.votes` field from runtime session record.

### Added
- Diagnostic logging in `cascade_health_filter.ml` for provider
  filtering decisions (API key drops, cloud-only fallback).
- Debug log on `event_forward.ml` event_bus unsubscribe failure.

## [0.126.0] - 2026-04-13

### Added
- `Context_reducer.cap_message_tokens` strategy to cap per-message token
  budgets while preserving recent turns and inserting a truncation marker
  when the middle is dropped.
- Swiss Verdict JSON schema v1 at `docs/schemas/swiss-verdict.schema.json`.
- `Harness.verdict_to_json`, `Harness.swiss_verdict_to_json`, and
  `Eval.run_metrics_to_json` for schema-aligned evaluation export.

### Changed
- Checkpoint delta metrics now use the `oas.checkpoint.*` namespace instead
  of flat `checkpoint_delta_*` names.

## [0.125.0] - 2026-04-13

### Removed
- `Api.named_cascade` type, constructor, `create_message_named`,
  `create_message_named_stream`, and `Builder.with_named_cascade`.
  Cascade FSM moved to MASC in Phase 2; this removes the dead OAS residue.
- `config/cascade.json` — cascade config is now MASC-owned SSOT.
- `Agent.t.named_cascade` field and `?named_cascade` parameter from
  `Agent.create` and `Agent.resume`.

### Changed
- `Tool_selector.default_rerank_fn`: replaced `~named_cascade` parameter
  with explicit `~cascade_name`, `~defaults`, and `?config_path`.

## [0.122.1] - 2026-04-12

### Fixed
- Anthropic: Messages API conformance — tool_choice nesting + thinking gate (#835)
- Anthropic: serialize temperature/top_p/top_k in build_body_assoc (#834)
- Anthropic: capabilities_for_model consults for_model_id (#825)
- Gemini: capabilities.supports_top_k = true (#833)
- Anthropic: capabilities.supports_top_k = true (#832)
- OpenAI: route silent capability drops through warn helper (#831)
- OpenAI: gate min_p/top_k on capabilities.supports_* (#830)
- MCP: truncate_output uses CJK-aware token estimator + UTF-8 boundary (#828)
- Ollama: send keep_alive as integer when value parses as int (#821)
- Builder: derive context_thresholds fallback from provider capabilities (#823)

### Changed
- Extract canonical CJK-aware text estimator to llm_provider (#829)
- Drop dead Gemini/GLM exception catches in create_message (#836)
- Add n > 0 guard to effective_max_context (#826)

## [0.122.0] - 2026-04-12

### Added
- Capabilities: OAS_OLLAMA_SUPPORTS_TOOL_CHOICE env override (#801)
- Cascade: per-entry api_key_env override in cascade config (#817)
- Agent: emit per-turn timing line to stderr for budget diagnosis (#816)
- Complete: log base_url + capture body on HTTP 5xx (#814)
- Ollama: pin keep_alive=-1 by default to prevent model eviction (#813)

### Fixed
- Pipeline: derive proactive_context_window from model capabilities (#815)
- Pipeline: remove per-LLM-request Eio.traceln spam for Unspecified priority default (#799)
- Cascade: apply per-model timeout to last provider (#805)

### Changed
- Discovery: drop legacy `OAS_LOCAL_QWEN_URL` env fallback; emits a one-time migration warning when the legacy var is set (#818)

## [0.121.0] - 2026-04-11

### Added
- Multi-vendor inference providers: Groq, DeepSeek, Alibaba, SiliconFlow (#789)
- Process-wide global registry for Metrics.t (#807)
- on_http_status hook for provider response counters (#804)

## [0.120.0] - 2026-04-11

### Fixed
- Retry: treat hard-quota 429s as non-retryable (#798)
- Retry: flat-error-string extraction (#795)

## [0.118.1] - 2026-04-09

### Changed
- `oas-review` now reports the shared SDK version instead of a stale hardcoded `0.46.0` string.
- `swarm_plan_cache` mode decoding now uses explicit `result` handling instead of exception-driven control flow while preserving legacy serialized mode names.

## [0.117.0] - 2026-04-09

### Added
- Runtime `glm:auto` expansion now yields ordered GLM execution fallbacks for named completions, streaming completions, and local capacity selection. Closes #738.
- `glm:auto` now expands to a multi-model GLM cascade during config/model resolution instead of a single default model. Closes #736.

### Changed
- `Constants.Inference_profile.agent_default` and `worker_default` now use `max_tokens = 16_384`; endpoint docs also clarify that port `8085` is the llama.cpp default. Closes #737.

### Fixed
- OpenAI-compatible requests now send explicit `Content-Length` headers to avoid chunked-encoding rejection by strict upstreams. Closes #735.
- `backend_openai` omits `tool_choice` for models that do not advertise tool-choice support, preserving backward compatibility for unknown models. Closes #725.
- Legacy tool parameter lists are normalized into JSON Schema before OpenAI-compatible/Ollama tool serialization. Closes #734.
- `complete_http` adds pre-flight JSON-body validation and richer 4xx diagnostics for malformed provider payloads. Closes #726.
- Cascade diagnostic logs now quote field values and gate debug output behind `OAS_CASCADE_DIAG`. Closes #724.
- Retry follow-up cleanup tightened wording, allocation behavior, and inline coverage around `retry.ml`. Closes #723.

## [0.111.0] - 2026-04-07

### Added
- `ollama` provider in cascade registry. `ollama:auto` routes to Ollama MLX
  server at `OLLAMA_HOST` or `http://127.0.0.1:11434`. Always available (no
  API key required). OpenAI-compatible endpoint, 262k max context.

## [0.110.0] - 2026-04-07

### Added
- `Agent.make_extend_turns_tool` with public `Agent.t` type for runtime turn extension. Closes #674.
- Slot pinning for llama-server KV cache reuse via `slot_id` parameter in sync and streaming paths. Closes #672.
- Per-model timeout for non-last cascade providers (default 1200s). Closes #679.
- End-to-end test proving context_injector to LLM delivery chain. Closes #673.

### Fixed
- 4 design debt items from boundary audit: removed stale references, aligned provider abstractions. Closes #667, #668, #669, #670.
- Preserve `Context.t` identity in `context_with_contract` to prevent hooks from reading empty data on resume. Closes #676.
- Sync `model_endpoints` before cascade parsing so `endpoint_for_model` can route `llama:model_id` correctly. Closes #677.

## [0.109.0] - 2026-04-06

### Added
- 17 new tool descriptor mappings for HITL (`ask_user_question`), web (`web_fetch`, `web_search`), task management (`task_create/update/list/get/output/stop`), team (`team_create/delete`), browser interaction (`navigate`, `computer`, `find`, `form_input`, `javascript_tool`, `tabs_create_mcp`, `upload_image`), and notebook (`notebook_read`). Addresses #646.
- Inline tests for `Mode_enforcer.builtin_descriptor` and `Mcp_schema.descriptor_for_builtin_tool`.

## [0.108.0] - 2026-04-06

### Added
- `max_turns` field in `BeforeTurnParams` hook event. Consumers can compute remaining turns without duplicating max_turns tracking. Closes #658.

## [0.107.0] - 2026-04-06

### Changed
- A2A `message_part_to_yojson` outputs v1.0 Part shape with `type` discriminator field. Text: `{"type":"text","text":"..."}`. File: nested `{"type":"file","file":{...}}` with `bytes`/`uri`. Data: `{"type":"data","data":{...}}`. Legacy format parsing preserved in `of_yojson`. Closes #591.

## [0.106.0] - 2026-04-06

### Added
- `Constants.Inference_profile.worker_default` (temp=0.2, max_tokens=4096) and `deterministic` (temp=0.0, max_tokens=4096) inference profiles for downstream coordinators.
- `Budget_strategy.context_metrics` type and constructor — aggregates usage ratio, compression phase, and limit proximity into a single value.
- Lifecycle transition guards (A2A pattern): `Lifecycle.validate_transition` enforces valid state machine transitions.
- Collaboration and Plan transition guards with typed error reporting.

## [0.105.0] - 2026-04-06

### Changed
- Consolidated hardcoded endpoint URLs (`http://127.0.0.1:8085`, `http://localhost:8085`) into `Constants.Endpoints` module. All production and test code references the SSOT constants instead of string literals. Prevents port drift across files (prior art: #557).
- `Constants.Anthropic.prompt_cache_min_chars` extracts the prompt caching threshold (3500 chars) from `backend_anthropic.ml`.
- `provider.ml` local URL detection replaced magic number 16 with `String.length`-based prefix check.
## [0.104.0] - 2026-04-06

### Added
- `Mode_enforcer.builtin_descriptor` derives `Tool.descriptor` from the builtin registry. Read-only tools get `Parallel_read`, mutation tools get `Sequential_workspace`, shell/external tools get `Exclusive_external`. Consumers can query descriptors for built-in tools without hardcoding.
- `agent_tools.concurrency_class_of_tool` now falls back to `builtin_descriptor` when a tool has no attached descriptor, enabling correct parallelization of read-only built-in tools.

## [0.103.0] - 2026-04-06

### Added
- `Tool.permission` type (`ReadOnly | Write | Destructive`) for per-tool side-effect metadata. Added as optional field on `Tool.descriptor`. Consumers use this to decide approval policy per tool instead of applying uniform approval to all tools.
- `Tool.permission` and `Tool.is_read_only` accessor functions.
- `permission` included in `descriptor_to_yojson` output.

## [0.102.0] - 2026-04-06

### Added
- `Agent_types.options.priority` field (`Request_priority.t option`). Enables the resume path to set scheduling priority without requiring a full `agent_config` override. When set, overrides `config.priority` on the resumed agent. Builder path already uses `Builder.with_priority`.

### Fixed
- Resume path no longer defaults to `Unspecified` priority when the caller intends a specific scheduling class (fixes #616).

## [0.101.0] - 2026-04-05

### Added
- `Tool_index.entry.aliases` field for additional BM25 tokens (e.g. Korean keywords). Aliases are appended to name+description during tokenization. Existing code using `{ name; description; group }` must add `aliases = []`.
- `TopK_bm25.confidence_threshold` parameter. When the top BM25 score falls below this threshold, `fallback_tools` are unioned with the results. `None` disables fallback. Distinct from `Tool_index.config.min_score` which filters individual documents.
- `TopK_bm25.fallback_tools` parameter for curated tool subsets to include on low-confidence queries.

### Changed
- `Tool_index.entry` record now requires `aliases` field (source-breaking for direct record construction).
- **BREAKING**: `TopK_llm` variant now uses `rerank_fn` closure instead of `selector_config`. The stub (`failwith "Phase 3"`) has been replaced with a working 2-stage implementation: BM25 pre-filter then LLM reranking via caller-injected closure.
- `TopK_llm` strategy: `select` may perform I/O via `rerank_fn`. Not idempotent for this strategy. `always_include` provides deterministic lower bound.
- `default_rerank_fn` provided for `Cascade_config`-based LLM reranking. Captures Eio resources in closure.

## [0.100.7] - 2026-04-04

### Added
- Shared inference-contract helpers in `Provider` for provider/model/modality/task resolution without forcing existing `model_spec` callers to migrate.
- `Context_overflow` CDAL result status for infra-level provider errors.

### Fixed
- Internal tool retry policy now preserves non-recoverable failures and clamps invalid retry counts before retry scheduling.

## [0.100.6] - 2026-04-04

### Added
- `PolicyChannel` for lazy tool policy propagation to spawned agents.

### Fixed
- Removed MASC-specific tool names from `tool_index` test fixtures to keep OAS boundary tests SDK-local.

## [0.100.5] - 2026-04-04

### Added
- `Response_harness` for deterministic text and tool-output extraction from model responses.
- Real A2A HTTP binding with `tasks/sendSubscribe` SSE streaming.
- `Inference_profile` as the single source of truth for inference defaults.
- `Tool_index` scoped retrieval and rebuild APIs for dynamic tool-set updates.
- Hook-level `system_prompt_override`, policy decision lineage, and a fail-closed decision validity matrix.

### Changed
- `Agent_tools` now treats `Exclusive_external` tool batches as a barrier-separated execution class.
- CDAL mode enforcement uses descriptor-driven classification instead of hardcoded heuristics.
- Turn pipeline now prunes in-memory tool results and enforces a message cap between turns.

### Fixed
- Hook tool-filter overrides are intersected with operator policy instead of widening access.
- Discovery now reports actual Ollama context size, and SDK-facing surfaces no longer leak MASC-specific references.

## [0.100.3] - 2026-04-04

### Added
- Cascade slot-full fallthrough for load distribution across providers.

### Changed
- Local endpoint defaults are consolidated behind `Discovery.default_endpoint`.
- HTTP mock servers in tests now use ephemeral ports instead of fixed allocations.

### Fixed
- Capability filtering fails closed on unknown limits instead of silently allowing them.
- Runtime rejects unknown provider names instead of coercing them to `Local`.
- Pricing distinguishes unknown models from genuinely free providers.
- Eio cancellation and runtime mutex handling were hardened around shutdown paths.

## [0.100.2] - 2026-04-03

### Added
- Tool execution scheduling metadata is now recorded in raw traces and CDAL proof-store tool traces.

### Changed
- `Hooks.PreToolUse`, `Hooks.PostToolUse`, and `Hooks.PostToolUseFailure` now carry `tool_use_id` plus deterministic schedule metadata.
- `Raw_trace.Tool_execution_started` records `planned_index`, `batch_index`, `batch_size`, and `concurrency_class`.

## [0.100.1] - 2026-04-03

### Added
- `Tool.concurrency_class` metadata for explicit tool execution contracts.

### Changed
- `Agent_tools.execute_tools` now schedules `Parallel_read` tools in parallel batches and runs workspace or external tools sequentially.
- Tools without declared concurrency metadata now default to conservative sequential execution.

## [0.99.8] - 2026-04-01

### Added
- Turn-level slot yielding for swarm efficiency (#538).
- Explicit permit API for turn-level slot yielding (#536).
- Custom tokenizer support for BM25 tool index (#540).
- OpenAI `auto` model resolution in cascade config (#541).

### Changed
- `Log` module globals use `Atomic.t` for data-race-free reads (#543).

## [0.99.7] - 2026-03-31

### Changed
- Remove 586 lines of redundant inline tests from `memory.ml` (821 → 209 lines). Covered by alcotest suites.
- Remove 8 dead re-exports and 1 internal-only function that were only consumed by the removed inline tests.

## [0.99.6] - 2026-03-31

### Changed
- Extract `Cascade_health_filter` and `Cascade_executor` from `cascade_config.ml` (983 → 721 lines). No public API changes.

## [0.99.5] - 2026-03-31

### Fixed
- Cascade no longer tries remaining providers on local resource exhaustion (EADDRNOTAVAIL, EMFILE, ENOBUFS). Saves wasted connection attempts when ephemeral ports or FDs are depleted.

## [0.99.4] - 2026-03-31

### Fixed
- Deduplicated `env_or` in `cascade_model_resolve.ml` (2 inline closures → 1 module-level function).
- Event forward idle-path optimization: skip `List.map`/`rev_append` on empty drain.
- Replace O(n) structural list comparison with O(1) counter check at loop exit.

## [0.99.3] - 2026-03-31

### Fixed
- O(n²) batch accumulation in `event_forward.ml` — `(@)` replaced with `List.rev_append`.
- Consolidated duplicate `Sys.getenv_opt` patterns in `review_agent.ml` and `mcp.ml` to use `Defaults` helpers.

## [0.99.2] - 2026-03-31

### Added
- `Defaults.int_env_or` / `Defaults.float_env_or` — typed env var helpers for int and float values.
- `OAS_MCP_HTTP_URL` env var — configurable MCP HTTP default endpoint.
- `OAS_REVIEW_MODEL` env var — configurable review agent model selection.
- `OAS_AGENT_MAX_RETRIES`, `OAS_AGENT_INITIAL_DELAY`, `OAS_AGENT_MAX_DELAY` env vars — configurable swarm retry policy.

### Removed
- `Event_forward.Webhook` variant — was unimplemented (always `failwith`). Use `Custom_target` for HTTP delivery.

### Fixed
- Periodic callback exceptions now logged via `Printf.eprintf` instead of silently swallowed.
- Fatal exceptions (`Out_of_memory`, `Stack_overflow`, `Sys.Break`) re-raised in periodic callbacks.

## [0.99.1] - 2026-03-31

### Added
- `Swarm_plan_cache` module — convergence trajectory caching and warm-start for swarm runs (#508).
  - `structural_fingerprint` — deterministic hash of config structure (agents, roles, mode, convergence params).
  - `template_of_state` — captures quality signals and per-agent scores from converged runs.
  - `hints_of_template` / `apply_hints` — tighter iteration bounds and agent reordering (Decentralized only).
  - `make_recording_callbacks` — opt-in integration via `on_converged` callback composition.
  - `fs_backend` — filesystem-backed cache with atomic writes.
- `Swarm_plan_cache.agent_score_to_json` / `agent_score_of_json` — public score serialization.
- `Checkpoint` delta support (#498) — `compute_delta` / `apply_delta` with full restore fallback.
- `Stub_tool_results` reducer strategy for observation masking (#500).
- Swarm checkpoint fidelity gate — `converged` flag preserved in checkpoint v2 (#499, #505).

### Fixed
- `Custom_role` JSON roundtrip — strip inner quotes from `ppx_deriving.show` output (#508).
- GLM default base URL changed to `api.z.ai` (#501).
- Provider mock token counts made explicit (#507).

## [0.98.0] - 2026-03-30

### Added
- `Slot_scheduler.snapshot` — non-blocking point-in-time capacity snapshot.
- `Slot_scheduler.try_with_permit` — non-blocking slot acquisition returning `'a option`.
- `Provider_throttle.capacity_source` type — `Discovered | Fallback` discriminator.
- `Provider_throttle.source`, `snapshot`, `try_permit`, `queue_length`, `max_concurrent` getters.
- `Cascade_throttle.capacity_info` type with `process_*` prefix for process-scoped counts.
- `Cascade_throttle.capacity` — single-endpoint capacity query.
- `Cascade_config.local_capacity_for_selections` — cascade-selection-scoped capacity query with cold start probe via `~sw ~net`.

### Changed
- `Provider_throttle.of_discovery_status` now tags result with `source = Discovered`.
- `Cascade_throttle.populate` promotes Fallback entries to Discovered when probe returns slot data.

## [0.97.0] - 2026-03-30

## [0.96.0] - 2026-03-30

### Added
- `Slot_scheduler` module — priority-aware slot scheduling with Eio.Mutex + Eio.Promise (#483).
- `Provider_throttle.with_permit_priority` — priority-aware permit acquisition.
- `Unspecified` variant in `Request_priority` — logs warning, dispatched as Proactive.
- `Request_priority.resolve` — resolves Unspecified to Proactive with warning.
- `priority` field in `Types.agent_config` — flows through Agent.run pipeline to cascade throttle.
- `?priority` parameter in `Api.create_message_named` and `create_message_named_stream`.

### Changed
- `Provider_throttle` now uses `Slot_scheduler` internally instead of `Eio.Semaphore`.
- Higher priority requests (Interactive) are dequeued before lower priority (Background) when slots are full.

## [0.95.0] - 2026-03-30

### Added
- `Request_priority` module — `Interactive | Proactive | Background` priority type for LLM completion scheduling (#478).
- `?priority` parameter added to all completion and cascade functions.
- `to_yojson`/`of_yojson` converters for `Request_priority`.

### Changed
- `complete_stream_cascade` now correctly threads `?priority` through to `complete_stream`.

## [0.94.0] - 2026-03-29

### Added
- Single-agent vs swarm baseline evaluation harness for comparing agent performance (#474).
- Operator-level tool policy — declarative allow/deny rules applied before tool dispatch (#473).

### Changed
- Pin CI and MCP SDK to OCaml 5.4.1 rollout (#472).
- Declare explicit scope limitations in project documentation (#471).

### Fixed
- Sync execution state back to original agent after contract runner handoff (#467).

## [0.93.2] - 2026-03-29

### Added
- Add `Swarm_types.agent_entry.extensions` so downstream consumers can attach explicit pass-through metadata to swarm entries.
- Add read-side `Proof_store` APIs for artifact ref resolution, JSON/JSONL reads, manifest/contract loading, and run listing.
- Annotate all 186 `.mli` files with explicit stability tiers and document the tier policy in `docs/api-stability.md`.

### Changed
- Move GitHub Actions off Node 20 by upgrading `actions/checkout` to `v5` and `actions/upload-artifact` to `v6`.

### Fixed
- Replace MASC-specific coordination keywords in `Context_intent` with coordinator-agnostic generic terms.
- Make `Fs_result.ensure_dir` recursive as documented and reject proof-store artifact refs with traversal-style `run_id` values.

## [0.93.1] - 2026-03-28

### Fixed
- Refactor swarm convergence bookkeeping into explicit state transitions so Eio async control flow no longer depends on ad-hoc `ref` accumulators.
- Make streaming mailbox draining cooperative and replace swarm round-robin selection's global `ref` counter with `Atomic.t`.

## [0.92.2] - 2026-03-27

### Changed
- Raise `mcp_protocol`, `mcp_protocol_eio`, and `mcp_protocol_http` minimum versions to `>= 1.0.0`, and pin CI's `mcp-protocol-sdk` checkout to the 1.0.1 SHA.

### Fixed
- Log swarm metric-evaluation failures to stderr and clarify ignored mailbox/usage paths in the runner to make convergence-loop debugging easier.

## [0.92.1] - 2026-03-27

### Fixed
- Replace mutable `event_forward` counters/state with `Atomic.t` and stop reporting unimplemented webhook delivery as success.
- Improve HTTP client error context for hostname/TLS setup failures and preserve `Eio.Cancel.Cancelled` during socket cleanup.
- Build MCP tool-result test fixtures via JSON parsing so local newer MCP SDK pins and CI's older schema both pass.

## [0.92.0] - 2026-03-26

### Changed
- `Swarm_types.metric_source` now uses `Argv_command of string list` for process metrics, and `Runner.eval_metric` executes argv directly via `Eio.Process.parse_out`.
  Migration: replace `Shell_command "cmd --flag value"` with `Argv_command ["cmd"; "--flag"; "value"]`.

### Fixed
- `scripts/release.sh` now validates version consistency against `lib/sdk_version.ml` and `agent_sdk.opam`, matching the current version single-source-of-truth layout.

## [0.91.2] - 2026-03-26

### Fixed
- Guard `Fiber.fork` bodies against exception leaks in resilience paths.
- Widen `cohttp-eio` 6.2 connect return typing and sync version metadata.
- Isolate outbound HTTP connections with per-request `Switch.run`.
- Replace `Unix.sleepf` with `Eio.Time.sleep` in tests.
- Replace blocking swarm metric process handling with `Eio.Process`.

### Changed
- Eliminate `assert false` and `failwith` from strict error handling paths.
- Remove mutable state and `ref` usage from trajectory and metrics.

## [0.91.1] - 2026-03-26

### Fixed
- Temp file resource leak in 6 cascade_config inline tests and 3 test files (Fun.protect pattern).
- `eval_baseline.load` used failwith instead of direct Result return.
- `event_forward` batch loop O(n) `List.length` check replaced with O(1) counter.
- `succession.metrics_of_json` inconsistent float parsing (try-with to to_float_option).

## [0.89.0] - 2026-03-24

### Added
- **Reflexion module** (`Reflexion`): act-evaluate-reflect-retry loop primitive based on MAR (Multi-Agent Reflexion) pattern. Separated concerns: Act/Diagnose/Critique/Aggregate. Integrates with Episodic memory for reflection persistence. `Reflexion.run` executes the loop, `format_reflection` formats failed verdicts, `on_stop_evaluator` wraps as hook adapter. 11 tests.
- **Clear_tool_results strategy** (`Context_reducer`): replaces processed tool results in older turns with short markers, preserving tool_use_id for API consistency. Safest and lightest form of context compaction (Anthropic recommendation).
- **PreCompact hook event** (`Hooks`): new lifecycle event emitted before context compression. Carries `messages`, `estimated_tokens`, and `budget_tokens` for pre-compaction intervention.
- `pre_compact` field in `Hooks.hooks` record.
- **Tool_index module** (`Tool_index`): pure OCaml BM25 implementation for dynamic tool exposure. Indexes tool descriptions at startup, retrieves top-K relevant tools per query. Confidence gate for automatic fallback. Group co-retrieval. 8 inline tests.
- **Retrieval_based strategy** (`Progressive_tools`): BM25-indexed tool retrieval per turn context. Extracts query from last user message. Falls back to full catalog below confidence threshold. `always_include` for essential tools.
- **Durable_event module** (`Durable_event`): event-sourced agent loop journal. 9 event types (Turn_started, Llm_request/response, Tool_called/completed, State_transition, Heartbeat, Checkpoint_saved, Error_occurred). Idempotency keys for replay-safe tool execution. Heartbeat lease detection. Replay summary for crash recovery. Full JSON round-trip. 14 tests.

### References
- MAR: Multi-Agent Reflexion (arxiv:2512.20845)
- Anthropic "Effective Context Engineering" (tool result clearing)
- ACON: Optimizing Context Compression (arxiv:2510.00615)
- ITR: Dynamic Tool Exposure (arxiv:2602.17046)
- Diagrid "Still Not Durable" (event sourcing gap analysis)

## [0.78.0] - 2026-03-20

### Added
- `max_context` field in `Provider_registry.entry` — default context window size per provider.
- Usage helpers in `Types`: `zero_api_usage`, `usage_of_response`.
- `cc` (Claude Code) provider integrated into `reg` pattern with `max_context:200_000`.

## [0.77.0] - 2026-03-20

### Added
- **Plan module** (`Plan`): goal decomposition into ordered steps with dependency DAG. Step lifecycle (Pending/Running/Done/Failed/Skipped), re-planning, progress tracking, JSON serialization. Complementary to Durable (Plan = what to do, Durable = how to execute reliably). 20 tests.
- `examples/plan_execute_demo.ml`: 4 scenarios (linear deployment, re-planning after failure, serialization, dependency graph).

### Design decisions
- Norm module deferred to MASC. Norms are inter-agent (social), not intra-agent. Principle: "agent itself = OAS, MASC = consumer".

## [0.76.0] - 2026-03-20

### Added
- **Governance layer**: runtime governance for agent decisions.
  - `Policy`: priority-ordered rule evaluation at 6 decision points (BeforeToolCall, BeforeHandoff, BeforeResponse, ResourceRequest, BeforeMemoryWrite, Custom) with 4 verdicts (Allow, Deny, AllowWithCondition, Escalate). 11 tests.
  - `Audit`: immutable log of policy decisions with capacity eviction, query filters, JSON export. 11 tests.
  - `Durable`: typed step chains with execution journal for crash recovery. Execute/resume/suspend with retry. JSON round-trip. 20 tests.
- `test_governance_integration.ml`: 9 integration tests (Policy+Audit, Durable+Audit, full governance flow).
- `examples/governance_demo.ml`: 6 scenarios (tool governance, handoff escalation, resource budget, durable pipeline, resume, serialization).
- `Fs_result`: Result-based filesystem operations replacing scattered try/with patterns.
- `Memory_tools`: agent-facing memory store/recall/forget tools.
- `Verified_output`: phantom-typed compile-time output verification (`unverified`/`verified` type tags).
- `Memory_access`: deny-by-default agent-scoped permission layer (prefix-based key patterns).
- Gemini native backend: `contents/parts` wire format, `thinkingConfig`, `functionCall/Response`, SSE streaming. 30 tests.
- Inline tests via `ppx_inline_test` for 13 modules.

### Changed
- 5 files migrated to `Fs_result`, removing 143 lines of duplicated I/O.
- `Thread.create` replaced with `Eio.Fiber.fork` in runtime server.
- `Digest.string` replaced with stable hash; 18 bare catch-all patterns eliminated.

## [0.75.0] - 2026-03-20

### Added
- **5-tier Memory**: Episodic (time-decaying salience, interaction history) and Procedural (pattern matching, success/failure tracking with confidence) memory tiers added to the existing Scratchpad/Working/Long_term model. 17 new tests.
- **.mli 100% coverage**: All 133 modules now have API contract files (.mli). 49 new .mli files across 6 PRs. Dead code discovered and removed during the process.
- **Coverage 83%**: 44 new test files covering structured output, runtime_client, transport, API, MCP, streaming, orchestrator, swarm, and more.

### Changed
- `Memory.stats` returns `int * int * int * int * int` (added episodic and procedural counts). **Breaking change** from 3-tuple.
- `Memory.tier` variant extended with `Episodic` and `Procedural`.
- `eval_collector.ml`: removed unnecessary `mutable` on `start_time` field.

## [0.71.0] - 2026-03-19

### Added
- **Named Cascade API**: `Api.named_cascade`, `Builder.with_named_cascade` — named cascade profiles integrated into Agent/Builder layer. Pipeline routes through `Cascade_config.complete_named` with automatic failover.
- `.mli` API contracts for 13 modules: runtime_projection, transport, a2a_task, mcp, conformance, direct_evidence, runtime_server, runtime_store, runtime_evidence, agent_config, otel_tracer, streaming, internal_query_engine. Total .mli coverage: 61 -> 74/128 (48% -> 58%).
- Stream accumulator tests (23 cases): `create_stream_acc`, `accumulate_event`, `finalize_stream_acc`, `map_http_error`.
- Coverage tests for cache, pipeline, streaming, structured modules.

### Changed
- **Exception handling**: 16 catch-all `| exn ->` patterns narrowed to specific types across 10 files. `async_agent.ml` now re-raises `Out_of_memory`/`Stack_overflow`/`Sys.Break` (bug fix).

## [0.70.0] - 2026-03-19

### Added
- `.mli` API contracts for 5 core modules: `harness.mli` (170L), `context_reducer.mli` (71L), `eval.mli` (126L), `checkpoint.mli` (62L), `session.mli` (51L). Total .mli count: 56 -> 61.

### Changed
- No new features. Stabilization release for v1.0 preparation.

## [0.69.0] - 2026-03-19

### Added
- **Provider Registry** (`Provider_registry`): extensible provider catalog with `register`/`unregister`/`find`/`available`/`find_capable`. Pre-populated with 5 known providers (llama, claude, gemini, glm, openrouter). Formalizes the hardcoded `known_providers` list from `Cascade_config`.
- **Capability Filter** (`Capability_filter`): composable predicates for capability-aware provider selection. `requires_tools`, `requires_streaming`, `requires_reasoning`, `requires_all`/`requires_any` combinators.

## [0.68.1] - 2026-03-19

### Fixed
- **Guardrails_async**: fix Eio cancellation swallowing — `try ... with _ -> ()` replaced with dedicated `Eio.Switch.run`. Removed unused `~sw` parameter from `run_input`/`run_output`/`guarded` (breaking API change, pre-v1.0).
- **Builder**: add `max_cost_usd >= 0.0` validation in `build_safe`. Negative budget no longer silently triggers `CostBudgetExceeded`.

### Added
- `runtime.mli`: public API contract for wire protocol types. Documents collaboration field migration to `Collaboration.t`. (v0.66 formalization)

## [0.68.0] - 2026-03-19

### Added
- **Eval baseline** (`Eval_baseline`): golden-file JSON comparison for regression detection. `save`/`load`/`compare` with tolerance-based metric diffs. `pass_at_k` metric. (#v0.68)
- **Eval report** (`Eval_report`): structured report combining baseline comparison, harness verdicts, and pass@k. JSON + human-readable output. (#v0.68)

## [0.67.0] - 2026-03-19

### Added
- **Async guardrails** (`Guardrails_async`): parallel input/output validation via `Eio.Fiber.all`. `input_validator`/`output_validator` types. `guarded` combinator gates LLM call on input validation, runs output validators post-response. (#v0.67)

## [0.65.0] - 2026-03-19

### Added
- **Working memory** (`Memory`): 3-tier facade over `Context.t`. Scratchpad (per-turn), Working (cross-turn), Long_term (external callback). `store`/`recall`/`forget`/`promote`. Fallback recall across tiers. (#v0.65)

## [0.62.0] - 2026-03-19

### Added
- **Cost tracker** (`Cost_tracker`): USD budget enforcement via `agent_config.max_cost_usd`. `check_budget` returns `CostBudgetExceeded` when exceeded. Structured `cost_report` with per-call averages.
- **Context offload** (`Context_offload`): large tool results (>threshold) written to filesystem, replaced with path + preview. Fail-open: on write failure, original content preserved.
- `Error.CostBudgetExceeded` agent error variant with `spent_usd`/`limit_usd` fields.
- `Builder.with_max_cost_usd` for chainable agent configuration.
- `agent_config.max_cost_usd: float option` field.

### Changed
- `Agent.run_loop` now checks cost budget alongside token budget before each turn.

## [0.61.0] - 2026-03-19

### Added
- `agent_config.initial_messages`: seed agent conversations with prior history on first run. (#214)
- **Streaming cascade** (`Complete.complete_stream_cascade`, `Cascade_config.complete_named_stream`): multi-provider streaming with failover. Failover on connection/HTTP errors before stream starts; committed once SSE begins. No mid-stream resume, no caching.

## [0.60.0] - 2026-03-19

### Added
- **MCP HTTP transport**: unified `Mcp.managed` type with `transport` variant (Stdio | Http). JSON config auto-detects via `"url"` key. (#209)
- `Agent_config.to_builder` connects MCP servers at build time with optional `~sw ~mgr`. (#209)
- `Checkpoint.t` `working_context` field for MASC context co-storage. Checkpoint version 3 -> 4 (backward compat). (#197)
- `Hooks.OnError` and `Hooks.OnToolError` hook events for LLM/tool failure callbacks. (#198)
- `message.name` and `message.tool_call_id` optional fields + `make_message` constructor. (#201)
- `Event_bus.filter_topic`, `filter_any`, `filter_all` filter combinators. (#199)
- `Swarm_types.swarm_config` `resource_check` and `max_concurrent_agents` fields. Runner enforces both. (#200)
- `Selection` module: `RoundRobin`, `Random`, `Custom` agent selection strategies. (#202)
- `Cascade_config.complete_named` `timeout_sec` parameter. (#208)
- docs: `stateful-tools.md` (closure, context, external patterns). (#203)
- docs: `config-externalization.md` (Env_config pattern). (#204)

### Fixed
- MCP HTTP: use `base_url` directly (no `/mcp` append). (#210)
- MCP HTTP: SSE response parser fallback + JSON-only Accept header. (#210)
- OpenAI SSE streaming: parse `reasoning_content` field. (#207)

## [0.59.0] - 2026-03-18

### Added
- `Cascade_config` module in `llm_provider`: named cascade profiles with JSON hot-reload and discovery-aware health filtering.
  - `parse_model_string`: "provider:model" string to `Provider_config.t` (llama, claude, gemini, glm, openrouter, custom).
  - `load_profile`: JSON config file loading with mtime-based hot-reload.
  - `filter_healthy`: Discovery-integrated local endpoint health filtering.
  - `complete_named`: convenience cascade execution combining config loading, health filtering, and failover.
- 14 new tests for cascade_config (parse, config, health).

## [0.58.0] - 2026-03-18

Version bump only. No functional changes since 0.57.0.

## [0.57.0] - 2026-03-18

### Added
- `Collaboration.t`: 3-Type session split (Session/Collaboration/Orchestrator) for multi-agent shared context. (#173)
- Runtime-Collaboration bridge + `swarm_config` collaboration field. (#190)
- `agent_telemetry` extended with `usage_stats` and `turn_count`. (#180)
- `Structured.extract_with_retry`: total usage tracking across retry attempts. (#181)
- Swarm retry truncation, agent retry, and budget enforcement. (#189)

### Fixed
- Self-review findings across 4 modules. (#174)
- Race semantics + hierarchical fiber limit. (#179)
- `async_agent` cancel now terminates the fiber via sub-switch. (#183)
- Retry jitter + minimum token threshold for prompt caching. (#186)
- Checkpoint history restore + retry jitter + cache threshold. (#188)
- Production hardening: jitter, cache threshold, agent retry, budget, checkpoint. (#187)

### Changed
- Vendor-neutral naming for local LLM provider. (#182)
- Replace model enum with string + `Model_registry`. (#184)
- Vendor-neutral capability naming + resolve model IDs eagerly. (#185)

## [0.56.0] - 2026-03-18

### Added
- `Complete.cache` interface + metrics hooks. (#149)
- `Async_agent`: async agent execution + `raw_trace` `Eio.Mutex` fix. (#150)
- Swarm telemetry bridge: expose Layer 1 trace refs to consumers. (#152)
- `Structured.extractor` API + `run_structured` for Agent-level extraction. (#155)
- `Append_instruction`: dynamic hook-based instruction injection. (#156)
- Anthropic prompt caching control in `llm_provider`. (#157)
- `Structured.extract_with_retry`: validation retry loop. (#158)
- Consumer API: high-level agent execution with telemetry. (#159)
- Consensus + hierarchical orchestration patterns. (#160)
- A2A Client: Agent-to-Agent protocol client. (#161)
- `Agent_typed`: phantom-type lifecycle state machine (experimental). (#162)

### Changed
- Removed Ollama provider. OpenAI-compatible is the only local path. (#151)

## [0.54.0] - 2026-03-18

### Added
- `Types.text_of_response`: extract text from `api_response` for MASC convergence. (#147)

## [0.53.0] - 2026-03-18

### Added
- `Complete.complete_with_retry`: exponential backoff retry for LLM completions. (#144)
- `Complete.complete_cascade`: multi-provider failover with retry per-provider. (#144)
- `Complete.complete_stream`: streaming completion with SSE event accumulation for both Anthropic and OpenAI-compatible providers. (#144)
- `Complete.is_retryable`: classify HTTP errors as retryable (429, 500, 502, 503, 529, network errors). (#144)
- `Provider_bridge`: convert legacy `Provider.config` to `Provider_config.t`. (#144)
- `Provider_bridge.cascade_to_provider_config`: convert legacy cascade to new cascade type. (#144)

## [0.51.0] - 2026-03-18

### Added
- `Autonomy_trace_analyzer`: quantify agent autonomy via diversity/divergence metrics on raw traces. Classifications: Autonomous/Scripted/Random. (#132)
- `Traced_swarm` (lib_swarm): `Runner.run` wrapper with automatic per-agent `Raw_trace` sinks. (#132)
- `autonomy_smoke_cli`: offline trace analysis and live multi-agent smoke testing. (#132)
- `Trajectory` module: Harbor-inspired trajectory recording with sandbox runner. (#136)
- `Repair_dangling_tool_calls` context reducer strategy. (#135)
- `Prune_tool_args` context reducer for ToolUse input truncation. (#127)
- `state_isolation` for subagent parent state control. (#130)
- Default `context_reducer` wired with repair + prune + drop_thinking. (#139)

### Fixed
- `test_raw_trace`: mock server returns plain JSON but pipeline routes to SSE parser. Switched to sync `Agent.run`. (#138)
- Shell argument quoting in examples, path traversal rejection. (#137)
- CI: exclude hanging integration tests from `dune runtest`. (#132)

### Changed
- `autonomy_smoke_cli` refactored to use `Traced_swarm.run_traced` (parallel Decentralized mode). (#132)

## [0.50.0] - 2026-03-18

### Added
- `.mli` API contracts for 13 modules: `builder`, `event_bus`, `log`, `orchestrator`, `capabilities`, `backend_anthropic`, `backend_openai`, `api_common`, `a2a_server`, `a2a_task_store`, `agent_registry`, `mcp_http`, `mcp_session`.
- `examples/swarm_review.ml`: 3-agent Supervisor-mode swarm PR review.
- `examples/codegen_agent.ml`: natural language to OCaml code generation.
- `bin/review_agent.ml`: cmdliner-based `oas-review` CLI with `--provider` selection.
- `test_http_client.ml`: 14 tests for HTTP client pure functions, SSE parsing, content block roundtrips, and Error_domain conversion.
- README: 3-layer architecture diagram, Swarm Engine section, updated module table and stability tiers.
- CHANGELOG: backfilled v0.41.0 through v0.46.0 entries.

### Changed
- `Log.t` fields `trace_id` and `span_id` changed from mutable to immutable (functional update via `with_trace_id`/`with_span_id`).
- README restructured with Layer 1/Layer 2 architecture, Swarm execution example.
- Version bumped to `0.50.0`.

## [0.46.0] - 2026-03-17

### Added
- `agent.mli`: `Agent.t` becomes abstract type with accessor functions (`Agent.state`, `Agent.lifecycle`, `Agent.tools`, `Agent.context`, `Agent.options`, `Agent.net`). (#124)
- `examples/review_agent.ml`: first real OAS agent — code review via gh CLI tool_use. (#123)

### Changed
- Library-internal code uses `Agent_types.t` directly; external consumers go through abstract `Agent.t` and accessor API. (#124)
- `direct_evidence.ml`, `orchestrator.ml`, `builder.ml` updated to use accessor-based patterns. (#124)

## [0.45.0] - 2026-03-17

### Added
- `Http_client` module in `llm_provider/`: `post_sync`, `post_stream` (Eio+cohttp), `read_sse`, `inject_stream_param`. Network errors captured as `http_error` ADT. (#122)

### Changed
- `streaming.ml` delegates HTTP to `Http_client.post_stream` + `read_sse`, removing ~180 lines of inline HTTP/SSE code. (#122)
- `complete.ml` refactored to share HTTP path via `Http_client.post_sync`. (#122)

## [0.44.0] - 2026-03-17

### Added
- Multi-provider SSE streaming: OpenAI-compatible and Ollama now support native SSE streaming (previously `UnsupportedProvider` error). (#122)
- `llm_provider/streaming.ml`: `parse_openai_sse_chunk`, `openai_stream_state`, `openai_chunk_to_events` for OpenAI delta-to-block conversion. (#122)
- `test_streaming_openai.ml`: 15 unit tests for chunk parsing and event conversion. (#122)

### Changed
- `provider.ml`: `supports_native_streaming` enabled for `openai_chat_capabilities`. (#122)
- Ollama Chat redirects to `/v1/chat/completions` for SSE compatibility. (#122)

## [0.43.0] - 2026-03-17

### Added
- Structured A2A errors: `A2a of a2a_error` ADT replaces `A2a of string`. 5 typed variants: `TaskNotFound`, `InvalidTransition`, `MessageSendFailed`, `ProtocolError`, `StoreCapacityExceeded`. (#118)
- Instance-based OTel tracer: each `create`/`create_eio` returns independent tracer with own span stack, preventing misattribution in concurrent agents. (#118)
- `Tool_set` module: O(1) tool lookup wired into agent core. (#114)
- `Provider_intf`: `supports_streaming` wired into pipeline Route. (#114)
- `Error_domain`: pipeline stage context tagging. (#114)
- 150 new tests, coverage 70.78% to 75.45%. (#120)

### Fixed
- `raw_trace.ml` reverted to `Stdlib.Mutex` (Eio context not guaranteed in all call sites). (#119)
- Concurrent mutable state in `transport` and `artifact_service` protected. (#117)
- OS Mutex replaced with `Eio.Mutex` across codebase, version strings unified. (#115)

### Changed
- `sdk_version` bumped to `"0.43.0"`. (#115)

## [0.42.0] - 2026-03-17

### Added
- `agent_sdk_swarm` library (`lib_swarm/`): Layer 2 Swarm Engine with 3 orchestration modes (Decentralized, Supervisor, Pipeline), convergence loop, `Eio.Mutex` state protection. (#112)
- `swarm_types.ml`: `agent_role`, `orchestration_mode`, `convergence_config`, `agent_entry`, `swarm_state`, `swarm_callbacks`, `swarm_result`. (#112)
- `runner.ml`: `eval_metric`, `run_single_pass`, `run` with convergence loop. (#112)
- `test_swarm.ml`: 13 unit tests for swarm types and metric evaluation. (#112)
- `llm_provider` sub-library (`lib/llm_provider/`): shared LLM types for OAS and MASC. (#111)
  - `types.ml`: role, content_block, message, SSE, tool types
  - `capabilities.ml`: provider capability flags + presets
  - `pricing.ml`: per-model cost estimation
  - `error.ml`: provider-level error types

### Changed
- Monolithic `agent_sdk.mli` (3928 lines) removed. Per-module `.mli` files replace it. (#113)
- OAS `types.ml` re-exports `llm_provider` types via `include` for nominal type equality. (#111)

## [0.41.0] - 2026-03-17

### Changed
- **Architecture restructuring**: protocol modules moved to `lib/protocol/`, agent modules to `lib/agent/`. (#109)
  - `(include_subdirs unqualified)` in `lib/dune` for automatic discovery
  - Module names and public API unchanged
- A2A task store: O(1) list operations + bounded in-memory capacity. (#110)

### Added
- Per-module `.mli` files for 6 core modules (initial batch from Phase 1-3). (#109)
- `pipeline.ml` + `pipeline.mli`: 6-stage turn pipeline. (#109)

## [0.40.0] - 2026-03-16

### Added
- v0.36: File-backed A2A task persistence (`a2a_task_store.ml`) — atomic write, validate_task_id, GC
- v0.37: MCP HTTP transport (`mcp_http.ml`) + SSE parser (`sse_parser.ml`) — JSON-RPC 2.0 over HTTP
- v0.38: Agent Registry (`agent_registry.ml`) — Hashtbl-based registry with capability lookup
- v0.39: Approval Pipeline (`approval.ml`) — composable multi-step approval evaluation
- v0.40: SDK CLI (`bin/oas_cli.ml`) + Agent Config (`agent_config.ml`) — cmdliner-based CLI entry point
- 7 new test suites (69 tests, 1,183 LOC)
- Error types: `HttpTransportFailed`, `DiscoveryFailed`

### Changed
- `sdk_version` bumped to `"0.40.0"`

## [0.30.0] - 2026-03-16

### Added
- Skill Registry (`skill_registry.ml`) — runtime skill loading/matching
- Agent Card (`agent_card.ml`) — agent metadata + capability declaration
- ElicitInput handler — interactive user input during agent runs

## [0.28.1] - 2026-03-16

### Fixed
- MCP `read_response` non-tail-recursive loop — stack overflow risk on long sessions
- MCP `mcp_tool_of_json` silent "tool" name fallback — now returns None

### Added
- 101 new tests, coverage 65.32% to 75.14%
- Export missing `[@@deriving yojson, show]` in `agent_sdk.mli`

## [0.28.0] - 2026-03-16

### Changed
- Split `sessions.ml` into `sessions_types.ml` + `sessions_store.ml` + `sessions_proof.ml`

### Added
- Cache cost tracking (`Provider.pricing_for_model`, `estimate_cost`)
- Context reduction strategies (`context_reducer.ml`)

## [0.27.0] - 2026-03-16

### Added
- Prompt caching for all providers (Anthropic `cache_control` ephemeral)
- Provider registry E2E integration tests
- Extracted module tests (`api_dispatch`, `api_ollama`)

## [0.26.0] - 2026-03-16

### Changed
- **agent.ml split** (944→671 lines, 29% reduction): Extracted `Agent_lifecycle` and `Agent_checkpoint` modules. (#90)
- **Unified sync/streaming turns**: `run_turn_core` with `api_strategy` parameter replaces duplicated turn functions (~170 lines dedup). (#90)
- **CI coverage gate**: 65% threshold enforced. (#90)

### Fixed
- **MCP race condition**: `next_id` in `Mcp.send_request` protected by `Eio.Mutex`. (#90)

### Added
- **Provider pricing**: OpenAI (gpt-4o, gpt-4o-mini, gpt-4.1, o3-mini) and local models (ollama/qwen/llama = 0.0). (#90)
- **MCP health/reconnect**: `Mcp.is_alive`, `Mcp.reconnect`, `Mcp.connect_all_best_effort`. (#90)
- **Transport status**: `Transport.status` query. (#90)
- **Provider registry**: Runtime custom provider registration (`register_provider`, `find_provider`). (#90)
- **test_api_dispatch**: 10 dispatch tests. **test_property_advanced**: 20 QCheck property tests. (#90)
- **docs/custom-providers.md**: vLLM example guide. (#90)

## [0.25.1] - 2026-03-16

### Fixed
- **Error classification**: OpenAI-compatible API errors in response body were classified as `NetworkError` (retryable) instead of `InvalidRequest` (non-retryable). Added `Openai_api_error` exception with correct routing in `Api.create_message`. (#86)
- **Crash on unexpected JSON**: `api_ollama.ml` used `assert false` for unreachable branch, which would crash with `Assert_failure` if triggered. Replaced with descriptive `failwith`. (#86)
- **Silent exception in worker thread**: `runtime_server.ml` raised `Failure` inside `Eio.Switch.run`, losing the error in the worker thread. Changed to `Result` propagation. (#86)

### Added
- **`tool_choice: None_`**: Disables tool use for a turn. Serializes to `{"type":"none"}` (Anthropic) / `"none"` (OpenAI). (#86)
- **`disable_parallel_tool_use`**: Config field to force sequential tool execution. Maps to `tool_choice.disable_parallel_tool_use` (Anthropic) / `parallel_tool_calls: false` (OpenAI). (#86)
- **`yojson` derivation** for `Sessions`, `Raw_trace`, and `Tool` types. (#87)
- **Makefile** with `make test`, `make coverage`, `make clean` targets. (#85)

## [0.25.0] - 2026-03-16

### Added
- **`Agent_turn` module** (new): Common turn logic extracted from `agent.ml`. Provides `prepare_turn`, `accumulate_usage`, `update_idle_detection`, `apply_context_injection`, `check_token_budget`, `make_tool_results`, and `filter_valid_messages`.

### Changed
- **Streaming path feature parity**: `run_turn_stream_with_trace` now includes `BeforeTurnParams` hook, `apply_turn_params`, `extra_system_context`, `tool_filter_override`, and `context_injector`.
- `agent.ml` reduced by ~50 lines through delegation to `Agent_turn`.

### Added (v0.25.0-rc)
- **Test harness framework** (`Harness`): 6-type pluggable verification — Behavioral, Adversarial, Performance, Regression, Swiss Cheese (multi-layer), Composability.
- **Provider mock** (`Provider_mock`): network-free scripted responses with cycling, convenience builders for text/tool_use/thinking responses.
- **Per-turn parameter adjustment** (`Hooks.turn_params`, `BeforeTurnParams`): hooks can adjust temperature, thinking_budget, tool_choice, tool_filter per turn via `AdjustParams` decision. Parameters revert after each API call.
- **Reasoning extraction** (`Hooks.extract_reasoning`): extracts thinking blocks, detects uncertainty markers, identifies tool selection rationale.
- **Dynamic context strategy** (`Context_reducer.Dynamic`): select windowing strategy at runtime based on turn count and message state.
- **Conditional orchestration** (`Orchestrator.conditional_plan`): `Branch`, `Loop`, `Sequence`, `Cond_parallel` with route conditions (`Always`, `ResultOk`, `TextContains`, `And`, `Or`, `Not`).
- **Context scope isolation** (`Context.isolated_scope`): `create_scope` with `propagate_up`/`propagate_down` key control for sub-agent delegation.

## [0.24.0] - 2026-03-16

### Added
- **Cascade failover** (`Provider.cascade`): multi-provider failover with primary + fallback list. Retry-aware cascade with `Retry.with_cascade`.
- **Idle detection**: fingerprint-based tool call repetition detection. Configurable `max_idle_turns` with `OnIdle` hook event and `IdleDetected` error.
- **Context compaction** (`Context_reducer`): 6 strategies — `Keep_last_n`, `Token_budget`, `Prune_tool_outputs`, `Merge_contiguous`, `Drop_thinking`, `Compose`. Turn-boundary-aware grouping preserves ToolUse/ToolResult pairs.
- **Context injection** (`Hooks.context_injector`): post-tool-execution hook that updates `Context` key-value store and appends extra messages to the conversation.
- **Cost tracking** (`Provider.pricing_for_model`, `Provider.estimate_cost`): per-model pricing with cumulative `estimated_cost_usd` in `usage_stats`.
- `test_bug_hunt.ml`: 7 bug candidate reproduction and fix verification tests.
- `test_e2e_v024.ml`: 5 end-to-end integration scenarios against live local LLM (gated behind `LLAMA_LIVE_TEST=1`).

### Fixed
- **B1 CRITICAL**: `context_injector` exception crashed the tool loop. Wrapped in `try-with`; exceptions are caught and logged silently.
- **B2 HIGH**: `create_message_cascade` with `clock=None` skipped all fallback providers. Fallbacks are now tried sequentially on retryable errors.
- **B3 HIGH**: Injected `extra_messages` could violate role alternation. Added role validation that drops messages creating same-role adjacency.
- **B4 HIGH**: `Token_budget` strategy returned empty message list when budget was smaller than the most recent turn. Added guard that always keeps the last turn.
- **B5 MEDIUM**: Idle detection failed for empty-empty fingerprint comparison (`[] <> []` = false). Changed `last_tool_calls` from `list` to `option` to distinguish "not yet set" from "empty set".
- **B6 MEDIUM**: `Drop_thinking` replaced thinking-only messages with `Text ""`. Changed to drop the entire message instead of inserting empty text.

### Changed
- `Agent.t.last_tool_calls`: internal type changed from `tool_call_fingerprint list` to `tool_call_fingerprint list option` (not in public API).

## [0.23.0] - 2026-03-15

### Breaking
- `Agent.t` is now an abstract type. Direct field access (`agent.state`, `agent.tools`, etc.) is replaced by accessor functions: `Agent.state`, `Agent.lifecycle`, `Agent.tools`, `Agent.context`, `Agent.options`, `Agent.net`.
- `test_add_message` test removed (external mutation of agent state is no longer possible).

### Migration from 0.22.x

`Agent.t` field access must be replaced with accessor functions:

```ocaml
(* Before (0.22.x) *)
let state = agent.state in
let tools = agent.tools in
let ctx = agent.context in

(* After (0.23.0) *)
let state = Agent.state agent in
let tools = Agent.tools agent in
let ctx = Agent.context agent in
```

`Builder.build` still works but emits a deprecation warning. Switch to `build_safe` for validation:

```ocaml
(* Before *)
let agent = Builder.build builder in

(* After *)
match Builder.build_safe builder with
| Ok agent -> (* use agent *)
| Error err -> Printf.eprintf "Config error: %s\n" (Error.to_string err)
```

### Added
- `Agent.state`, `Agent.lifecycle`, `Agent.tools`, `Agent.context`, `Agent.options`, `Agent.net` accessor functions.
- `Builder.build_safe : t -> (Agent.t, Error.sdk_error) result` with validation:
  - `max_turns > 0`
  - `max_tokens > 0`
  - `thinking_budget` requires `enable_thinking = true`
- `Error.InvalidConfig` variant for configuration validation errors.
- CI pipeline: GitHub Actions with OCaml 5.1.x + 5.4.x matrix, bisect_ppx coverage, odoc, version-check.
- `examples/` directory with 6 examples (3 moved from `bin/`, 3 new).
- `CONTRIBUTING.md` with build instructions, code style, PR expectations.
- Module stability tiers in README (Stable / Evolving / Experimental).

### Deprecated
- `Builder.build` -- use `Builder.build_safe` for validated construction.
- `Provider.local_qwen` -- use `Provider.Local` constructor directly.
- `Provider.local_mlx` -- use `Provider.Local` constructor directly.

### Changed
- README restructured for third-party onboarding: Installation, Quickstart, Provider table, stability tiers.
- Demo executables moved from `bin/` to `examples/`.
- Version consistency enforced by CI (dune-project == agent_sdk.ml).

## [0.22.0] - 2026-03-14

### Added
- hook lifecycle evidence in direct raw traces:
  - `hook_invoked` records
  - `post_tool_use_failure` hook event
- session/proof getters:
  - `Sessions.get_hook_summary`
  - `Sessions.get_tool_catalog`
- `proof_bundle` now includes:
  - `hook_summary`
  - `tool_catalog`

### Changed
- direct-agent conformance summaries now include hook event and tool catalog counts.
- direct evidence persists a `tool-catalog` artifact for consumer-safe tool contract reads.
- raw trace summaries now expose hook counts and hook names.

## [0.21.0] - 2026-03-14

### Added
- `Direct_evidence` stable consumer getters:
  - `get_proof_bundle`
  - `get_conformance`
- `Tool.descriptor_to_yojson`
- richer shell/tool descriptor fields:
  - `chaining_allowed`
  - `redirection_allowed`
  - `pipes_allowed`
  - `examples`

### Changed
- direct-agent worker summaries now expose stable identity and lifecycle fields:
  - `worker_id`
  - `runtime_actor`
  - `primary_alias`
  - `accepted_at`
  - `ready_at`
  - `first_progress_at`
- runtime participants backfill the same identity/lifecycle vocabulary for summary-first consumers.
- `Conformance.summary` now includes latest worker status, aliases, and resolved runtime/model.
- `Conformance.check.code` now includes direct-evidence and identity/runtime consistency failures.

## [0.20.0] - 2026-03-14

### Added
- `Direct_evidence` module for direct-agent proof bundle materialization:
  - `persist`
  - `get_worker_run`
  - `run_conformance`
- `Agent.lifecycle_snapshot` for direct-agent lifecycle status visibility.
- `Tool` minimal descriptor surface:
  - `descriptor`
  - optional shell constraints
  - optional workdir policy hint
- machine-stable conformance failure codes on `Conformance.check.code`
- `oas_direct_conformance_demo` executable

### Changed
- direct-agent evidence can now be written into the same session-scoped runtime store layout that `Sessions.get_proof_bundle` expects.
- `Sessions.worker_run` now carries `role` and `aliases`.
- direct-agent conformance reuses the same `Sessions` and `Conformance` path instead of requiring consumer-side reconstruction.

## [0.19.0] - 2026-03-14

### Added
- Consumer-safe worker lifecycle surface:
  - `Sessions.get_latest_accepted_worker_run`
  - `Sessions.get_latest_ready_worker_run`
  - `Sessions.get_latest_running_worker_run`
  - `Sessions.get_latest_completed_worker_run`
  - `Sessions.get_latest_failed_worker_run`
  - `Sessions.get_latest_validated_worker_run`
- `worker_run` now exposes lifecycle and runtime selection metadata:
  - `status`
  - `requested_provider`
  - `requested_model`
  - `requested_policy`
  - `resolved_provider`
  - `resolved_model`
  - `last_progress_at`
- `proof_bundle` now carries:
  - `latest_accepted_worker_run`
  - `latest_ready_worker_run`
  - `latest_running_worker_run`
  - `latest_completed_worker_run`
- Conformance checks now cover lifecycle-oriented consistency:
  - latest accepted/ready/running/completed ordering
  - lifecycle timestamp monotonicity
  - resolved runtime presence consistency

### Changed
- Runtime participants persist requested and resolved provider/model metadata into session state.
- Runtime telemetry now records resolved provider/model for worker live/completed/failed events.
- Runtime store writes session/proof/evidence files atomically to reduce resume/read races on background consumers.
- `oas_conformance_demo` now exposes latest worker lifecycle ids in its summary output.

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
 
