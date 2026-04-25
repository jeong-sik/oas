# Changelog

All notable changes to `agent_sdk` are documented in this file.

Historical note: release notes for `0.100.3`, `0.100.5`, and `0.100.6` were
backfilled on 2026-04-04 from existing git tags. The dates below reflect the
original tag dates. `0.100.4` was never tagged or released.

## Unreleased

## [0.175.0] - 2026-04-25

### Added

- **HTTP SSE stream idle timeout parity.** `Complete.complete_stream`
  now applies `stream_idle_timeout_s` to every HTTP streaming path,
  including Anthropic, OpenAI-compatible, Gemini, GLM, and Ollama native
  NDJSON streams. The timeout remains caller-owned and surfaces stalled
  endpoints as retryable timeout-shaped network errors.
- **CLI subprocess stdout idle timeout.** Shared non-interactive CLI
  subprocess execution now accepts `?clock` and
  `?stdout_idle_timeout_s`, so silent CLI hangs can be interrupted
  without imposing a total runtime cap.

### Fixed

- **Pipeline retry honors effective tool contract.** Missing-required-tool retry
  now consults the resolved completion contract (post provider-capability
  resolution) instead of the originally requested contract. Providers that
  intentionally relax unsupported tool requirements no longer enter forced
  retry loops.
- **Per-response usage is preserved.** Ollama and CLI transports keep
  usage on each returned response instead of letting aggregate turn
  accounting overwrite or drop the provider payload.
- **Runtime participant events preserve raw trace run ids.** Runtime
  participant lifecycle events now carry `raw_trace_run_id` through to
  the top-level EventBus `run_id`, keeping generic trace correlation
  intact for downstream consumers.
- **Provider pricing entries refreshed again.** Added pricing for
  `gpt-5.5` alongside the existing current OpenAI model table refresh.

## [0.173.0] - 2026-04-25

### Added

- **Bounded Ollama NDJSON streams.** `Complete.complete_stream` and
  `Http_client.read_ndjson` now accept optional `?clock` and
  `?stream_idle_timeout_s`. When set, each NDJSON line read is wrapped in
  `Eio.Time.with_timeout_exn`; a stalled stream raises `Eio.Time.Timeout`,
  which `catch_network` already maps to a retryable `NetworkError`. No new
  error surface, no default injected at the SDK layer — callers decide the
  idle budget.

### Fixed

- **Provider pricing entries refreshed.** Added pricing for current
  `gpt-5.4`, `gpt-5.4-mini`, `gpt-5.3-codex`, and `gpt-5.2` from the
  official OpenAI rate cards. `gpt-5.3-codex-spark` left unpriced because
  the Codex rate card marks research-preview pricing as non-final.
- **Streamed `response.model` no longer blank.** When a provider omits
  `model` from the streamed final chunk, `patch_telemetry` now fills it
  from the configured `model_id` so downstream consumers receive a stable
  identifier.

## [0.172.0] - 2026-04-24

### Added

- **Native NDJSON streaming for Ollama.** `Complete.complete_stream_http`
  Ollama branch now speaks the native `/api/chat` endpoint and parses the
  newline-delimited JSON wire format directly, replacing the prior detour
  through Backend_openai's SSE shape. Adds `Http_client.read_ndjson`,
  `Streaming.parse_ollama_ndjson_chunk`, and
  `Streaming.ollama_chunk_to_events` as new public exports.
- **Restored Ollama timing telemetry.** Streaming responses now carry
  `prompt_eval_count`, `prompt_eval_duration`, `eval_count`, and
  `eval_duration` — the four Ollama-only fields the OpenAI compat detour
  was stripping. Token-count usage and `inference_timings` on streaming
  responses are byte-identical to the non-streaming path
  (masc-mcp #8968 / #8969).

## [0.171.0] - 2026-04-24

### Added

- **Strict required-tool satisfaction hooks.** Completion contracts can now
  validate required tool calls with a caller-supplied typed predicate, so
  read-only/observability tools no longer have to count as productive action
  at runtime boundaries.

## [0.170.9] - 2026-04-24

### Added

- **Truth-layer evidence primitives.** `Event_envelope`, `Effect_evidence`, and
  `Runtime_health` provide cross-runtime event causality, tool-effect decision
  evidence, and runtime health snapshot shapes. `Mode_enforcer` now records
  effect evidence for every pre-tool decision, and `Proof_capture` persists
  those rows under `evidence/effects.json` for downstream proof consumers
  (#1158).
- **`Capabilities.emits_usage_tokens` + `capabilities_for_provider_label`.**
  `Llm_provider.Capabilities.capabilities` gains an
  `emits_usage_tokens : bool` field (default `true`) that captures whether
  a provider's standard response carries `input_tokens`/`output_tokens`.
  CLI-class wrappers that strip usage before returning (`codex_cli`,
  `gemini_cli`, `kimi_cli`) declare it `false`; all direct APIs keep the
  default. A companion `capabilities_for_provider_label : string ->
  capabilities option` lookup and a matching
  `Capability_filter.emits_usage_tokens` predicate let adapters that
  track provider kind as a string query this flag without reinventing
  a provider allowlist. Downstream metrics/coverage layers (e.g.
  masc-mcp `Provider_adapter.is_structurally_unmetered_provider`) can
  now consume the SDK directly as the SSOT (#1173).

### Fixed

- **Split pipeline stages now build messages through the shared constructor.** The post-split `stage_input`, `stage_collect`, and `stage_execute` paths now use `make_message` instead of stale record literals, so newly required fields such as `metadata` stay aligned with the shared message shape and downstream pipeline builds stop breaking after the stage split (#1151).
- **Kimi CLI session reuse now matches the actual CLI contract.** `transport_kimi_cli` now passes config files via `--config-file`, keeps `--session <id>` stable across turns, and stops assuming `--continue` is valid with an explicit session id. This preserves the intended token-saving delta prompt behavior for keeper-style multi-turn sessions without relying on a CLI flag combination that `kimi` rejects.

## [0.170.8] - 2026-04-24

### Fixed

- **`scripts/release.sh` now refuses to tag from anywhere other than
  a main branch synced with `origin/main`.** Previously the script
  ran `git tag -a` against the current `HEAD`, so running it from a
  feature branch (or before the release PR was merged) left the tag
  on a release-cut commit whose SHA was rewritten by GitHub's PR
  rebase merge. The tag then pointed to a commit no longer reachable
  from main, which broke release-provenance audits and every
  downstream consumer that pinned by SHA-equivalent. The script now
  fails fast when `git rev-parse --abbrev-ref HEAD` is not the
  integration branch or when local HEAD differs from `origin/main`,
  with the remediation printed inline (#1136, #1135, #1168).

## [0.170.7] - 2026-04-24

### Fixed

- **Atomic file writes no longer race on a shared tmp path.** Previously
  both `Fs_result.write_file` (blocking path) and the `Eio.Path` callers
  in `Checkpoint_store.save`, `A2a_task_store.store_task`,
  `Memory_file_backend.persist`, and `Durable_event.save_to_file` derived
  the tmp name from the target (`<path>.tmp`). Two fibers writing to the
  same logical file could therefore race, with writer A's `rename`
  consuming the shared tmp before writer B's `rename` ran, surfacing as
  `Eio.Io Fs Not_found (renameat …)`. A new `Fs_atomic_eio.save_atomic`
  helper gives every writer a unique tmp suffix (pid + wall-clock ns +
  `Atomic` counter) and best-effort fsyncs the tmp file and parent
  directory; the five call sites now delegate to this helper (or the
  unified `Fs_result.write_file`). `A2a_task_store.store_task` only
  updates its in-memory cache on `Ok`, so cache and disk can no longer
  drift when the atomic write fails after flush (#1165).

### Notes

- **Race test skipped under `BISECT_ENABLE=yes`.** `test_atomic_write_race`
  is gated off when the Coverage report job re-runs the suite with bisect
  instrumentation, because the extra io_uring submissions from bisect hooks
  blow the CI container's memlock budget and cause unrelated downstream
  tests to fail with `Unix_error(ENOMEM, "io_uring_queue_init")`. The
  happy-path line coverage of `Fs_atomic_eio.save_atomic` is retained
  indirectly via the existing `test_checkpoint_store`,
  `test_a2a_task_store`, and `test_memory_file_backend` suites (#1165).

## [0.170.6] - 2026-04-24

### Changed

- **SDK independence boundary now enforced in CI.** The `SDK Independence
  Gate` job rejects PRs that reintroduce cross-SDK imports, keeping
  `agent_sdk` consumable without pulling downstream-specific modules
  (#1160).

## [0.170.5] - 2026-04-24

### Added

- **Truth-layer evidence primitives.** New types and helpers for
  recording evidence alongside LLM outputs so callers can persist
  rationale and citations without bespoke serialisers (#1158).

### Fixed

- **KIMI direct API aligned with `KIMI_API_KEY` only.** The provider
  previously fell back across several env var candidates, so setting
  the wrong one silently routed traffic to the default key. Routing is
  now keyed exclusively on `KIMI_API_KEY` (#1159).
- **kimi CLI session reuse matched to actual CLI contract.** Session
  IDs are threaded through follow-up turns instead of being dropped
  after the first turn (#1157).
- **HTTP MCP reconnect state preserved across transport restarts** —
  the client no longer loses its resume token when the underlying
  socket is replaced (#1156).

## [0.170.4] - 2026-04-23

### Fixed

- **`llm_provider` parses usage fields from kimi-cli JSONL output.**
  Token counts now surface on KIMI CLI responses; previously
  `prompt_tokens` and `completion_tokens` were dropped (#1155).

### Changed

- **MCP fixture names genericised in transport tests** so new
  providers can reuse the harness without name collisions (#1154).

## [0.170.3] - 2026-04-22

### Added

- **Native timeout handling for `Agent.run`.** Uses Eio's clock
  primitives instead of callback-based workarounds, so timeouts
  compose with the surrounding switch (#1006, #1150).
- **Structured replay metadata in checkpoints.** Replay flows persist
  a typed payload instead of free-form strings, enabling downstream
  tooling to reason about replay provenance (#1149).
- **Structured `network_error_kind` on `NetworkError`.** The
  `llm_provider` error surface classifies transport failures
  (DNS / connect / read / idle) so callers can pick a retry policy
  without string-matching error messages (#1147).

### Fixed

- **HTTP client drains response body to prevent CLOSE\_WAIT
  accumulation.** Long-lived cascades had been leaking sockets into
  CLOSE\_WAIT, eventually exhausting the local port pool (#965,
  #1148).
- **Pipeline message constructor drift resolved** — stages no longer
  produce messages that downstream stages cannot decode after the
  6-stage split (#1151).

### Changed

- **Pipeline split by stage (prepare / route / retry).** `pipeline.ml`
  was broken up along the three stages that were already documented in
  the architecture notes, reducing module size and clarifying
  responsibilities (#1146, #1152).

## [0.170.2] - 2026-04-22

### Fixed

- **Raw trace generation now flushes gracefully on timeout.** When the
  surrounding operation cancelled, the trace backend used to lose its
  in-flight buffer because shutdown raced with cancellation; a typed
  flush hook now drains before the backend tears down (#1141).

## [0.170.1] - 2026-04-22

### Added

- **`NotFound` variant on `api_error` for HTTP 404.** Callers can now
  distinguish 404 from generic HTTP failures without string-matching
  the status code inside error messages (#1139).

### Changed

- **`Otel_tracer` span records made immutable** so they are safe to
  share across Eio fibers without an owning-fiber lock (#1138).

### Fixed

- **Streamed telemetry populated via non-HTTP transports.** Previously
  only HTTP-backed streams emitted telemetry; CLI and in-process
  transports now produce the same span shape (#1140).

## [0.170.0] - 2026-04-21

### Added

- **Tiered recall prompt assembly and budgeting.** Agents can now carry optional typed `tiered_memory = { long_term; mid_term; short_term }` through `Agent.options` and `Builder.with_tiered_memory`. Turn preparation renders a pinned synthetic User recall block after leading system messages, keeps a fixed `LONG -> MID -> SHORT` order, and omits blank tiers (#1133).

### Changed

- **Context reduction and compaction now account for pinned recall tokens.** Reducers reserve the recall token budget before trimming raw history, and proactive/emergency compaction watermarks are computed from `raw history + recall` while only stored raw messages are compacted (#1133).

### Notes

- **Registry coverage now fails loudly when `Provider_kind` and the provider registry drift.** Added a dedicated regression test asserting every `Provider_kind` resolves to a registered entry, closing a silent coverage gap in the provider registry surface (#1132).

## [0.169.0] - 2026-04-21

### Added

- **Typed `Provider_kind` sum type, hoisted to its own module.** `Llm_provider.Provider_kind.t` now lives in a standalone compilation unit (`lib/llm_provider/provider_kind.ml`) so it can be shared by records in `Types` without creating a dependency cycle with `Provider_config`. `Provider_config` re-exports the type via manifest rebinding (`type provider_kind = Provider_kind.t = | Anthropic | ... | Kimi | ...`) so every existing `Provider_config.Anthropic` / `Provider_config.string_of_provider_kind` caller keeps compiling unchanged (#1122).

- **`Provider_kind.of_string` canonical parser.** Accepts the canonical lowercase forms emitted by `to_string` plus the documented legacy aliases (`claude → Anthropic`, `openai → OpenAI_compat`, `llama → Ollama`). Case-insensitive; leading/trailing whitespace trimmed. Returns `None` for unknown inputs so callers fail fast instead of silently defaulting to a wrong provider (#1122).

- **Hand-written `Provider_kind.pp` / `show` / `to_yojson` / `of_yojson`.** Preserve the existing lowercase wire format (`"anthropic"`, not `"Anthropic"`) so records embedding the variant (`Types.inference_telemetry.provider_kind`) can use derived yojson without breaking on-disk or over-the-wire payloads (#1122).

- **`Provider_kind.all : t list` and `Provider_kind.default_api_key_env : t -> string option`** (re-exported as `Provider_config.all_provider_kinds` and `Provider_config.default_api_key_env`). `all` is the canonical enumeration used by tests, CLI completion, and future QCheck generators; `default_api_key_env` centralizes the per-kind env-var convention (`Anthropic → Some "ANTHROPIC_API_KEY"`, …). `Provider.default_api_key_env_of_kind` now delegates through the sum type (#1126).

- **Kimi Code provider support.** Added `Kimi` (direct Anthropic-compatible `/v1/messages`) and `Kimi_cli` (subprocess transport via `kimi --print`) variants plus their transports, capabilities, and registry entries (#1125).

### Changed

- **`Types.inference_telemetry.provider_kind` is now `Provider_kind.t option`** (was `string option`). Wire format unchanged — the derived yojson transits through the hand-written `Provider_kind.{to,of}_yojson` so every record serializes the same lowercase strings (`"ollama"`, `"anthropic"`, `"openai_compat"`, …). Existing readers that pattern-matched on the string literal (`Some "ollama"`) move to constructors (`Some Provider_config.Ollama`) at the two callsites in `Complete.complete_cascade` (#1122).

- **`agent_config.resolve_provider` now dispatches on `Provider_kind.of_string`** instead of an ad-hoc `match provider_str with | "anthropic" | ... | _ -> ...` ladder. Closes three drift bugs reported in the tick-6 audit: the SDK-emitted `"openai_compat"` string now resolves to OpenAICompat (was dropping to a broken registry fallback), the documented `"claude"` alias routes to Anthropic, and parsing is case-insensitive (#1123).

- **`Provider_bridge.resolve_auto_model_id` takes `Provider_kind.t` directly**, not a stringified `provider_name`. Removes the dead `"openai"` / `"openrouter"` branches (unreachable from the prior stringify path) and makes future variant additions fail loudly in the compiler rather than silently falling into a wildcard (#1124).

### Notes

- Tests: 44/44 in `test_provider_config` (+13 over 0.164.0 covering parser, serializers, enumeration, and wire-format regressions), 31+4 in the newly-registered `test_agent_config_deep` (was silently unregistered in `test/dune` before #1123), full `@test/runtest` green.

## [0.164.0] - 2026-04-21

### Fixed

- **Codex CLI transport honors request model IDs.** `transport_codex_cli` now passes a non-empty, non-`auto` `Provider_config.model_id` through `codex exec --model`, matching Claude Code and Gemini CLI behavior while preserving `auto` as "use the user's CLI default".
- **ApprovalRequired fallback no longer emits an operator-facing WARN without a callback.** The existing fail-open behavior is unchanged, but `agent_tools` now records the fallback at debug level so consumers do not see an unactionable warning on every approval-less tool execution.

### Added

- **`Hooks.on_idle_escalated` adds runtime-computed idle severity.** Callers can opt into a structured idle hook carrying `nudge` / `final_warning` / `skip` severity while keeping the legacy `on_idle` path for compatibility. `skip_at` reuses `max_idle_turns`; `final_at` is configurable per agent via `idle_final_warning_at`.
- **Tool error retry classification now honors explicit runtime error classes.** `Types.tool_error` carries an optional typed `error_class`, and `Tool_retry_policy.decide` now prefers that explicit classification over legacy `failure_kind` inference. Recoverable errors marked `Deterministic` no longer blind-retry.

### Changed

- **`response_format` now uses a typed variant surface** for agent config, builders, provider config, and checkpoints: `Off | JsonMode | JsonSchema of Yojson.Safe.t` (issue #957). `Builder.with_response_format_json`, `Provider_config.make ?response_format_json`, and checkpoint decoding of legacy `response_format_json: bool` remain as compatibility shims. In this step, provider request builders honor `JsonMode`; `JsonSchema _` is preserved through config/persistence but not yet emitted as provider-native schema parameters.

- **Completion contract violations now carry typed contract IDs.** `Error.CompletionContractViolation.contract` now uses `Completion_contract_id.t` instead of `string`. `Agent_sdk.Completion_contract` and `Agent_sdk.Completion_contract_id` are re-exported; downstream code that serialized the old string field should switch to `Completion_contract_id.to_string`.

### Notes

- **Version boundary realigned for downstream pins.** `main` now advertises `0.164.0` so post-`0.163.0` public API growth no longer masquerades as the earlier `0.163.0` floor used by downstream SHA pins and compatibility checks.

## [0.163.0] - 2026-04-20

### Added

- **`Hooks.Nudge` accepted from `before_turn`.** Previously only `OnIdle` could return `Nudge`; `before_turn` returning `Nudge` was silently dropped by the pipeline (`stage_input` only handled `ElicitInput`). The decision matrix and `legal_decisions_for_stage` now list `K_Nudge` for `before_turn`, and `pipeline.ml stage_input` appends the nudge text as a User-role message before tool preparation so it reaches the model in the same turn. Mirrors the `on_idle` Nudge handler at `pipeline.ml:392`. Generic primitive — payload is opaque text, no domain knowledge in OAS.

## [0.162.0] - 2026-04-19

### Added

- **`Event_bus` envelopes carry `caused_by`.** `envelope.caused_by : string option` links every event back to the originating run. Three emitters wire it through: `orchestrator` (`AgentStarted` → `AgentCompleted`/`AgentFailed`, PR #1019), `agent` handoff (`HandoffRequested` → `HandoffCompleted`, PR #1020), `agent_tools` (`ToolCalled` → `ToolCompleted`, PR #1021). `event_forward` surfaces the field on the delivery payload (PR #1028). Enables causation tracing across a single run without re-parsing agent logs.
- **`Tool_retry_policy.error_class` variant** (PR #1027). Contract-first typed classification of tool errors drives retry decisions; replaces string-based pattern matching at callsites.
- **`Agent_turn.idle_granularity` opt-in variant** (PR #1024). Fine-grained `is_idle` reporting for callers that need sub-turn idle signals without changing the default coarse-grained behavior.
- **Inference profile exposes `top_p` / `top_k` / `min_p`** (PR #1015). Constants-layer extension so cascade configs can pin sampling parameters without provider-specific escapes.

### Fixed

- **CLI transports disable MCP by default in headless mode** (PR #999). `transport_claude_code` / `transport_codex_cli` / `transport_gemini_cli` stop inheriting user MCP config for non-interactive invocations unless explicitly opted in via `OAS_*_ALLOWED_MCP`. Eliminates the "connection refused to dead MCP port" noise observed in keeper logs.
- **`Hooks.OnToolError` emitted on tool execution failure** (PR #1031). Prior behavior left callers relying on `stop_reason` heuristics; now the failure path fires a dedicated hook with the error payload.
- **`Hooks.OnError` emitted on tool-not-found dispatch failure** (PR #1035 — deferred, see PR description).
- **`llm_provider` honors `~cwd` at the OS level** via `env -C <dir>` prefix (PR #1016). Earlier implementation relied on the CLI's own `--cwd` flag, which was inconsistent across providers and silently dropped by some wrappers.

### Changed

- **`LLM_ENDPOINTS` parsing unified** via `Discovery.parse_llm_endpoints_env` (PR #1014). Transports and discovery now share a single comma-split + trim routine.

### Chore

- **Test registry cleanup.** Six orphan tests wired into `test/dune` — `test_agent_config` (#1030), `test_agent_turn_budget_unit` (#1026), `test_agent_tool` (#1033), `test_agent_typed` (#1034), `test_agent_pipeline` (#1036), `test_agent_lifecycle` (#1037). No behavior change; tests were previously not run.

## [0.161.0] - 2026-04-19

### Fixed

- **`transport_gemini_cli.build_args` no longer emits `--system-prompt`.** Gemini CLI (>=0.38) rejects the flag with `Unknown arguments: system-prompt` and the subprocess exits with code 1, so any cascade member using `gemini_cli:auto` with a system prompt was failing every turn. System text is now folded into the `-p` argument as labelled `[System]` / `[User]` blocks via the new `effective_prompt` helper, which keeps the role distinction without requiring a CLI flag. `None` / empty system prompts pass through unchanged, so the argv for prompt-only callers is byte-identical to pre-0.161.0.

## [0.160.1] - 2026-04-18

### Fixed

- **Completion contract no longer rejects no-ToolUse responses when the model was cut off mid-turn.** `completion_contract.validate_response` now treats `stop_reason = MaxTokens` or `Unknown "pause_turn"` as resumable for `Require_tool_use` / `Require_specific_tool`, so callers can continue the turn (or raise `max_tokens`) instead of seeing a spurious `CompletionContractViolation`. Observed on Anthropic Haiku 4.5 where extended thinking consumes the 8192-token output budget before a ToolUse block emits and the API returns `pause_turn`. `EndTurn`, `StopToolUse`, `StopSequence`, and unknown reasons other than `pause_turn` continue to reject no-ToolUse responses (PR #1001).

## [0.160.0] - 2026-04-18

### Added

- **Event bus backpressure policy + stats + purpose labels** (PR #998).

## [0.159.0] - 2026-04-18

### Added

- **Env-driven CLI flags for non-interactive transports.** `transport_claude_code`, `transport_codex_cli`, and `transport_gemini_cli` now read a small, opt-in set of `OAS_*` environment variables at `build_args` time and append matching CLI flags. Config records are unchanged; unset env means "no flag added" so existing callers see identical argv. Intended for MASC keeper-style callers that want to tighten MCP / approval surface without a code change.
  - **Claude Code:** `OAS_CLAUDE_STRICT_MCP=1` → `--strict-mcp-config`; `OAS_CLAUDE_MCP_CONFIG=<file|json>` → `--mcp-config` (only when `config.mcp_config = None`); `OAS_CLAUDE_DISALLOWED_TOOLS="a,b"` → repeated `--disallowedTools` flags. LSP / hooks / auto-memory stay ON — no `--bare`.
  - **Codex CLI:** `OAS_CODEX_CONFIG="k=v,k2=v2"` → `-c k=v -c k2=v2` (the only way to toggle MCP / hooks / sandbox at the Codex boundary); `OAS_CODEX_SANDBOX=read-only|workspace-write|danger-full-access` → `-s`; `OAS_CODEX_PROFILE=<name>` → `-p`; `OAS_CODEX_SKIP_GIT=1` → `--skip-git-repo-check`.
  - **Gemini CLI:** `OAS_GEMINI_NO_MCP=1` → `--allowed-mcp-server-names ""` (empty whitelist = all MCP OFF); `OAS_GEMINI_ALLOWED_MCP="a,b"` → per-server whitelist; `OAS_GEMINI_APPROVAL_MODE=default|auto_edit|yolo|plan` → `--approval-mode` (supersedes `config.yolo` when set); `OAS_GEMINI_EXTENSIONS="a,b"` → repeated `-e` flags.
- `lib/llm_provider/cli_common_env.{ml,mli}` centralises env parsing (`get`, `bool`, `list`, `kv_pairs`) so the three transports agree on truthy values and splitting rules.

### Notes

- Gemini CLI has no runtime flag to disable hooks — hook lifecycle remains governed by the `gemini hooks` subcommand, outside transport scope.
- Codex CLI exposes no dedicated `--no-mcp` / `--no-hooks` flags; every toggle there flows through `-c key=value` TOML overrides.
- The stale "Gemini CLI does not yet expose flags for any of them" comment in `transport_gemini_cli.ml` was corrected — the parity config fields (`mcp_config` / `allowed_tools` / `max_turns` / `permission_mode`) are still unused because wiring them directly would silently reinterpret existing callers. Use the env vars above instead.

## [0.157.1] - 2026-04-18

### Fixed

- **TLS EOFs now stay on the retryable network-error path.** `llm_provider/http_client.ml` now maps `End_of_file` and `Sys_error` into `NetworkError`, so transient peer-close / socket-teardown failures no longer escape as top-level worker exceptions and instead flow through the existing retry/cascade logic (PR #995).
- **OpenAI-compatible sampling defaults now respect model capability records.** `apply_sampling_defaults` no longer auto-injects `min_p=0.05` for Gemini/GLM-style OpenAI-compatible models that advertise `supports_min_p = false`, which removes the warn-and-drop path while preserving the existing default for Qwen-class and unknown localhost endpoints.

## [0.155.1] - 2026-04-17

### Changed

- **OAS telemetry now stays inside the OAS boundary.** Provider fallback observability was removed from the public metrics/Event_bus surface so OAS remains a single-provider runtime and orchestration policy stays outside the SDK.
- **Runtime telemetry preserves raw-trace linkage and anomaly detail structurally.** Runtime participant events now carry `raw_trace_run_id`, `stop_reason`, `completion_anomaly`, and `failure_cause`, and sessions publish a `runtime-raw-trace-json` artifact so external consumers can correlate runtime state with raw traces without reverse-parsing free-form strings.

## [0.154.0] - 2026-04-17

### Added

- **`Content_replacement_event_bridge`.** Observer-only wrappers around `Content_replacement_state.record_replacement` / `record_kept` that publish `Custom("content_replacement_frozen", ...)` after successful state mutation, with an explicit `action` discriminator and `seen_count_after` payload (PR #982).
- **`Slot_scheduler_event_bridge`.** Stateless publisher that projects `Slot_scheduler.snapshot` onto `Custom("slot_scheduler_queue", ...)` with a derived `state = idle | queued | saturated` discriminator for downstream congestion observers (PR #983).
- **`Hooks.PostCompact` + `hooks.post_compact`.** Observer-only post-compaction lifecycle surface fired after successful proactive and emergency compaction, preserving the existing Event_bus behavior while exposing the reduced message set to hook consumers (PR #985).

### Fixed

- **Direct API dispatch now patches latency telemetry.** `Api.create_message` measures wall-clock request time on the non-cascade path and overwrites the parser-layer `request_latency_ms = 0` sentinel, so responses created through `Pipeline.stage_route -> Api.create_message` report real latency instead of zeros (PR #972).

### Removed (operator-facing)

- **`oas-review` CLI binary (`bin/review_agent.ml`).** Operator/example tool, not part of the SDK runtime contract. The same agent is still buildable from the example tree via `dune exec examples/review_agent.exe -- <owner/repo> <pr_number>` for anyone who wants the script. Audit reference: `docs/_audit/2026-04-17-coordination-leak-candidates.md` (PR #978).
- **`oas-autonomy-smoke` CLI binary (`bin/autonomy_smoke_cli.ml`).** Operator-facing diagnostics that wrapped `Autonomy_trace_analyzer` to compare divergence across runs. The underlying library module `Autonomy_trace_analyzer` remains exported through `Agent_sdk.Autonomy_trace_analyzer`; downstream consumers that want the same diagnostic loop can call the library directly instead of relying on the CLI. Audit reference: same file as above.

Net effect: the `agent_sdk` opam package no longer publishes `oas-review` or `oas-autonomy-smoke` binaries. `oas` and `oas-runtime` are unchanged. The `lib/` API surface is unchanged.

## [0.155.0] - 2026-04-17

Verification tests + real-world examples for the v0.154.0 event surface.

### Added

- **`test/test_event_integration.ml`** — end-to-end assertions that
  the new variants actually emit where they should:
  - Orchestrator error path publishes `AgentFailed` alongside
    `AgentCompleted(Error _)`.
  - `Agent.run_with_handoffs` emits `HandoffRequested` then
    `HandoffCompleted` in order, with the sub-prompt in `reason`.
  - `Hooks.invoke` dispatches `OnContextCompacted` payloads correctly
    and `Hooks.empty.on_context_compacted` defaults to `None`.
- **`test/test_multivendor_live.ml`** — live smoke test that drives
  the golden transcript against every reachable provider
  (Anthropic, OpenAI, Gemini via OpenAI-compat, and any
  OpenAI-compatible local endpoint discovered via `LLM_ENDPOINTS`:
  llama-server, Ollama, vLLM, LM Studio, TGI, …). Each case skips
  gracefully when its prerequisite is missing, so CI without
  credentials stays green. Verifies Invariants I1/I2
  (provider-agnostic native variants, envelope preservation) against
  real providers rather than mocks only.
- **`examples/agent_failure_observability.ml`** — subscribes to an
  Event_bus and drives an orchestrator failure path so the
  `AgentFailed` payload (`agent_name`, `task_id`, `error`, `elapsed`)
  is visible at runtime. No LLM / network required.
- **`examples/handoff_lifecycle.ml`** — two-agent handoff with an
  inline mock OpenAI-compatible server; prints the
  `HandoffRequested` → `HandoffCompleted` lifecycle so the `reason`
  and `elapsed` fields are self-documenting.

### Changed

- **`test_orchestrator.ml::test_event_bus_receives_completed`**
  no longer assumes `AgentCompleted` is the last event — since
  v0.154.0 the companion `AgentFailed` follows it on error paths,
  so the test now looks events up by payload shape and also
  asserts the `AgentFailed` companion is emitted.

## [0.154.0] - 2026-04-17

Event system cleanup + boundary enforcement.

### Added

- **`Event_bus.AgentFailed`** payload variant. Emitted alongside
  `AgentCompleted` whenever a task ends with `Error`. Subscribers that
  want to match on failure directly no longer need to destructure the
  `result` Result.t. Provider-agnostic.
- **`Event_bus.HandoffRequested`** and **`HandoffCompleted`** payload
  variants. Emitted at the sub-agent run bracket inside
  `Agent.run_with_handoffs`. Mirrors OpenAI Agents SDK
  `handoff_requested` / `handoff_occurred`. Provider-agnostic.
- **`Hooks.OnContextCompacted`** hook event + `on_context_compacted`
  field on the `hooks` record. Fires at the same call sites as
  `Event_bus.ContextCompacted`. Use this hook for audit / metrics;
  Event_bus remains for async observation.
- **`Journal_bridge.make`** now accepts `?correlation_id` and `?run_id`
  so journal events bridged onto `Event_bus` share the same envelope as
  the surrounding agent run.
- **`docs/EVENT-CATALOG.md`** single source of truth for every event
  surface (Event_bus, Hooks, Durable journal, Runtime protocol, LLM
  wire stream, A2A), reserved Custom namespaces, multi-vendor matrix,
  and the Hook vs Event decision matrix.
- **`test/test_multivendor_events.ml`** asserts Event_bus taxonomy
  invariants (envelope preservation, event_type_name stability, golden
  lifecycle transcript) that every provider must honor.

### Changed (Breaking)

- **Runtime events decomposed per variant.** Previously all 13
  `Runtime.event_kind` variants flattened into one
  `Custom("runtime.event", json)`; now each gets its own name:
  `runtime.session_started`, `runtime.turn_recorded`,
  `runtime.agent_became_live`, …, `runtime.session_failed`. Subscribers
  can filter by topic without JSON parsing. The stdout
  `Event_message` protocol write is unchanged (primary transport).
- **Durable Custom names normalized colon → dot.**
  `durable:turn_started` → `durable.turn_started` (8 names). Matches
  runtime and provider namespace convention.
- **`Event_forward.event_type_name` drops redundant `"custom."`
  prefix.** `Custom("runtime.session_started", _)` now maps to
  `"runtime.session_started"` (was `"custom.runtime.session_started"`).
  The Custom name itself is already a namespaced identifier.
- **`Event_bus.TaskStateChanged` removed.** Dead variant from
  v0.31–0.35 A2A roadmap — declared but never emitted, no consumers.
  The SSE-only `A2a_server.task_event` is a separate, unrelated type
  and is unaffected.
- **`Journal_bridge.make` signature** changed from
  `bus:Event_bus.t -> Durable_event.event -> unit` to
  `bus:Event_bus.t -> ?correlation_id:string -> ?run_id:string ->
   unit -> (Durable_event.event -> unit)`. Call sites: add `()`.
- **`Hooks.hooks` record** gains `on_context_compacted` field; `empty`
  and `compose` updated. Pattern matches on `Hooks.hook_event` must add
  an `OnContextCompacted` arm.
- **`Event_bus.payload` pattern matches** must add arms for the four
  new variants (`AgentFailed`, `HandoffRequested`, `HandoffCompleted`)
  and drop the `TaskStateChanged` arm.

### Refactored

- `eval_collector.ml` replaces `_ -> ()` wildcard with explicit arms
  per payload variant so future variants don't silently drop.

### Migration for downstream consumers

The only known consumer (masc-mcp) subscribes to native Event_bus
variants via explicit arms; expect compile errors against v0.154.0 for:
- new variants (add arms for `AgentFailed`, `HandoffRequested`,
  `HandoffCompleted`)
- removed variant (delete `TaskStateChanged` arm)
- `Hooks.hooks` record field addition
- `Journal_bridge.make` signature

External consumers of `Event_forward` JSONL / HTTP output must update
`event_type` string matching rules to drop the `"custom."` prefix and
adopt the dot convention for runtime/durable events.

See `docs/EVENT-CATALOG.md` for the full taxonomy and boundary
guidance.

## [0.153.1] - 2026-04-17

### Changed

- **Documentation alignment with code reality.** README rewritten so the architecture diagram, provider table, scope-limitations table, and version statement reflect what is actually wired in `lib/` and `lib_swarm/`. The literal version string has been removed from the README; `lib/sdk_version.ml` is the only source of truth (PR #976).
- **SDK boundary tightened: OAS docs no longer name any specific downstream coordinator.** Comment in `lib/agent/agent_types.ml`, `docs/sdk-independence-principle.md`, RFCs, and supporting docs were generalized to "downstream consumer" / "external coordinator". The OAS-vs-named-coordinator analysis at `docs/design/cascade-boundary-analysis.md` was moved to `docs/archive/2026-04/` (PR #976). `CHANGELOG.md` itself is intentionally untouched in that PR — it is a historical record.
- **Personal-project disclaimer added at the top of README.** Makes the no-SLA / no-support posture explicit (PR #976).
- **Repo hygiene.** `TODO.md` removed from tracking; `.gitignore` broadened to absorb common operator-local scratch (`_build_*/`, one-off `*.py`, ad-hoc analysis `.md`) so future drops don't slip into commits (PR #977).
- **Audit tracker added.** `docs/_audit/2026-04-17-coordination-leak-candidates.md` records the read-only sweep of `lib/`, `lib_swarm/`, `bin/`, and RFCs for coordination-layer leaks. Four low-risk candidates documented; structure otherwise clean (PR #978).

No code or API changes. Bump captures the documentation/hygiene cycle as a tagged release boundary.
## [0.153.0] - 2026-04-17

### Changed

**`Budget_strategy.default_summarizer` is now exported in `budget_strategy.mli`.** Previously it was only used internally as the fallback for `strategies_for_phase` and `reduce_for_budget` when no custom `summarizer` was supplied. With the 0.152.0 addition of `Agent.options.summarizer`, downstream consumers writing wrapper summarizers had to re-implement the extractive default byte-for-byte to preserve the `[Summary of N earlier messages]` shape. Exporting lets them delegate instead.

Signature: `val default_summarizer : Types.message list -> string`. No behavior change; this is strictly an API surface addition.

3 new tests in `test_budget_strategy.ml`: empty → `[No prior context]` marker, header + role prefix shape, per-message truncation at 100 chars.

## [0.152.0] - 2026-04-17

### Added

**`Agent_types.options.summarizer : (message list -> string) option`.** Exposes the Budget_strategy Emergency-phase summarizer callback as a per-agent option so downstream consumers can inject a domain-aware summary function. When `None` (default), the built-in `Budget_strategy.default_summarizer` is used and behavior is unchanged.

- `Builder.with_summarizer` setter registers a custom callback.
- `pipeline.ml` (`proactive_compact` + `emergency_compact`) threads `?summarizer:agent.options.summarizer` into `Budget_strategy.reduce_for_budget`.
- The existing `?summarizer` optional parameter on `Budget_strategy.reduce_for_budget` is unchanged; it is simply now reachable from `Agent.options`.

Motivation: consumers that embed application-specific structured markers in message bodies (e.g. `[STATE]...[/STATE]` blocks) need a way to strip or transform those markers before they are re-injected as compacted history. Previously the only path was post-hoc scrubbing via `context_reducer`, which runs **after** `reduce_for_budget` — by that point the `[Summary of N earlier messages]` string is already materialized. Exposing the summarizer on `Agent.options` closes that gap while keeping OAS agnostic of any specific marker syntax.

PR #973.

## [0.151.0] - 2026-04-16

### Added

**`Anthropic cache_extended_ttl` field.** Opt-in 1-hour prompt cache TTL for Anthropic Messages API callers. When `cache_extended_ttl = true`, cache control blocks are stamped with the extended-beta TTL instead of the default 5-minute window.

PR #962.

## [0.150.0] - 2026-04-16

### Removed (breaking, operator-facing)

**`OAS_OLLAMA_SUPPORTS_TOOL_CHOICE` env var deleted.** The process-wide env knob coupled deployment config to library semantics: a single boolean forced every Ollama-served model into the same `supports_tool_choice` setting, and the SDK silently matched on the runtime environment instead of the caller's declared config. Consumers that want a model-specific override now declare it per-call; see Added below.

Deleted:
- `Capabilities.parse_ollama_supports_tool_choice_env` (`lib/llm_provider/capabilities.ml`)
- `Capabilities.ollama_supports_tool_choice_default` (module-init env read)
- 12 inline `%test` cases exercising the env parser
- `ollama_capabilities.supports_tool_choice` is now hardcoded `false`; the documented Qwen3.5+Jinja opt-in path is through the new `supports_tool_choice_override` field.

### Added

**`Provider_config.supports_tool_choice_override : bool option`.** Lets the caller declare per-config whether their model honors `tool_choice:required`. `None` falls through to the per-kind default in `Capabilities` and the per-model override in `Capabilities.for_model_id`. `Some b` wins over both.

Rationale: keeps the SDK model-agnostic. The SDK no longer matches on `model_id` substrings ("qwen") to guess model-side behavior — the consumer (e.g. a cascade loader that knows it deployed Qwen3.5 with the Jinja chat template) declares the fact on each `Provider_config.t`.

Consumed by `backend_openai.build_request`: when `supports_tool_choice_override = Some b`, `b` is used instead of the capability record to decide whether the `tool_choice` body field is sent.

### Migration

- Callers that set `OAS_OLLAMA_SUPPORTS_TOOL_CHOICE=1` in deployment: drop the env var, pass `~supports_tool_choice_override:true` to `Provider_config.make` instead (or declare it in your cascade config and thread it through).
- Callers that did not set the env var: no behavioral change (default was already `false`).

## [0.148.0] - 2026-04-15

### Removed (breaking)

**Legacy cascade API fully evicted.** 0.146.0 removed `lib/llm_provider/cascade_*` modules but the older primary+fallbacks cascade API remained (Api/Retry/Provider/Builder/Agent layers). This release deletes it too, making OAS unambiguously single-provider.

Deleted:
- `type Provider.cascade` + `Provider.cascade` constructor (`lib/provider.ml/.mli`)
- `Api.create_message_cascade` (`lib/api.ml/.mli`)
- `Retry.with_cascade` (`lib/llm_provider/retry.ml/.mli`)
- `Builder.with_cascade`, `Builder.with_fallback` (`lib/agent/builder.ml/.mli`)
- `options.cascade` field on agent options (`lib/agent/agent_types.ml/.mli`, `lib/agent/agent.mli`)
- `cascade` field on `Agent_card.agent_info` + cascade_providers export (`lib/protocol/agent_card.ml/.mli`)
- `Pipeline.stage_route` cascade branch (now always `Api.create_message` single-provider)
- Tests: cascade cases in `test_deep_coverage.ml`, `test_e2e_v024.ml`, `test_bug_hunt.ml`, `test_provider.ml`, `test_builder_coverage.ml`
- Example: `examples/custom_provider.ml` (cascade demo) + `examples/dune` stanza

### Migration

Callers that relied on `Builder.with_cascade` / `Builder.with_fallback`:
1. Implement your own cascade loop around `Agent.run` / `Api.create_message`, OR
2. Use MASC's `lib/cascade/` if you're building in the MASC ecosystem.

Canonical pattern (MASC `oas_worker_named.ml` `try_cascade`):
```ocaml
let rec try_cascade remaining last_err =
  match remaining with
  | [] -> Error (format_exhausted_error last_err)
  | provider :: rest ->
    match Agent.run_with_provider agent ~provider with
    | Ok r -> Ok r
    | Error e when should_retry e -> try_cascade rest (Some e)
    | Error e -> Error e
```

### Rationale

Two parallel cascade systems existed in OAS: the `cascade.json`-based FSM (removed in 0.146.0) and this legacy primary+fallbacks record. Both are orchestration responsibilities, not SDK responsibilities. The SDK is now a pure `Provider_config.t → api_response` function.

## [0.146.0] - 2026-04-15

### Removed (breaking)

**Cascade completely evicted from OAS.** OAS is now a pure single-provider SDK.

Deleted modules (lib/llm_provider/):
- `cascade_config.ml` + `.mli` — parser/resolver utilities
- `cascade_config_loader.ml` + `.mli` — cascade.json hot-reload
- `cascade_fsm.ml` + `.mli` — pure decision FSM
- `cascade_health_filter.ml` — error classification
- `cascade_health_tracker.ml` + `.mli` — provider success/fail + cooldown
- `cascade_model_resolve.ml` + `.mli` — glm:auto alias resolution
- `cascade_throttle.ml` + `.mli` — local endpoint slot table

Deleted tests:
- `test/test_cascade.ml`, `test_cascade_config.ml`, `test_cascade_config_ext.ml`, `test_cascade_deep.ml`

Deleted example:
- `examples/cascade_failover.ml`

### Internal

- `Provider_bridge.resolve_auto_model_id` inlined (was delegating to `Cascade_model_resolve`)
- `Provider_bridge.resolve_glm_model_id`, `resolve_glm_coding_model_id` inlined (direct `Zai_catalog` calls)
- Stale docstring references (`{!Cascade_config}`) left in `constants.ml`, `provider_registry.mli`, `provider.ml`/`.mli` — to be purged in follow-up cleanup

### Migration

Cascade ownership moved to MASC (masc-mcp#7382 scaffold + #7386 migrate).
Consumers previously using `Llm_provider.Cascade_*` should:
1. Implement their own cascade loop around `Complete.complete`, OR
2. Use MASC's `lib/cascade/` modules (import MASC's masc_cascade or replicate the pattern)

Reference: MASC `oas_worker_named.ml` `try_cascade` function shows the canonical single-provider-loop pattern.

### Rationale

Cascade is an orchestrator responsibility, not an SDK responsibility. OAS was accumulating orchestration code (FSM, health tracking, throttle tables, weighted selection) that tightly coupled the SDK to MASC's operational model. Extracting cascade to MASC lets OAS focus on one job: "give me a Provider_config.t, I'll call it and return the response."

## [0.144.0] - 2026-04-15

### Removed (breaking)

- `Cascade_executor` module deleted (839 lines). Zero remaining callers
  after Judge and Tool_selector migrated to single-provider in 0.142.0
  and 0.143.0. Callers that need multi-provider failover drive their
  own loop around `Complete.complete` (MASC's `oas_worker_named.ml`
  `try_cascade` is the reference pattern).

### Internal

- Stale docstring references to `Cascade_executor` removed from
  `Context_reducer`, `Text_estimate`, and `test_complete_http`.

Note: `Cascade_config`, `Cascade_fsm`, `Cascade_health_tracker`,
`Cascade_health_filter`, `Cascade_throttle`, `Cascade_model_resolve`
remain for this release — they are SDK utilities that MASC still
consumes directly. A follow-up PR moves them into MASC and removes
them from OAS entirely (MAJOR bump).

## [0.143.0] - 2026-04-15

### Changed (breaking)

- `Tool_selector.default_rerank_fn` now takes `~provider:Provider_config.t`
  instead of `~cascade_name`, `~defaults`, `?config_path`, `?clock`. OAS
  no longer resolves cascade.json inside the rerank closure — callers
  that want multi-provider failover pick a single `Provider_config.t`
  per rerank (MASC selects from `cascade.json` and passes the winner).
- Implementation: replaces 4-step cascade resolve + `Cascade_executor.
  complete_cascade_with_accept` with a direct `Complete.complete` call.
  Rerank still overrides sampling (temperature 0.0, max_tokens 200) for
  deterministic, short replies. BM25 fallback on LLM failure unchanged.

### Migration

Before:
```ocaml
let rerank = Tool_selector.default_rerank_fn
  ~sw ~net ~cascade_name:"tool_selector"
  ~defaults:["llama:auto"] ~k:5 ()
```

After:
```ocaml
(* Caller resolves one provider from whatever source and passes it in. *)
let provider = (* ... *) in
let rerank = Tool_selector.default_rerank_fn
  ~sw ~net ~provider ~k:5 ()
```

Rationale: cascade is MASC's responsibility. OAS is a single-provider SDK.

## [0.142.0] - 2026-04-15

### Changed (breaking)

- `Judge.judge` now takes `~provider:Provider_config.t` instead of
  resolving a named cascade internally. OAS no longer owns cascade
  orchestration — callers that want multi-provider failover pick a
  single `Provider_config.t` per call (MASC selects from `cascade.json`
  and passes the winner).
- `Judge.judge_config` dropped `cascade_name` and `max_turns` fields.
  `max_turns` was dead (single-turn evaluation is the only mode);
  `cascade_name` belongs in the orchestrator, not in OAS.
- `Judge.judge` dropped `?clock` and `?config_path` parameters.
  Single-provider calls do not retry with backoff and do not read
  `cascade.json`.

### Migration

Before:
```ocaml
let cfg = { Judge.default_config () with cascade_name = "my-judge" } in
Judge.judge ~sw ~net ?clock ?config_path ~config:cfg ~context ()
```

After:
```ocaml
(* Caller resolves one provider from whatever source (cascade.json,
   a static value, etc.) and passes it in. *)
let provider = (* ... *) in
let cfg = Judge.default_config () in
Judge.judge ~sw ~net ~provider ~config:cfg ~context ()
```

Rationale: cascade is MASC's responsibility. OAS is a single-provider SDK.

## [0.140.0] - 2026-04-15

### Removed (breaking)

- `Cascade_config.complete_named`, `Cascade_config.complete_named_stream` —
  the named-cascade convenience wrappers. Callers should compose the
  primitives directly:
    1. `Cascade_config.resolve_model_strings` (config lookup + defaults)
    2. `Cascade_config.expand_model_strings_for_execution`
    3. `Cascade_config.parse_model_strings`
    4. `Cascade_config.filter_healthy`
    5. `Cascade_executor.complete_cascade_with_accept` (or `_stream`)
  In-tree consumers `judge.ml` (oas#925) and `tool_selector.ml` (oas#926)
  were migrated to this pattern in the previous two releases.
- `Cascade_config.complete_cascade_with_accept`,
  `Cascade_config.complete_cascade_stream` re-export shims — call
  `Cascade_executor.complete_cascade_with_accept` /
  `Cascade_executor.complete_cascade_stream` directly.
- `Cascade_config.filter_healthy_internal` private helper — only callers
  were the deleted wrappers above. Public `Cascade_config.filter_healthy`
  is unchanged.

### Migration

```ocaml
(* Before: *)
Cascade_config.complete_named ~sw ~net ?clock ?config_path
  ~name ~defaults ~messages ~temperature ~max_tokens ()

(* After: *)
let model_strings =
  Cascade_config.resolve_model_strings ?config_path ~name ~defaults ()
  |> Cascade_config.expand_model_strings_for_execution
in
let providers =
  Cascade_config.parse_model_strings ~temperature ~max_tokens model_strings
in
let healthy = Cascade_config.filter_healthy ~sw ~net providers in
Cascade_executor.complete_cascade_with_accept ~sw ~net ?clock
  ~accept:(fun _ -> Ok ()) healthy ~messages ~tools:[]
```

## [0.139.0] - 2026-04-15

### Added
- `Cascade_config.resolve_model_strings_with_trace` — structured selection
  trace for cascade decisions. Returns the ordered model list plus a
  `selection_trace` with per-candidate `config_weight`, `effective_weight`,
  `success_rate`, and `in_cooldown`. Enables dashboards/telemetry to
  surface *why* a provider was chosen first without re-deriving state.
- `Cascade_health_tracker.provider_info` / `all_providers` — structured
  snapshot API for the rolling-window health tracker. Returns
  `provider_info` records (key, success rate, consecutive failures,
  cooldown state + expiry, events in window). Complements the existing
  string-based `provider_summary`.

### Notes
- No breaking changes. Existing consumers of `resolve_model_strings_traced`
  and `provider_summary` are unaffected.

## [0.138.0] - 2026-04-15

### Fixed
- `Context_reducer` now strips orphaned `ToolResult` blocks before the API
  call so compacted contexts do not trip `tool_call_id` validation on
  OpenAI-compatible providers (#917).
- `Utf8_sanitize` strips disallowed control characters to prevent
  downstream JSON serialization faults (#916).

## [0.137.0] - 2026-04-15

### Added
- Weighted cascade routing for named/default profiles:
  - `cascade.json` model arrays now accept `{ "model", "weight" }` entries.
  - First-attempt provider selection uses weighted random ordering while
    preserving deterministic fallback priority.
  - Health-aware weight adjustment and cooldown-aware filtering feed the
    effective provider order before execution (#911).

### Fixed
- Anthropic response parsing now initializes telemetry placeholders
  consistently with the other provider backends (#912).
- OpenAI-compatible request builders strip orphaned `ToolResult` blocks
  before serialization so compacted contexts no longer trip invalid
  `tool_call_id` errors on GLM/Groq/DeepSeek paths (#914).

## [0.136.0] - 2026-04-14

### Added
- `Event_bus.ContextOverflowImminent` payload — proactive warning emitted
  before context overflow occurs, with `estimated_tokens`, `limit_tokens`,
  and `ratio` fields (#901).
- `Event_bus.ContextCompactStarted` payload — marks compaction start with
  a `trigger` field (`proactive`, `emergency`, or `operator`) (#901).
- `Context_reducer.estimate_next_turn_overhead` — estimates fixed overhead
  (system prompt + tool descriptions + output reserve) for budget projection
  before the next turn (#901).
- `Cascade_executor.truncate_to_context_strict` — strict variant of
  `truncate_to_context` that returns `Error (\`Over_budget (est, budget))`
  instead of silently passing over-budget content. Original function
  unchanged for backward compatibility (#901).

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
 
