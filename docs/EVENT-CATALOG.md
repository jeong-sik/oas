# OAS Event Catalog

Single source of truth for every event surface exposed by OAS. This
document covers *what* events exist, *where* they are emitted, *how* they
relate, and the contracts downstream consumers can rely on.

**Scope**: `agent_sdk` library (`lib/`).
**Status**: Stable catalog; entries marked *Evolving* may change with
deprecation notice.
**Last updated**: v0.154.1.

---

## 1. Five event surfaces at a glance

OAS carries five independent event surfaces. They are listed here in order
of external relevance.

| # | Surface | Module | Transport | Audience |
|---|---------|--------|-----------|---------|
| 1 | **Event_bus** | `Event_bus` | In-process pub/sub | Library consumers (subscribers) |
| 2 | **Hooks** | `Hooks` | Synchronous callback | Library consumers (interception/audit) |
| 3 | **Durable journal** | `Durable_event` | In-memory append-only + JSONL | Crash recovery + event-sourced replay |
| 4 | **Runtime protocol** | `Runtime` + `Runtime_server_*` | stdout JSON-RPC-ish protocol | `oas_runtime` subprocess consumers |
| 5 | **LLM wire stream** | `Types.sse_event` | Provider-specific SSE → normalized | Internal (streaming accumulation) |
| 6 | **A2A task stream** | `A2a_server.task_event` | HTTP SSE | External A2A clients |

Surfaces 3 and 4 **bridge into** Surface 1 via `Journal_bridge` and
`Runtime_server_types.emit_event` respectively (see §7).

Surfaces 5 and 6 are independent and do not flow to Event_bus.

---

## 2. Surface 1: `Event_bus` (public pub/sub)

**Header**: `lib/event_bus.mli`. Stability: Evolving.

### 2.1 Envelope

Every event carries a common envelope:

```ocaml
type envelope = {
  correlation_id: string;  (* session-level, stable across a run *)
  run_id: string;          (* per-run identifier *)
  ts: float;               (* Unix epoch seconds *)
}
```

**Contract**: `correlation_id` is constant for all events belonging to the
same logical session. `run_id` is unique per agent run. Envelopes are
filled by producers, never rewritten by subscribers.

### 2.2 Native payload variants

Pattern-matchable OCaml sum type. **Stable across every provider.**

| Variant | Emit site | Semantic |
|---------|-----------|---------|
| `AgentStarted` | `orchestrator.ml:run_task` | Orchestrator begins work on a task |
| `AgentCompleted` | `orchestrator.ml:run_task` (success) | Result type captures success/error |
| `AgentFailed` | `orchestrator.ml:run_task` (error branch) | Explicit failure companion to `AgentCompleted` |
| `TurnStarted` | `pipeline/pipeline.ml`, `pipeline/pipeline_input.ml` | Start of a single agent turn |
| `TurnCompleted` | `pipeline/pipeline.ml`, `pipeline/pipeline_collect.ml` | End of a single agent turn |
| `ToolCalled` | `agent/agent_tools.ml` | Tool invocation requested by LLM |
| `ToolCompleted` | `agent/agent_tools.ml` | Tool invocation result available |
| `HandoffRequested` | `agent/agent_handoff.ml` | Agent delegates control to another agent |
| `HandoffCompleted` | `agent/agent_handoff.ml` | Handoff target finished |
| `ElicitationCompleted` | `pipeline/pipeline.ml`, `pipeline/pipeline_input.ml` | User elicitation round completed |
| `ContextOverflowImminent` | `pipeline/pipeline.ml` | Projected next-turn tokens will exceed budget |
| `ContextCompactStarted` | `pipeline/pipeline.ml` | Compaction begun (before completion) |
| `ContextCompacted` | `pipeline/pipeline.ml`, `pipeline/pipeline_compaction.ml` | Compaction completed (before_tokens → after_tokens) |
| `ContentReplacementReplaced` / `ContentReplacementKept` | `content_replacement_event_bridge.ml` | Tool-result content replacement decision froze |
| `SlotSchedulerObserved` | `slot_scheduler_event_bridge.ml` | Queue/slot snapshot of the provider scheduler |
| `Custom (name, json)` | anywhere | Extension point — see §2.3 |

**Invariants**:
- **I1 Provider-agnostic**: every native payload field is meaningful regardless of which provider serves the underlying LLM.
- **I2 Stable envelope**: envelope field set is identical across providers.
- **I6 Multi-vendor**: a native variant is only added if its semantic exists in ≥2 vendor SDKs.

### 2.3 Custom namespaces (reserved)

`Custom(name, json)` is the open-ended extension point. **The `name`
string must be a dot-separated, lowercase, snake-case namespaced
identifier.** The following prefixes are reserved:

| Prefix | Owner | Purpose |
|--------|-------|---------|
| `runtime.*` | OAS | Events from `Runtime` session protocol (§5) |
| `durable.*` | OAS | Events from `Durable_event` journal (§4) |
| `provider.*` | OAS | Provider-specific escape hatch — e.g. `provider.anthropic.cache_hit`, `provider.openai.reasoning_tokens`, `provider.gemini.safety_rating` |
| `oas.*` | OAS | Reserved for future OAS use |
| `<downstream>.*` | Downstream | Any other prefix; pick one and stick to it |

**Downstream publishers SHOULD NOT use OAS's `Event_bus` as a general-
purpose telemetry channel for their own domain events.** Create your own
`Event_bus.t` instance for your events and bridge into OAS's where
necessary via a forwarder.

### 2.4 Filters, subscriptions, and draining

```ocaml
val subscribe : ?filter:filter -> t -> subscription
val drain     : subscription -> event list
```

Filters compose: `filter_any`, `filter_all`, `filter_agent`,
`filter_tools_only`, `filter_topic`, `filter_correlation`, `filter_run`.

### 2.5 What is **not** in the native taxonomy (by design)

- **Thinking / reasoning events** — provider semantics diverge (Anthropic
  encrypted `thinking` block; OpenAI `reasoning` summary; DeepSeek-R1
  raw reasoning; Qwen3 soft thinking). Ride in `provider.<name>.thinking`
  until an RFC reconciles semantics.
- **Prompt caching events** — Anthropic `cache_creation_input_tokens`,
  OpenAI `prompt_tokens_details.cached_tokens`. Different billing
  semantics; use `provider.<name>.cache_hit` for now.

---

## 3. Surface 2: `Hooks` (synchronous callback)

**Header**: `lib/hooks.mli`. Stability: Evolving.

Hooks are synchronous interception points registered on `Agent` at build
time. Use Hooks when you need to **audit** or **interfere with** a step;
use Event_bus when you only need to **observe**.

Available hooks (post-v0.154.0):

| Hook | Signature | Purpose |
|------|-----------|---------|
| `before_turn` | `Types.agent_state -> unit` | Called before a turn starts |
| `after_turn` | `Types.agent_state -> unit` | Called after a turn completes |
| `pre_tool_use` | `tool_name:string -> input:Yojson.Safe.t -> tool_policy` | Filter/modify tool invocation |
| `post_tool_use` | `tool_name:string -> output:Types.tool_result -> unit` | Observe tool result |
| `on_context_compacted` | `agent_state -> before_tokens:int -> after_tokens:int -> phase:string -> unit` | **New in v0.154.0** — observe compaction for audit/metrics |
| `elicit` | `question:string -> elicitation_response` | Interactively gather user input |

See `lib/hooks.mli` for full contract.

### 3.1 Hook vs Event decision matrix

| Use Hook when… | Use Event_bus when… |
|----------------|---------------------|
| You need to modify the step's behavior (approve/deny/edit) | You are strictly observing |
| You need synchronous back-pressure | Async fan-out is fine |
| Call site guarantees delivery | Best-effort delivery is acceptable |
| Scope is a single agent | Scope may cross agents/sessions |
| Failure should abort the agent | Subscriber failure is isolated |

---

## 4. Surface 3: Durable journal

**Header**: `lib/durable_event.mli`. Stability: Evolving. @since 0.89.0.

Event-sourced record of everything needed to reconstruct agent state on
crash recovery. Events are immutable; append-only.

### 4.1 Journal variants

| Variant | Purpose |
|---------|---------|
| `Turn_started` | New agent turn |
| `Llm_request` | LLM call issued (turn, model, input_tokens) |
| `Llm_response` | LLM call returned (output_tokens, stop_reason, duration_ms) |
| `Tool_called` | Tool invoked with idempotency key + input hash |
| `Tool_completed` | Tool finished; output recorded for replay skip |
| `State_transition` | Agent state machine change |
| `Checkpoint_saved` | Persistence milestone |
| `Error_occurred` | Error recorded with domain + detail |

### 4.2 Projection onto Event_bus

When a journal is created with `Journal_bridge.make ~bus ()`, each append
is mirrored to the Event_bus as a `Custom("durable.<kind>", json)` event
— one name per journal variant, dot-separated.

| Journal variant | Custom name |
|-----------------|-------------|
| `Turn_started` | `durable.turn_started` |
| `Llm_request` | `durable.llm_request` |
| `Llm_response` | `durable.llm_response` |
| `Tool_called` | `durable.tool_called` |
| `Tool_completed` | `durable.tool_completed` |
| `State_transition` | `durable.state_transition` |
| `Checkpoint_saved` | `durable.checkpoint_saved` |
| `Error_occurred` | `durable.error_occurred` |

**Correlation**: `Journal_bridge.make` accepts `?correlation_id` and
`?run_id` so durable events can be attached to the same envelope chain
as the surrounding agent run.

### 4.3 Deduplication advisory

`agent/agent_tools.ml` publishes native `ToolCalled`/`ToolCompleted`
events AND appends to the journal. If you attach `Journal_bridge` to the
journal AND subscribe to Event_bus, **both** native and `durable.*`
versions will arrive for the same tool call. This is intentional — the
native variants are provider-agnostic snapshots, the `durable.*` variants
carry replay metadata (idempotency keys). Downstream code that counts
tool calls from Event_bus should filter on one or the other.

---

## 5. Surface 4: Runtime protocol

**Header**: `lib/runtime.ml`. Stability: Evolving.

Session-level runtime for `oas_runtime` subprocess usage. Runtime emits
on **two channels simultaneously**:

1. **Primary**: JSON-RPC-ish protocol message `Event_message { session_id; event }` written to stdout.
2. **Secondary**: Event_bus publish as `Custom("runtime.<kind>", json)`.

### 5.1 Runtime event variants

| Variant | Custom name |
|---------|-------------|
| `Session_started` | `runtime.session_started` |
| `Session_settings_updated` | `runtime.session_settings_updated` |
| `Turn_recorded` | `runtime.turn_recorded` |
| `Agent_spawn_requested` | `runtime.agent_spawn_requested` |
| `Agent_became_live` | `runtime.agent_became_live` |
| `Agent_output_delta` | `runtime.agent_output_delta` |
| `Agent_completed` | `runtime.agent_completed` |
| `Agent_failed` | `runtime.agent_failed` |
| `Artifact_attached` | `runtime.artifact_attached` |
| `Checkpoint_saved` | `runtime.checkpoint_saved` |
| `Finalize_requested` | `runtime.finalize_requested` |
| `Session_completed` | `runtime.session_completed` |
| `Session_failed` | `runtime.session_failed` |

**Name collision note**: `Runtime.Agent_completed` (session-level
participant lifecycle) is distinct from `Event_bus.AgentCompleted`
(single-task orchestrator result). They live in different modules with
different payload shapes. Disambiguate by Custom name prefix:
`runtime.agent_completed` vs native `AgentCompleted`.

**Structured completion/failure metadata**:
- `Runtime.participant_event` now carries optional `raw_trace_run_id`,
  `stop_reason`, `completion_anomaly`, and `failure_cause`.
- New sessions artifact `runtime-raw-trace-json` publishes the latest
  raw-trace run refs, summaries, and validations so external consumers
  can correlate live runtime events with persisted raw traces without
  reconstructing the proof bundle first.

---

## 6. Surface 5: LLM wire stream (`Types.sse_event`)

**Header**: `lib/llm_provider/types.ml`. Stability: Internal.

Normalized representation of provider SSE streams. Each provider's
`api_<name>.ml` parses its wire format and produces `Types.sse_event`
values; `streaming.ml` accumulates them into a final `Types.api_response`.

| Variant | Semantic |
|---------|---------|
| `MessageStart` | Stream begun (id, model, initial usage) |
| `ContentBlockStart` | New content block — text, thinking, or tool_use |
| `ContentBlockDelta` | Incremental content (text / thinking / input JSON) |
| `ContentBlockStop` | Content block finished |
| `MessageDelta` | Partial message update (stop_reason, usage) |
| `MessageStop` | Stream finished |
| `Ping` | Keepalive |
| `SSEError` | Stream-level error |

### 6.1 Provider mapping

| Provider wire event | `sse_event` |
|---------------------|-------------|
| Anthropic `message_start` | `MessageStart` |
| Anthropic `content_block_delta` | `ContentBlockDelta` |
| OpenAI `response.created` | `MessageStart` |
| OpenAI `response.output_text.delta` | `ContentBlockDelta(TextDelta)` |
| OpenAI `response.output_item.added` | `ContentBlockStart` |
| OpenAI `response.completed` | `MessageStop` |
| Gemini `streamGenerateContent` chunks | Synthesized via `emit_synthetic_events` |

SSE events **do not reach Event_bus**. They are consumed by the stream
accumulator inside the provider API module, and only the final
`api_response` (and whatever Event_bus events the agent layer emits
around that) become observable.

---

## 7. Surface 6: A2A task stream

**Header**: `lib/protocol/a2a_server.ml`. Stability: Evolving.

HTTP SSE stream emitted by the A2A protocol server for external A2A
clients. **Independent of Event_bus.**

| Variant | Purpose |
|---------|---------|
| `TaskStateChanged` | Task transitioned to an intermediate state |
| `TaskCompleted` | Task reached a terminal state |

---

## 8. Multi-vendor compatibility matrix

Every OAS-supported LLM vendor produces the **same native Event_bus
payload variants** when running via OAS. Provider-specific signals go
through `Custom("provider.<name>.<event>", ...)`.

| Provider | Native: Agent/Turn/Tool | Native: Handoff | Native: Context* | Provider-specific Custom |
|----------|------------------------|-----------------|------------------|--------------------------|
| Anthropic | ✓ | ✓ | ✓ | `provider.anthropic.cache_hit`, `provider.anthropic.thinking` |
| OpenAI (Chat/Responses) | ✓ | ✓ | ✓ | `provider.openai.reasoning_tokens`, `provider.openai.prompt_cache_hit` |
| Gemini | ✓ | ✓ | ✓ | `provider.gemini.safety_rating`, `provider.gemini.thinking` |
| GLM / ZhipuAI | ✓ | ✓ | ✓ | `provider.glm.*` |
| OpenRouter | ✓ | ✓ | ✓ | inherits upstream, namespaced under `provider.openrouter.*` |
| Groq / DeepSeek / Alibaba / SiliconFlow / xAI | ✓ | ✓ | ✓ | `provider.<name>.*` |
| llama.cpp / llama-server (local) | ✓ | ✓ | ✓ | `provider.llama.*` |
| Ollama (local) | ✓ | ✓ | ✓ | `provider.ollama.*` |
| vLLM / LM Studio / TGI / MLX (local) | ✓ | ✓ | ✓ | `provider.<runtime>.*` |
| Custom (`custom:model@url`) | ✓ | ✓ | ✓ | caller-defined |

**Verification**: `test/test_multivendor_events.ml` asserts a golden
Event_bus sequence (`AgentStarted → TurnStarted → ToolCalled →
ToolCompleted → TurnCompleted → AgentCompleted`) across every available
provider. Missing credentials / endpoints skip gracefully.

---

## 9. Event forwarding

**Header**: `lib/event_forward.mli`. Stability: Evolving.

Delivers Event_bus events to external targets (JSONL files, custom
callbacks) in a separate Eio fiber. Best-effort; failures log a warning.

### 9.1 `event_type` mapping

For each Event_bus event, `Event_forward.event_type_name` returns a flat
string identifier:

| Event_bus variant | event_type |
|-------------------|-----------|
| `AgentStarted` | `agent.started` |
| `AgentCompleted` | `agent.completed` |
| `AgentFailed` | `agent.failed` |
| `TurnStarted` / `TurnCompleted` | `turn.started` / `turn.completed` |
| `ToolCalled` / `ToolCompleted` | `tool.called` / `tool.completed` |
| `HandoffRequested` / `HandoffCompleted` | `handoff.requested` / `handoff.completed` |
| `ElicitationCompleted` | `elicitation.completed` |
| `ContextOverflowImminent` | `context.overflow_imminent` |
| `ContextCompactStarted` / `ContextCompacted` | `context.compact_started` / `context.compacted` |
| `ContentReplacementReplaced` / `ContentReplacementKept` | `content_replacement.replaced` / `content_replacement.kept` |
| `SlotSchedulerObserved` | `slot_scheduler.observed` |
| `Custom(name, _)` | `name` (unchanged — the name is already a namespaced identifier) |

### 9.2 Targets

| Target | Behavior |
|--------|---------|
| `File_append { path }` | Appends one JSON object per line to `path` |
| `Custom_target { name; deliver }` | Invokes `deliver` for each event |

---

## 10. How to add a new event

1. **Decide the surface**. Is this:
   - A general agent lifecycle signal (≥2 providers)? → `Event_bus` native variant.
   - An audit/interception point? → `Hooks` callback.
   - A replay-relevant record? → `Durable_event` variant.
   - Provider-specific? → `Custom("provider.<name>.<event>", json)`.
   - Downstream domain event? → downstream's own Event_bus, not OAS's.

2. **Provider-agnostic check** (native variants only, per I6):
   - Confirm the semantic exists in Anthropic + OpenAI + Gemini.
   - Payload fields must make sense regardless of which provider is running.
   - If provider-specific info leaks in, redesign the payload or use `Custom`.

3. **Update artifacts**:
   - Add to `Event_bus.mli` with doc comment + `@since`.
   - Filter branch in `event_bus.ml`.
   - `event_type_name` arm in `event_forward.ml`.
   - `agent_name_of_event` arm.
   - `event_to_payload` arm.
   - Emit site(s) in the appropriate module.
   - Add this doc row.
   - Update `test/test_multivendor_events.ml` golden transcript if the new event is part of the standard lifecycle.

4. **Exhaustiveness**: confirm `lib/eval_collector.ml` and other `match`
   sites on `Event_bus.payload` cover the new variant (they use explicit
   arms, not `_`, so the compiler will flag omissions).

## 11. How to add a new provider

1. **Hosted API**: create `lib/api_<name>.ml` implementing the same
   normalized streaming interface (`Types.sse_event` output).
   OpenAI-compatible endpoints don't need a new module — add an entry to
   the routing table in `api_openai.ml` or via `custom:model@url`.
2. **SSE normalization**: map the provider's wire events to
   `Types.sse_event` constructors. Document the mapping in §6.1.
3. **Provider-specific extensions**: if the provider exposes unique
   signals (caching, reasoning summaries, safety ratings) that users want
   to observe, publish them as `Custom("provider.<name>.<event>", json)`.
4. **Verification**: add a row to the multi-vendor matrix (§8) and,
   if desired, a live-mode assertion in `test_multivendor_events.ml`.
5. **No Event_bus taxonomy change**. Adding a provider must not add a
   native variant — if it seems to, revisit I6.

---

## 12. Industry comparison

| Concept | OAS | OpenAI Agents SDK | Claude Agent SDK | LangGraph |
|---------|-----|-------------------|------------------|-----------|
| Primary surface | `Event_bus` + `Hooks` | `RawResponsesStreamEvent` / `RunItemStreamEvent` / `AgentUpdatedStreamEvent` | Hooks (18 events) | `astream_events` |
| Event shape | Tagged OCaml sum (pattern match) | Flat `type` string + JSON | Hook callback arguments | Flat `event` string + data/metadata |
| Raw LLM stream | `Types.sse_event` (internal) | `RawResponsesStreamEvent` | hidden | `on_chat_model_stream` |
| Tool events | `ToolCalled`/`ToolCompleted` | `tool_called` / `tool_output` | `PreToolUse` / `PostToolUse` | `on_tool_start` / `on_tool_end` |
| Handoff events | `HandoffRequested` / `HandoffCompleted` | `handoff_requested` / `handoff_occurred` | — | via node transitions |
| Interception | Hooks | on_handoff callbacks | Hook return value | interrupts |

---

## 13. Stability

Entries in this catalog carry the stability tier of their module (see
`docs/api-stability.md`). Breaking changes are tracked in `CHANGELOG.md`
under a dated **Breaking** section.
