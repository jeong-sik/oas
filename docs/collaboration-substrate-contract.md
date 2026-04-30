# Collaboration Substrate Observation Contract

## Purpose

This document defines the OAS-side contract for downstream applications that
add a real-time collaboration substrate around OAS agents.

OAS does not own the collaboration substrate. Editors, CRDT documents,
WebSocket relays, Git graph renderers, shared TODO claims, turn queues,
operator dashboards, and repo/worktree coordination remain external
coordinator responsibilities. OAS only guarantees the generic runtime and
event semantics needed to correlate those downstream observations with agent
turns, tools, runtime sessions, raw traces, and OpenTelemetry spans.

## Phase 1 Downstream Shape

The expected Phase 1 embedding can use:

| Concern | Downstream implementation | OAS boundary |
|---------|---------------------------|--------------|
| Text collaboration | Yjs document updates, usually over y-websocket | Treat CRDT updates as external observations; correlate by `correlation_id`, `run_id`, and optional `caused_by`. |
| Editor projection | CodeMirror 6 state, selections, decorations | Emit editor observations outside OAS; do not add editor concepts to OAS native events. |
| Git graph projection | cytoscape.js graph snapshots or deltas | Emit graph observations outside OAS; keep VCS terms in downstream payloads. |
| TODO claim | Optimistic write, verify-after-sync protocol | Keep claim ownership and winner rules downstream; OAS can expose `turn.ready` and tool events for correlation. |
| Turn queue | Shared queue or scheduler projected from collaboration state | Keep queue ordering downstream; OAS exposes agent turn lifecycle and slot scheduler observations only. |
| Telemetry | OpenTelemetry spans/metrics exported by the embedding app | Use OAS trace/span IDs where available and include OAS event IDs as span links or attributes. |

## Event Flow

Downstream collaboration observations should not be published onto OAS's
default `Event_bus.t` as domain events. Use a downstream bus or forwarder,
then bridge only the correlation fields needed to join with OAS events.

Recommended flow:

1. OAS emits agent/runtime events through `Event_bus`, `Runtime`, raw trace, or
   OTel spans.
2. The downstream collaboration layer observes CRDT/editor/graph/claim/queue
   state changes.
3. The downstream layer emits a collaboration observation payload matching
   `docs/schemas/collaboration-event-v1.schema.json`.
4. The downstream layer joins both streams by `correlation_id`, `run_id`,
   `caused_by`, `trace_id`, `span_id`, and optional downstream IDs.

## Event Names

Use dot-separated, lowercase event names. These names are recommendations for
external collaboration streams, not new OAS native `Event_bus` variants.

| Event type | Meaning |
|------------|---------|
| `collaboration.document.synced` | A CRDT document reached a synced state with a peer or relay. |
| `collaboration.document.update_applied` | A CRDT update was applied locally or received remotely. |
| `collaboration.presence.updated` | Ephemeral actor presence changed. |
| `collaboration.selection.updated` | Editor cursor or selection projection changed. |
| `collaboration.graph.snapshot` | A VCS graph snapshot became visible to consumers. |
| `collaboration.graph.delta` | A VCS graph delta was applied. |
| `collaboration.todo.claim_observed` | A TODO claim write, readback, or verify result was observed. |
| `collaboration.turn_queue.observed` | A shared turn queue state was observed. |

Downstreams may add more specific names under the same prefix, but should keep
the payload schema compatible when they want generic OAS correlation tooling to
consume the stream.

## Payload Contract

The JSON schema in `docs/schemas/collaboration-event-v1.schema.json` is the
wire contract. It is intentionally permissive where downstream implementations
vary, but strict about stable join keys and observation metadata.

Required fields:

| Field | Purpose |
|-------|---------|
| `schema_version` | Currently `1`. |
| `event_type` | Dot-separated event name, for example `collaboration.document.update_applied`. |
| `observed_at` | Unix epoch seconds when the embedding app observed the state. |
| `correlation_id` | OAS session-level correlation ID or downstream equivalent. |
| `run_id` | OAS run ID or downstream actor/run ID. |
| `substrate.kind` | External substrate family, for example `crdt`, `editor`, `vcs_graph`, `todo_claim`, or `turn_queue`. |

Optional fields carry implementation details:

| Field | Purpose |
|-------|---------|
| `event_id` | Stable event identifier for downstream dedupe and span links. |
| `caused_by` | OAS `run_id`, OAS `correlation_id`, or downstream event ID that triggered this observation. |
| `trace_id` / `span_id` | OpenTelemetry IDs if the embedding app has an active span. |
| `actor` | External actor identity and role in the collaboration substrate. |
| `document` | CRDT/editor document identifiers, provider, update metadata, and revision. |
| `transport` | WebSocket or other transport metadata. |
| `vcs` | Repository/ref/commit identifiers for graph projections. |
| `todo_claim` | TODO claim observation state, claimant, winner, logical clock, and optional convergence delay; not OAS-owned task semantics. |
| `turn_queue` | Queue snapshot metadata, not OAS-owned scheduling semantics. |
| `attributes` | Extra scalar attributes for downstream-specific dimensions. |

## OpenTelemetry Mapping

When exporting collaboration observations to OpenTelemetry:

| Payload field | OTel mapping |
|---------------|-------------|
| `event_type` | Span event name, or span name for short-lived observation spans. |
| `event_id` | `event.id` attribute; use as a span link target when supported. |
| `correlation_id` | `oas.correlation_id` attribute. |
| `run_id` | `oas.run_id` attribute. |
| `caused_by` | `oas.caused_by` attribute or span link. |
| `substrate.kind` | `collaboration.substrate.kind` attribute. |
| `substrate.provider` | `collaboration.substrate.provider` attribute. |
| `document.id` | `collaboration.document.id` attribute. |
| `document.update_bytes` | `collaboration.document.update_bytes` metric attribute/value. |
| `transport.kind` | `network.transport` or `collaboration.transport.kind` attribute. |
| `vcs.commit_id` | `vcs.commit.id` attribute. |
| `todo_claim.state` | `collaboration.todo_claim.state` attribute. |
| `turn_queue.depth` | `collaboration.turn_queue.depth` metric attribute/value. |

OAS-owned metric names keep the `oas.*` prefix. Collaboration-specific metrics
should use `collaboration.*` unless the OpenTelemetry spec already defines a
better semantic-convention attribute.

## Boundary Rules

- Do not add Yjs, y-websocket, CodeMirror, cytoscape, or browser dependencies
  to `agent_sdk`.
- Do not add TODO claim or turn queue state machines to OAS.
- Do not add downstream UI or repo graph payloads as native `Event_bus`
  variants.
- Prefer `Event_bus.TurnReady`, `ToolCalled`, `ToolCompleted`,
  `TurnCompleted`, runtime events, raw-trace refs, and OTel span IDs as join
  points.
- Add a native OAS event only when the semantic is provider-agnostic and
  belongs to the single-agent runtime itself.
