# RFC-OAS-007: Runtime Sync and Transport Boundary

**Status**: Draft
**Date**: 2026-04-30
**Scope**: `Runtime`, `Event_bus`, `Durable_event`, `Runtime_store`, documentation
**One sentence**: OAS should expose compatibility-critical, domain-neutral runtime observations that a collaboration layer can consume, while leaving CRDT state, clustering, and binary transport choices outside the SDK.

## Related Documents

- `README.md` — Scope limitations and single-process runtime statement.
- `docs/sdk-independence-principle.md` — Downstream vocabulary and ownership boundary.
- `docs/_audit/2026-04-17-coordination-leak-candidates.md` — Coordination-leak audit.
- `docs/EVENT-CATALOG.md` — OAS event surfaces and reserved event namespaces.
- `lib/runtime.mli` — Runtime protocol types.

## Problem Statement

Several downstream collaboration designs need three structural performance
capabilities:

1. A CRDT or CRDT-like state synchronization layer with efficient merge and
   replay behavior.
2. A small active-agent grouping policy, commonly sized around three to five
   participants per local collaboration cell.
3. Binary transport for high-volume collaboration updates.

These are useful requirements for a collaboration workbench, but they are not
OAS runtime responsibilities. Pulling them into `agent_sdk` would violate the
single-process runtime contract and would make OAS own shared workspace state,
multi-process scheduling, and wire-framing policy.

The SDK still has one important role: it must keep its runtime/proof/event
surfaces explicit enough that an embedding collaboration system can consume them
without scraping logs or inferring hidden state.

## Non-Goals

OAS will not:

- Implement a CRDT document store or choose a CRDT library.
- Store shared workspace state across processes.
- Assign participants to collaboration clusters.
- Own real-time cursor, presence, claim, or queue semantics.
- Replace the current runtime JSON protocol with MessagePack or another binary
  codec in this RFC.
- Add external dependencies for collaboration state or binary encoding.

## Boundary Contract

### OAS-Owned Primitives

OAS owns domain-neutral, single-runtime primitives:

| Primitive | Existing surface | Contract |
|-----------|------------------|----------|
| Runtime session snapshots | `Runtime.session` | Current state of one runtime session. |
| Runtime protocol events | `Runtime.Event_message` and `runtime.*` Event_bus bridge | Sequenced observations on the runtime protocol; mirrored observations on `Event_bus`. |
| Event causality | `Event_bus.envelope` | `correlation_id`, `run_id`, and `caused_by` allow downstream reconstruction. |
| Durable replay facts | `Durable_event` | Single-agent replay metadata and crash-recovery facts. |
| Runtime artifacts | `Runtime.artifact` | Opaque outputs attached to one runtime session. |

These primitives may be consumed by a collaboration system, but they do not
define that system's shared-state model.

### Downstream-Owned Responsibilities

An embedding collaboration system owns:

| Responsibility | Reason |
|----------------|--------|
| CRDT algorithm choice | Merge strategy depends on editor, document model, and persistence policy. |
| Shared document schema | OAS does not own files, branches, canvases, or cross-session state. |
| Participant grouping | Cluster size and membership are scheduling policy. |
| Binary frame format | Codec, compression, batching, and network transport are deployment policy. |
| Backpressure across processes | OAS can expose local events; global throttling belongs above the SDK. |

## Recommended Integration Shape

The downstream layer should treat OAS as an event producer and command target,
not as the shared-state substrate.

```text
OAS runtime process
  -> Runtime.Event_message / Event_bus / Durable_event
  -> downstream adapter
  -> collaboration document update
  -> downstream sync transport
```

For inbound work, the flow stays equally narrow:

```text
downstream collaboration decision
  -> Runtime.Request_message / Runtime.Control_request_message
  -> OAS runtime process
```

This keeps the SDK reusable for non-collaboration embeddings and prevents a
specific workbench architecture from leaking into the library.

## Track 2 Mapping

| Track 2 concern | OAS position | Allowed OAS work |
|-----------------|--------------|------------------|
| Efficient CRDT merge/replay | External | Preserve compatibility-critical runtime event fields and causality IDs. |
| Three-to-five participant grouping | External | Expose participant names and runtime states already present in `Runtime.session`. |
| MessagePack or binary migration | External or protocol dependency | Document compatibility requirements; do not add a codec here without a separate protocol RFC. |
| Observation-driven coordination | External | Emit domain-neutral observations; do not encode coordination policy. |

## Binary Transport Guidance

The runtime protocol currently serializes messages as JSON. A downstream
transport can still move these messages over binary frames by treating the JSON
payload as opaque bytes, or it can wrap OAS runtime events in its own binary
envelope.

A future OAS binary protocol RFC must satisfy all of these conditions before
touching `lib/`:

1. It preserves `Runtime.protocol_message` semantics exactly.
2. It has a text-format fallback for debugging and compatibility.
3. It does not require OAS to depend on a collaboration-specific codec.
4. It defines version negotiation in `Runtime.init_response.capabilities`.
5. It has focused roundtrip tests for every `Runtime.protocol_message` variant.

Until then, the SDK should optimize the semantic contract, not the frame
encoding.

## Compatibility Requirements

Downstream adapters should rely only on compatibility-critical fields:

- `session_id`
- `phase`
- `participants`
- `last_seq`
- `correlation_id`
- `run_id`
- `caused_by`
- `protocol_version`
- `capabilities`

Adapters should not parse console logs, provider-specific text, or private
module state. If an adapter needs a generic runtime fact that is unavailable,
that fact should be proposed as a small OAS event or runtime field with a
provider-neutral name.

## Acceptance Criteria

This RFC is satisfied when:

- The boundary is documented without adding collaboration-specific dependencies.
- `lib/` remains free of downstream coordinator vocabulary.
- The existing runtime/event surfaces are the recommended integration points.
- Any future code change can be reviewed against this RFC before adding shared
  state, clustering, or binary transport logic to OAS.
