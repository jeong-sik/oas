# Runtime Output Schema Index

This is the operator-facing map for OAS runtime output surfaces. It answers:
where is the type or schema truth, who produces it, who consumes it, and which
test catches obvious path drift.

Machine-readable catalog: `docs/schema-surfaces/runtime-output-surfaces.v1.json`.

## Current Surfaces

| Surface | Output | Schema/type truth | Validation |
| --- | --- | --- | --- |
| `oas.event_bus.v1` | In-process agent lifecycle events | `lib/event_bus.mli`, `docs/EVENT-CATALOG.md` | Event bus and envelope tests |
| `oas.runtime_protocol.v2` | `oas_runtime` canonical NDJSON protocol messages | `lib/runtime.mli` | Runtime protocol roundtrip tests |
| `oas.runtime_sync_window.v1` | Runtime replay window JSON for offline and external resume adapters | `lib/runtime_sync.mli`, `docs/schemas/runtime-sync-window-v1.json` | `Runtime_sync.of_json`, schema version, and input pause/resume fixture tests |
| `oas.runtime_report.v1` | Runtime session report artifact / protocol response | `lib/runtime.mli`, `lib/runtime_projection.mli` | Runtime type and projection tests |
| `oas.runtime_proof.v1` | Runtime proof artifact / protocol response | `lib/runtime.mli`, `lib/runtime_projection.mli` | Runtime type and proof projection tests |
| `oas.runtime_telemetry_report.v1` | Runtime telemetry JSON/Markdown artifacts | `lib/runtime_evidence.mli`, `lib/sessions_types.mli` | Runtime session artifact and session type tests |
| `oas.runtime_evidence_bundle.v1` | Runtime evidence bundle JSON artifact | `lib/runtime_evidence.mli`, `lib/sessions_types.mli` | Runtime session evidence tests |
| `oas.raw_trace_record.v1` | Raw trace JSONL rows | `lib/raw_trace.mli` | Raw trace roundtrip tests |
| `oas.raw_trace_manifest.v1` | Raw trace manifest artifact | `lib/runtime_evidence.mli`, `lib/sessions_types.mli` | Runtime session and sessions type tests |
| `oas.structured_schema.v1` | Structured output schema helper | `lib/structured.mli`, `lib/base/types.mli` | Structured schema tests |

## Rules

- OAS-owned runtime semantics live in OCaml `.mli` files first.
- Cross-repo or downstream payloads use versioned JSON schema under `docs/schemas/`.
- Downstream product domains should not become native OAS event variants.
- When a schema source or test file moves, update the machine-readable catalog
  in the same change.
- This index lists production output surfaces only. Internal execution-topology
  types are not a seventh runtime schema and do not authorize external writes.
- Until an explicit production single-writer hard cut, `Durable_event` remains
  the durable journal authority. A later cut must make Event_bus, Raw_trace,
  durable persistence, and dashboard data read projections of that one writer;
  it must not introduce a second event-history SSOT.
