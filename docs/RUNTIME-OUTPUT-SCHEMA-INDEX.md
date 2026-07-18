# Runtime Output Schema Index

This is the operator-facing map for OAS runtime output surfaces. It answers:
where is the type or schema truth, who produces it, who consumes it, and which
test catches obvious path drift.

Machine-readable catalog: `docs/schema-surfaces/runtime-output-surfaces.v1.json`.

## Current Surfaces

| Surface | Output | Schema/type truth | Validation |
| --- | --- | --- | --- |
| `oas.event_bus.v1` | In-process agent lifecycle events | `lib/event_bus.mli`, `docs/EVENT-CATALOG.md` | Event bus and envelope tests |
| `oas.raw_trace_record.v1` | Raw trace JSONL rows | `lib/raw_trace.mli` | Raw trace roundtrip tests |
| `oas.harness_report.v1` | Harness report JSON/Markdown/JUnit artifacts | `lib/harness_report.mli` | Harness runner and CLI report tests |
| `oas.eval_report.v1` | Evaluation report JSON/text section | `lib/eval_report.mli` | Eval report and CLI baseline tests |
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
