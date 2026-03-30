# Swarm Checkpoint Fidelity Gate

Status: Active
Related issues: `#487`

## Purpose

Define the loss budget for swarm checkpoint serialization/deserialization.
Until this gate passes, no swarm-level delta or plan caching work (P3-2, P3-3) may proceed.

## Loss Inventory

Full roundtrip path: `swarm_state` -> `of_state` -> `to_json` -> JSON -> `load` -> `restore` -> `swarm_state`

### Fields with no loss

| Field | Save | Restore | Notes |
| --- | --- | --- | --- |
| `current_iteration` | exact | exact | |
| `best_metric` | exact | exact | |
| `best_iteration` | exact | exact | |
| `patience_counter` | exact | exact | |
| `history[].iteration` | exact | exact | |
| `history[].metric_value` | exact | exact | |
| `history[].elapsed` | exact | exact | |
| `history[].timestamp` | exact | exact | |
| `config` | snapshot (names) | `base_config` arg | By design: closures not serializable |

### Fields with loss

| Field | Serialized as | Restored as | Impact | Decision |
| --- | --- | --- | --- | --- |
| `agent_statuses` | not saved | `create_state` -> all `Idle` | **Medium** | Accept: current statuses are ephemeral runtime state |
| `converged` | not saved | `create_state` -> `false` | **Medium** | **Fixed** (checkpoint v2, oas#499) |
| `history[].agent_results` status | `show_agent_status` string | all -> `Idle` | **Medium** | **Fixed** (structured JSON serialization, this PR) |
| `history[].trace_refs` | not saved | `[]` | **Low** | Accept: debug links, no runtime impact |

### Details

**`agent_statuses` (accept)**:
Current agent statuses (`Idle`, `Working`, `Done_ok`, `Done_error`) are transient runtime state.
After a swarm is restored and agents re-execute, statuses are set fresh.
Preserving stale statuses from a previous run would be misleading.

**`converged` (fix required)**:
The `converged` flag determines whether the convergence loop should continue.
If a swarm checkpointed after convergence and is restored, `converged = false`
causes unnecessary re-execution. This is a correctness bug.

**`history[].agent_results` status (fix required)**:
Historical agent statuses are diagnostic information.
`Done_ok { elapsed; text; telemetry }` and `Done_error { elapsed; error; telemetry }` carry
performance data (`elapsed`), result summaries (`text`/`error`), and aggregated telemetry.
Restoring all as `Idle` loses this diagnostic data.

The fix requires parsing `show_agent_status` output back into the variant.
Since `show_agent_status` is derived (`[@@deriving show]`), the format is stable.
However, `Done_ok` and `Done_error` contain nested records with `telemetry: agent_telemetry`.
A simpler v1 fix: serialize `agent_status` to structured JSON instead of `show` format.

**`history[].trace_refs` (accept)**:
`Raw_trace.run_ref` contains references to external trace stores.
These are debugging aids with no runtime behavioral impact.
Restoring empty trace refs does not change swarm execution.

## Gate conditions

The gate passes when:

1. All **High** impact fields: 0 (none identified)
2. All **Medium** impact fields: each has an explicit decision (accept or fix)
   - `agent_statuses`: **accepted** (ephemeral runtime state)
   - `converged`: **fix required** (correctness bug)
   - `history[].agent_results` status: **fix required** (diagnostic data loss)
3. Fixes for `converged` and `agent_results` status must be implemented and tested
4. Each accepted loss must have documented justification (above)

## Fix plan

### Fix 1: Add `converged` to checkpoint

- Add `converged: bool` to `swarm_checkpoint.t`
- Save: `cp.converged <- state.converged`
- Restore: `state.converged <- cp.converged`
- JSON: `("converged", \`Bool cp.converged)`
- Bump `checkpoint_version` to 2, accept v1 with `converged = false` default
- Test: save converged state, restore, verify `converged = true`

### Fix 2: Structured `agent_status` serialization

- Replace `show_agent_status` with structured JSON:
  ```json
  {"status": "Done_ok", "elapsed": 1.5, "text": "result", "telemetry": {...}}
  ```
- Parse back to the full variant with all nested fields
- The `telemetry` sub-record needs its own JSON serialization
- This is a larger change that requires `agent_telemetry_to_json` / `agent_telemetry_of_json`
- Test: roundtrip each variant (`Idle`, `Working`, `Done_ok`, `Done_error`)

### Sequencing

Fix 1 is small and independent. Fix 2 is medium complexity.
Both are required before the gate passes.
P3-2 (swarm plan caching) and P3-3 must wait.
