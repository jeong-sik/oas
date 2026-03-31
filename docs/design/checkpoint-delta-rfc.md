# Checkpoint Delta RFC

Status: Draft

Related issues: `#484`, `#485`, `#487`, `#488`

## Problem

`Context.diff` only covers key-value metadata in `Context.t`. The main checkpoint body in [`lib/checkpoint.mli`](../../lib/checkpoint.mli) still restores as a full snapshot: messages, system prompt, tools, MCP sessions, usage, and working context are all loaded wholesale by [`lib/agent/agent_checkpoint.ml`](../../lib/agent/agent_checkpoint.ml).

That is safe, but it is a poor fit for long-running sessions:

- restore cost grows with the full message list
- most turns mutate only a small part of the checkpoint
- append-only assumptions are false in the current codebase

This RFC defines a conservative `Checkpoint.delta` format for agent checkpoints. The design favors correctness and fallback behavior over aggressive compression.

## Goals

- add an incremental representation for `Checkpoint.t`
- support message-list mutations without assuming append-only history
- fail closed to full restore when the base checkpoint or delta is unsafe
- keep the first implementation compatible with the current `Checkpoint.t` shape and versioning model

## Non-goals

- cross-run temporal queries or run-window semantics
- swarm checkpoint delta; swarm fidelity is tracked separately in `#487`
- semantic summarization as part of delta application
- read-path activation on day one; rollout starts with shadow apply only

## Current constraints

### Checkpoint format

Current agent checkpoints use `checkpoint_version = 4` in [`lib/checkpoint.ml`](../../lib/checkpoint.ml). `Checkpoint.of_json` can still load versions `1` through `4`, but normalizes them into the current in-memory `Checkpoint.t`.

### Resume path

[`lib/agent/agent_checkpoint.ml`](../../lib/agent/agent_checkpoint.ml) builds resumed agent state from a full `Checkpoint.t`. There is no delta read path today.

### Message mutation inventory

The checkpoint message list is not append-only. The first delta design must assume arbitrary splice operations.

| Code path | Mutation shape | Why append-only fails |
| --- | --- | --- |
| [`lib/agent/agent_handoff.ml`](../../lib/agent/agent_handoff.ml) | overwrite existing `ToolResult`, or append a replacement `ToolResult` if none exists | a previous result can be rewritten in place |
| [`lib/succession.ml`](../../lib/succession.ml) | drop `Thinking` blocks and insert synthetic `ToolResult` blocks for dangling tool calls | the normalized message list can insert and delete historical blocks |
| [`lib/context_reducer.ml`](../../lib/context_reducer.ml) | keep-last, keep-first-and-last, summarize-old, clear tool results, prune tool args, repair dangling tool calls, merge contiguous messages | reducers can replace middle ranges, not just append |

Because those mutations already exist in production code, the delta kernel must model message edits as indexed splices.

## Proposed data model

Delta v1 is scoped to canonical agent checkpoints with `checkpoint_version = 4`.

```ocaml
type message_splice = {
  start_index: int;
  delete_count: int;
  insert: Types.message list;
}

type delta_op =
  | Splice_messages of message_splice list
  | Patch_context of Context.diff
  | Replace_system_prompt of string option
  | Replace_usage of Types.usage_stats
  | Replace_turn_count of int
  | Replace_tools of Types.tool_schema list
  | Replace_tool_choice of Types.tool_choice option
  | Replace_sampling of {
      temperature: float option;
      top_p: float option;
      top_k: int option;
      min_p: float option;
      enable_thinking: bool option;
      thinking_budget: int option;
    }
  | Replace_limits of {
      disable_parallel_tool_use: bool;
      cache_system_prompt: bool;
      max_input_tokens: int option;
      max_total_tokens: int option;
      response_format_json: bool;
    }
  | Replace_mcp_sessions of Mcp_session.info list
  | Replace_working_context of Yojson.Safe.t option

type delta = {
  delta_version: int;
  base_checkpoint_version: int;
  base_checkpoint_hash: string;
  result_checkpoint_hash: string;
  created_at: float;
  operations: delta_op list;
}
```

### Design choices

- `Splice_messages` uses a list of splices, not a single splice. This handles disjoint edits (e.g., `Drop_thinking` removing blocks at multiple positions while `replace_tool_result` modifies a different region).
- non-message fields use whole-field replacement, not nested patch trees.
- `Patch_context` reuses the existing `Context.diff` abstraction instead of inventing a second metadata diff format.
- v1 delta is defined only for canonical checkpoint version 4. Older checkpoint versions must first be rebaselined through a full load and full save.

## Canonical form and hashing

Hash-based integrity checking requires deterministic serialization. The canonical form is:

- JSON keys sorted alphabetically at every nesting level
- floats rendered with `Printf.sprintf "%.17g"` (IEEE 754 round-trip)
- no trailing whitespace or indentation
- hash algorithm: SHA-256 over the canonical JSON byte string

Both `compute_delta` and `apply_delta` must use the same canonical form. Any change to the canonical form requires a new `delta_version`.

## Compute algorithm

`Checkpoint.compute_delta : t -> t -> delta option`

1. Reject non-v4 inputs. Older checkpoints must rebaseline to a full v4 checkpoint first.
2. Canonicalize both checkpoints with stable JSON encoding, then SHA-256 hash them.
3. Build message splices using LCS-based diff (common-prefix/common-suffix detection, then minimal splice set for disjoint edits).
4. Emit replace operations for any non-message field whose value changed.
5. If the delta would be larger than the target full checkpoint, return `None` and keep the full checkpoint path.

### Message diff rule

The v1 algorithm emits a `message_splice list`:

Each splice has:
- `start_index`: position in the base message list
- `delete_count`: number of messages removed from the base checkpoint
- `insert`: replacement slice from the target checkpoint

Splices are ordered by `start_index` descending (apply from end to avoid index shifting). Multiple splices are needed because:

- `Drop_thinking` can remove Thinking blocks at arbitrary positions
- `replace_tool_result` can modify a ToolResult independently of other changes
- `context_reducer` strategies can drop middle ranges while keeping first and last

A single wide splice that replaces the entire differing window is always valid as a fallback, but wastes space when edits are sparse. The compute algorithm should prefer minimal splice sets when possible, falling back to a single wide splice when the diff is too complex.

## Apply algorithm

`Checkpoint.apply_delta : t -> delta -> (t, Error.sdk_error) result`

1. Require `delta.delta_version = 1` and `delta.base_checkpoint_version = 4`.
2. Hash the supplied base checkpoint and compare with `base_checkpoint_hash`.
3. Apply operations in order to a copy of the base checkpoint.
4. Re-hash the result and compare with `result_checkpoint_hash`.
5. Run structural validation on the rebuilt checkpoint.
6. On any failure, return an error and let the caller fall back to full restore.

### Structural validation

Validation after apply must fail if any of these invariants are broken:

- message indices are out of range
- `ToolUse`/`ToolResult` pairing is orphaned in a way the runtime rejects
- serialized checkpoint cannot round-trip through `Checkpoint.to_json` / `Checkpoint.of_json`

The apply path should validate, not silently repair. Silent repair changes semantics and hides bad deltas.

## Compatibility matrix

| Stored full checkpoint | Delta input | Behavior |
| --- | --- | --- |
| v1-v3 | none | existing full restore path |
| v1-v3 | delta v1 | load full checkpoint, rebaseline to full v4, ignore delta |
| v4 | delta v1 | supported |
| v4 | unknown delta version | reject delta, use full restore |

This keeps the implementation small and avoids mixing delta semantics with the existing v1-v4 full-checkpoint compatibility logic.

## Concurrency and writer rules

- delta generation is single-writer per session
- the writer must compute delta from the exact base checkpoint identified by `base_checkpoint_hash`
- if another writer commits a newer full checkpoint first, the delta must be discarded and recomputed from the new base
- the first release does not support delta-chain merges across concurrent writers

This is effectively compare-and-swap on checkpoint identity.

## Delta-chain policy

Read amplification and corruption blast radius both grow with chain length. v1 therefore uses a short chain plus automatic rebasing:

- maximum chain depth: `10`
- maximum wall-clock age from the last full checkpoint: `1 hour`
- if either limit is crossed, write a fresh full checkpoint and restart the chain

## Rollout plan

### Phase 1: shadow write

- keep writing full checkpoints
- compute delta sidecars in parallel
- apply the sidecar back onto the base checkpoint in shadow mode
- compare the rebuilt checkpoint to the original target checkpoint

No read path switches to delta in this phase.

### Phase 2: guarded read path

- enable delta read only when shadow mismatch rate is low enough
- preserve automatic full-restore fallback on every delta apply failure
- disable delta read immediately if mismatch or apply-failure rate crosses the release threshold

## Validation plan

### Structural gate

- qcheck property: `apply_delta old (compute_delta old new) = new`
- serializer roundtrip after apply
- corrupted hash, bad indices, and unknown op versions must all fail closed

### Semantic gate

The semantic gate operates during shadow phase (Phase 1) to validate that delta-rebuilt checkpoints produce equivalent agent behavior. This is separate from the hash-based structural gate:

- **Hash gate** (apply path): exact canonical JSON equality. Ensures the delta reconstruction is bit-identical to the original checkpoint. This is the production correctness gate.
- **Semantic gate** (shadow phase): behavioral equivalence. Validates that even if a future delta format produces structurally different but semantically equivalent checkpoints, the agent behavior is preserved.

Semantic checks:
- for turns with `ToolUse`: preserve tool-call fingerprint equality
- for turns without `ToolUse`: preserve structure checks that matter for resume quality
- reuse [`lib/checkpoint_validation.ml`](../../lib/checkpoint_validation.ml) as a partial validator, but do not treat its current `continuity_check` as sufficient by itself
- raw text similarity score is advisory, not a gate

## Version skew with MASC

MASC consumes OAS checkpoints but should not assume delta support implicitly.

- capability handshake should advertise `checkpoint_delta_v1`
- if the consumer does not support delta, OAS keeps serving full checkpoints
- if producer and consumer disagree on delta version, the system must fall back to full restore rather than attempting best-effort apply

## Operational metrics

OAS-level metrics for observing delta behavior. Consumers (e.g. coordinators) may bridge these to their own metrics systems.

| Metric | Type | Purpose |
| --- | --- | --- |
| `checkpoint_delta_apply_total` | Counter | delta apply attempts |
| `checkpoint_delta_apply_failures_total` | Counter | delta apply failures that triggered full-restore fallback |
| `checkpoint_delta_size_bytes` | Histogram | delta payload size distribution |
| `checkpoint_full_restore_fallback_total` | Counter | fallback occurrences from any cause (bad hash, validation failure, chain depth exceeded) |

OAS provides both the delta compute/apply API and the metrics. Consumers instrument and aggregate as needed.

## Release gate and feature flag

### Feature flag

Delta read path is controlled by `OAS_DELTA_CHECKPOINT` environment variable:

- `false` (default): full checkpoint restore only, delta sidecar computed for shadow comparison
- `true`: delta read path active, with automatic fallback on apply failure

### Release gate

- delta apply failure rate > 5% over a rolling 1-hour window: delta read path auto-disabled, reverts to full restore
- mismatch rate during shadow phase > 1% over a rolling 1-hour window: blocks Phase 2 activation
- both thresholds are evaluated against `checkpoint_delta_apply_failures_total / checkpoint_delta_apply_total`

### Deployment order

1. OAS deploys first: new `Checkpoint.delta` type, `compute_delta`, `apply_delta`
2. Consumer deploys second: shadow write, metrics bridge, feature flag
3. Phase 2 activation: flip `OAS_DELTA_CHECKPOINT=true` after shadow mismatch rate is below threshold

This ordering preserves backward compatibility. MASC with old OAS ignores delta. New MASC with new OAS runs shadow mode until validated.

## Follow-up work

- `#487`: finish the swarm checkpoint fidelity gate before reusing delta ideas in swarm state
- `#488`: implement `Checkpoint.delta`, shadow metrics, and full-restore fallback
- `#486`: specify cross-run read-side prerequisites separately; this RFC stays single-run only
