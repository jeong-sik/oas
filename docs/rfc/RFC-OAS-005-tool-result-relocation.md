# RFC-OAS-005: Tool Result Relocation Architecture

- **Status**: Draft
- **Author**: Vincent (jeong-sik)
- **Created**: 2026-04-13
- **OAS Version**: v0.128.0
- **Related**: Track 1 (oas#873 per-result cap), Claude Code `toolResultStorage.ts`. Downstream consumer-side sanitization is tracked in those consumers' own repositories.

---

## 1. Problem

Large tool results (file dumps, grep output, API responses) consume context window
without bound. Track 1 added truncation caps, but truncation **loses data** permanently.

### 1.1 Observed Failure

uranium666 keeper: 280 ToolResult blocks x 7K chars = 1.95M in a single checkpoint.
Track 1 reduced this to ~160K via truncation + stub, but the original data is gone.

### 1.2 Claude Code Reference

Claude Code uses **relocation** (data movement, not data loss):
- Per-result: content > 50K -> persist to disk, 2KB preview in message
- Per-message: aggregate 200K budget, largest results persisted first
- Decision freezing: `ContentReplacementState` with seenIds + replacements Map
- Prompt cache stability: frozen decisions produce byte-identical output across turns

Source: `src/utils/toolResultStorage.ts` (1041 lines), `src/constants/toolLimits.ts`

---

## 2. Design

### 2.1 Principle: Relocate, Don't Truncate

Move large content to filesystem. Keep a preview in the message. Preserve access
to the full content via `read` API. Freeze replacement decisions for cache stability.

### 2.2 Two New Modules

**`Tool_result_store`**: Session-scoped filesystem persistence.
- Disk layout: `<storage_dir>/<session_id>/tool-results/<tool_use_id>.txt`
- Atomic writes via `Fs_result.write_file` (.tmp + rename)
- `persist` returns preview string; `read` retrieves full content
- Fail-open: write error preserves original content

**`Content_replacement_state`**: Decision freezing for prompt cache stability.
- `seen_ids`: all tool_use_ids ever processed (Hashtbl for O(1) lookup)
- `replacements`: tool_use_ids that were replaced (subset of seen_ids)
- Monotonicity invariant: once frozen, a decision never reverts
- JSON serializable for checkpoint persistence via `Context.t`

### 2.3 Integration Points

1. **`make_tool_results`** (creation-time): per-result persist when content > threshold
2. **Context reducer** (turn-time): per-message aggregate budget enforcement
3. **Checkpoint** (save-time): CRS serialized into `Context.t` metadata

### 2.4 Relationship to `Context_offload`

`Context_offload` (v0.62.0) is a simpler predecessor:
- Timestamp-based filenames (no tool_use_id keying)
- No decision freezing, no checkpoint persistence, no `read` API
- Superseded by this RFC. Marked `[@deprecated]` with migration note.

---

## 3. Phased Delivery

### Phase 1: Per-Result Persist + Decision Freezing

New files: `tool_result_store.ml/.mli`, `content_replacement_state.ml/.mli`
Modified: `agent_turn.ml`, `agent_types.ml`, `pipeline.ml`, `agent_sdk.ml`

### Phase 2: Aggregate Budget + Checkpoint

New reducer strategy: `Relocate_tool_results`
CRS persistence via `Context.t` metadata key `"__crs"`

### Phase 3: Observability + Cleanup

Event_bus metrics, env var overrides, session cleanup API.

---

## 4. Testing

- **Unit**: persist/read round-trip, preview generation, freeze monotonicity
- **Property (qcheck)**: idempotency, stability, serialization round-trip
- **Integration**: pipeline mock through `make_tool_results` with relocation
- **TLA+ (Phase 2)**: `Replaced(id) => []Replaced(id)` temporal invariant
