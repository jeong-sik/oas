# RFC-OAS-033: thinking_control_format vocabulary leaf SSOT

| | |
|---|---|
| Status | Implemented |
| Author | jeong-sik (audit: provider SSOT sweep, 2026-06-30) |
| Created | 2026-06-30 |
| Target | `lib/llm_provider/capability_vocab.ml`, `capabilities.ml`(+`.mli`), `provider_catalog.ml`, `capability_manifest.ml`, `model_catalog.ml`, `reasoning_dialect.ml` |
| Supplements | RFC-OAS-023 (capability axis reshape), RFC-OAS-029 §1.2 (string classifier bypassing typed kind) |
| Boundary | OAS internal refactor — no wire/contract change. Deriving/show/equality surface preserved via alias. |

## 0. Summary

This RFC records the accepted and implemented leaf-SSOT ownership for
`thinking_control_format` / `preserve_thinking_control_format`.

Before the migration, these closed sum variants were owned by `Capabilities`,
while `capability_vocab` was the documented SSOT home for operator-facing wire
vocabulary. Because `capability_vocab` is a leaf module with no dependency on
`Capabilities`, it could only expose string lists, and every consumer that
needed `variant <-> wire-string` reimplemented literal matches. That was the
cycle-break bug: the leaf owned the vocabulary intent, but not the variant.

Current `main` has adopted Option A from this RFC:

- `capability_vocab.ml` owns the `thinking_control_format` and
  `preserve_thinking_control_format` variants.
- It owns one canonical table per vocabulary and derives `values` and
  `*_of_string` from that table.
- `capabilities.ml` re-exports the variants via type/constructor aliases, so
  existing `Capabilities.*` call sites compile unchanged.
- `provider_catalog.ml` delegates parsing to `Capability_vocab`.

This document is therefore a design record and verification contract for the
landed implementation, not a request to perform a future relocation.

## 1. Original Evidence (2026-06-30 SSOT Audit)

This section preserves the pre-migration audit that justified the change. It
does not describe the current code after the relocation landed.

The same closed vocabulary is matched independently at:

| site | form | dimension |
|---|---|---|
| `capabilities.ml:623` `thinking_control_format_of_manifest_string` | 8-arm literal match, comment admits *"Mirrors Provider_catalog... kept local to avoid a dependency cycle"* | variant-fragmentation (P1) |
| `capabilities.ml:636` `preserve_thinking_control_format_of_manifest_string` | table + lookup, duplicated | variant-fragmentation (P1) |
| `provider_catalog.ml:206` `parse_thinking_control_format` | 8-arm literal match | string-match-classifier (P1) |
| `provider_catalog.ml:229` `parse_preserve_thinking_control_format` | 5-arm literal match | string-match-classifier (P2) |
| `capabilities.ml:670` `modality_priority_of_catalog_string` | 4-arm literal match (same pattern, `Modality` variant) | string-match-classifier |

Sibling parsers in the **same file** already delegate correctly — `parse_reasoning_replay` and `parse_assistant_tool_content_format` call `Capability_vocab.*_of_string` — proving the delegation pattern is established and these sites bypass it. The bypass exists for one reason: `capability_vocab` owns those variants (it defines `reasoning_replay_override`, `assistant_tool_content_format`) but **does not own** `thinking_control_format` (that lives in `Capabilities`, which the leaf cannot depend on).

At the time of the audit, `capability_vocab.ml:1-6` documented the intent
explicitly:
> *"This module intentionally has no dependency on Capabilities... the canonical wire vocabulary must live in a leaf module to avoid duplicate enum tables and dependency cycles."*

The pre-migration state violated that intent for `thinking_control_format`: the
enum table was duplicated because the variant was on the wrong side of the
cycle break.

## 2. Implemented Design

### 2.1 Option A — variants owned by `capability_vocab` (ADOPTED)

The variant definitions live in `capability_vocab.ml`, alongside the existing
`reasoning_replay_override` / `assistant_tool_content_format`. Each vocabulary
has a single canonical table, and every public parser/value surface is derived
from that table:

- `values = List.map fst table`
- `of_string = List.assoc_opt normalized table`
- `to_string` is intentionally absent unless a real serializer consumer is
  introduced; if added later, it must be reverse-derived from the same table,
  not a third hand-written match.

`capabilities.ml` is a consumer with a type alias and constructor re-export:

```ocaml
type thinking_control_format = Capability_vocab.thinking_control_format =
  | No_thinking_control
  | Thinking_object
  | ...
```

The OCaml constructor alias keeps `Capabilities.No_thinking_control` valid at
existing call sites, including `reasoning_dialect.ml` and the matches across
`backend_openai_request`, `backend_anthropic`, `api_openai`, and related files.
Duplicated parsers are replaced by `Capability_vocab.*_of_string`
delegation.

**Dependency direction:** `capability_vocab` (leaf) ← `capabilities` ← everyone else. No cycle: `capability_vocab` still depends on nothing. `capabilities` already depends on `capability_vocab` (it aliases `reasoning_replay_override` today), so the new edge is not even new.

The same pattern applies to `preserve_thinking_control_format`.

### 2.2 Option B — absorb `capability_vocab` into `capabilities`

Eliminates the cycle by collapsing the leaf into the owner. Rejected: it destroys the leaf-module invariant other consumers rely on and forces every `capability_vocab` user to pull `Capabilities`.

### 2.3 Option C — functor parameterising the parser over the variant

Over-engineered for a flat 8-constructor enum. Rejected.

## 3. Completed Migration

| step | change | blast radius |
|---|---|---|
| 1 | `capability_vocab`: add `type thinking_control_format` + `preserve_thinking_control_format` + one canonical table per vocabulary; derive `values` and `of_string` from that table. | leaf only |
| 2 | `capabilities.ml` (+`.mli`): replace owned type defs with aliases `= Capability_vocab.*`; re-export constructors | type-only, call sites unchanged |
| 3 | `provider_catalog.ml`: `parse_thinking_control_format` / `parse_preserve_thinking_control_format` delegate to `Capability_vocab` | parser collapse |
| 4 | manifest/catalog unknown values remain fail-closed with observable diagnostics at their call sites | silent-failure guard |
| 5 | tests: exported values parse through the canonical table; future additions must update the table and aliases together | drift guard |

The migration is complete for the thinking-control vocabularies. This RFC no
longer asks maintainers to perform these steps.

## 4. Verification Contract

- **Compiler**: aliases keep existing constructors visible while preserving
  exhaustiveness checks at match sites.
- **Single-table proof**: exported `values` must be derived from the same table
  used by `of_string`; a round-trip test alone is not enough because independent
  encodings can round-trip accidentally.
- **Property test**: `List.for_all (fun s -> Capability_vocab.thinking_control_format_of_string s <> None) Capability_vocab.thinking_control_format_values` plus equivalent preserve-thinking coverage.
- **Delegation test**: provider/catalog parsing must delegate to
  `Capability_vocab` instead of reintroducing local literal matches.
- **Warning preservation**: manifest/catalog unknown-value call sites must keep
  observable warning/error behavior after delegating to `Capability_vocab`;
  `None` without a warning is a silent failure regression.

## 5. Why not bundle into the SSOT-audit subset PRs

The 2026-06-30 audit produced three surgical PRs (#2333 provider-kind
round-trip, #2334 stop_reason wire SSOT, #2335 catch-all exhaustiveness) that
were zero-runtime-change. This RFC's move was also zero runtime change, but it
touched broader type ownership and parser delegation. Keeping it separate made
the review boundary explicit.

## 6. Non-goals

- `stop_reason_wire` per-backend mapping consolidation (separate variant-fragmentation finding).
- `provider_registry` `max_context` drift (262_144 hardcoded shadowing capability-verified 256_000/128_000) — semantic fix, separate PR.
- `provider.ml:335` documented dual fallback — intentional per docstring, not addressed here.
- `reasoning_dialect.streaming` dead field / `Template_parser` stub variant — related SSOT debt (the streaming-reasoning variant is declared but no live parser consumes it); tracked separately, may fold into this RFC's migration if the streaming field is also relocated.

## 7. Remaining Follow-Up

- `Modality.t` priority still follows a similar vocabulary pattern. It can be
  evaluated separately; it is not a blocker for the implemented
  `thinking_control_format` relocation.
- Any future `to_string` surface for these vocabularies must be introduced only
  with a real serializer consumer and must be derived from the canonical table.
