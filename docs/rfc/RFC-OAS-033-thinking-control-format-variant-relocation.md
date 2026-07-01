# RFC-OAS-033: Relocate thinking_control_format variants to capability_vocab (leaf SSOT)

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (audit: provider SSOT sweep, 2026-06-30) |
| Created | 2026-06-30 |
| Target | `lib/llm_provider/capability_vocab.ml`, `capabilities.ml`(+`.mli`), `provider_catalog.ml`, `capability_manifest.ml`, `model_catalog.ml`, `reasoning_dialect.ml` |
| Supplements | RFC-OAS-023 (capability axis reshape), RFC-OAS-029 §1.2 (string classifier bypassing typed kind) |
| Boundary | OAS internal refactor — no wire/contract change. Deriving/yojson surface preserved via alias. |

## 0. Summary

`thinking_control_format` / `preserve_thinking_control_format` are closed sum variants **owned by `Capabilities`**, but `capability_vocab` — the documented SSOT home for the operator-facing wire vocabulary — was deliberately kept a **leaf module with no dependency on `Capabilities`** to break a cycle. Because the leaf cannot hold the variant, it stores the vocabulary as bare **string lists** (`thinking_control_format_values`, `preserve_thinking_control_format_values`). Every consumer that needs `variant ↔ wire-string` therefore re-implements the same hand-rolled literal match. The duplication has already drifted once and is structurally guaranteed to drift again.

This RFC relocates the **variant definitions themselves** into `capability_vocab`, making it the single owner of both the variant and its wire mapping. `Capabilities` re-exports them via type/constructor alias, so the ~80 call-site references in `reasoning_dialect.ml` and 11 other files compile unchanged. The five duplicated parsers collapse to one `Capability_vocab.thinking_control_format_of_string` delegation.

## 1. Evidence (confirmed in the 2026-06-30 SSOT audit)

The same closed vocabulary is matched independently at:

| site | form | dimension |
|---|---|---|
| `capabilities.ml:623` `thinking_control_format_of_manifest_string` | 8-arm literal match, comment admits *"Mirrors Provider_catalog... kept local to avoid a dependency cycle"* | variant-fragmentation (P1) |
| `capabilities.ml:636` `preserve_thinking_control_format_of_manifest_string` | table + lookup, duplicated | variant-fragmentation (P1) |
| `provider_catalog.ml:206` `parse_thinking_control_format` | 8-arm literal match | string-match-classifier (P1) |
| `provider_catalog.ml:229` `parse_preserve_thinking_control_format` | 5-arm literal match | string-match-classifier (P2) |
| `capabilities.ml:670` `modality_priority_of_catalog_string` | 4-arm literal match (same pattern, `Modality` variant) | string-match-classifier |

Sibling parsers in the **same file** already delegate correctly — `parse_reasoning_replay` and `parse_assistant_tool_content_format` call `Capability_vocab.*_of_string` — proving the delegation pattern is established and these sites bypass it. The bypass exists for one reason: `capability_vocab` owns those variants (it defines `reasoning_replay_override`, `assistant_tool_content_format`) but **does not own** `thinking_control_format` (that lives in `Capabilities`, which the leaf cannot depend on).

`capability_vocab.ml:1-6` documents the intent explicitly:
> *"This module intentionally has no dependency on Capabilities... the canonical wire vocabulary must live in a leaf module to avoid duplicate enum tables and dependency cycles."*

The current state violates that intent for `thinking_control_format`: the enum table is duplicated anyway, because the variant is on the wrong side of the cycle break.

## 2. Design

### 2.1 Option A — relocate variants into `capability_vocab` (RECOMMENDED)

Move the **variant definitions** out of `capabilities.ml` into `capability_vocab.ml`, alongside the existing `reasoning_replay_override` / `assistant_tool_content_format`. Define a single canonical table there and derive every public vocabulary surface from it:

- `values = List.map fst table`
- `of_string = List.assoc_opt normalized table`
- `to_string` only if a real serializer consumer is introduced in the same PR; if present, it must be reverse-derived from the same table, not a third hand-written match.

The current consumers only need `values` and `of_string`, so the initial move should not add an unused `to_string` surface.

`capabilities.ml` becomes a consumer with a type alias and constructor re-export:

```ocaml
type thinking_control_format = Capability_vocab.thinking_control_format =
  | No_thinking_control
  | Thinking_object
  | ...
```

The OCaml constructor alias keeps `Capabilities.No_thinking_control` valid at every existing call site — the ~80 references in `reasoning_dialect.ml` and the matches across `backend_openai_request`, `backend_anthropic`, `api_openai`, etc. compile unchanged. The five duplicated parsers are replaced by `Capability_vocab.thinking_control_format_of_string`.

**Dependency direction:** `capability_vocab` (leaf) ← `capabilities` ← everyone else. No cycle: `capability_vocab` still depends on nothing. `capabilities` already depends on `capability_vocab` (it aliases `reasoning_replay_override` today), so the new edge is not even new.

Identical treatment for `preserve_thinking_control_format` and (optionally, same pattern) `Modality.t` priority.

### 2.2 Option B — absorb `capability_vocab` into `capabilities`

Eliminates the cycle by collapsing the leaf into the owner. Rejected: it destroys the leaf-module invariant other consumers rely on and forces every `capability_vocab` user to pull `Capabilities`.

### 2.3 Option C — functor parameterising the parser over the variant

Over-engineered for a flat 8-constructor enum. Rejected.

## 3. Migration (batched)

| step | change | blast radius |
|---|---|---|
| 1 | `capability_vocab`: add `type thinking_control_format` + `preserve_thinking_control_format` + one canonical table per vocabulary; derive `values` and `of_string` from that table. Add `to_string` only with a same-PR consumer and derive it from the table. | leaf only |
| 2 | `capabilities.ml` (+`.mli`): replace `type` defs with alias `= Capability_vocab.*`; re-export constructors | type-only, all callers unchanged |
| 3 | `provider_catalog.ml`: `parse_thinking_control_format` / `parse_preserve_thinking_control_format` → delegate to `Capability_vocab` | 2 parsers collapse |
| 4 | `capabilities.ml`: delete `thinking_control_format_of_manifest_string` + `preserve_thinking_control_*_of_manifest_string`; callers use `Capability_vocab` | 2 local parsers collapse |
| 5 | `capability_manifest.ml` / `model_catalog.ml`: route through `Capability_vocab` | parser sites |
| 6 | tests: table-derived `values`, every value parses, every constructor is represented exactly once, unknown inputs still fail closed and existing warning call sites stay observable | guard |

Steps 1–2 are one PR (the move). Steps 3–5 are a second PR (the delegation swap, mechanical). Step 6 lands with each.

## 4. Verification

- **Compiler**: after the move, the only `match thinking_control_format` arms that compile are exhaustive ones. Adding a variant to `capability_vocab` flags every match site at compile time — the property the current duplication defeats.
- **Single-table proof**: tests must assert that exported `values` are derived from the same table used by `of_string`; the table must represent every constructor exactly once. A round-trip test is allowed only as a secondary check, because three independent encodings can still round-trip accidentally.
- **Property test**: `List.for_all (fun s -> Capability_vocab.thinking_control_format_of_string s <> None) Capability_vocab.thinking_control_format_values` plus constructor coverage against the table.
- **Delegation test**: assert the surviving parser returns identical results to the deleted local matches (pin output before deletion).
- **Warning preservation**: manifest/catalog unknown-value call sites must keep the existing observable warning behavior after delegating to `Capability_vocab`; `None` without a warning is a silent failure regression.

## 5. Why not bundle into the SSOT-audit subset PRs

The 2026-06-30 audit produced three surgical PRs (#2333 provider-kind round-trip, #2334 stop_reason wire SSOT, #2335 catch-all exhaustiveness) that are zero-runtime-change. This RFC's move is **also zero runtime change** but touches ~12 files and ~80 alias references; bundling it would block the small PRs on a large review. Hence separate.

## 6. Non-goals

- `stop_reason_wire` per-backend mapping consolidation (separate variant-fragmentation finding).
- `provider_registry` `max_context` drift (262_144 hardcoded shadowing capability-verified 256_000/128_000) — semantic fix, separate PR.
- `provider.ml:335` documented dual fallback — intentional per docstring, not addressed here.
- `reasoning_dialect.streaming` dead field / `Template_parser` stub variant — related SSOT debt (the streaming-reasoning variant is declared but no live parser consumes it); tracked separately, may fold into this RFC's migration if the streaming field is also relocated.

## 7. Open questions

- Should `Modality.t` priority values move to `capability_vocab` in the same pass? Same pattern, lower blast radius — recommended yes.
- Deriving (`[@@deriving yojson, show]`) currently lives next to the `Capabilities` type. After the move it must live next to the `capability_vocab` type; `Capabilities` alias carries the polyvars through. Needs a compile check in step 2.
