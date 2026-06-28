---
title: RFC-OAS-024 — Pure Core / Impure Shell (Env Config Injection)
---

# RFC-OAS-024: Pure Core / Impure Shell — Env Config Injection

| Field | Value |
|---|---|
| Status | Draft |
| Repo | `jeong-sik/oas` |
| Supersedes | PR #2213 (`base/model_registry` call-time default model), PR #2214 (`discovery` call-time endpoints) — both symptomatic |
| Relates | RFC-OAS-014 (base↔llm_provider inversion), RFC-OAS-018 (catalog externalization), PR #1536 (closed as antipattern reinforcement), #2189 (dual-truth template origin), #2207 (value+helper semver invariant) |
| Audit | First boundary map passed through a 5-proposition adversarial review; 5 blockers + 7 majors surfaced. Revisions below fold them in. |

## 0. Summary

`lib/` reads the process environment from ~54 production sites (raw `Sys.getenv` plus `Cli_common_env` aliasing) and captures some of them at module load. This is a hidden dependency for a library published as `agent_sdk`. The direction: split OAS into a **pure core** that receives all configuration as arguments, and an **impure shell** (boot) that reads env (+ filesystem) once and materializes a config record + a provider registry.

This RFC does **not** propose a big-bang rewrite. It corrects the recent "resolve at call time" drift (#2185/#2189/#2191/#2193/#2200/#2213/#2214), which moved `getenv` from module-load to call-time without removing the hidden dependency — symptom suppression, not a fix.

The first draft of this RFC's boundary map was **adversarially audited** and was broken in five ways. Those breakages are documented in §2 and folded into the cut lines (§6) and phasing (§7).

## 1. Background & Motivation

Two bugs, one root.

**Bug A — module-load env capture.** `let x = Cli_common_env.get "VAR"` at top level freezes the env value when the module loads. Setting `VAR` later in the same process has no effect. Sites: `base/model_registry.ml:8`, `defaults.ml:~50-57`, `base/types.ml:158` (transitive).

**Bug B — the "call-time" drift.** The 7-PR series patched Bug A by adding per-module `resolve_* ()` functions while **retaining** the captured value as a `[@@deprecated]` "compatibility snapshot" (template established by #2189, reinforced by #2200). Result: two sources of truth that drift by construction; the migration completion signal is permanently suppressed because the snapshot is never deleted. #2213 and #2214 cargo-culted this template onto a model concept leaking into `base/` and an Ollama vendor special-case that RFC-OAS-018 Phase 4 has slated for deletion (and that PR #1536 already tried and was closed as "antipattern reinforcement").

**Why neither is the root.** Moving `getenv` from module-load to call-time still leaves the library reading the process environment directly. For an SDK, the correct structure is dependency injection: the caller (CLI main, or whoever embeds OAS) reads env once and passes a config value down. Then neither module-load capture nor call-time capture can exist — the core is pure by construction.

Relevant invariants already in the repo:
- `Provider_config.make` already accepts `api_key : Secret.t` (injection interface exists).
- `Provider_registry` is a mutable registry with `create`/`register`/`find` (injection infrastructure exists).
- #2207 established the semver invariant: keep module-load **values** (env-free baselines or pure constants) for stability; read env via a **separate** call-time helper. #2189/#2213/#2214 misapplied this as license to keep env-**captured** values as "snapshots" — but #2207's value was env-free, not captured.

## 2. Adversarial Audit Results (first boundary map)

Five propositions were each attacked by a hostile reviewer with line-level evidence. Two upheld (A, E — partial), three **refuted** (B, C, D). The first draft's three foundational premises were all false.

| # | Hole | Severity | What the draft claimed | Reality |
|---|---|---|---|---|
| H1 | `Cli_common_env.get` is a one-line `Sys.getenv_opt` alias with **33 call sites / 16 files** | blocker | "21 getenv sites" (complete impure surface) | Real surface ~2×. A core that bans `Sys.getenv` but permits `Cli_common_env.*` achieves nothing. |
| H2 | `provider_config_of_agent` is `@stability Stable`, `@since 0.155.0/0.161.0`, re-exported as `Agent_sdk.Provider.*` | blocker | "Internal → signature change is semver-free" | Public Stable surface on the published opam package. |
| H3 | `runtime_server.ml:49-55` reads `default_config.model` at 3 production spawn sites (243/573/665) | blocker | "ZERO lib-internal consumers of `default_config`" | Making `default_config.model` env-free silently drops `OAS_DEFAULT_MODEL` for every runtime-server-spawned participant. |
| H4 | `Provider_registry.default ()` forces `Yojson.Safe.from_file` on `OAS_PROVIDER_CATALOG` (`provider_catalog.ml:609-624`); `provider.ml:737` calls it per `Custom_registered` resolve with no cache | blocker | "shell reads env purely at startup" | Shell is env + filesystem, and the read repeats per request today. |
| H5 | `base/model_registry.ml:8` is the actual module-load capture; `types.ml:158` reads it transitively | blocker | cut line 3 names only `types.ml:158` | The capture is one hop deeper, inside the proposed pure core (`base/`). |
| H6 | pricing impurity is the `_overrides` **Atomic** (`pricing.ml:29`), not `OAS_PRICING_*` getenv (zero production callers) | major | "move `OAS_PRICING_*` to config field" | Misidentified. Atomic must become a config-carried list. |
| H7 | `mcp.ml:19` `output_token_budget` re-reads env **per call**; relocating to boot is per-call→per-process semantic change, breaks 23 inline tests | major | "movable without breaking callers" | Requires declared semantic change + test refactor. |
| H8 | `provider.ml` has 5+ env-read sites (185/189/196-197/407/426/579-582), not 2; `streaming.ml:264` is a different file | major | cut line 2 names 407/426 only | Undercounted. `api_key_from_env:579-582` bypasses `Secret.t` entirely. |
| H9 | Cited lines `discovery.ml:735,764` and `mcp.ml:597,606` are inside `let%test` blocks | minor | cited as production reads | Inventory was built from a grep that did not exclude test regions. Production reads: `discovery.ml:50,56,62,68`; `mcp.ml:19,410`. |
| H10 | `env_parse.with_env` does `Unix.putenv` **write** (re-exported `cli_common_env.ml:151`) | minor | goal forbids reads only | A core that forbids reads but permits writes is incoherent. |
| H11 | `Provider_registry.t` embeds PATH lookups (`path_entries`) — not a pure data record | minor | "pass registry as arg → consumer pure" | Registry itself must be split pure/impure first. |
| H12 | `Provider_config.t` / `provider_kind` leak into public Provider signatures (`provider.mli:233,240,263-267`) | minor | "`Provider_config.make` is Internal" | Type leakage contradicts the Internal annotation; field additions must stay additive. |

**Net:** direction sound, first boundary map broken. Below is the corrected version.

## 3. Non-goals

- No big-bang rewrite. Incremental, each phase its own PR with its own gate (mirrors RFC-OAS-018 §4).
- No vendor/model literal removal — that is RFC-OAS-018's catalog externalization (this RFC is orthogonal: it fixes *how env is read*, not *what literals exist*).
- No base↔llm_provider dependency inversion — that is RFC-OAS-014. This RFC composes with it (the env seam's natural home is `agent_sdk.foundation` once PR-1 lands) but does not depend on it.
- No new "compatibility snapshots." Existing `[@@deprecated]` snapshots are **deleted** once their callers migrate, not retained.

## 4. Goal — Pure Core definition (corrected)

A module belongs to the **pure core** iff it performs **none** of:

1. `Sys.getenv` / `Unix.getenv` reads
2. `Cli_common_env.get/.bool/.int/.float/.list` calls (they are `getenv` aliases)
3. `Unix.putenv` writes (`env_parse.with_env`)
4. module-load env capture (`let x = <env-reading fn> ()` at top level, including transitive)
5. global `Atomic` reads carrying env-derived data (`pricing.ml:29`)
6. env-gated filesystem IO forced at construction (`provider_catalog.ml:609-624`)

A core that satisfies only (1) is pure one indirection deep. The drift guards (§10) enforce all six.

## 5. Impure Surface Inventory (corrected, test regions excluded)

Production env-read surface, re-derived with `let%test` / `[@@@coverage exclude]` excluded:

| Class | Sites | Notes |
|---|---|---|
| raw `Sys.getenv_opt` | `provider.ml:185,189,196-197,407,426,579-582,635,767`, `backend_ollama.ml:367`, `complete_sync.ml:115`, `pricing.ml:503,517`, `mcp.ml:410`, `constants.ml:249`, `paths.ml:4`, `streaming.ml:264` | (excludes `discovery.ml:735,764` and `mcp.ml:597,606` which are test setup) |
| `Cli_common_env.*` (alias) | **33 sites / 16 files**: `base/model_registry.ml:8`, `base/util.ml`, `defaults.ml`, `llm_provider/{backend_ollama,capability_manifest,discovery,model_catalog,provider_catalog,provider_config,provider_registry,zai_catalog}.ml`, `provider.ml`, `runtime_store.ml:88-92`, `tool_result_store.ml:24-38` | The dominant surface; H1 |
| module-load capture | `base/model_registry.ml:8`, `defaults.ml:~50-57` (`fallback_provider`, `local_llm_url`), `base/types.ml:158` (transitive via `default_model_id`) | H5 + extension |
| env-derived Atomic | `pricing.ml:29` (`_overrides`), read in `pricing_for_model_opt` via `_get_overrides` | H6 |
| env-gated file IO | `provider_catalog.ml:609-624` (forced inside `Provider_registry.default`) | H4 |
| env write | `env_parse.with_env` (`Unix.putenv`), re-exported `cli_common_env.ml:151` | H10 |

## 6. Cut Lines (corrected)

| # | Site | Action |
|---|---|---|
| 1 | `provider.ml:737` `Provider_registry.default ()` (inside `provider_config_of_agent`, `Custom_registered` branch) | **Do NOT change the signature** (H2: Stable/public). Either (a) add optional `?registry:Provider_registry.t option = None`, or (b) pass a materialized registry to the 3 internal callers (`structured.ml:128`, `pipeline_stage_route.ml:33,85`) and leave the public function calling `default ()` internally. Cache the registry once at boot — today it rebuilds per `Custom_registered` resolve. |
| 1b | `Provider_registry.t` purity (H4, H11) | Split into (i) a pure seed (built-in defaults, no env, no file IO) and (ii) an impure overlay (`PATH` lookup via `path_entries`, `OAS_PROVIDER_CATALOG` file load, `env_name`). Only the seed may enter the core. |
| 2 | `provider.ml` env reads: 185, 189, 196-197, 407, 426, 579-582, 767 (H8) | Materialize resolved api keys + base URLs in boot; inject as `Secret.t` / resolved `string`. `api_key_from_env:579-582` bypasses `Secret.t` — route it through the same materialized injection. |
| 3 | `base/model_registry.ml:8` + `base/types.ml:158` (H5) | Make `default_model_id` an env-free pure literal `"claude-sonnet-4-6-20250514"`; add a separate `OAS_DEFAULT_MODEL` resolver in the shell. **Both files cut together** (types.ml depends on model_registry.ml). |
| 3b | `defaults.ml:~50-57` (`fallback_provider`, `local_llm_url`) | Parallel to cut 3: pure-literal baselines + shell resolvers. Delete the `[@@deprecated]` snapshots after callers migrate. |
| 3c | `runtime_server.ml:49-55`, `agent_sdk.ml:202-228`, `agent_turn.ml:212-213` (H3) | **Migrate consumers first.** These read `default_config.model` (env-derived) at production spawn points. They must receive a resolver result from boot before `default_config` can become env-free. |
| 4 | dual registry: `Provider.registry` Hashtbl vs `Provider_registry.t` | Unify lookup semantics (provider.ml:737 comment notes they already disagree). Depends on cut 1b. A behavioral merge, not a type merge. |
| 5 | `Cli_common_env` alias surface (33 sites / 16 files) — H1 | **Resolved (option a, §9 Q1):** add `?getenv` seam to `Cli_common_env.get/.bool/.int/.float/.list` now; Phase 7 migrates the 33 sites to config-record fields; §10 lint enforces zero `Cli_common_env` calls in core. End-state identical for (a)/(b); (a) touches 1 module for the seam. |
| 6 | `runtime_store.ml:88-92` (`OAS_RUNTIME_SESSION_ROOT`), `tool_result_store.ml:24-38` (`OAS_TOOL_RESULT_*`) | Inject `session_root` + thresholds as config-record fields materialized in boot. Same class as runtime-env knobs. |
| 7 | `pricing.ml:29` `_overrides` Atomic (H6) | Become a config-carried `pricing_entry list` threaded into `pricing_for_model_opt`. `install_pricing_overrides` / `clear_pricing_overrides` relocate to boot (or removed). Note `OAS_PRICING_FILE`/`OAS_PRICING_OVERRIDES` getenv have **zero production callers** — wire into boot or drop. |
| 8 | `mcp.ml:19` `output_token_budget` (H7) | Declare per-call→per-process semantic change. Refactor 23 `Unix.putenv`-based inline tests to inject `~budget` (`truncate_output ~budget text`). `OAS_MCP_ALLOW_SHELL_COMMANDS` (pure bool) moves cleanly. |
| 9 | `env_parse.with_env` (`Unix.putenv` write) — H10 | Policy decision (Q7): forbid writes in core → relocate to shell; or permit and document the asymmetry. |
| — | `discovery.ml` `LLM_ENDPOINTS`, `scan_local_endpoints` | Move cleanly — `discover ~sw ~net ~endpoints` is already env-free at `:509-511`. Make `~ports` a **required** arg of `scan_local_endpoints` so no caller re-couples env to the network path. |

## 7. Phasing

Each phase = own PR, own gate. Ordering respects dependencies surfaced by the audit.

- **Phase 0 — Foundations.** (a) Split `Provider_registry` into pure seed + impure overlay (cut 1b). (b) Decide the `Cli_common_env` strategy (cut 5; Q1). (c) Audit and list **every** module-load env capture (grep top-level `let _ = <env-fn> ()` transitively) — not just the three known. (d) Re-derive the getenv inventory with test-region exclusion. Gate G0: lib behavior byte-for-byte identical; pure-seed registry has no env/file IO.
- **Phase 1 — Provider resolution cut (cuts 1, 2).** `?registry` optional arg on `provider_config_of_agent`; materialize api keys + base URLs in boot; cache registry once. Gate G1: `provider.ml` performs no raw `getenv`; existing tests green; no public signature break.
- **Phase 2 — default_config / model_registry (cuts 3, 3b, 3c).** **Migrate `runtime_server.ml`, `agent_sdk.ml`, `agent_turn.ml` consumers to receive resolved `config.model` from boot FIRST.** Then make `default_model_id` / `default_config` env-free baselines. Preserve the `OAS_DEFAULT_MODEL` contract via the shell resolver (Q2). Gate G2: `base/` has no env capture; `OAS_DEFAULT_MODEL` still honored end-to-end.
- **Phase 3 — Runtime stores (cut 6).** `session_root` + tool-result thresholds as config fields. Gate G3: `runtime_store` / `tool_result_store` perform no `Cli_common_env` calls.
- **Phase 4 — Pricing Atomic (cut 7).** Thread `pricing_entry list` through `pricing_for_model_opt`; relocate install/clear to boot. Gate G4: no env-derived `Atomic` reads in core.
- **Phase 5 — MCP semantic change (cut 8).** Declare per-call→per-process for `output_token_budget`; refactor 23 tests to `~budget` injection. Gate G5: tests green with injected budget.
- **Phase 6 — Dual registry unify (cut 4).** Behavioral merge of `Provider.registry` and `Provider_registry.t`. Depends on Phase 0 (cut 1b) and Phase 1.
- **Phase 7 — `Cli_common_env` migration + lint (cut 5, 9).** Migrate the remaining 33 sites to the chosen seam (Q1). Enforce the pure-core definition (§4) with a CI lint (§10). Delete all retained `[@@deprecated]` snapshots. Gate G7: lint clean; zero snapshots retained.

RFC-OAS-014 composition: once 014 PR-1 (`agent_sdk.foundation`) lands, the env seam and `Cli_common_env` move into foundation. Phases 0–7 are written to land on the current layout and survive that move.

## 8. Backward Compatibility

- `provider_config_of_agent`: Stable/public. Phase 1 uses the backwards-compatible `?registry` option (or option b). No minor bump required for the signature.
- `Types.default_config`: stays a **value** (#2207 invariant). The `model` field becomes an env-free literal baseline; `OAS_DEFAULT_MODEL` override is preserved via a shell resolver that the boot layer substitutes. The `model_registry.mli:9` documented contract ("overridable via `OAS_DEFAULT_MODEL`") is honored, not silently removed (H3/H5).
- `Provider_config.t` / `provider_kind`: already leak into public Provider signatures (H12). Field additions stay **additive/optional**. A precursor phase to make them opaque is left as a separate RFC.
- `OAS_MCP_OUTPUT_MAX_TOKENS`: per-call → per-process is a declared semantic change (Phase 5), called out in CHANGELOG with a migration note for any caller doing mid-process `putenv`.
- Retained `[@@deprecated]` snapshots (`#2189` `local_llm_url`/`fallback_provider`, `#2213` `default_model_id`, `#2214` `ollama_endpoint`) are deleted once callers migrate (Phase 7). No new snapshots.

## 9. Open Questions (decision-required)

1. **`Cli_common_env` strategy** — **RESOLVED (option a).** Add a `?getenv:(string -> string option) = Sys.getenv_opt` seam to `Cli_common_env.get`/`.bool`/`.int`/`.float`/`.list` now (centralized, one module touched for the seam). The end-state is identical for both options — zero `Cli_common_env` calls in core — so (b) is rejected as pure churn: it reaches the same end-state by editing 16 files instead of 1 just to install the seam. Phase 7 migrates the 33 call sites to read config-record fields, and the §10 lint enforces the zero. The seam exists for the shell and for tests, not for the core.
2. **`OAS_DEFAULT_MODEL` contract** — **RESOLVED (preserve).** `default_model_id` becomes an env-free literal `"claude-sonnet-4-6-20250514"`; `OAS_DEFAULT_MODEL` is honored by a boot resolver the shell substitutes into `config.model`. The `model_registry.mli:9` contract is updated from *module-load* evaluation to *boot-time* evaluation — no silent removal (H3/H5). Deprecation (option b) rejected: no evidence of intent to drop the override; the override stays, it just moves out of `base/`.
3. **`provider_config_of_agent`** — `?registry` optional arg (backwards-compatible, but the public function never becomes registry-pure) vs route registry only to internal callers (public function keeps calling `default ()`). Both semver-safe.
4. **`Provider_registry` split line** — is the pure seed just built-in defaults, with `PATH` + catalog-file + `env_name` all in boot? Need a concrete type design.
5. **`pricing` Atomic** — thread `pricing_entry list` through every `pricing_for_model_opt` caller (purer, signature change) vs keep the Atomic but relocate install/clear to boot (localized, less pure).
6. **`mcp` per-call live-reload** — feature or accident? 23 tests rely on it; no production `putenv` caller was identified. Needs a grep of `bin/` and runtime config surface before declaring it test-only.
7. **`env_parse.with_env` (`Unix.putenv` write)** — forbidden in core (relocate to shell) or permitted with documented asymmetry vs reads?
8. **Type leakage of `Provider_config.t`/`provider_kind`** — precursor RFC to make them opaque before non-additive changes, or accept that field additions are always additive?

## 10. Verification / Drift Guards

Exit gates per phase are in §7. A CI lint enforces the §4 pure-core definition after Phase 7:

```bash
# Pure-core modules must match zero of these (allow-list: foundation env seam, catalog loader, test fixtures)
PATTERN_READ='Sys\.getenv|Unix\.getenv'
PATTERN_ALIAS='Cli_common_env\.(get|bool|int|float|list|kv_pairs)'
PATTERN_WRITE='Unix\.putenv|Env_parse\.with_env'
PATTERN_CAPTURE='^let [a-z_]+ = .*(resolve_|Cli_common_env|Sys\.getenv)'  # top-level env-reading let
# Atomic + file-IO gates enforced by review checklist until automated
```

Allow-list: the foundation env-seam module, the catalog TOML loader, and test fixtures. This is the structural guard that prevents a future "#2215" from re-introducing call-time `getenv` as "fix."

## 11. Relationship to the triggering PRs

- **#2213, #2214** — **both MERGED 2026-06-28 (during this RFC's audit).** Cannot close. The drift entered `main`; this RFC is the remediation. Their changes are absorbed/reverted by the phases: #2213's `default_model_id_value ?getenv` + retained snapshot → Phase 2 moves the resolver out of `base/` (to the shell) and **deletes** the snapshot; #2214's `resolve_ollama_endpoint` + `ollama_defaults`/`llama_defaults` function conversion → Phase 1's uniform `*_defaults` migration subsumes the Ollama-only case, and cut 1 removes the per-`Custom_registered` `default ()` rebuild that motivated it. A follow-up revert PR is **not** opened — the changes are small and the phases touch the same lines, so folding them forward is cheaper than revert-then-redo.
- **#2189**: the dual-truth template origin. Its snapshots are deleted in Phase 7 once `Cli_common_env` migration lands.
- **#2200**: kept as the canonical correct shape (env-free baseline + call-time helper, no dual-truth).
