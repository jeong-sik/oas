## Background

This PR implements the `oas:legacy-purge-safe` group from the adversarial code audit (see `PRIORITY-ACTION-PLAN.md` and `reports/legacy-purge.md`). The goal is to safely remove or harden legacy workarounds that create security, concurrency, or maintainability risks without changing production behavior.

## What changed

### Security / sandboxing
- **Removed `file` and `exec` auth types from the provider catalog.** These allowed catalog entries to read credentials from arbitrary files or execute arbitrary commands. Parsing now rejects them with a clear error.
- **Stopped injecting a mock Anthropic API key in the observable agent example.** Users must set their own key or use `OAS_MOCK=1`.
- **Renamed hardcoded example/mock API-key env names** to clearly example/test-only names (`OAS_STREAMING_EXAMPLE_API_KEY`, `OAS_TEST_MOCK_API_KEY`).

### Legacy library cleanup
- **Replaced `Str` with `Re` in `Metric_contract`.** `Str` is domain-unsafe under OCaml 5.x; `Re` is already a project dependency.
- **Removed the `threads` dependency from `lib/dune`.** OAS uses Eio fibers, not systhreads.

### Dead code removal
- **Deleted `Fd_throttle_hook`.** It was a process-wide global hook with unclear production value; `Provider_throttle.with_permit_priority` now simply calls the wrapped function.
- **Deleted the dedicated `Fd_throttle_hook` unit-test file** and removed it from `test/dune`.

### Mutable global state
- **Moved `paused_inputs` from a module-level `Hashtbl` into `Runtime_server_types.state`.** Multiple runtime-server instances in the same process no longer share paused participants.
- **Replaced the NUL-delimited composite key** with a tuple key `(session_id, request_id)`, removing a trust-boundary collision risk.

### Documentation
- **Documented `Cli_common_env.with_env` limitations:** OCaml's `Unix` module has no portable `unsetenv`, so restoring an unset variable sets it to empty, and the helper must not be used for production secrets.

## Test updates

- Added a regression test in `test_provider_catalog_coverage` asserting that `file` and `exec` auth types are rejected.
- Updated `test_provider_runtime_binding` to remove file/exec fixtures and assertions.

## Verification

```bash
scripts/dune-local.sh build lib/llm_provider/llm_provider.cma lib/agent_sdk.cma
scripts/dune-local.sh build @install
scripts/dune-local.sh runtest lib/llm_provider
scripts/dune-local.sh runtest lib
scripts/dune-local.sh runtest test/test_provider_catalog_coverage.exe test/test_provider_runtime_binding.exe
```

All passed.

## Notes / blockers

- `Unix.unsetenv` is not available in this OCaml environment, so `with_env` keeps `Unix.putenv name ""` for the unset-restore path (treated as unset by `get`).
- `str` is kept in `lib/dune` because other modules in `lib/` still use `Str`; this PR only replaces the scoped usage in `Metric_contract`.
- `ISSUE_OCAML_5_SMELLS.md` did not exist in this worktree, so no deletion was needed.
