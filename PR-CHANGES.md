# PR Changes — `oas:legacy-purge-safe`

## Scope

Implements P0/P1 items from the `oas/legacy-purge-safe` section of the adversarial audit action plan.

## Changes

### 1. Provider catalog: remove sandbox-unsafe `file`/`exec` auth types

- `lib/llm_provider/provider_catalog.ml{,i}`: removed `File` and `Exec` constructors from `auth_mode`; parsing now returns a clear "removed auth type" error.
- `lib/provider_runtime_binding.ml{,i}`: removed corresponding `File`/`Exec` constructors and mapping branches.
- `lib/llm_provider/provider_registry.ml`: removed `File`/`Exec` availability checks.
- `test/test_provider_catalog_coverage.ml`: removed file-auth/exec-auth fixtures; added regression test verifying both types are rejected.
- `test/test_provider_runtime_binding.ml`: removed file-auth/exec-auth fixtures and assertions.

### 2. Replace `Str` with `Re` in `metric_contract.ml`

- `lib/metric_contract.ml`: replaced `Str.regexp`/`Str.search_forward`/`Str.matched_group` with `Re.Perl.re`/`Re.exec`/`Re.Group.get`.
- `lib/dune`: added `re` to the `agent_sdk` library dependencies.

### 3. Remove `threads` from `lib/dune`

- `lib/dune`: removed `threads` from `agent_sdk` library dependencies. Code review confirmed no direct `Thread` module usage in `lib/`.

### 4. Purge `Fd_throttle_hook`

- Deleted `lib/llm_provider/fd_throttle_hook.ml{,i}`.
- `lib/llm_provider/provider_throttle.ml`: `with_permit_priority` now directly calls `f ()` instead of going through the global hook.
- `test/test_fd_throttle_hook.ml`: deleted.
- `test/dune`: removed `test_fd_throttle_hook` from both test lists.

### 5. Move `paused_inputs` from global state into `Runtime_server_types.state`

- `lib/runtime_server_types.ml{,i}`: moved `paused_participant` type here; added `paused_inputs_mu` and `paused_inputs` fields to `state`.
- `lib/runtime_server.ml`: removed module-level `paused_inputs_mu`/`paused_inputs`; `store_paused_input` and `take_paused_input` now accept `state`. Replaced NUL-delimited composite key with tuple key `(session_id, request_id)`.

### 6. `cli_common_env.with_env` documentation

- `lib/llm_provider/cli_common_env.ml`: added comment explaining that OCaml's `Unix` module has no portable `unsetenv`, so restoring an unset variable sets it to empty (which `get` treats as unset), and that the helper must not be used for production secrets.

### 7. Example / mock key cleanup

- `examples/observable_agent.ml`: removed `Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key"` injection; users now set the key explicitly or use `OAS_MOCK=1`.
- `examples/streaming.ml`: changed hardcoded `api_key_env = "DUMMY_KEY"` to example-only `OAS_STREAMING_EXAMPLE_API_KEY`.
- `lib/provider_mock.ml`: changed `api_key_env = "MOCK_API_KEY"` to test-only `OAS_TEST_MOCK_API_KEY`.

## Blockers / Departures from Plan

- `Unix.unsetenv` is not exposed by OCaml's `Unix` module in this environment (OCaml 5.4.1), so `with_env` retains `Unix.putenv name ""` for the unset-restore path, with an explicit comment documenting the limitation.
- The `str` library dependency in `lib/dune` is retained because several other modules in `lib/` still use `Str`. This PR only replaces `Str` in the scoped `metric_contract.ml`; full `str` removal is left to a broader cleanup.
- `ISSUE_OCAML_5_SMELLS.md` does not exist in this worktree, so no deletion was needed.

## Verification

- `scripts/dune-local.sh build lib/llm_provider/llm_provider.cma lib/agent_sdk.cma` — passed.
- `scripts/dune-local.sh build @install` — passed.
- `scripts/dune-local.sh runtest lib/llm_provider` — passed.
- `scripts/dune-local.sh runtest lib` — passed.
- `scripts/dune-local.sh runtest test/test_provider_catalog_coverage.exe test/test_provider_runtime_binding.exe` — passed.
