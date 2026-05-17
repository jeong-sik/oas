# RFC-OAS-021: ApprovalRequired Missing Callback Policy

| | |
|---|---|
| Status | Draft |
| Author | vincent |
| Created | 2026-05-17 |
| Target | `agent_sdk` (oas) |
| Related | RFC-OAS-013 (keeper tool disclosure), RFC-OAS-008 (typed tool identification), masc-mcp RFC-0109 (keeper admission denial boundary) |

## 0. Summary

`ApprovalRequired` used to execute the tool when no approval callback was
registered. The behavior preserved compatibility, but it made a safety boundary
depend on a missing runtime hook:

```ocaml
ApprovalRequired, approval = None -> execute_without_callback
```

This RFC adds an explicit policy to `Agent_options`:

```ocaml
type missing_approval_callback_policy =
  | Execute_without_callback
  | Reject_without_callback
```

The default remains `Execute_without_callback` for compatibility. Runtime
profiles that need fail-closed safety can select `Reject_without_callback`.

## 1. Problem

`Hooks.ApprovalRequired` communicates a policy decision: a tool must not execute
until a caller approves it. If the caller forgot to register an approval
callback, the old SDK behavior logged a debug line and executed anyway.

That shape is worse than a stopped agent:

- the tool side effect still happens;
- the missing callback is low-visibility;
- downstream runtimes cannot distinguish "approved" from "approval hook absent";
- adding only logs or counters would be telemetry-as-fix, not a boundary fix.

## 2. Decision

Missing approval callbacks are a first-class policy choice.

- `Execute_without_callback` keeps legacy behavior and is the default.
- `Reject_without_callback` returns a deterministic non-retryable tool error:
  `Tool rejected: approval required but no approval callback is registered`.

The policy is carried in `Agent_options`, configurable through `Builder`, and
passed into `Agent_tools.execute_tools`.

## 3. API

### 3.1 Hooks

```ocaml
type missing_approval_callback_policy =
  | Execute_without_callback
  | Reject_without_callback
```

### 3.2 Agent Options

```ocaml
type options =
  { ...
  ; missing_approval_callback_policy : Hooks.missing_approval_callback_policy
  }
```

Default:

```ocaml
missing_approval_callback_policy = Hooks.Execute_without_callback
```

### 3.3 Builder

```ocaml
val with_missing_approval_callback_policy :
  Hooks.missing_approval_callback_policy -> t -> t
```

### 3.4 Tool Execution

`Agent_tools.execute_tools` receives the policy explicitly:

```ocaml
~missing_approval_callback_policy:Hooks.missing_approval_callback_policy
```

On `ApprovalRequired` with no callback:

| Policy | Result |
|---|---|
| `Execute_without_callback` | execute tool, preserving legacy behavior |
| `Reject_without_callback` | return deterministic non-retryable tool error |

## 4. Compatibility

This is a non-breaking runtime default. Existing agents continue executing when
the callback is absent unless they opt into `Reject_without_callback`.

The only signature change is internal SDK plumbing where `execute_tools` is
called from `Agent_trace` and tests. Public builder users get an additive setter.

## 5. Verification

Focused gates for the implementation PR:

```bash
scripts/dune-local.sh build \
  test/test_approval.exe \
  test/test_builder.exe \
  test/test_builder_coverage.exe

./_build/default/test/test_approval.exe
./_build/default/test/test_builder.exe
./_build/default/test/test_builder_coverage.exe
git diff --check
```

Expected coverage:

- legacy no-callback behavior still executes by default;
- fail-closed policy rejects without executing;
- rejection is deterministic and non-retryable;
- builder setter stores the selected policy;
- default options expose `Execute_without_callback`.

## 6. Migration

1. Add the policy type in `Hooks`.
2. Add the option field to `Agent_options`.
3. Add the builder setter and default.
4. Pass the policy through `Agent_trace` to `Agent_tools`.
5. Keep compatibility default until downstream runtime profiles can opt in.
6. After downstream adoption, consider whether new safety-focused constructors
   should default to `Reject_without_callback`.

## 7. Acceptance

- [x] Missing approval callback behavior is explicit policy, not an implicit
  debug fallback.
- [x] Compatibility default remains unchanged.
- [x] Fail-closed mode rejects with deterministic non-retryable error.
- [x] Builder and options expose the policy.
- [x] Focused tests cover default and fail-closed behavior.
- [ ] Downstream runtime profile adoption has landed and this RFC records the
  PR number.

## 8. References

- `lib/agent/agent_tools.ml`: approval gate handling.
- `lib/base/hooks.ml`: approval hook action and missing-callback policy.
- `test/test_approval.ml`: default and fail-closed behavior.
- `reports/keeper-stop-analysis-20260517.html`: OAS #18 / S13 finding.
