# RFC-OAS-039: Settle pre-tool input before execution

| | |
|---|---|
| Status | Draft |
| Author | vincent (with Claude analysis) |
| Created | 2026-07-28 |
| Target | `agent_sdk` |
| Related | [[RFC-OAS-025]] (forced tool-use enforcement boundary) |

## 0. Summary

`Pre_tool_use` is the first hook stage that knows the exact tool occurrence and
input. This RFC admits `ElicitInput` at that stage and settles it through the
existing caller-configured `options.elicitation` callback before the tool
invocation is opened.

- `Answer _` authorizes the exact call.
- `Declined` and `Timeout` produce a typed non-retryable `ToolResult` without
  executing the tool.
- A missing callback fails closed as `Hook_execution_failed`; no invocation or
  effect is started.

The before-turn `Agent.provide_input` API is intentionally not reused. It
appends a `User` message and has different transcript semantics.

## 1. Problem

Before this change, the hook decision matrix accepted `ElicitInput` only at
`Before_turn`. A caller that needs approval for one exact tool call could not
ask after the provider had selected both the tool and its input.

Encoding a pending decision as a successful tool result is incorrect:
`tool_result_outcome` has only `Tool_succeeded` and `Tool_failed`, and provider
wire formats require every emitted result to describe a completed call.

## 2. Decision

The legal decisions are:

```ocaml
| Before_turn  -> [ K_Continue; K_ElicitInput; K_Nudge ]
| Pre_tool_use -> [ K_Continue; K_Block; K_ElicitInput ]
```

When a `Pre_tool_use` hook returns `ElicitInput request`,
`Agent_tools.execute_tools` consults the configured callback:

```ocaml
match options.elicitation request with
| Answer _ -> execute_the_exact_call ()
| Declined -> blocked_tool_result "Tool execution was declined by the caller"
| Timeout -> blocked_tool_result "Tool execution approval timed out"
```

The callback result is a typed gate response. It is not appended as a `User`
message. A refusal is represented as the same model-visible, deterministic
failure shape used by `Hooks.Block`, and no tool lifecycle event is emitted for
an effect that never started.

## 3. Fail-closed boundary

An asynchronous durable suspension is not implemented by this RFC.

Opening an invocation and returning `Error.InputRequired` is insufficient:

1. the generic execution runner closes ordinary `Error` results as failed;
2. merely finding an open invocation during resume cannot prove that the host
   approved it;
3. concurrent calls can create multiple pending decisions while a single
   request payload exposes only one;
4. checkpointing a sibling result while another call is pending creates a
   partial tool-result set that the durable resume topology correctly rejects.

A future asynchronous API therefore needs a typed journal event for the gate
request and its answer, a stage-specific public response method, atomic refusal
settlement, and an explicit partial-batch recovery contract. Until those types
exist, a missing callback fails before `open_invocation`.

## 4. Public contract

The hook decision matrix in `Hooks`, the README, and the event catalog all list
`ElicitInput` as legal at `Pre_tool_use`.

The configured callback is honored consistently at both legal elicitation
stages, with stage-specific transcript behavior:

- `Before_turn`: an `Answer` may add a user message before provider dispatch.
- `Pre_tool_use`: an `Answer` authorizes the exact call and adds no user
  message.

## 5. Verification

Focused tests must prove:

1. `ElicitInput` is legal at `Pre_tool_use` and remains illegal at every other
   unsupported stage.
2. `Answer` executes the exact call once.
3. `Declined` and `Timeout` execute it zero times and return a typed
   non-retryable failure.
4. Missing callback support fails closed before invocation/effect execution.
5. The public hook matrix and user-facing documentation agree with runtime
   validation.

## 6. Non-goals

- Adding a third provider-wire tool result state.
- Treating `_meta` as a control-flow protocol.
- Reusing before-turn `Agent.provide_input` for tool-gate answers.
- Claiming asynchronous or crash-resumable approval without durable typed
  request/response evidence.
