# Tool Execution Modes

OAS batches tool calls within a turn using only the caller-declared
[`execution_mode`](../lib/base/tool.mli) carried by `Tool.descriptor`:

- `Concurrent` — calls in a contiguous batch may overlap. OAS restores result
  order after `Eio.Fiber.List.map` completes.
- `Serial` — calls execute one at a time in model order.

A missing descriptor means `Serial`. OAS does not inspect a tool name, command,
path, input, effect, or product identity to choose a mode.

## Declaring a mode

```ocaml
open Agent_sdk

let lookup_tool =
  let descriptor = { Tool.execution_mode = Concurrent } in
  Tool.create
    ~descriptor
    ~name:"lookup"
    ~description:"Look up a value"
    ~parameters:[]
    (fun _args -> Ok { Types.content = "value"; _meta = None })
```

The caller is authoritative. External-effect decisions are independent from
this structural scheduling declaration. An embedding application settles such
decisions before dispatch and may return `Hooks.Block reason` from its
`PreToolUse` hook to reject a call explicitly.

See also:

- `lib/agent/agent_tools.ml` for batching.
- `examples/tool_use.ml` for mixed explicit modes.
- `examples/async_agent_demo.ml` for async agents using tool descriptors.
