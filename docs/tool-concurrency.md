# Tool Concurrency and Descriptor Classification

OAS batches tool calls within a single turn based on each tool's
[concurrency_class](lib/base/tool.mli). The runtime groups consecutive tools
with the same class into batches and executes them as follows:

- `Parallel_read` — independent read-only tools run concurrently via
  `Eio.Fiber.List.map`. Results are returned in the original order regardless
  of completion order.
- `Sequential_workspace` — workspace-mutating tools run one at a time, in the
  order requested by the model.
- `Exclusive_external` — external/network tools run in isolation. Any parallel
  batch before or after is flushed so the external call never overlaps with
  another tool.

## Setting the concurrency class

Pass a `Tool.descriptor` with `concurrency_class` to `Tool.create` or
`Tool.create_with_context`:

```ocaml
open Agent_sdk

let read_file_tool =
  let descriptor =
    { Tool.kind = Some "fs"
    ; mutation_class = Some Tool.Read_only
    ; concurrency_class = Some Tool.Parallel_read
    ; permission = Some Tool.ReadOnly
    ; evidence_role = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  Tool.create
    ~descriptor
    ~name:"read_file"
    ~description:"Read a file from the workspace"
    ~parameters:[]
    (fun _args -> Ok { Types.content = "contents" })
```

## External read-only tools must NOT be `Parallel_read`

A tool that calls an external HTTP API (web search, page fetch, payment
lookup, etc.) should be classified as `Exclusive_external`, not `ReadOnly`
and not `Parallel_read`. Concurrent external calls can:

- exceed provider rate limits,
- produce non-deterministic ordering,
- incur unnecessary cost,
- violate provider terms of service.

Example:

```ocaml
let web_search_tool =
  let descriptor =
    { Tool.kind = Some "web"
    ; mutation_class = Some Tool.External_effect
    ; concurrency_class = Some Tool.Exclusive_external
    ; permission = Some Tool.ReadOnly
    ; evidence_role = None
    ; shell = None
    ; notes = []
    ; examples = []
    }
  in
  Tool.create ~descriptor ~name:"web_search" ~parameters:[] handler
```

## How host runtimes wire the class

Host runtimes should derive an OAS `Tool.descriptor` at their tool boundary.
Read-only workspace tools map to `Parallel_read`; writes map to
`Sequential_workspace`; destructive tools map to `Exclusive_external`.
External-network tools such as web search or web fetch should also map to
`Exclusive_external` so the OAS runtime never batches them in parallel.

See also:

- `lib/agent/agent_tools.ml` for the batching implementation.
- `examples/tool_use.ml` for a runnable example with mixed concurrency classes.
- `examples/async_agent_demo.ml` for async agents that use tools.
