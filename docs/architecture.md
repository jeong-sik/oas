# OAS Architecture

## Layer overview

```
┌─────────────────────────────────────────────┐
│  lib_swarm/  (agent_sdk_swarm)              │
│  Layer 2: Swarm Engine                      │
│  runner.ml, swarm_types.ml, checkpoint.ml   │
├─────────────────────────────────────────────┤
│  lib/  (agent_sdk)                          │
│  Layer 1: Agent Runtime                     │
│  Agent, Pipeline, Provider, Error, Tools    │
├─────────────────────────────────────────────┤
│  OCaml 5.x + Eio                            │
│  Structured concurrency, effect handlers    │
└─────────────────────────────────────────────┘
```

## Layer 1: Agent Runtime

Single-agent execution engine.

| Module | Responsibility |
|--------|---------------|
| `Agent` | Lifecycle: create, run, resume |
| `Pipeline` | 6-stage turn: prep, reduce, route, extract, tools, update |
| `Provider` | Multi-provider API dispatch (Anthropic, OpenAI, Ollama) |
| `Error` | 8-domain structured ADT errors |
| `Hooks` | BeforeTurn, AfterTurn, PreToolUse, PostToolUse hooks |
| `Tracing` | TRACER module type, Null/Fmt/OTel implementations |
| `Metrics` | Counter + histogram collection, OTLP JSON export |
| `Context_reducer` | Token budget management via message summarization |
| `Guardrails` | Tool filtering, output validation |
| `Tool_set` | Tool registration, schema generation |

## Layer 2: Swarm Engine

Multi-agent parallel execution with convergence.

| Module | Responsibility |
|--------|---------------|
| `Runner` | 3 modes: Decentralized, Supervisor, Pipeline |
| `Swarm_types` | Config, state, callbacks, resource_budget |
| `Swarm_checkpoint` | Save/restore convergence state |
| `Test_helpers` | Mock factories for zero-LLM testing |

## Error model

```
sdk_error
├── Api of api_error          (7 variants: RateLimited, AuthError, ...)
├── Agent of agent_error      (4 variants: MaxTurns, TokenBudget, ...)
├── Mcp of mcp_error          (5 variants: ServerStart, Initialize, ...)
├── Config of config_error    (3 variants: MissingEnv, Unsupported, ...)
├── Serialization of ...      (3 variants: JsonParse, VersionMismatch, ...)
├── Io of io_error            (2 variants: FileOp, Validation)
├── Orchestration of ...      (3 variants: UnknownAgent, Timeout, ...)
├── A2a of a2a_error          (5 variants: TaskNotFound, InvalidTransition, ...)
└── Internal of string
```

All error domains use record payloads for context. Pattern matching is exhaustive — adding a new domain causes compile errors at all handler sites.

## Concurrency model

Eio structured concurrency: every fiber runs within a `Switch.t`.
- Switch guarantees cleanup on exit (normal or exception)
- Timeouts propagate through the fiber tree
- No resource leaks by construction

```
Switch.run @@ fun sw ->
  Eio.Fiber.all [
    (fun () -> agent_1.run ~sw prompt);
    (fun () -> agent_2.run ~sw prompt);
  ]
(* Both fibers complete or cancel here — no orphans *)
```
