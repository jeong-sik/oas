# agent-sdk

[![CI](https://github.com/jeong-sik/oas/actions/workflows/ci.yml/badge.svg)](https://github.com/jeong-sik/oas/actions/workflows/ci.yml)

A native OCaml implementation of the Anthropic Agent SDK using OCaml 5.x + Eio for structured concurrency. Supports Anthropic Messages API, OpenAI-compatible endpoints, and Gemini.

- OCaml package: `agent_sdk`
- OCaml module: `Agent_sdk`
- Requires: OCaml >= 5.1, Dune >= 3.11

## Installation

```bash
opam pin add agent_sdk git+https://github.com/jeong-sik/oas.git#main --yes
```

For development (with test dependencies):

```bash
git clone https://github.com/jeong-sik/oas.git && cd oas

# Required fork pins (OCaml 5.4 compat)
opam pin add bisect_ppx git+https://github.com/patricoferris/bisect_ppx.git#5.2 --no-action --yes
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git#v1.2.0 --no-action --yes

opam install . --deps-only --with-test --yes
dune build @all
```

## Quickstart

```ocaml
open Agent_sdk

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let agent =
    Agent.create ~net
      ~config:
        { Types.default_config with
          name = "hello";
          system_prompt = Some "You are a helpful assistant. Be concise.";
        }
      ~tools:[] ()
  in
  match Agent.run ~sw agent "What is 2 + 2?" with
  | Ok response ->
      List.iter
        (function Types.Text t -> print_endline t | _ -> ())
        response.content
  | Error e -> prerr_endline (Error.to_string e)
```

This assumes a local LLM server (llama-server) on port 8085. For Anthropic API, configure a provider:

```ocaml
let agent =
  Agent.create ~net
    ~config:{ Types.default_config with name = "hello" }
    ~options:{ Agent.default_options with
      provider = Some (Provider.anthropic_sonnet ());
    }
    ~tools:[] ()
```

See `examples/` for more: `basic_agent.ml`, `tool_use.ml`, `streaming.ml`, `review_agent.ml`, `swarm_review.ml`, `governance_demo.ml`, `plan_execute_demo.ml`, `autonomy_primitives_demo.ml`.

## Provider support

| Provider | Constructor | Endpoint |
|----------|-----------|----------|
| Local (llama-server) | `Provider.Local { base_url }` | `http://127.0.0.1:8085` |
| Anthropic | `Provider.Anthropic` | `https://api.anthropic.com` |
| OpenAI-compatible | `Provider.OpenAICompat { base_url; ... }` | Any `/chat/completions` |
| Gemini | `Provider.Gemini` | `https://generativelanguage.googleapis.com` |
| OpenRouter | `Provider.openrouter ()` | `https://openrouter.ai/api/v1` |

`Provider.resolve` returns `(base_url * api_key * headers, sdk_error) result`. Missing env vars produce `Error`, not silent fallback.

## Architecture

OAS has a 3-layer architecture. Each layer is a separate opam library with explicit dependencies.

```
Layer 3: MASC  (external — multi-process coordination)
            |
Layer 2: agent_sdk_swarm  (lib_swarm/)
            |  Swarm execution: Decentralized / Supervisor / Pipeline
            |  Convergence loop, metric-driven iteration
            |
Layer 1: agent_sdk  (lib/)
            |  Single-agent runtime: turn loop, tool dispatch, hooks
            |
            +-- lib/agent/       Agent lifecycle, turns, tools, handoff, builder
            +-- lib/pipeline/    6-stage turn pipeline
            +-- lib/protocol/    A2A, MCP, Agent Card, Agent Registry
            +-- lib/llm_provider/  Shared LLM types, HTTP client, streaming
            +-- lib/*.ml         Context, Hooks, Guardrails, Orchestrator, etc.
```

### Layer 1: Agent Runtime (`agent_sdk`)

| Module | Role |
|--------|------|
| `Types` | Domain types: message, role, stop_reason, content_block, SSE events |
| `Provider` | LLM endpoint abstraction (Local / Anthropic / OpenAICompat / Ollama) |
| `Api` | HTTP client: `create_message` (sync) + `create_message_stream` (SSE) |
| `Agent` | Multi-turn agent loop with automatic tool_use handling (abstract `Agent.t`) |
| `Tool` / `Tool_set` | Tool definition, JSON Schema generation, O(1) lookup |
| `Builder` | Fluent API for agent construction with `build_safe` validation |
| `Hooks` | Lifecycle hooks: BeforeTurn, AfterTurn, PreToolUse, PostToolUse, OnStop |
| `Context` | Cross-turn shared state (scoped key-value store, `Yojson.Safe.t` values) |
| `Guardrails` | Tool filtering (AllowList/DenyList/Custom) + per-turn call limits |
| `Error` / `Error_domain` | 2-level structured errors: 7 domain variants + Internal, poly-variant mapping |
| `Orchestrator` | Multi-agent task orchestration (Sequential, Parallel, FanOut, Pipeline, Conditional) |
| `Log` | Structured logging with level filtering and composable sinks |
| `Mcp` | MCP client (NDJSON-over-stdio, server lifecycle, paginated tool listing) |
| `Streaming` | Multi-provider SSE parsing (Anthropic + OpenAI-compatible) |
| `Pipeline` | 6-stage turn pipeline with Provider_intf routing |
| `Contract` | Runtime contracts: instruction layers, triggers, tool grants |
| `Memory` | 5-tier memory: Scratchpad, Working, Episodic, Procedural, Long_term |
| `Memory_access` | Deny-by-default agent-scoped memory permissions |
| `Verified_output` | Phantom-typed compile-time output verification |
| `Policy` | Priority-ordered rule evaluation at decision points |
| `Audit` | Immutable log of policy decisions and agent actions |
| `Durable` | Typed step chains with execution journal for crash recovery |
| `Plan` | Goal decomposition with dependency DAG and re-planning |

### Layer 2: Swarm Engine (`agent_sdk_swarm`)

| Module | Role |
|--------|------|
| `Swarm_types` | agent_role, orchestration_mode, convergence_config, agent_entry |
| `Runner` | 3-mode swarm execution (Decentralized / Supervisor / Pipeline), convergence loop |
| `Swarm_checkpoint` | Swarm state persistence for resume |
| `Test_helpers` | Mock agent builders for zero-LLM testing |

## Module stability tiers

Not all modules are equally stable. Use this to gauge risk when depending on a module.
Every `.mli` now carries an explicit `@stability` tag and `@since` marker.
For the full 186-file classification, see `docs/api-stability.md`.

**Stable** -- safe to depend on, breaking changes only on minor version bumps:

`Types`, `Error`, `Agent`, `Builder`, `Tool`, `Tool_set`, `Hooks`, `Provider`, `Guardrails`, `Raw_trace`, `Checkpoint`, `Checkpoint_store`, `Context`, `Context_reducer`

**Evolving** -- API may change between minor versions:

`Streaming`, `Structured`, `Orchestrator`, `Collaboration`, `Memory`, `Policy`, `Proof_store`, `Cdal_proof`, `Swarm_types`, `Runner`

**Internal** -- implementation details with no compatibility promise:

`lib/agent/*` submodules, `lib/protocol/*`, `lib/llm_provider/*` backends, parser/transport helpers, and other implementation-specific support modules

## Tool definition

```ocaml
(* Simple handler *)
let calc_tool =
  Tool.create ~name:"calculator"
    ~description:"Evaluate a math expression"
    ~parameters:[{
      Types.name = "expression";
      description = "The math expression";
      param_type = String; required = true;
    }]
    (fun args ->
      let open Yojson.Safe.Util in
      match args |> member "expression" |> to_string_option with
      | Some expr -> Ok (Printf.sprintf "Result: %s" expr)
      | None -> Error "missing expression")

(* Context-aware handler *)
let counter_tool =
  Tool.create_with_context ~name:"counter"
    ~description:"Increment and return counter"
    ~parameters:[]
    (fun ctx _input ->
      let n = match Context.get ctx "count" with
        | Some (`Int n) -> n + 1 | _ -> 1 in
      Context.set ctx "count" (`Int n);
      Ok (string_of_int n))
```

## Swarm execution

```ocaml
open Agent_sdk_swarm

(* closure-based agents — no real LLM needed for testing *)
let reviewer = Swarm_types.{
  name = "reviewer";
  run = (fun ~sw:_ prompt -> Ok { (* mock response *) });
  role = Evaluate;
}

let config = Swarm_types.{
  entries = [reviewer; writer; verifier];
  mode = Supervisor;
  convergence = { metric_fn = my_metric; threshold = 0.9;
                  max_iterations = 5; patience = 2 };
  callbacks = Swarm_types.default_callbacks;
}

let result = Runner.run ~sw config "Review this PR"
(* result.converged, result.iterations, result.final_metric *)
```

## Hooks and guardrails

```ocaml
let my_hooks = { Hooks.empty with
  pre_tool_use = Some (function
    | Hooks.PreToolUse { tool_name; _ } ->
        Printf.printf "Calling: %s\n" tool_name;
        Hooks.Continue
    | _ -> Hooks.Continue);
}

let guardrails = {
  Guardrails.tool_filter = Guardrails.AllowList ["calculator"];
  max_tool_calls_per_turn = Some 5;
}
```

## Build and test

```bash
dune build @all
dune runtest

# Coverage report
make coverage

# Harness datasets
oas eval record-trace --session /path/to/raw-trace.ndjson --out evals/replay.jsonl
oas eval run --dataset examples/evals/replay.jsonl --out _build/evals
oas eval run --config oas.json --dataset examples/evals/mixed.jsonl --out _build/evals
oas eval save-baseline --dataset examples/evals/replay.jsonl --out evals/replay-baseline.json
oas eval run --dataset examples/evals/replay.jsonl --baseline evals/replay-baseline.json --out _build/evals
oas eval run --config oas.json --dataset evals/replay.jsonl --out _build/evals

# `trace_replay` cases run offline, `fixture` cases use --config live in the same dataset
# Baseline compare/save currently target one evaluated case; use `--case <id>` when a dataset produces multiple runs

# Integration tests (requires local LLM server)
LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
LLAMA_LIVE_TEST=1 dune exec ./test/test_streaming_e2e.exe

# Run examples
dune exec examples/basic_agent.exe
dune exec examples/review_agent.exe -- jeong-sik/oas 123
```

## Constraints and trade-offs

- HTTP response body limit: 10MB.
- SSE streaming uses real-time event callbacks. Retry wraps outside the stream.
- tool_use loop returns the last response and stops when max_turns is exceeded.
- Runs on a single Eio domain. No multi-core parallelism.
- Prompt caching tracks `cache_creation_input_tokens` and `cache_read_input_tokens` in both streaming and non-streaming modes (since v0.4.0).
- Swarm convergence loop is cooperative — if an agent blocks indefinitely, the loop stalls.

## Scope limitations

OAS is a single-process agent runtime and swarm engine. The following concerns are explicitly out of scope.

| What | Owner | Why not here |
|------|-------|-------------|
| Worktree / filesystem coordination | `masc-mcp` | Multi-process file locking and git worktree lifecycle are coordinator concerns, not SDK concerns. |
| MCP transport implementation | `mcp-protocol-sdk` | OAS consumes `mcp_protocol` as an opam dependency. Transport details (NDJSON framing, stdio management) live there. |
| Operator dashboard / visibility | `masc-mcp` | Real-time agent status, room views, and keeper dashboards belong to the coordination layer that aggregates across processes. |
| Repo-level task and room management | `masc-mcp` | Task queues, claim semantics, and room state are coordination-plane concepts that span multiple agents and sessions. |
| Workflow scheduling / SaaS isolation | Application layer | Cron triggers, tenant isolation, and multi-user access control are problems for the system that embeds OAS. |
| Long-term persistence / vector storage | Application layer | Session state, memory backends, and embedding indexes are injected via callbacks, not owned by the SDK. |

If you find yourself pulling one of these responsibilities into `agent_sdk` or `agent_sdk_swarm`, that is a sign the change belongs in a different repository.

## Versioning

0.77.0

We follow semver intent within the 0.x series:
- **0.x.0**: May contain breaking changes with migration guide in CHANGELOG.
- **0.x.y** (y > 0): Additive features and bug fixes only.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and code style.
