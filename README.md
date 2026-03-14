# agent-sdk

[![CI](https://github.com/jeong-sik/oas/actions/workflows/ci.yml/badge.svg)](https://github.com/jeong-sik/oas/actions/workflows/ci.yml)

A native OCaml implementation of the Anthropic Agent SDK using OCaml 5.x + Eio for structured concurrency. Supports Anthropic Messages API, OpenAI-compatible endpoints, and Ollama.

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
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git#main --no-action --yes
opam pin add mcp_protocol_eio git+https://github.com/jeong-sik/mcp-protocol-sdk.git#main --no-action --yes

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

See `examples/` for more: `basic_agent.ml`, `tool_use.ml`, `streaming.ml`, `proof_demo.ml`.

## Provider support

| Provider | Constructor | Endpoint |
|----------|-----------|----------|
| Local (llama-server) | `Provider.Local { base_url }` | `http://127.0.0.1:8085` |
| Anthropic | `Provider.Anthropic` | `https://api.anthropic.com` |
| OpenAI-compatible | `Provider.OpenAICompat { base_url; ... }` | Any `/chat/completions` |
| Ollama | `Provider.Ollama { base_url; mode }` | `http://127.0.0.1:11434` |
| OpenRouter | `Provider.openrouter ()` | `https://openrouter.ai/api/v1` |

`Provider.resolve` returns `(base_url * api_key * headers, sdk_error) result`. Missing env vars produce `Error`, not silent fallback.

## Architecture

```
Types -> Provider -> Api -> Agent -> Tool
                      |
                   Streaming (SSE)
                      |
              Hooks / Guardrails / Context
```

| Module | Role |
|--------|------|
| `Types` | Domain types: message, role, stop_reason, content_block, SSE events |
| `Provider` | LLM endpoint abstraction (Local / Anthropic / OpenAICompat / Ollama) |
| `Api` | HTTP client: `create_message` (sync) + `create_message_stream` (SSE) |
| `Agent` | Multi-turn agent loop with automatic tool_use handling |
| `Tool` | Tool definition, JSON Schema generation, execution (Simple / WithContext) |
| `Builder` | Fluent API for agent construction |
| `Hooks` | Lifecycle hooks: BeforeTurn, AfterTurn, PreToolUse, PostToolUse, OnStop |
| `Context` | Cross-turn shared state (key-value store, `Yojson.Safe.t` values) |
| `Guardrails` | Tool filtering (AllowList/DenyList/Custom) + per-turn call limits |
| `Error` | 2-level structured errors: 7 domain variants + Internal |
| `Retry` | Structural error classification + exponential backoff with jitter |
| `Streaming` | SSE event parsing and usage tracking |
| `Structured` | Type-safe structured output extraction (tool_use + tool_choice) |
| `Session` | Session lifecycle, metadata, turn counting, resume |
| `Checkpoint` | Agent state snapshots (messages, usage, config preservation) |
| `Handoff` | Sub-agent delegation (`transfer_to_*` tool pattern) |
| `Subagent` | Isolated-context child agent spawning |
| `Orchestrator` | Multi-agent task orchestration |
| `Skill` | Reusable agent capability bundles (markdown parsing) |
| `Context_reducer` | Message windowing (keep_last, token_budget, custom) |
| `Mcp` | MCP client (mcp-protocol-sdk wrapper, server lifecycle) |
| `Contract` | Runtime contracts: instruction layers, triggers, tool grants |
| `Otel_tracer` | OpenTelemetry-compatible tracer (OTLP JSON export) |
| `Conformance` | Harness conformance checking and reporting |
| `Direct_evidence` | Direct-agent proof bundle materialization |
| `Runtime` | Session/event/command/report/proof protocol types |
| `Client` | Persistent runtime client (connect, query, finalize) |

## Module stability tiers

Not all modules are equally stable. Use this to gauge risk when depending on a module.

**Stable** -- safe to depend on, breaking changes only on minor version bumps:

`Types`, `Error`, `Provider`, `Api`, `Agent`, `Tool`, `Hooks`, `Guardrails`, `Context`, `Builder`

**Evolving** -- API may change between minor versions:

`Streaming`, `Structured`, `Orchestrator`, `Checkpoint`, `Mcp`, `Session`, `Skill`, `Subagent`, `Handoff`, `Contract`, `Context_reducer`, `Event_bus`

**Experimental** -- may be redesigned or removed:

`Runtime`, `Transport`, `Client`, `Sessions`, `Conformance`, `Direct_evidence`, `Raw_trace`, `Otel_tracer`, `Checkpoint_store`

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

# Integration tests (requires local LLM server)
LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
LLAMA_LIVE_TEST=1 dune exec ./test/test_streaming_e2e.exe

# Run examples
dune exec examples/basic_agent.exe
dune exec examples/tool_use.exe
dune exec examples/streaming.exe
```

## Constraints and trade-offs

- HTTP response body limit: 10MB.
- SSE streaming uses real-time event callbacks. Retry wraps outside the stream.
- tool_use loop returns the last response and stops when max_turns is exceeded.
- Runs on a single Eio domain. No multi-core parallelism.
- Prompt caching tracks `cache_creation_input_tokens` and `cache_read_input_tokens` in both streaming and non-streaming modes (since v0.4.0).

## Versioning

0.22.0

We follow semver intent within the 0.x series:
- **0.x.0**: May contain breaking changes with migration guide in CHANGELOG.
- **0.x.y** (y > 0): Additive features and bug fixes only.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and code style.
