# `agent_sdk`

OCaml 5.x + Eio 기반 agent runtime kernel.

- repo: `https://github.com/jeong-sik/oas`
- local workspace path in this environment: `~/me/workspace/yousleepwhen/oas`
- opam package: `agent_sdk`
- OCaml module: `Agent_sdk`

`masc-mcp` 같은 orchestration layer가 재사용할 수 있도록, single-agent loop 위에 `hooks`, `permission-aware guardrails`, `sessions`, `skills`, `subagents`를 얹는 방향으로 설계되어 있다.

## Module Map

| Module | Role |
| --- | --- |
| `Types` | message/content/tool schema/streaming events |
| `Provider` | Anthropic / local / OpenAI-compatible endpoint resolution |
| `Api` | `create_message`, `create_message_stream`, SSE parsing |
| `Agent` | multi-turn tool loop, permission checks, handoffs, subagents |
| `Tool` | tool definition, kind metadata, JSON schema export |
| `Hooks` | lifecycle / permission / subagent hook events |
| `Guardrails` | tool visibility, permission mode, output limits |
| `Context` | shared mutable key-value context |
| `Session` | resumable session metadata and lifecycle state |
| `Skill` | markdown + frontmatter parser for reusable prompt skills |
| `Subagent` | typed subagent spec and handoff target conversion |
| `Handoff` | delegated sub-agent tool contract |
| `Retry` | HTTP/network retry classification |

## Feature Matrix

| Feature | Status | Notes |
| --- | --- | --- |
| Multi-turn tool loop | Stable | `Agent.run` |
| SSE streaming | Stable | `Api.create_message_stream` + parsed SSE events |
| Hooks | Stable | turn/tool/session/permission/subagent events |
| Permission-aware guardrails | Stable | `permission_mode`, allow/ask/deny lists, output caps |
| Shared context | Stable | `Context.t` |
| Handoff tools | Stable | `Agent.run_with_handoffs` |
| Sessions | Stable | `Session.t`, hook-visible lifecycle |
| Skill markdown loader | Experimental | lightweight frontmatter subset |
| Typed subagent specs | Experimental | `Subagent.t` -> `Handoff.handoff_target` |
| Prompt caching | Planned | not implemented yet |
| Multimodal content blocks | Planned | not implemented yet |
| MCP resources / prompts | Planned | integration surface not added yet |

## Quick Start

```ocaml
open Agent_sdk

let echo_tool =
  Tool.create
    ~name:"echo"
    ~description:"Echo back the input"
    ~parameters:[
      {
        Types.name = "message";
        description = "Text to echo";
        param_type = Types.String;
        required = true;
      };
    ]
    (fun input ->
      let message = Yojson.Safe.Util.(input |> member "message" |> to_string) in
      Ok message)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let provider =
      {
        Provider.provider = Provider.Local { base_url = "http://127.0.0.1:8085" };
        model_id = "qwen3.5-35b";
        api_key_env = "LOCAL_LLM_KEY";
      }
    in
    let agent =
      Agent.create
        ~net:env#net
        ~provider
        ~config:{
          Types.default_config with
          name = "echo-agent";
          system_prompt = Some "You are a concise assistant.";
          max_turns = 5;
        }
        ~tools:[echo_tool]
        ()
    in
    match Agent.run ~sw agent "Say hello" with
    | Ok response ->
        response.Types.content
        |> List.iter (function
             | Types.Text text -> print_endline text
             | _ -> ())
    | Error err ->
        prerr_endline err
```

## Tool Kinds and Guardrails

`Tool.create` and `Tool.create_with_context` support `?kind`.

```ocaml
let edit_tool =
  Tool.create
    ~kind:Types.File_edit
    ~name:"write_file"
    ~description:"Write a file"
    ~parameters:[]
    (fun _ -> Ok "done")
```

`Guardrails.permission_mode` drives runtime behavior:

- `Default`: read-only tools auto-allow, others require approval
- `Accept_edits`: read-only and file-edit tools auto-allow
- `Dont_ask`: tools that need approval are rejected
- `Bypass_permissions`: everything allows unless explicitly denied
- `Plan`: read-only tools only

Approval-requiring tools emit `Hooks.PermissionRequest`.

## Skills and Subagents

`Skill` parses markdown files with a lightweight frontmatter subset:

```md
---
name: code-review
description: Review code changes
allowed-tools: Read, Grep
argument-hint: revision range
disable-model-invocation: true
---
Review the diff in $ARGUMENTS.
```

`Subagent` parses Claude-style subagent metadata and can be converted into a handoff target:

```ocaml
let reviewer_skill = Skill.load ".claude/skills/review.md"

let reviewer =
  Subagent.of_markdown
    ~skills:[reviewer_skill]
    {|
---
name: reviewer
tools: grep, read
disallowed-tools: edit
permission-mode: accept-edits
max-turns: 3
---
Review the implementation and summarize issues.
|}
```

Run with typed subagents:

```ocaml
let result = Agent.run_with_subagents ~sw agent ~specs:[reviewer] "delegate"
```

## Build and Test

```bash
dune build
dune runtest
```

Selected executables:

```bash
dune exec ./test/test_local_llm.exe
dune exec ./test/test_streaming_e2e.exe
```

## Constraints

- HTTP response body hard limit: 10MB
- streaming retry is not automatic; callers should wrap `create_message_stream` externally
- frontmatter parsing is intentionally lightweight, not full YAML
- skill/subagent filesystem loading is local-only and synchronous

## Version

`0.4.0`
