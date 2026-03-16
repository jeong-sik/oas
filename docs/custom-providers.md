# Custom Provider Registration

OAS supports runtime provider registration for third-party LLM endpoints
(vLLM, TGI, custom inference servers) without modifying the SDK source.

## Quick Start (v0.27.0+)

```ocaml
open Agent_sdk

(* 1. Register the provider implementation *)
let () =
  Provider.register_provider {
    name = "vllm-local";
    request_kind = Provider.Openai_chat_completions;
    request_path = "/v1/chat/completions";
    capabilities = {
      Provider.default_capabilities with
      supports_tools = true;
      supports_tool_choice = true;
    };
    build_body = (fun ~config ~messages ?tools () ->
      Api.build_openai_body
        ~provider_config:{
          provider = OpenAICompat {
            base_url = "http://localhost:8000";
            auth_header = None;
            path = "/v1/chat/completions";
            static_token = None;
          };
          model_id = "my-model";
          api_key_env = "DUMMY";
        }
        ~config ~messages ?tools ());
    parse_response = Api.parse_openai_response;
    resolve = (fun _cfg ->
      Ok ("http://localhost:8000", "dummy",
          [("Content-Type", "application/json")]));
  }

(* 2. Create a config using the convenience helper *)
let provider_cfg = Provider.custom_provider
  ~name:"vllm-local"
  ~model_id:"my-finetuned-model"
  ()

(* 3. Use it with an Agent — works end-to-end *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let agent = Agent.create ~net
    ~options:{ Agent.default_options with
      provider = Some provider_cfg;
    }
    () in
  match Agent.run ~sw agent "Hello from custom provider" with
  | Ok response ->
    List.iter (function
      | Types.Text s -> print_endline s
      | _ -> ()
    ) response.content
  | Error e ->
    Printf.eprintf "Error: %s\n" (Error.to_string e)
```

## How It Works

1. `Provider.register_provider` stores the implementation in a global registry
2. `Provider.custom_provider ~name:"vllm-local" ()` creates a config with
   `Custom_registered { name }` variant
3. When `api.ml` dispatches, it looks up the registry by name and calls
   `impl.build_body`/`impl.parse_response`
4. Streaming falls back to sync + synthetic SSE events (same as Ollama)

## Registration API

```ocaml
(* Register *)
Provider.register_provider impl

(* Lookup *)
Provider.find_provider "vllm-local"  (* -> provider_impl option *)

(* List all *)
Provider.registered_providers ()  (* -> string list *)

(* Convenience config constructor *)
Provider.custom_provider ~name:"vllm-local" ~model_id:"my-model" ()
```

## provider_impl Record

| Field | Type | Purpose |
|-------|------|---------|
| `name` | `string` | Unique registry key |
| `request_kind` | `request_kind` | Determines body format |
| `request_path` | `string` | HTTP path (e.g. `/v1/chat/completions`) |
| `capabilities` | `capabilities` | Feature flags for the provider |
| `build_body` | `agent_state -> messages -> ?tools -> unit -> string` | Constructs request JSON |
| `parse_response` | `string -> api_response` | Parses HTTP response body |
| `resolve` | `config -> (url * key * headers) result` | Resolves connection details |

## Thread Safety

The registry is protected by `Eio.Mutex`. Concurrent registration and
lookup from multiple fibers is safe.

## Limitations

- The registry is global mutable state. Register providers before
  spawning agent fibers to avoid ordering issues.
- Built-in providers (Anthropic, OpenAI, Ollama, Local) are not in
  the registry. They use the `Provider.config` variant directly.
- Custom providers do not support native SSE streaming. They fall back
  to sync API calls with synthetic SSE events emitted afterward.
