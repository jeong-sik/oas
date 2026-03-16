# Custom Provider Registration

OAS v0.26.0 introduces a runtime provider registry, allowing third-party
LLM endpoints (vLLM, TGI, custom inference servers) to be integrated
without modifying the SDK source.

## Registration

```ocaml
open Agent_sdk

let () =
  Provider.register_provider {
    name = "vllm-local";
    request_kind = Provider.Openai_chat_completions;
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
```

## Lookup

```ocaml
match Provider.find_provider "vllm-local" with
| Some impl -> (* use impl.build_body, impl.parse_response *)
| None -> (* fall back to built-in providers *)
```

## Listing registered providers

```ocaml
let names = Provider.registered_providers ()
(* ["vllm-local"; ...] *)
```

## Thread safety

The registry is protected by `Eio.Mutex`. Concurrent registration and
lookup from multiple fibers is safe.

## Limitations

- The registry is global mutable state. Register providers before
  spawning agent fibers to avoid ordering issues.
- Built-in providers (Anthropic, OpenAI, Ollama, Local) are not in
  the registry. They are dispatched via the `Provider.config` variant.
  The registry is for extending beyond the built-in set.
