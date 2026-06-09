(** Request-serialization snapshot ratchet (WP9).

    Pins the CURRENT request-body wire output of each provider backend's
    request builder against an inline expected string. The pinned strings
    record present behavior — they are a regression fence, not an assertion
    that the current shape is "correct". When a serializer changes on
    purpose, regenerate the expected string in the same PR and review the
    diff.

    Determinism note: every field in the fixture is supplied explicitly so
    the wire string is a pure function of the fixture, not of the host
    environment. In particular:
    - [model_id] ("oas-snapshot-fixture-model") matches no static capability
      prefix and is not expected in any capability manifest, so capability
      lookup falls through to per-kind defaults.
    - [supports_tool_choice_override:true] pins whether the OpenAI/GLM
      [tool_choice] field is emitted, independent of any manifest entry.
    - [keep_alive:"-1"] pins the Ollama [keep_alive] field, independent of
      the [OAS_OLLAMA_KEEP_ALIVE] env var.
    - No model in this fixture reports [supports_seed], so no [seed] field is
      injected and [OAS_DEFAULT_SEED] does not affect the output. *)

open Alcotest
open Llm_provider
open Types

(* ── Shared fixture ─────────────────────────────────── *)

let msg role content : message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

(* One tool declaration in the SDK's input_schema shape. *)
let tool_decl =
  `Assoc
    [ "name", `String "get_weather"
    ; "description", `String "Get weather for a city"
    ; ( "input_schema"
      , `Assoc
          [ "type", `String "object"
          ; "properties", `Assoc [ "city", `Assoc [ "type", `String "string" ] ]
          ; "required", `List [ `String "city" ]
          ] )
    ]
;;

(* A representative conversation exercising a ToolUse turn followed by a
   ToolResult, so the snapshot covers assistant tool-call serialization and
   user/tool-result serialization (block ordering included). *)
let messages =
  [ msg User [ Text "What's the weather in Seoul?" ]
  ; msg
      Assistant
      [ ToolUse
          { id = "call_1"
          ; name = "get_weather"
          ; input = `Assoc [ "city", `String "Seoul" ]
          }
      ]
  ; msg
      User
      [ ToolResult
          { tool_use_id = "call_1"
          ; content = "Sunny, 25C"
          ; is_error = false
          ; json = None
          ; content_blocks = None
          }
      ]
  ]
;;

let cfg ~kind ~base_url ~tool_choice =
  Provider_config.make
    ~kind
    ~model_id:"oas-snapshot-fixture-model"
    ~base_url
    ~api_key:"test-key"
    ~max_tokens:1024
    ~temperature:0.7
    ~tool_choice
    ~disable_parallel_tool_use:true
    ~supports_tool_choice_override:true
    ~keep_alive:"-1"
    ()
;;

let openai_cfg = cfg ~kind:OpenAI_compat ~base_url:"https://api.openai.com/v1"
let anthropic_cfg = cfg ~kind:Anthropic ~base_url:"https://api.anthropic.com"

let gemini_cfg =
  cfg ~kind:Gemini ~base_url:"https://generativelanguage.googleapis.com/v1beta"
;;

let glm_cfg = cfg ~kind:Glm ~base_url:"https://open.bigmodel.cn/api/paas/v4"
let ollama_cfg = cfg ~kind:Ollama ~base_url:"http://127.0.0.1:11434"

(* RFC-OAS-023 fixture. Unlike the shared [cfg] above, this one uses the REAL
   fleet model id ([deepseek-v4-flash], the live default in masc
   [runtime.toml]) and deliberately OMITS [supports_tool_choice_override], so
   the OpenAI-compat request builder runs the real capability lookup
   ([Capabilities.for_model_id]) instead of the override. The Ollama Cloud
   provider speaks the OpenAI-compat Chat Completions wire
   (openai-http / https://ollama.com/v1), so [OpenAI_compat] is the
   matching kind for the fleet wire. *)
let deepseek_cfg ~tool_choice =
  Provider_config.make
    ~kind:OpenAI_compat
    ~model_id:"deepseek-v4-flash"
    ~base_url:"https://ollama.com/v1"
    ~api_key:"test-key"
    ~max_tokens:1024
    ~temperature:0.7
    ~tool_choice
    ~disable_parallel_tool_use:true
    ()
;;

(* ── Helpers ────────────────────────────────────────── *)

let snapshot label expected actual = check string label expected actual

(* Substring containment, used for legible per-field assertions in addition
   to the full-string ratchet. *)
let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* ── OpenAI-compatible ──────────────────────────────── *)

let openai_forced_expected =
  {|{"parallel_tool_calls":false,"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}}],"tool_choice":{"type":"function","function":{"name":"get_weather"}},"temperature":0.7,"model":"oas-snapshot-fixture-model","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}],"role":"assistant","content":null},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}],"max_tokens":1024}|}
;;

let openai_required_expected =
  {|{"parallel_tool_calls":false,"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}}],"tool_choice":"required","temperature":0.7,"model":"oas-snapshot-fixture-model","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}],"role":"assistant","content":null},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}],"max_tokens":1024}|}
;;

(* None_ omits both [tools] and [tool_choice] entirely (it does not emit
   "none"). Pinned as current behavior, not a serializer fix. *)
let openai_none_expected =
  {|{"temperature":0.7,"model":"oas-snapshot-fixture-model","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}],"role":"assistant","content":null},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}],"max_tokens":1024}|}
;;

let test_openai_forced () =
  let body =
    Backend_openai_request.build_request
      ~config:(openai_cfg ~tool_choice:(Tool "get_weather"))
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "openai tool_choice=forced(Tool) snapshot" openai_forced_expected body;
  check
    bool
    "parallel_tool_calls:false present when disabled"
    true
    (contains ~needle:{|"parallel_tool_calls":false|} body);
  check bool "role:tool present" true (contains ~needle:{|"role":"tool"|} body);
  check
    bool
    "forced tool_choice names the function"
    true
    (contains
       ~needle:{|"tool_choice":{"type":"function","function":{"name":"get_weather"}}|}
       body)
;;

let test_openai_required () =
  let body =
    Backend_openai_request.build_request
      ~config:(openai_cfg ~tool_choice:Any)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "openai tool_choice=required(Any) snapshot" openai_required_expected body;
  check
    bool
    {|Any maps to tool_choice:"required"|}
    true
    (contains ~needle:{|"tool_choice":"required"|} body)
;;

let test_openai_none () =
  let body =
    Backend_openai_request.build_request
      ~config:(openai_cfg ~tool_choice:None_)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "openai tool_choice=none(None_) snapshot" openai_none_expected body;
  check
    bool
    "None_ omits tool_choice field"
    false
    (contains ~needle:{|"tool_choice"|} body);
  check bool "None_ omits tools field" false (contains ~needle:{|"tools"|} body)
;;

(* ── DeepSeek via OpenAI-compat (RFC-OAS-023 routing fence) ───
   This fixture pins the CURRENT live-fleet wire for [deepseek-v4-flash]
   through the OpenAI-compat backend. It exercises the REAL capability
   lookup (no [supports_tool_choice_override]), so it runs through the
   prefix dispatcher fixed in RFC-OAS-023.

   It is a regression FENCE, not a discrimination proof: with
   [enable_thinking=None] and [max_tokens=1024], none of the capability
   fields the DeepSeek route changes ([thinking_control_format],
   [max_output_tokens]) alter the OpenAI chat-completions request body,
   so the wire is byte-identical before/after the de-anonymization. The
   actual proof that [deepseek-v4-flash] resolves to [Deepseek_v4_flash]
   lives in the inline [let%test "for_model_id_static: specific model IDs
   get correct (not shadowed) capabilities"] in [capabilities.ml] and in
   [test_capabilities.ml]'s cloud-suffix route checks. *)
let deepseek_required_expected =
  {|{"parallel_tool_calls":false,"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}}],"tool_choice":"required","reasoning_effort":"none","temperature":0.7,"model":"deepseek-v4-flash","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}],"role":"assistant","content":null},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}],"max_tokens":1024}|}
;;

let test_deepseek_required () =
  let body =
    Backend_openai_request.build_request
      ~config:(deepseek_cfg ~tool_choice:Any)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot
    "deepseek-v4-flash openai-compat tool_choice=required(Any) snapshot"
    deepseek_required_expected
    body;
  check
    bool
    "tool_choice emitted (capability lookup resolves supports_tool_choice)"
    true
    (contains ~needle:{|"tool_choice":"required"|} body);
  check
    bool
    "model id is the real fleet id"
    true
    (contains ~needle:{|"model":"deepseek-v4-flash"|} body)
;;

(* ── Anthropic ──────────────────────────────────────── *)

let anthropic_forced_expected =
  {|{"tool_choice":{"disable_parallel_tool_use":true,"type":"tool","name":"get_weather"},"tools":[{"name":"get_weather","description":"Get weather for a city","input_schema":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}],"temperature":0.7,"model":"oas-snapshot-fixture-model","max_tokens":1024,"messages":[{"role":"user","content":[{"type":"text","text":"What's the weather in Seoul?"}]},{"role":"assistant","content":[{"type":"tool_use","id":"call_1","name":"get_weather","input":{"city":"Seoul"}}]},{"role":"user","content":[{"type":"tool_result","tool_use_id":"call_1","content":"Sunny, 25C","is_error":false}]}],"stream":false}|}
;;

let test_anthropic_forced () =
  let body =
    Backend_anthropic.build_request
      ~config:(anthropic_cfg ~tool_choice:(Tool "get_weather"))
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "anthropic tool_choice=forced(Tool) snapshot" anthropic_forced_expected body;
  check
    bool
    "disable_parallel_tool_use nested inside tool_choice"
    true
    (contains
       ~needle:{|"tool_choice":{"disable_parallel_tool_use":true,"type":"tool"|}
       body);
  check bool "tool_use block present" true (contains ~needle:{|"type":"tool_use"|} body);
  check
    bool
    "tool_result block present"
    true
    (contains ~needle:{|"type":"tool_result"|} body);
  (* Block ordering: text/tool_use/tool_result appear in message order. *)
  check
    bool
    "no top-level disable_parallel field"
    false
    (contains ~needle:{|,"disable_parallel_tool_use":true}|} body
     && not (contains ~needle:{|"type":"tool"|} body))
;;

(* ── Gemini ─────────────────────────────────────────── *)

let gemini_any_expected =
  {|{"toolConfig":{"functionCallingConfig":{"mode":"ANY"}},"tools":[{"functionDeclarations":[{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}]}],"generationConfig":{"temperature":0.7,"maxOutputTokens":1024},"contents":[{"role":"user","parts":[{"text":"What's the weather in Seoul?"}]},{"role":"model","parts":[{"functionCall":{"name":"get_weather","args":{"city":"Seoul"}}}]},{"role":"user","parts":[{"functionResponse":{"name":"get_weather","response":{"result":"Sunny, 25C"}}}]}]}|}
;;

let test_gemini_any () =
  let body =
    Backend_gemini.build_request
      ~config:(gemini_cfg ~tool_choice:Any)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "gemini tool_choice=Any snapshot" gemini_any_expected body;
  check
    bool
    "functionDeclarations present"
    true
    (contains ~needle:{|"functionDeclarations"|} body);
  check
    bool
    "functionCallingConfig present"
    true
    (contains ~needle:{|"functionCallingConfig":{"mode":"ANY"}|} body);
  (* #1840: Gemini has no parallel-disable; the field must be absent even
     though disable_parallel_tool_use=true. *)
  check
    bool
    "no parallel_tool_calls on the wire (#1840)"
    false
    (contains ~needle:"parallel_tool_calls" body);
  check
    bool
    "no disable_parallel on the wire (#1840)"
    false
    (contains ~needle:"disable_parallel" body)
;;

(* ── GLM (OpenAI-compatible wire format) ────────────── *)

let glm_any_expected =
  {|{"parallel_tool_calls":false,"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}}],"tool_choice":"required","temperature":0.7,"model":"oas-snapshot-fixture-model","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}],"role":"assistant","content":""},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}],"max_tokens":1024}|}
;;

let test_glm_any () =
  let body =
    Backend_glm.build_request
      ~config:(glm_cfg ~tool_choice:Any)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "glm tool_choice=Any snapshot" glm_any_expected body;
  check
    bool
    "parallel_tool_calls:false present when disabled"
    true
    (contains ~needle:{|"parallel_tool_calls":false|} body);
  check
    bool
    {|Any maps to tool_choice:"required"|}
    true
    (contains ~needle:{|"tool_choice":"required"|} body)
;;

(* ── Ollama (native /api/chat shape) ────────────────── *)

let ollama_any_expected =
  {|{"options":{"temperature":0.7,"num_predict":1024},"tools":[{"type":"function","function":{"name":"get_weather","description":"Get weather for a city","parameters":{"type":"object","properties":{"city":{"type":"string"}},"required":["city"]}}}],"keep_alive":-1,"stream":false,"think":false,"model":"oas-snapshot-fixture-model","messages":[{"role":"user","content":"What's the weather in Seoul?"},{"tool_calls":[{"id":"call_1","type":"function","function":{"name":"get_weather","arguments":{"city":"Seoul"}}}],"role":"assistant","content":null},{"role":"tool","tool_call_id":"call_1","content":"Sunny, 25C"}]}|}
;;

let test_ollama_any () =
  let body =
    Backend_ollama.build_request
      ~config:(ollama_cfg ~tool_choice:Any)
      ~messages
      ~tools:[ tool_decl ]
      ()
  in
  snapshot "ollama tool_choice=Any snapshot" ollama_any_expected body;
  check
    bool
    "ollama options carries temperature + num_predict"
    true
    (contains ~needle:{|"options":{"temperature":0.7,"num_predict":1024}|} body);
  check bool "keep_alive pinned to -1" true (contains ~needle:{|"keep_alive":-1|} body);
  (* Ollama native /api/chat takes no tool_choice / parallel_tool_calls. *)
  check bool "no tool_choice field" false (contains ~needle:{|"tool_choice"|} body)
;;

(* ── Suite ──────────────────────────────────────────── *)

let () =
  run
    "snapshot_provider_serialization"
    [ ( "openai"
      , [ test_case "tool_choice forced(Tool)" `Quick test_openai_forced
        ; test_case "tool_choice required(Any)" `Quick test_openai_required
        ; test_case "tool_choice none(None_)" `Quick test_openai_none
        ; test_case "deepseek-v4-flash required(Any)" `Quick test_deepseek_required
        ] )
    ; "anthropic", [ test_case "tool_choice forced(Tool)" `Quick test_anthropic_forced ]
    ; "gemini", [ test_case "tool_choice Any" `Quick test_gemini_any ]
    ; "glm", [ test_case "tool_choice Any" `Quick test_glm_any ]
    ; "ollama", [ test_case "tool_choice Any" `Quick test_ollama_any ]
    ]
;;
