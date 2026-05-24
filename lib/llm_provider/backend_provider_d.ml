(** Provider_d-compatible API response parsing, message serialization,
    and request building.

    Pure functions operating on {!Llm_provider.Types}.
    {!build_request} uses {!Provider_config.t} (no agent_sdk coupling).

    @since 0.92.0 decomposed into Backend_provider_d_serialize,
    Backend_provider_d_parse *)

open Types

(* ── Re-exports from serialization ─────────────────────── *)

let tool_calls_to_provider_d_json =
  Backend_provider_d_serialize.tool_calls_to_provider_d_json
;;

let provider_d_content_parts_of_blocks =
  Backend_provider_d_serialize.provider_d_content_parts_of_blocks
;;

let provider_d_messages_of_message =
  Backend_provider_d_serialize.provider_d_messages_of_message
;;

let provider_k_messages_of_message =
  Backend_provider_d_serialize.provider_k_messages_of_message
;;

let tool_choice_to_provider_d_json =
  Backend_provider_d_serialize.tool_choice_to_provider_d_json
;;

let build_provider_d_tool_json = Backend_provider_d_serialize.build_provider_d_tool_json
let strip_orphaned_tool_results = Backend_provider_d_serialize.strip_orphaned_tool_results
let strip_thinking_blocks = Backend_provider_d_serialize.strip_thinking_blocks

(* ── Re-exports from parsing ──────────────────────────── *)

let strip_json_markdown_fences = Backend_provider_d_parse.strip_json_markdown_fences
let usage_of_provider_d_json = Backend_provider_d_parse.usage_of_provider_d_json

let parse_provider_d_response_result =
  Backend_provider_d_parse.parse_provider_d_response_result
;;

(* ── Re-exports from request building ─────────────────── *)

let warn_capability_drop = Backend_provider_d_request.warn_capability_drop
let effective_tool_choice = Backend_provider_d_request.effective_tool_choice
let effective_tools = Backend_provider_d_request.effective_tools
let structured_schema_of_config = Backend_provider_d_request.structured_schema_of_config

let provider_d_json_schema_payload =
  Backend_provider_d_request.provider_d_json_schema_payload
;;

let response_format_to_provider_d_json =
  Backend_provider_d_request.response_format_to_provider_d_json
;;

let response_format_of_config = Backend_provider_d_request.response_format_of_config
let build_request = Backend_provider_d_request.build_request

[@@@coverage off]
(* === Inline tests === *)

let%test "tool_choice_to_provider_d_json Auto" =
  tool_choice_to_provider_d_json Auto = `String "auto"
;;

let%test "tool_choice_to_provider_d_json Any" =
  tool_choice_to_provider_d_json Any = `String "required"
;;

let%test "tool_choice_to_provider_d_json None_" =
  tool_choice_to_provider_d_json None_ = `String "none"
;;

let%test "tool_choice_to_provider_d_json Tool name" =
  let result = tool_choice_to_provider_d_json (Tool "my_tool") in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_tool"
;;

let%test "provider_k passes named tool_choice through (no coerce)" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5"
      ~base_url:Zai_catalog.general_base_url
      ~tool_choice:(Tool "calculator")
      ()
  in
  effective_tool_choice cfg
  = Some
      (`Assoc
          [ "type", `String "function"
          ; "function", `Assoc [ "name", `String "calculator" ]
          ])
;;

let%test "provider_k passes tool_choice any through (no coerce)" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5"
      ~base_url:Zai_catalog.general_base_url
      ~tool_choice:Any
      ()
  in
  effective_tool_choice cfg = Some (`String "required")
;;

let%test "provider_k drops tool_choice none" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5"
      ~base_url:Zai_catalog.general_base_url
      ~tool_choice:None_
      ()
  in
  effective_tool_choice cfg = None
;;

let%test "provider_k drops tools when tool_choice none" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5"
      ~base_url:Zai_catalog.general_base_url
      ~tool_choice:None_
      ()
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    build_request ~config:cfg ~messages:[] ~tools:[ tool_json ] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  (not (List.mem_assoc "tool_choice" assoc)) && not (List.mem_assoc "tools" assoc)
;;

(* === Capability-gated sampling param tests (oas#827) === *)

let%test "provider_k drops min_p when model does not support it" =
  (* Provider_k's provider_k_capabilities inherits supports_min_p = false from
     default_capabilities.  Even when the caller sets min_p explicitly
     (via higher-level config inheritance or agent default), backend_provider_d must
     omit it from the wire body — ZAI rejects the request with
     "property 'min_p' is unsupported". *)
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.general_base_url
      ~min_p:0.05
      ()
  in
  let json = build_request ~config:cfg ~messages:[] () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  not (List.mem_assoc "min_p" (to_assoc json))
;;

let%test "provider_k drops top_k when model does not support it" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.general_base_url
      ~top_k:40
      ()
  in
  let json = build_request ~config:cfg ~messages:[] () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  not (List.mem_assoc "top_k" (to_assoc json))
;;

let%test "ollama preserves min_p (llama.cpp supports it)" =
  (* provider_h_3 via Ollama has supports_min_p = true in provider_m_capabilities.
     The capability-gated path must still pass min_p through for
     providers that do support it. *)
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Ollama
      ~model_id:"provider_h-3.5:35b-a3b-nvfp4"
      ~base_url:"http://127.0.0.1:11434"
      ~min_p:0.05
      ()
  in
  let json = build_request ~config:cfg ~messages:[] () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  match json |> member "min_p" with
  | `Float f -> Float.abs (f -. 0.05) < 1e-6
  | _ -> false
;;

let%test "strip_json_markdown_fences plain text unchanged" =
  strip_json_markdown_fences "{\"key\":\"value\"}" = "{\"key\":\"value\"}"
;;

let%test "strip_json_markdown_fences strips json fences" =
  let input = "```json\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"
;;

let%test "strip_json_markdown_fences strips plain fences" =
  let input = "```\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"
;;

let%test "strip_json_markdown_fences short string unchanged" =
  strip_json_markdown_fences "hi" = "hi"
;;

let%test "tool_calls_to_provider_d_json extracts ToolUse blocks" =
  let blocks =
    [ Text "hello"; ToolUse { id = "tc1"; name = "fn1"; input = `Assoc [ "x", `Int 1 ] } ]
  in
  let result = tool_calls_to_provider_d_json blocks in
  List.length result = 1
;;

let%test "tool_calls_to_provider_d_json empty for no tool_use" =
  tool_calls_to_provider_d_json [ Text "no tools" ] = []
;;

let%test "provider_d_content_parts_of_blocks filters text and image" =
  let blocks =
    [ Text "hello"
    ; Thinking { thinking_type = "reasoning"; content = "..." }
    ; ToolUse { id = "tc1"; name = "fn"; input = `Null }
    ]
  in
  let result = provider_d_content_parts_of_blocks blocks in
  List.length result = 1
;;

let%test "build_provider_d_tool_json converts input_schema to parameters" =
  let tool_json =
    `Assoc
      [ "name", `String "my_fn"
      ; "description", `String "does stuff"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let result = build_provider_d_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_fn"
  && result
     |> member "function"
     |> member "parameters"
     |> member "type"
     |> to_string
     = "object"
;;

let%test "build_provider_d_tool_json non-assoc passthrough" =
  build_provider_d_tool_json (`String "bad") = `String "bad"
;;

let%test "usage_of_provider_d_json parses usage" =
  let json =
    `Assoc [ "usage", `Assoc [ "prompt_tokens", `Int 100; "completion_tokens", `Int 50 ] ]
  in
  match usage_of_provider_d_json json with
  | Some u -> u.input_tokens = 100 && u.output_tokens = 50
  | None -> false
;;

let%test "usage_of_provider_d_json null usage returns None" =
  let json = `Assoc [ "usage", `Null ] in
  usage_of_provider_d_json json = None
;;

let%test "usage_of_provider_d_json missing usage returns None" =
  let json = `Assoc [] in
  usage_of_provider_d_json json = None
;;

let%test "usage_of_provider_d_json with cached_tokens" =
  let json =
    `Assoc
      [ ( "usage"
        , `Assoc
            [ "prompt_tokens", `Int 100
            ; "completion_tokens", `Int 50
            ; "prompt_tokens_details", `Assoc [ "cached_tokens", `Int 30 ]
            ] )
      ]
  in
  match usage_of_provider_d_json json with
  | Some u -> u.cache_read_input_tokens = 30
  | None -> false
;;

let%test "parse_provider_d_response_result basic text response" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "chatcmpl-1"
          ; "model", `String "model-d-4"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "stop"
                    ; "message", `Assoc [ "content", `String "Hello world" ]
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp ->
    resp.id = "chatcmpl-1" && resp.model = "model-d-4" && resp.stop_reason = EndTurn
  | Error _ -> false
;;

let%test "parse_provider_d_response_result tool calls" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "cmpl-2"
          ; "model", `String "model-d-4"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "tool_calls"
                    ; ( "message"
                      , `Assoc
                          [ "content", `Null
                          ; ( "tool_calls"
                            , `List
                                [ `Assoc
                                    [ "id", `String "call_1"
                                    ; "type", `String "function"
                                    ; ( "function"
                                      , `Assoc
                                          [ "name", `String "get_weather"
                                          ; "arguments", `String "{\"city\":\"Seoul\"}"
                                          ] )
                                    ]
                                ] )
                          ] )
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.stop_reason = StopToolUse
  | Error _ -> false
;;

let%test "parse_provider_d_response_result max_tokens stop reason" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "cmpl-3"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "length"
                    ; "message", `Assoc [ "content", `String "truncated" ]
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.stop_reason = MaxTokens
  | Error _ -> false
;;

let%test "parse_provider_d_response_result error returns Error" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc [ "error", `Assoc [ "message", `String "rate limited" ] ])
  in
  match parse_provider_d_response_result json_str with
  | Error msg -> msg = "rate limited"
  | Ok _ -> false
;;

let%test "provider_d_messages_of_message user text" =
  let msg =
    { role = User
    ; content = [ Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message user with tool_result" =
  let msg =
    { role = User
    ; content =
        [ Text "follow up"
        ; ToolResult
            { tool_use_id = "tc1"; content = "result"; is_error = false; json = None }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 2
;;

let%test "build_request strips orphaned tool results from wire messages" =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  let messages =
    [ { role = Assistant
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content =
          [ Text "follow up"
          ; ToolResult
              { tool_use_id = "orphan-id"
              ; content = "stale"
              ; is_error = false
              ; json = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config:cfg ~messages () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let roles =
    body
    |> member "messages"
    |> to_list
    |> List.map (fun json -> json |> member "role" |> to_string)
  in
  roles = [ "assistant"; "user" ]
;;

let%test "provider_d_messages_of_message assistant with tool_calls" =
  let msg =
    { role = Assistant
    ; content = [ ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message system" =
  let msg =
    { role = System
    ; content = [ Text "system prompt" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message user empty content" =
  let msg =
    { role = User; content = []; name = None; tool_call_id = None; metadata = [] }
  in
  let result = provider_d_messages_of_message msg in
  result = []
;;

let%test "provider_d_messages_of_message user with image" =
  let msg =
    { role = User
    ; content =
        [ Image { media_type = "image/png"; data = "abc123"; source_type = "base64" }
        ; Text "describe this"
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message user with document" =
  let msg =
    { role = User
    ; content =
        [ Document
            { media_type = "application/pdf"; data = "abc"; source_type = "base64" }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message user with audio" =
  let msg =
    { role = User
    ; content =
        [ Audio { media_type = "audio/wav"; data = "audiodata"; source_type = "base64" } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
;;

let%test "provider_d_messages_of_message assistant text only" =
  let msg =
    { role = Assistant
    ; content = [ Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" |> to_string = "hello"
;;

let%test "provider_d_messages_of_message assistant excludes reasoning from content" =
  let msg =
    { role = Assistant
    ; content =
        [ Thinking { thinking_type = "reasoning"; content = "hidden chain of thought" }
        ; Text "final answer"
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" |> to_string = "final answer"
  && json |> member "reasoning_content" = `Null
;;

let%test "provider_k_messages_of_message preserves reasoning_content separately" =
  let msg =
    { role = Assistant
    ; content =
        [ Thinking { thinking_type = "reasoning"; content = "step one" }
        ; ToolUse { id = "tc1"; name = "calc"; input = `Assoc [ "expr", `String "2+2" ] }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_k_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" |> to_string = ""
  && json |> member "reasoning_content" |> to_string = "step one"
  && json |> member "tool_calls" |> to_list |> List.length = 1
;;

let%test "provider_d_messages_of_message assistant blank text with tool_calls" =
  let msg =
    { role = Assistant
    ; content = [ Text ""; ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" = `Null
;;

let%test "provider_d_messages_of_message Tool role with ToolResult" =
  let msg =
    { role = Tool
    ; content =
        [ ToolResult
            { tool_use_id = "tc1"
            ; content = "result data"
            ; is_error = false
            ; json = None
            }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  List.length result = 1
  &&
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "tool"
;;

let%test "provider_d_messages_of_message Tool role without ToolResult fallback to user" =
  let msg =
    { role = Tool
    ; content = [ Text "fallback" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let result = provider_d_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "user"
;;

let%test "build_provider_d_tool_json with parameters field" =
  let tool_json =
    `Assoc
      [ "name", `String "my_fn"
      ; "description", `String "does stuff"
      ; "parameters", `Assoc [ "type", `String "object" ]
      ]
  in
  let result = build_provider_d_tool_json tool_json in
  let open Yojson.Safe.Util in
  result
  |> member "function"
  |> member "parameters"
  |> member "type"
  |> to_string
  = "object"
;;

let%test "build_provider_d_tool_json converts legacy parameter list to json schema" =
  let tool_json =
    `Assoc
      [ "name", `String "my_fn"
      ; "description", `String "does stuff"
      ; ( "parameters"
        , `List
            [ `Assoc
                [ "name", `String "query"
                ; "description", `String "search query"
                ; "param_type", `String "string"
                ; "required", `Bool true
                ]
            ; `Assoc
                [ "name", `String "limit"
                ; "description", `String "max results"
                ; "param_type", `String "integer"
                ; "required", `Bool false
                ]
            ] )
      ]
  in
  let result = build_provider_d_tool_json tool_json in
  let open Yojson.Safe.Util in
  let parameters = result |> member "function" |> member "parameters" in
  parameters |> member "type" |> to_string = "object"
  && parameters
     |> member "properties"
     |> member "query"
     |> member "type"
     |> to_string
     = "string"
  && parameters
     |> member "properties"
     |> member "limit"
     |> member "type"
     |> to_string
     = "integer"
  && List.mem "query" (parameters |> member "required" |> to_list |> List.map to_string)
;;

let%test "build_provider_d_tool_json skips malformed legacy parameter entries" =
  let tool_json =
    `Assoc
      [ "name", `String "my_fn"
      ; ( "parameters"
        , `List
            [ `Assoc
                [ "description", `String "missing name"
                ; "param_type", `String "string"
                ; "required", `Bool true
                ]
            ; `Assoc
                [ "name", `String "query"
                ; "description", `String "search query"
                ; "required", `Bool true
                ]
            ] )
      ]
  in
  let result = build_provider_d_tool_json tool_json in
  let open Yojson.Safe.Util in
  let parameters = result |> member "function" |> member "parameters" in
  parameters
  |> member "properties"
  |> member "query"
  |> member "type"
  |> to_string
  = "string"
  && parameters |> member "properties" |> member "" = `Null
  && List.mem "query" (parameters |> member "required" |> to_list |> List.map to_string)
;;

let%test "build_provider_d_tool_json missing all optional fields" =
  let tool_json = `Assoc [] in
  let result = build_provider_d_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "function" |> member "name" |> to_string = "tool"
  && result |> member "function" |> member "description" |> to_string = ""
;;

let%test "build_provider_d_tool_json list passthrough" =
  build_provider_d_tool_json (`List [ `String "bad" ]) = `List [ `String "bad" ]
;;

let%test "response_format_to_provider_d_json wraps raw json schema" =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  match response_format_to_provider_d_json (Types.JsonSchema schema) with
  | Some json ->
    let open Yojson.Safe.Util in
    json |> member "type" |> to_string = "json_schema"
    && json |> member "json_schema" |> member "name" |> to_string = "structured_output"
    && json
       |> member "json_schema"
       |> member "schema"
       |> member "type"
       |> to_string
       = "object"
  | None -> false
;;

let%test "response_format_to_provider_d_json preserves named schema envelope" =
  let schema =
    `Assoc
      [ "name", `String "math_response"
      ; "strict", `Bool true
      ; ( "schema"
        , `Assoc
            [ "type", `String "object"
            ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "number" ] ]
            ] )
      ]
  in
  match response_format_to_provider_d_json (Types.JsonSchema schema) with
  | Some json ->
    let open Yojson.Safe.Util in
    json |> member "json_schema" |> member "name" |> to_string = "math_response"
    && json |> member "json_schema" |> member "strict" |> to_bool
    && json
       |> member "json_schema"
       |> member "schema"
       |> member "type"
       |> to_string
       = "object"
  | None -> false
;;

let%test "strip_json_markdown_fences no closing fence" =
  let input = "```json\n{\"key\":\"value\"}" in
  strip_json_markdown_fences input = input
;;

let%test "strip_json_markdown_fences empty content" = strip_json_markdown_fences "" = ""

let%test "parse_provider_d_response_result unknown finish_reason" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "something_new"
                    ; "message", `Assoc [ "content", `String "text" ]
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.stop_reason = Unknown "something_new"
  | Error _ -> false
;;

let%test "parse_provider_d_response_result end_turn finish_reason" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "end_turn"
                    ; "message", `Assoc [ "content", `String "done" ]
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false
;;

let%test "parse_provider_d_response_result null content" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "stop"
                    ; "message", `Assoc [ "content", `Null ]
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false
;;

let%test "parse_provider_d_response_result list content" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "stop"
                    ; ( "message"
                      , `Assoc [ "content", `List [ `String "part1"; `String "part2" ] ] )
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [ Text t ] -> String.length t > 0
     | _ -> false)
  | Error _ -> false
;;

let%test "parse_provider_d_response_result list content with assoc text blocks" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "m"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "stop"
                    ; ( "message"
                      , `Assoc
                          [ ( "content"
                            , `List
                                [ `Assoc [ "text", `String "block1" ]
                                ; `Assoc [ "text", `String "block2" ]
                                ] )
                          ] )
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [ Text t ] -> t = "block1block2"
     | _ -> false)
  | Error _ -> false
;;

let%test "parse_provider_d_response_result with reasoning_content" =
  let json_str =
    Yojson.Safe.to_string
      (`Assoc
          [ "id", `String "c1"
          ; "model", `String "provider_g-r1"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "stop"
                    ; ( "message"
                      , `Assoc
                          [ "content", `String "answer"
                          ; "reasoning_content", `String "I thought about it"
                          ] )
                    ]
                ] )
          ])
  in
  match parse_provider_d_response_result json_str with
  | Ok resp ->
    (* N-of-M followup to PR #1525 (backend_provider_f.has_tool_use). Same
       content_block catch-all family — enumerate every variant so a
       future block type can't silently inherit "no thinking". *)
    List.exists
      (fun (block : Types.content_block) ->
         match block with
         | Thinking _ -> true
         | Text _
         | RedactedThinking _
         | ToolUse _
         | ToolResult _
         | Image _
         | Document _
         | Audio _ -> false)
      resp.content
  | Error _ -> false
;;

let%test "parse_provider_d_response_result JSON list wrapping" =
  let inner =
    `Assoc
      [ "id", `String "c1"
      ; "model", `String "m"
      ; ( "choices"
        , `List
            [ `Assoc
                [ "finish_reason", `String "stop"
                ; "message", `Assoc [ "content", `String "ok" ]
                ]
            ] )
      ]
  in
  let json_str = Yojson.Safe.to_string (`List [ inner ]) in
  match parse_provider_d_response_result json_str with
  | Ok resp -> resp.id = "c1"
  | Error _ -> false
;;

let%test "parse_provider_d_response_result error without message" =
  let json_str = Yojson.Safe.to_string (`Assoc [ "error", `Assoc [] ]) in
  match parse_provider_d_response_result json_str with
  | Error msg -> msg = "Unknown API error"
  | Ok _ -> false
;;

let%test "usage_of_provider_d_json prompt_tokens_details null" =
  let json =
    `Assoc
      [ ( "usage"
        , `Assoc
            [ "prompt_tokens", `Int 50
            ; "completion_tokens", `Int 25
            ; "prompt_tokens_details", `Null
            ] )
      ]
  in
  match usage_of_provider_d_json json with
  | Some u -> u.cache_read_input_tokens = 0
  | None -> false
;;

let%test "provider_d_content_parts_of_blocks image block" =
  let blocks =
    [ Image { media_type = "image/png"; data = "abc"; source_type = "base64" } ]
  in
  let result = provider_d_content_parts_of_blocks blocks in
  List.length result = 1
;;

let%test "provider_d_content_parts_of_blocks document block" =
  let blocks =
    [ Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" } ]
  in
  let result = provider_d_content_parts_of_blocks blocks in
  List.length result = 1
;;

let%test "provider_d_content_parts_of_blocks audio block" =
  let blocks =
    [ Audio { media_type = "audio/wav"; data = "abc"; source_type = "base64" } ]
  in
  let result = provider_d_content_parts_of_blocks blocks in
  List.length result = 1
;;

let%test "provider_d_content_parts_of_blocks redacted thinking filtered" =
  let blocks = [ RedactedThinking "secret"; Text "visible" ] in
  let result = provider_d_content_parts_of_blocks blocks in
  List.length result = 1
;;

let%test "provider_d_content_parts_of_blocks tool_result filtered" =
  let blocks =
    [ ToolResult { tool_use_id = "t1"; content = "result"; is_error = false; json = None }
    ]
  in
  provider_d_content_parts_of_blocks blocks = []
;;

let%test "build_request includes tool_choice for model with supports_tool_choice=true" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"http://localhost"
      ~tool_choice:Any
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"
;;

let%test "build_request includes tool_choice for unknown model (backward compat)" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"mystery-xyz-v1"
      ~base_url:"http://localhost"
      ~tool_choice:Any
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"
;;

let%test "build_request omits tool_choice when tool_choice=None" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"http://localhost"
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields -> not (List.exists (fun (k, _) -> k = "tool_choice") fields)
  | _ -> false
;;

let%test "provider_k build_request drops tool_choice when unsupported" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.coding_base_url
      ~tool_choice:(Tool "calc")
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields -> not (List.exists (fun (k, _) -> k = "tool_choice") fields)
  | _ -> false
;;

let%test
    "provider_k build_request replays reasoning_content without leaking it into content"
  =
  let config =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.coding_base_url
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking { thinking_type = "reasoning"; content = "use calculator" }
          ; ToolUse
              { id = "call_1"; name = "calc"; input = `Assoc [ "expr", `String "2+2" ] }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = build_request ~config ~messages () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  let assistant = body |> member "messages" |> index 0 in
  assistant |> member "content" |> to_string = ""
  && assistant |> member "reasoning_content" |> to_string = "use calculator"
;;

let%test "build_request uses json_schema response_format when output_schema is set" =
  let schema =
    `Assoc
      [ "title", `String "Math Response"
      ; "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"https://api.provider_d.com/v1"
      ~output_schema:schema
      ()
  in
  let body = build_request ~config ~messages:[] () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  body |> member "response_format" |> member "type" |> to_string = "json_schema"
  && body
     |> member "response_format"
     |> member "json_schema"
     |> member "name"
     |> to_string
     = "math_response"
  && body |> member "response_format" |> member "json_schema" |> member "schema" = schema
  && body
     |> member "response_format"
     |> member "json_schema"
     |> member "strict"
     |> to_bool
;;

let%test "build_request prefers output_schema over json_object mode" =
  let schema = `Assoc [ "type", `String "object" ] in
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d"
      ~base_url:"https://api.provider_d.com/v1"
      ~response_format_json:true
      ~output_schema:schema
      ()
  in
  let body = build_request ~config ~messages:[] () |> Yojson.Safe.from_string in
  let open Yojson.Safe.Util in
  body |> member "response_format" |> member "type" |> to_string = "json_schema"
;;

let%test "supports_tool_choice_override=Some false drops tool_choice on unknown model" =
  (* Unknown model_id defaults to supports_tool_choice=true. Override
     to Some false must take precedence and drop the tool_choice field. *)
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"mystery-xyz-v1"
      ~base_url:"http://localhost"
      ~tool_choice:Any
      ~supports_tool_choice_override:false
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields -> not (List.exists (fun (k, _) -> k = "tool_choice") fields)
  | _ -> false
;;

let%test
    "supports_tool_choice_override=Some true forces tool_choice on capability-false model"
  =
  (* Use an unknown model whose capability record defaults to
     supports_tool_choice=false, then force-enable it via override. *)
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"mystery-xyz-v1"
      ~base_url:"http://localhost"
      ~tool_choice:Any
      ~supports_tool_choice_override:true
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"
;;

let%test "build_request serializes thinking object for provider_g-v4-flash" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"provider_g-v4-flash"
      ~base_url:"https://api.provider_g.com"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  thinking |> member "type" |> to_string = "enabled"
  && thinking |> member "reasoning_effort" = `Null
  && json |> member "reasoning_effort" |> to_string = "low"
;;

let%test "build_request serializes disabled thinking for provider_g-v4-pro" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"provider_g-v4-pro"
      ~base_url:"https://api.provider_g.com"
      ~enable_thinking:false
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" |> member "type" |> to_string = "disabled"
;;

let%test "build_request emits reasoning_effort for Provider_d reasoning models" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-d-5.1"
      ~base_url:"https://api.provider_d.com/v1"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "reasoning_effort" |> to_string = "low"
  && json |> member "thinking" = `Null
  && json |> member "enable_thinking" = `Null
;;

let%test "build_request emits thinking object only for Provider_c K2.5" =
  let config =
    Provider_config.make
      ~kind:Provider_c
      ~model_id:"provider_c-k2.5"
      ~base_url:"https://api.provider_b.ai/v1"
      ~enable_thinking:false
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "thinking" |> member "type" |> to_string = "disabled"
  && json |> member "reasoning_effort" = `Null
  && json |> member "chat_template_kwargs" = `Null
;;

let%test "build_request emits enable_thinking for Provider_h provider kind" =
  let config =
    Provider_config.make
      ~kind:Provider_h
      ~model_id:"provider_h-plus"
      ~base_url:"https://provider_h.aliyuncs.com/compatible-mode/v1"
      ~enable_thinking:true
      ~thinking_budget:50
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "enable_thinking" |> to_bool = true
  && json |> member "thinking_budget" |> to_int = 50
  && json |> member "chat_template_kwargs" = `Null
;;

let%test "build_request omits thinking params for No_thinking_control" =
  (* Generic unknown Provider_d-compatible model ids fall back to
     No_thinking_control and must not emit any provider-specific thinking
     parameter. *)
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"model-n-3.3-70b"
      ~base_url:"http://localhost"
      ~enable_thinking:true
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  (* model-n-3.3-70b resolves to default_capabilities (No_thinking_control),
     so neither thinking nor chat_template_kwargs should appear *)
  json |> member "thinking" = `Null && json |> member "chat_template_kwargs" = `Null
;;

let%test "max_tokens clamped to capability ceiling when caller exceeds cap" =
  (* provider_k-4-flash has max_output_tokens = Some 4_096 in for_model_id.
     When caller requests 8_000, backend must clamp to 4_096 to
     avoid server 400 rejection. Regression guard for S07. *)
  let config =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-4-flash"
      ~base_url:Zai_catalog.general_base_url
      ~max_tokens:8_000
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "max_tokens" |> to_int = 4_096
;;

let%test "max_tokens passed through when within capability cap" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-4-flash"
      ~base_url:Zai_catalog.general_base_url
      ~max_tokens:2_000
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "max_tokens" |> to_int = 2_000
;;

let%test "build_request emits chat_template_kwargs for provider_l (Chat_template_kwargs)" =
  (* provider_l-ultra-253b resolves to provider_l_capabilities which has
     thinking_control_format = Chat_template_kwargs. The serializer
     must emit {"chat_template_kwargs": {"enable_thinking": true}}
     when enable_thinking is set. This is the only thinking branch
     that lacked a test — Thinking_object and No_thinking_control
     already have coverage. *)
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"provider_l-ultra-253b"
      ~base_url:"https://integrate.api.nvidia.com"
      ~enable_thinking:true
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let ctk = json |> member "chat_template_kwargs" in
  ctk |> member "enable_thinking" |> to_bool = true && json |> member "thinking" = `Null
;;

let%test "build_request emits chat_template_kwargs for provider_h_3" =
  let config =
    Provider_config.make
      ~kind:Provider_d_compat
      ~model_id:"provider_h-3.5-35b-a3b"
      ~base_url:"http://localhost"
      ~enable_thinking:true
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let ctk = json |> member "chat_template_kwargs" in
  ctk |> member "enable_thinking" |> to_bool = true && json |> member "thinking" = `Null
;;

let%test "build_request omits seed when model does not support it" =
  (* provider_k-5.1 inherits default_capabilities.supports_seed = false.
     The capability gate must exclude the "seed" field from the
     wire body — Provider_k rejects unknown params. *)
  let config =
    Provider_config.make
      ~kind:Provider_config.Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "seed" = `Null
;;

let%test "strip_thinking_blocks removes Thinking from all messages" =
  let messages =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content =
          [ Text "hi"; Thinking { thinking_type = "reasoning"; content = "step 1" } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let stripped = strip_thinking_blocks messages in
  List.for_all
    (fun (msg : message) ->
       not
         (List.exists
            (fun (block : Types.content_block) ->
               match block with
               | Thinking _ -> true
               | Text _
               | RedactedThinking _
               | ToolUse _
               | ToolResult _
               | Image _
               | Document _
               | Audio _ -> false)
            msg.content))
    stripped
;;
