(** Tests for api.ml -- content block JSON, build_body_assoc, parse_response *)

open Alcotest
open Agent_sdk

let member_absent json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> true
  | `Assoc _ | `List _ | `Bool _ | `Float _ | `Int _ | `Intlit _ | `String _ -> false
;;

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len
    then false
    else if String.sub text idx sub_len = sub
    then true
    else loop (idx + 1)
  in
  if sub_len = 0 then true else loop 0
;;

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project")
  then dir
  else (
    let parent = Filename.dirname dir in
    if String.equal parent dir
    then fail "could not locate dune-project"
    else find_repo_root parent)
;;

let source_path path =
  if Filename.is_relative path
  then (
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | Some root -> Filename.concat root path
    | None -> Filename.concat (find_repo_root (Sys.getcwd ())) path)
  else path
;;

let read_source path = In_channel.with_open_text (source_path path) In_channel.input_all

let install_repo_model_catalog () =
  let path = source_path "models.toml" in
  match Llm_provider.Model_catalog.load_file path with
  | Ok catalog -> Llm_provider.Model_catalog.set_global catalog
  | Error message -> fail (Printf.sprintf "failed to load %s: %s" path message)
;;

let with_provider_catalog json f =
  let previous = Llm_provider.Provider_catalog.global () in
  let restore () =
    match previous with
    | Some catalog -> Llm_provider.Provider_catalog.set_global catalog
    | None -> Llm_provider.Provider_catalog.clear_global ()
  in
  match Llm_provider.Provider_catalog.of_json (Yojson.Safe.from_string json) with
  | Error msg -> fail ("provider catalog parse failed: " ^ msg)
  | Ok catalog ->
    Llm_provider.Provider_catalog.set_global catalog;
    Fun.protect ~finally:restore f
;;

let declared_openai_compat_provider_catalog =
  {|{
  "schema_version": 1,
  "providers": [
    {
      "id": "api-deepseek-v4",
      "kind": "openai_compat",
      "base_url": "https://api.deepseek.com",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat",
      "capabilities": {
        "max_context_tokens": 1000000,
        "max_output_tokens": 384000,
        "supports_tools": true,
        "supports_tool_choice": true,
        "supports_required_tool_choice": false,
        "supports_named_tool_choice": false,
        "supports_reasoning": true,
        "supports_extended_thinking": true,
        "supports_reasoning_budget": false,
        "accepted_reasoning_efforts": ["high", "max"],
        "thinking_control_format": "thinking_object",
        "supports_response_format_json": true,
        "supports_native_streaming": true,
        "supports_caching": true,
        "supports_prompt_caching": false
      }
    },
    {
      "id": "api-minimax-m3",
      "kind": "openai_compat",
      "base_url": "https://api.minimaxi.com/v1",
      "request_path": "/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat",
      "capabilities": {
        "max_context_tokens": 524288,
        "supports_tools": true,
        "supports_tool_choice": false,
        "supports_required_tool_choice": false,
        "supports_named_tool_choice": false,
        "supports_reasoning": true,
        "supports_extended_thinking": true,
        "supports_reasoning_budget": false,
        "thinking_control_format": "thinking_object_adaptive",
        "reasoning_output_format": "split_reasoning_fields",
        "reasoning_replay": "preserve_always",
        "supports_response_format_json": false,
        "supports_structured_output": false,
        "supports_multimodal_inputs": true,
        "supports_image_input": true,
        "supports_native_streaming": true
      }
    },
    {
      "id": "api-qwen36",
      "kind": "openai_compat",
      "base_url": "https://declared-qwen.example/v1",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat",
      "capabilities": {
        "max_context_tokens": 131072,
        "supports_tools": true,
        "supports_tool_choice": true,
        "supports_parallel_tool_calls": true,
        "supports_reasoning": true,
        "supports_extended_thinking": true,
        "supports_reasoning_budget": true,
        "thinking_control_format": "chat_template_kwargs",
        "preserve_thinking_control_format": "chat_template_kwargs_preserve_thinking",
        "supports_native_streaming": true
      }
    }
  ]
}|}
;;

let with_declared_openai_compat_provider_catalog f =
  with_provider_catalog declared_openai_compat_provider_catalog f
;;

let declared_provider_config name model_id : Provider.config =
  { Provider.provider = Provider.Custom_registered { name }; model_id; api_key_env = "" }
;;

let check_declared_provider_kind name expected =
  let registry = Llm_provider.Provider_registry.default () in
  match Llm_provider.Provider_registry.find registry name with
  | Some entry ->
    check bool (name ^ " typed provider kind") true (entry.defaults.kind = expected)
  | None -> fail ("missing declared provider: " ^ name)
;;

(* Helper: compare content_block via show string *)
let check_block msg expected actual =
  check string msg (Types.show_content_block expected) (Types.show_content_block actual)
;;

(* ------------------------------------------------------------------ *)
(* content_block_to_json / content_block_of_json round-trips            *)
(* ------------------------------------------------------------------ *)

let test_text_round_trip () =
  let block = Types.Text "hello world" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "text" block parsed
  | None -> fail "returned None"
;;

let test_thinking_round_trip () =
  let block =
    Types.Thinking { signature = Some "sig123"; content = "I think therefore I am" }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "thinking" block parsed
  | None -> fail "returned None"
;;

let test_redacted_thinking_round_trip () =
  let block = Types.RedactedThinking "redacted_data_blob" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "redacted_thinking" block parsed
  | None -> fail "returned None"
;;

let test_tool_use_round_trip () =
  let block =
    Types.ToolUse
      { id = "tu_001"; name = "calculator"; input = `Assoc [ "expr", `String "2+2" ] }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_use" block parsed
  | None -> fail "returned None"
;;

let test_tool_result_round_trip () =
  let block =
    Types.ToolResult
      { tool_use_id = "tu_001"
      ; content = "4"
      ; outcome = Tool_succeeded
      ; json = Types.try_parse_json "4"
      ; content_blocks = None
      }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result" block parsed
  | None -> fail "returned None"
;;

let test_tool_result_error_round_trip () =
  let block =
    Types.ToolResult
      { tool_use_id = "tu_002"
      ; content = "failed"
      ; outcome = Tool_failed { failure_kind = Reported_tool_error; error_class = None }
      ; json = None
      ; content_blocks = None
      }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result_error" block parsed
  | None -> fail "returned None"
;;

let test_image_round_trip () =
  let block =
    Types.Image { media_type = "image/png"; data = "abc"; source_type = Types.Base64 }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "image" block parsed
  | None -> fail "returned None"
;;

let test_document_round_trip () =
  let block =
    Types.Document
      { media_type = "application/pdf"; data = "pdf"; source_type = Types.Base64 }
  in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "document" block parsed
  | None -> fail "returned None"
;;

let test_unknown_type_returns_none () =
  let json = `Assoc [ "type", `String "future_block"; "data", `String "x" ] in
  match Api.content_block_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for unknown type"
;;

let test_reasoning_details_requires_details_list () =
  let cases =
    [ `Assoc [ "type", `String "reasoning_details" ]
    ; `Assoc [ "type", `String "reasoning_details"; "details", `Null ]
    ]
  in
  List.iter
    (fun json ->
       match Api.content_block_of_json json with
       | None -> ()
       | Some _ -> fail "expected malformed reasoning_details to fail closed")
    cases
;;

let test_kimi_message_to_json_tool_result_uses_text_blocks () =
  let msg =
    { Types.role = Tool
    ; content =
        [ Types.ToolResult
            { tool_use_id = "tu_001"
            ; content = "5"
            ; outcome = Tool_succeeded
            ; json = Some (`Int 5)
            ; content_blocks = None
            }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = Llm_provider.Api_common.kimi_message_to_json msg in
  let open Yojson.Safe.Util in
  let block = json |> member "content" |> index 0 in
  let nested = block |> member "content" |> to_list in
  check string "role serialized as user" "user" (json |> member "role" |> to_string);
  check string "tool_result type" "tool_result" (block |> member "type" |> to_string);
  check string "tool_use_id" "tu_001" (block |> member "tool_use_id" |> to_string);
  check int "nested content count" 1 (List.length nested);
  check
    string
    "nested text block type"
    "text"
    (List.hd nested |> member "type" |> to_string);
  check string "nested text block text" "5" (List.hd nested |> member "text" |> to_string)
;;

(* ------------------------------------------------------------------ *)
(* build_body_assoc                                                     *)
(* ------------------------------------------------------------------ *)

let make_state
      ?(model = (Types.default_config ~model:"test-model").model)
      ?thinking_budget
      ?tool_choice
      ?enable_thinking
      ?preserve_thinking
      ?response_format
      ()
  =
  let response_format =
    Option.value
      response_format
      ~default:(Types.default_config ~model:"test-model").response_format
  in
  let config =
    { (Types.default_config ~model:"test-model") with
      model
    ; max_tokens = Some 1024
    ; response_format
    ; system_prompt = Some "You are helpful."
    ; thinking_budget
    ; tool_choice
    ; enable_thinking
    ; preserve_thinking
    }
  in
  { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }
;;

let make_manual_thinking_state ?thinking_budget ?enable_thinking () =
  make_state ~model:"claude-opus-4-5" ?thinking_budget ?enable_thinking ()
;;

let make_adaptive_thinking_state ?thinking_budget ?enable_thinking ?response_format () =
  make_state
    ~model:"claude-sonnet-4-6"
    ?thinking_budget
    ?enable_thinking
    ?response_format
    ()
;;

let test_build_body_basic () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check string "model present" "test-model" (json |> member "model" |> to_string);
  check bool "stream false" false (json |> member "stream" |> to_bool);
  check string "system prompt" "You are helpful." (json |> member "system" |> to_string)
;;

(* The legacy Agent SDK builder must use the same catalog resolver as the
   standalone Anthropic backend. A hard-coded 4096 here would truncate
   current Sonnet requests even though the catalog declares a 64000 output
   ceiling. *)
let test_build_body_uses_catalog_output_cap () =
  let config = make_state ~model:"claude-sonnet-4-6" () in
  let config = { config with config = { config.config with max_tokens = None } } in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check int "catalog max_tokens" 64_000 (json |> member "max_tokens" |> to_int)
;;

(* A raw OpenAI-compatible endpoint is not an endpoint declaration for a bare
   OpenAI model id. It must therefore use the unknown-model fallback instead
   of inheriting the gpt-4.1 catalog row from a different provider boundary. *)
let test_build_openai_body_uses_unknown_model_fallback () =
  let provider_config : Provider.config =
    { provider =
        Provider.OpenAICompat
          { base_url = "http://test"
          ; auth_header = None
          ; path = "/v1/chat/completions"
          ; static_token = None
          }
    ; model_id = "gpt-4.1"
    ; api_key_env = "TEST_KEY"
    }
  in
  let config = make_state ~model:"gpt-4.1" () in
  let config = { config with config = { config.config with max_tokens = None } } in
  let body =
    Api.build_openai_body ~provider_config ~config ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  (* No caller override and no catalog ceiling: the field is omitted so the
     provider applies the model's own limit — no invented fallback. *)
  check
    bool
    "unknown-model max_tokens omitted"
    true
    (match body |> member "max_tokens" with
     | `Null -> true
     | _ -> false)
;;

let test_build_body_with_thinking_budget () =
  (* Thinking is gated on [enable_thinking = Some true]; a budget
     without enable_thinking must NOT emit a thinking block.
     Matches backend_anthropic.build_request semantics. *)
  let config =
    make_manual_thinking_state ~enable_thinking:true ~thinking_budget:1024 ()
  in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  check string "thinking type" "enabled" (thinking |> member "type" |> to_string);
  check int "budget_tokens" 1024 (thinking |> member "budget_tokens" |> to_int)
;;

let test_build_body_with_enable_thinking_rejects_unspecified_budget () =
  (* Manual-budget Claude requests require a caller-declared budget. OAS must
     not invent one when the caller only toggles thinking. *)
  let config = make_manual_thinking_state ~enable_thinking:true () in
  match Api.build_body_assoc ~config ~messages:[] ~stream:false () with
  | _ -> fail "expected missing manual thinking budget rejection"
  | exception Invalid_argument message ->
    check
      string
      "rejection"
      "Api_anthropic.build_body_assoc: manual-budget thinking requires an explicit \
       thinking_budget"
      message
;;

let test_build_body_disabled_thinking_rejects_reasoning_effort () =
  let config = make_manual_thinking_state ~enable_thinking:false () in
  let config =
    { config with
      config =
        { config.config with
          reasoning_effort = Some Llm_provider.Reasoning_effort.Medium
        }
    }
  in
  match Api.build_body_assoc ~config ~messages:[] ~stream:false () with
  | _ -> fail "expected disabled-thinking reasoning-effort rejection"
  | exception Invalid_argument message ->
    check
      string
      "rejection"
      "Api_anthropic.build_body_assoc: model \"claude-opus-4-5\" cannot set \
       reasoning_effort \"medium\" when enable_thinking=false"
      message
;;

let test_build_body_adaptive_thinking () =
  let config = make_adaptive_thinking_state ~enable_thinking:true () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  check string "thinking type" "adaptive" (thinking |> member "type" |> to_string);
  check bool "budget_tokens omitted" true (thinking |> member "budget_tokens" = `Null);
  check bool "effort omitted" true (json |> member "output_config" = `Null)
;;

let test_build_body_adaptive_output_config_preserves_format () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let config =
    make_state
      ~model:"claude-opus-4-8"
      ~enable_thinking:true
      ~response_format:(Types.JsonSchema schema)
      ()
  in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  let output_config = json |> member "output_config" in
  check string "thinking type" "adaptive" (thinking |> member "type" |> to_string);
  check bool "budget_tokens omitted" true (thinking |> member "budget_tokens" = `Null);
  check bool "effort omitted" true (output_config |> member "effort" = `Null);
  check
    string
    "format type"
    "json_schema"
    (output_config |> member "format" |> member "type" |> to_string)
;;

let test_build_body_rejects_adaptive_numeric_budget () =
  let config =
    make_adaptive_thinking_state ~enable_thinking:false ~thinking_budget:5000 ()
  in
  match Api.build_body_assoc ~config ~messages:[] ~stream:false () with
  | _ -> fail "expected adaptive numeric-budget rejection"
  | exception Invalid_argument message ->
    check
      string
      "rejection"
      "Api_anthropic.build_body_assoc: thinking_budget is unsupported by adaptive \
       thinking"
      message
;;

let test_build_body_without_thinking () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let has_thinking = List.exists (fun (k, _) -> k = "thinking") assoc in
  check bool "no thinking key" false has_thinking
;;

let test_build_body_with_tool_choice () =
  let config = make_state ~tool_choice:(Types.Tool "calculator") () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tc = json |> member "tool_choice" in
  check string "tool_choice type" "tool" (tc |> member "type" |> to_string);
  check string "tool_choice name" "calculator" (tc |> member "name" |> to_string)
;;

let test_build_body_with_tools () =
  let tool_json = `Assoc [ "name", `String "calc"; "description", `String "calc" ] in
  let assoc =
    Api.build_body_assoc
      ~config:(make_state ())
      ~messages:[]
      ~tools:[ tool_json ]
      ~stream:true
      ()
  in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check bool "stream true" true (json |> member "stream" |> to_bool);
  let tools = json |> member "tools" |> to_list in
  check int "1 tool" 1 (List.length tools)
;;

let test_build_openai_body_with_json_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = "gpt-mini"
        ; response_format = Types.JsonSchema schema
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~config:state ~messages:[] () |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let response_format = json |> member "response_format" in
  check
    string
    "response_format.type"
    "json_schema"
    (response_format |> member "type" |> to_string);
  check
    string
    "json_schema.name"
    "structured_output"
    (response_format |> member "json_schema" |> member "name" |> to_string);
  check
    string
    "json_schema.schema.type"
    "object"
    (response_format
     |> member "json_schema"
     |> member "schema"
     |> member "type"
     |> to_string)
;;

let test_build_body_sampling_params_anthropic () =
  (* Regression: the Anthropic agent_sdk request path previously omitted
     temperature/top_p/top_k entirely, silently defaulting every Claude
     agent to Anthropic's server-side temperature = 1.0 + top_p = 1. *)
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          max_tokens = Some 1024
        ; temperature = Some 0.3
        ; top_p = Some 0.85
        ; top_k = Some 20
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check (float 1e-6) "temperature 0.3" 0.3 (json |> member "temperature" |> to_number);
  check (float 1e-6) "top_p 0.85" 0.85 (json |> member "top_p" |> to_number);
  check int "top_k 20" 20 (json |> member "top_k" |> to_int)
;;

let test_build_body_sampling_params_omitted_when_none () =
  (* When the caller does not set a sampling param, the Anthropic body
     must not carry the key at all — relying on Anthropic's server-side
     defaults rather than encoding some OAS-layer default. *)
  let state = make_state () in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  check
    bool
    "no temperature key"
    false
    (List.exists (fun (k, _) -> k = "temperature") assoc);
  check bool "no top_p key" false (List.exists (fun (k, _) -> k = "top_p") assoc);
  check bool "no top_k key" false (List.exists (fun (k, _) -> k = "top_k") assoc);
  check bool "no min_p key" false (List.exists (fun (k, _) -> k = "min_p") assoc)
;;

let test_build_openai_body_with_provider_m_sampling () =
  let provider_config =
    declared_provider_config "dashscope" "dashscope-3.5-35b-a3b-ud-q8-xl"
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; temperature = Some 0.6
        ; top_p = Some 0.95
        ; top_k = Some 20
        ; min_p = Some 0.01
        ; enable_thinking = Some false
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  check
    (option (float 0.001))
    "top_p"
    (Some 0.95)
    (json |> member "top_p" |> to_float_option);
  check (option int) "top_k" (Some 20) (json |> member "top_k" |> to_int_option);
  check
    (option (float 0.001))
    "min_p"
    (Some 0.01)
    (json |> member "min_p" |> to_float_option);
  check bool "enable_thinking false" false (json |> member "enable_thinking" |> to_bool);
  check
    bool
    "chat_template_kwargs absent"
    true
    (member_absent json "chat_template_kwargs")
;;

let test_build_openai_body_deepseek_thinking_drops_sampling () =
  with_declared_openai_compat_provider_catalog (fun () ->
    (* Declared DeepSeek endpoints drop temperature/top_p while thinking is
       enabled, matching Backend_openai_request.build_request. Thinking defaults
       on, so no enable_thinking is set here. *)
    let provider_config =
      declared_provider_config "api-deepseek-v4" "deepseek-v4-flash"
    in
    let state =
      { Types.config =
          { (Types.default_config ~model:"test-model") with
            model = provider_config.model_id
          ; temperature = Some 0.7
          ; top_p = Some 0.9
          }
      ; messages = []
      ; turn_count = 0
      ; usage = Types.empty_usage
      }
    in
    let json =
      Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    check
      (option (float 0.001))
      "temperature dropped while thinking"
      None
      (json |> member "temperature" |> to_float_option);
    check
      (option (float 0.001))
      "top_p dropped while thinking"
      None
      (json |> member "top_p" |> to_float_option))
;;

let test_build_openai_body_deepseek_disabled_thinking_keeps_sampling () =
  with_declared_openai_compat_provider_catalog (fun () ->
    (* With thinking explicitly disabled the suppression must not fire. *)
    let provider_config =
      declared_provider_config "api-deepseek-v4" "deepseek-v4-flash"
    in
    let state =
      { Types.config =
          { (Types.default_config ~model:"test-model") with
            model = provider_config.model_id
          ; temperature = Some 0.7
          ; top_p = Some 0.9
          ; enable_thinking = Some false
          }
      ; messages = []
      ; turn_count = 0
      ; usage = Types.empty_usage
      }
    in
    let json =
      Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    check
      (option (float 0.001))
      "temperature kept when thinking disabled"
      (Some 0.7)
      (json |> member "temperature" |> to_float_option);
    check
      (option (float 0.001))
      "top_p kept when thinking disabled"
      (Some 0.9)
      (json |> member "top_p" |> to_float_option))
;;

(* [None_] on a non-GLM provider that supports tool_choice must serialize as
   ["none"] with the tools list kept, so the provider enforces the caller's
   prohibition. Regression for #2505: an unconditional-omit arm attached tools
   with no tool_choice, letting the provider default (auto) resurrect calls
   the caller explicitly forbade. *)
let test_build_openai_body_tool_choice_none_serializes () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let provider_config =
      declared_provider_config "api-deepseek-v4" "deepseek-v4-flash"
    in
    let state = make_state ~tool_choice:Types.None_ () in
    let state =
      { state with config = { state.config with model = provider_config.model_id } }
    in
    let tool_json = `Assoc [ "name", `String "calc"; "description", `String "calc" ] in
    let json =
      Api.build_openai_body
        ~provider_config
        ~config:state
        ~messages:[]
        ~tools:[ tool_json ]
        ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    check
      string
      "tool_choice none serialized"
      "none"
      (json |> member "tool_choice" |> to_string);
    check int "tools kept" 1 (json |> member "tools" |> to_list |> List.length))
;;

(* When the provider cannot express tool_choice at all, [None_] must drop the
   tools list along with the field: tools with no tool_choice would default to
   auto on the provider side, violating the prohibition (#2505). *)
let test_build_openai_body_tool_choice_none_without_capability_drops_tools () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let provider_config = declared_provider_config "api-minimax-m3" "minimax-m3" in
    let state = make_state ~tool_choice:Types.None_ () in
    let state =
      { state with config = { state.config with model = provider_config.model_id } }
    in
    let tool_json = `Assoc [ "name", `String "calc"; "description", `String "calc" ] in
    let json =
      Api.build_openai_body
        ~provider_config
        ~config:state
        ~messages:[]
        ~tools:[ tool_json ]
        ()
      |> Yojson.Safe.from_string
    in
    check bool "tool_choice absent" true (member_absent json "tool_choice");
    check bool "tools absent" true (member_absent json "tools"))
;;

let test_build_openai_body_with_qwen_preserve_thinking () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let provider_config = declared_provider_config "api-qwen36" "qwen36-35b-a3b-mtp" in
    let state = make_state ~enable_thinking:true ~preserve_thinking:true () in
    let state =
      { state with config = { state.config with model = provider_config.model_id } }
    in
    let json =
      Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    let ctk = json |> member "chat_template_kwargs" in
    check bool "enable_thinking true" true (ctk |> member "enable_thinking" |> to_bool);
    check bool "preserve_thinking true" true (ctk |> member "preserve_thinking" |> to_bool))
;;

let test_build_openai_body_qwen_preserve_replays_reasoning () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let provider_config = declared_provider_config "api-qwen36" "qwen36-35b-a3b-mtp" in
    let state = make_state ~enable_thinking:true ~preserve_thinking:true () in
    let state =
      { state with config = { state.config with model = provider_config.model_id } }
    in
    let messages =
      [ { Types.role = Types.Assistant
        ; content =
            [ Types.Thinking { signature = None; content = "keep this" }
            ; Types.Text "answer"
            ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    let json =
      Api.build_openai_body ~provider_config ~config:state ~messages ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    let assistant = json |> member "messages" |> index 1 in
    check
      string
      "reasoning_content replayed"
      "keep this"
      (assistant |> member "reasoning_content" |> to_string))
;;

let test_build_openai_body_kimi_latest_omits_thinking_param () =
  let provider_config = declared_provider_config "kimi" "kimi-k2.7-code" in
  let state = make_state ~model:provider_config.model_id ~preserve_thinking:true () in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  check bool "thinking absent" true (member_absent json "thinking");
  check
    bool
    "chat_template_kwargs absent"
    true
    (member_absent json "chat_template_kwargs");
  check bool "preserve_thinking absent" true (member_absent json "preserve_thinking")
;;

let test_build_openai_body_kimi_latest_replays_reasoning () =
  let provider_config = declared_provider_config "kimi" "kimi-k2.7-code" in
  let state = make_state ~model:provider_config.model_id ~preserve_thinking:false () in
  let messages =
    [ { Types.role = Types.Assistant
      ; content =
          [ Types.Thinking { signature = None; content = "keep latest" }
          ; Types.Text "answer"
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 1 in
  check
    string
    "reasoning_content replayed"
    "keep latest"
    (assistant |> member "reasoning_content" |> to_string)
;;

let deepseek_provider_config : Provider.config =
  declared_provider_config "api-deepseek-v4" "deepseek-v4-pro"
;;

let minimax_m3_provider_config : Provider.config =
  declared_provider_config "api-minimax-m3" "minimax-m3"
;;

let test_build_openai_body_deepseek_uses_dialect_controls () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let state =
      { Types.config =
          { (Types.default_config ~model:"test-model") with
            model = deepseek_provider_config.model_id
          ; enable_thinking = Some true
          ; reasoning_effort = Some Llm_provider.Reasoning_effort.High
          ; temperature = Some 0.7
          ; top_p = Some 0.9
          }
      ; messages = []
      ; turn_count = 0
      ; usage = Types.empty_usage
      }
    in
    let json =
      Api.build_openai_body
        ~provider_config:deepseek_provider_config
        ~config:state
        ~messages:[]
        ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    check
      string
      "thinking enabled"
      "enabled"
      (json |> member "thinking" |> member "type" |> to_string);
    check string "exact high" "high" (json |> member "reasoning_effort" |> to_string);
    check bool "temperature omitted" true (json |> member "temperature" = `Null);
    check bool "top_p omitted" true (json |> member "top_p" = `Null))
;;

let test_build_openai_body_minimax_uses_public_projection_dialect_controls () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let state =
      make_state
        ~model:minimax_m3_provider_config.model_id
        ~enable_thinking:true
        ~tool_choice:Types.Auto
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
      Api.build_openai_body
        ~provider_config:minimax_m3_provider_config
        ~config:state
        ~messages:[]
        ~tools:[ tool_json ]
        ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    check
      string
      "thinking adaptive"
      "adaptive"
      (json |> member "thinking" |> member "type" |> to_string);
    check bool "reasoning split enabled" true (json |> member "reasoning_split" |> to_bool);
    check bool "tools preserved" false (member_absent json "tools");
    check bool "tool_choice omitted" true (member_absent json "tool_choice");
    check bool "reasoning_effort omitted" true (member_absent json "reasoning_effort");
    check
      bool
      "chat_template_kwargs omitted"
      true
      (member_absent json "chat_template_kwargs"))
;;

let test_build_openai_body_deepseek_replays_tool_reasoning_only () =
  with_declared_openai_compat_provider_catalog (fun () ->
    let assistant_content tool =
      if tool
      then
        [ Types.Thinking { signature = None; content = "call the tool" }
        ; Types.ToolUse
            { id = "call_1"
            ; name = "calculator"
            ; input = `Assoc [ "expr", `String "2+2" ]
            }
        ]
      else
        [ Types.Thinking { signature = None; content = "plain thought" }
        ; Types.Text "answer"
        ]
    in
    let assistant_msg tool =
      { Types.role = Types.Assistant
      ; content = assistant_content tool
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    in
    let state =
      { Types.config =
          { (Types.default_config ~model:"test-model") with
            model = deepseek_provider_config.model_id
          }
      ; messages = []
      ; turn_count = 0
      ; usage = Types.empty_usage
      }
    in
    let plain =
      Api.build_openai_body
        ~provider_config:deepseek_provider_config
        ~config:state
        ~messages:[ assistant_msg false ]
        ()
      |> Yojson.Safe.from_string
    in
    let tool =
      Api.build_openai_body
        ~provider_config:deepseek_provider_config
        ~config:state
        ~messages:[ assistant_msg true ]
        ()
      |> Yojson.Safe.from_string
    in
    let open Yojson.Safe.Util in
    let plain_assistant = plain |> member "messages" |> index 0 in
    let tool_assistant = tool |> member "messages" |> index 0 in
    check
      bool
      "plain reasoning omitted"
      true
      (plain_assistant |> member "reasoning_content" = `Null);
    check
      string
      "tool reasoning_content"
      "call the tool"
      (tool_assistant |> member "reasoning_content" |> to_string))
;;

let test_build_openai_body_omits_provider_m_only_fields_for_generic_compat () =
  let provider_config = Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" () in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; top_p = Some 0.9
        ; top_k = Some 40
        ; min_p = Some 0.05
        ; enable_thinking = Some false
        ; response_format = Types.JsonMode
        ; tool_choice = Some Types.Any
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    Api.build_openai_body
      ~provider_config
      ~config:state
      ~messages:[]
      ~tools:[ tool_json ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check
    (option (float 0.001))
    "top_p kept"
    (Some 0.9)
    (json |> member "top_p" |> to_float_option);
  check (option int) "top_k omitted" None (json |> member "top_k" |> to_int_option);
  check
    (option (float 0.001))
    "min_p omitted"
    None
    (json |> member "min_p" |> to_float_option);
  check
    bool
    "chat_template_kwargs omitted"
    false
    (List.mem_assoc "chat_template_kwargs" assoc);
  check bool "response_format preserved" true (List.mem_assoc "response_format" assoc);
  check bool "tool_choice preserved" true (List.mem_assoc "tool_choice" assoc);
  check bool "tools preserved" true (List.mem_assoc "tools" assoc)
;;

let test_build_openai_body_rejects_glm_forced_tool_choice () =
  let provider_config = declared_provider_config "glm" "glm-5" in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; enable_thinking = Some true
        ; tool_choice = Some (Types.Tool "calculator")
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  match
    Api.build_openai_body_result
      ~provider_config
      ~config:state
      ~messages:[]
      ~tools:[ tool_json ]
      ()
  with
  | Error reason ->
    check bool "mentions tool_choice" true (contains_substring ~sub:"tool_choice" reason);
    check bool "mentions tool name" true (contains_substring ~sub:"calculator" reason)
  | Ok _ -> fail "expected Result.Error for unsupported GLM named tool_choice"
;;

(* Complement of [test_build_openai_body_glm_preserves_reasoning_content]: the
   same Preserved-Thinking configuration WITHOUT a declared Z.AI endpoint gets
   no GLM dialect behavior. Before the typed-dialect reshape, the prefix-only
   [is_glm_request] None-branch replayed prior-turn reasoning_content for any
   "glm-…" model id regardless of endpoint. *)
let test_build_openai_body_bare_glm_gets_no_glm_dialect () =
  let messages =
    [ { Types.role = Types.Assistant
      ; content =
          [ Types.Thinking { signature = None; content = "I should call the calculator." }
          ; Types.ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = "glm-5"
        ; enable_thinking = Some true
        ; preserve_thinking = Some true
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~config:state ~messages () |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  check
    bool
    "no reasoning_content replay without declared endpoint"
    false
    (List.mem_assoc "reasoning_content" (to_assoc assistant));
  check
    bool
    "no GLM thinking object without declared endpoint"
    false
    (List.mem_assoc "thinking" (to_assoc json))
;;

(* GLM's tool_choice [None_] coercion (omit the field, drop the tools list —
   [test_build_openai_body_glm_tool_choice_none_omits_tools]) applies only
   behind a declared Z.AI endpoint; a bare "glm-…" model id keeps the generic
   OpenAI-compatible ["none"] serialization with tools intact. Before the
   typed-dialect reshape, the prefix classifier dropped both. *)
let test_build_openai_body_bare_glm_tool_choice_none_is_generic () =
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = "glm-5"
        ; tool_choice = Some Types.None_
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    Api.build_openai_body ~config:state ~messages:[] ~tools:[ tool_json ] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  check
    string
    "generic none tool_choice serialized"
    "none"
    (json |> member "tool_choice" |> to_string);
  check bool "tools preserved" true (List.mem_assoc "tools" (to_assoc json))
;;

(* Explicit typed GLM provider under the default clear_thinking=true (no
   [preserve_thinking]): the typed dialect resolves to No_replay, so
   prior-turn reasoning_content stays out of the request history even though
   the request is GLM. Locks the non-replay arm of the typed
   [replay_policy] promotion in [reasoning_dialect_for_request]. *)
let test_build_openai_body_glm_default_clear_thinking_skips_replay () =
  let provider_config = declared_provider_config "glm" "glm-5" in
  let messages =
    [ { Types.role = Types.Assistant
      ; content =
          [ Types.Thinking { signature = None; content = "I should call the calculator." }
          ; Types.ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; enable_thinking = Some true
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  check
    bool
    "no reasoning_content replay under default clear_thinking"
    false
    (List.mem_assoc "reasoning_content" (to_assoc assistant));
  check
    bool
    "clearing GLM request clears thinking"
    true
    (json |> member "thinking" |> member "clear_thinking" |> to_bool)
;;

let test_build_openai_body_glm_preserves_reasoning_content () =
  let provider_config = declared_provider_config "glm" "glm-5" in
  let messages =
    [ { Types.role = Types.Assistant
      ; content =
          [ Types.Thinking { signature = None; content = "I should call the calculator." }
          ; Types.ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; enable_thinking = Some true
        ; preserve_thinking = Some true
        ; tool_choice = Some Types.Auto
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  check string "content kept empty" "" (assistant |> member "content" |> to_string);
  check
    string
    "reasoning_content replayed"
    "I should call the calculator."
    (assistant |> member "reasoning_content" |> to_string);
  check
    string
    "auto tool_choice preserved"
    "auto"
    (json |> member "tool_choice" |> to_string);
  check
    bool
    "preserved GLM request clears no thinking"
    false
    (json |> member "thinking" |> member "clear_thinking" |> to_bool)
;;

(* Complement of [test_build_openai_body_glm_preserves_reasoning_content]: that
   test locks the [clear_thinking = false] (preserve) side of the GLM SSOT
   ([Provider_config.glm_clear_thinking_value]). This locks the [true]
   (non-preserve) side via the api_openai.ml builder. Before #2282 consolidated
   the two thinking builders, api_openai.ml hardcoded [clear_thinking = true]
   here while backend_openai_request.ml derived it from the config; the
   consolidation made api_openai.ml respect the config, so both sides of the SSOT
   now need a regression guard through the api builder. With [preserve_thinking =
   Some false] the SSOT yields [not false = true]. *)
let test_build_openai_body_glm_clears_thinking_when_not_preserving () =
  let provider_config = declared_provider_config "glm" "glm-5" in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; enable_thinking = Some true
        ; preserve_thinking = Some false
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  check
    string
    "GLM thinking enabled"
    "enabled"
    (json |> member "thinking" |> member "type" |> to_string);
  check
    bool
    "non-preserving GLM request clears thinking"
    true
    (json |> member "thinking" |> member "clear_thinking" |> to_bool)
;;

let test_build_openai_body_openai_compat_glm_uses_openai_wire_dialect () =
  let provider_config =
    { Provider.provider =
        Provider.OpenAICompat
          { base_url = "https://openrouter.ai/api/v1"
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "glm-5"
    ; api_key_env = ""
    }
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; enable_thinking = Some true
        ; tool_choice = Some Types.Auto
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    Api.build_openai_body
      ~provider_config
      ~config:state
      ~messages:[]
      ~tools:[ tool_json ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check bool "thinking omitted for non-zai glm" false (List.mem_assoc "thinking" assoc);
  check
    bool
    "chat_template_kwargs omitted for non-zai glm"
    false
    (List.mem_assoc "chat_template_kwargs" assoc);
  check
    string
    "auto tool_choice preserved"
    "auto"
    (json |> member "tool_choice" |> to_string)
;;

let test_openai_api_uses_backend_thinking_builder_ssot () =
  let api_source = read_source "lib/api_openai.ml" in
  [ "chat_template_kwargs"
  ; "reasoning_effort"
  ; "thinking_budget"
  ; "clear_thinking"
  ; "enable_thinking"
  ; "preserve_thinking"
  ]
  |> List.iter (fun field ->
    check
      bool
      (field ^ " not duplicated in api_openai.ml")
      false
      (contains_substring ~sub:("\"" ^ field ^ "\"") api_source))
;;

let test_build_openai_body_glm_tool_choice_none_omits_tools () =
  let provider_config = declared_provider_config "glm" "glm-5" in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = provider_config.model_id
        ; tool_choice = Some Types.None_
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    Api.build_openai_body
      ~provider_config
      ~config:state
      ~messages:[]
      ~tools:[ tool_json ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check bool "tool_choice omitted for glm none" false (List.mem_assoc "tool_choice" assoc);
  check bool "tools omitted for glm none" false (List.mem_assoc "tools" assoc)
;;

(* A provider registered with [kind = Glm] must serialize with the full GLM
   dialect for an opaque model id. Provider identity comes only from the typed
   registry declaration; model text carries no semantic role. *)
let test_build_openai_body_registered_glm_gets_glm_dialect () =
  let model_id = "opaque-model" in
  check_declared_provider_kind "glm" Llm_provider.Provider_config.Glm;
  let provider_config = declared_provider_config "glm" model_id in
  let messages =
    [ { Types.role = Types.Assistant
      ; content =
          [ Types.Thinking { signature = None; content = "I should call the calculator." }
          ; Types.ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = model_id
        ; enable_thinking = Some true
        ; preserve_thinking = Some true
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  check
    string
    "reasoning_content replayed for registered glm"
    "I should call the calculator."
    (assistant |> member "reasoning_content" |> to_string);
  check
    bool
    "preserved registered glm request clears no thinking"
    false
    (json |> member "thinking" |> member "clear_thinking" |> to_bool)
;;

(* Same registry-kind proof for [glm-coding] through the tool-choice coercion
   arm: GLM has no [tool_choice:"none"] representation, so both the field and
   the tools list are dropped. Before the shared projection, the degraded
   generic config serialized ["none"] and kept the tools list. *)
let test_build_openai_body_registered_glm_coding_tool_choice_none_omits_tools () =
  let model_id = "opaque-model" in
  check_declared_provider_kind "glm-coding" Llm_provider.Provider_config.Glm;
  let provider_config = declared_provider_config "glm-coding" model_id in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with
          model = model_id
        ; tool_choice = Some Types.None_
        }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let tool_json =
    `Assoc
      [ "name", `String "calculator"
      ; "description", `String "math"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let json =
    Api.build_openai_body
      ~provider_config
      ~config:state
      ~messages:[]
      ~tools:[ tool_json ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check
    bool
    "tool_choice omitted for registered glm-coding none"
    false
    (List.mem_assoc "tool_choice" assoc);
  check
    bool
    "tools omitted for registered glm-coding none"
    false
    (List.mem_assoc "tools" assoc)
;;

(* A [Custom_registered] name absent from both provider registries fails
   closed through the public builder: validation and the serializer's dialect
   projection share one [custom_registered_projection] resolver, so the only
   observable outcome is this single typed error — never a silently degraded
   generic body. *)
let test_build_openai_body_unknown_registered_provider_fails_closed () =
  let provider_config =
    declared_provider_config "no-such-registered-provider" "charglm-3"
  in
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with model = "charglm-3" }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  match Api.build_openai_body_result ~provider_config ~config:state ~messages:[] () with
  | Ok _ -> fail "expected fail-closed error for unknown Custom_registered provider"
  | Error reason ->
    check
      bool
      "names the unknown provider"
      true
      (contains_substring
         ~sub:{|"no-such-registered-provider" not found in provider registries|}
         reason)
;;

(* ------------------------------------------------------------------ *)
(* parse_response                                                       *)
(* ------------------------------------------------------------------ *)

let test_parse_response_complete () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_test",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."},
      {"type": "thinking", "signature": "sig", "thinking": "Let me think..."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|}
  in
  let resp = Api.parse_response json in
  check string "id" "msg_test" resp.id;
  check string "model" "claude-sonnet-4-6-20250514" resp.model;
  check int "content count" 2 (List.length resp.content);
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | sr -> fail (Printf.sprintf "expected EndTurn, got %s" (Types.show_stop_reason sr)));
  match resp.usage with
  | Some u when u.Types.input_tokens = 100 && u.output_tokens = 50 -> ()
  | _ -> fail "expected usage input=100 output=50"
;;

let test_parse_response_tool_use () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_tu",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "tool_use",
    "content": [
      {"type": "tool_use", "id": "tu_1", "name": "calc", "input": {"x": 1}}
    ],
    "usage": null
  }|}
  in
  let resp = Api.parse_response json in
  (match resp.stop_reason with
   | Types.StopToolUse -> ()
   | sr ->
     fail (Printf.sprintf "expected StopToolUse, got %s" (Types.show_stop_reason sr)));
  check bool "usage is None" true (resp.usage = None);
  match resp.content with
  | [ Types.ToolUse { id = "tu_1"; name = "calc"; _ } ] -> ()
  | _ -> fail "expected single ToolUse"
;;

let test_parse_response_unknown_stop () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_unk",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "new_future_reason",
    "content": [],
    "usage": null
  }|}
  in
  let resp = Api.parse_response json in
  match resp.stop_reason with
  | Types.Unknown "new_future_reason" -> ()
  | sr -> fail (Printf.sprintf "expected Unknown, got %s" (Types.show_stop_reason sr))
;;

let test_parse_openai_response_strips_fenced_json () =
  let json_str =
    {|{
    "id": "chatcmpl_test",
    "model": "dashscope",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "```json\n{\"engine\":\"default\",\"tool_calling\":false}\n```"
      }
    }]
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    (match resp.content with
     | [ Types.Text text ] ->
       Alcotest.(check string)
         "stripped json"
         "{\"engine\":\"default\",\"tool_calling\":false}"
         text
     | _ -> Alcotest.fail "expected stripped text block")
;;

let test_parse_openai_response_reasoning_content () =
  let json_str =
    {|{
    "id": "chatcmpl_think",
    "model": "dashscope-3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "The answer is 42.",
        "reasoning_content": "Let me think step by step about the meaning of life."
      }
    }],
    "usage": {"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    check int "2 content blocks" 2 (List.length resp.content);
    (match resp.content with
     | [ Types.Thinking { signature; content }; Types.Text text ] ->
       check bool "thinking unsigned" true (signature = None);
       check
         string
         "thinking content"
         "Let me think step by step about the meaning of life."
         content;
       check string "text" "The answer is 42." text
     | _ -> Alcotest.fail "expected [Thinking; Text]")
;;

let test_parse_openai_response_reasoning_with_tools () =
  let json_str =
    {|{
    "id": "chatcmpl_think_tool",
    "model": "dashscope-3.5-35b",
    "choices": [{
      "finish_reason": "tool_calls",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": null,
        "reasoning_content": "I need to call the calculator.",
        "tool_calls": [{
          "id": "call_1",
          "type": "function",
          "function": { "name": "calc", "arguments": "{\"expr\":\"2+2\"}" }
        }]
      }
    }]
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    check int "2 content blocks" 2 (List.length resp.content);
    (match resp.content with
     | [ Types.Thinking { content; _ }; Types.ToolUse { name; _ } ] ->
       check string "thinking" "I need to call the calculator." content;
       check string "tool name" "calc" name
     | _ -> Alcotest.fail "expected [Thinking; ToolUse]");
    (match resp.stop_reason with
     | Types.StopToolUse -> ()
     | sr ->
       Alcotest.fail
         (Printf.sprintf "expected StopToolUse, got %s" (Types.show_stop_reason sr)))
;;

let test_parse_openai_response_blank_reasoning () =
  let json_str =
    {|{
    "id": "chatcmpl_blank",
    "model": "dashscope-3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Just text",
        "reasoning_content": "   "
      }
    }]
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    check int "1 content block (blank reasoning filtered)" 1 (List.length resp.content);
    (match resp.content with
     | [ Types.Text "Just text" ] -> ()
     | _ -> Alcotest.fail "expected [Text] only, blank reasoning should be filtered")
;;

let test_parse_openai_response_no_reasoning () =
  let json_str =
    {|{
    "id": "chatcmpl_no_think",
    "model": "dashscope-3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Hello world"
      }
    }]
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    check int "1 content block" 1 (List.length resp.content);
    (match resp.content with
     | [ Types.Text "Hello world" ] -> ()
     | _ -> Alcotest.fail "expected [Text]")
;;

let test_parse_openai_response_ollama_reasoning () =
  let json_str =
    {|{
    "id": "chatcmpl_ollama",
    "model": "dashscope-3.5:35b-a3b-nvfp4",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "The answer is 42.",
        "reasoning": "Ollama uses reasoning field instead of reasoning_content."
      }
    }],
    "usage": {"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    check int "2 content blocks (thinking + text)" 2 (List.length resp.content);
    (match resp.content with
     | [ Types.Thinking { content = t; _ }; Types.Text text ] ->
       check
         string
         "reasoning text"
         "Ollama uses reasoning field instead of reasoning_content."
         t;
       check string "content text" "The answer is 42." text;
       (* Token accounting is observation-only: without a provider-reported
          count OAS must not estimate one from text length. *)
       (match resp.telemetry with
        | Some tel ->
          check (option int) "reasoning_tokens absent" None tel.reasoning_tokens
        | None -> Alcotest.fail "telemetry should be present")
     | _ -> Alcotest.fail "expected [Thinking; Text]")
;;

let test_parse_openai_response_reasoning_content_preferred () =
  let json_str =
    {|{
    "id": "chatcmpl_both",
    "model": "dashscope-3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Answer.",
        "reasoning_content": "preferred field",
        "reasoning": "fallback field"
      }
    }],
    "usage": {"prompt_tokens": 5, "completion_tokens": 5, "total_tokens": 10}
  }|}
  in
  match Api.parse_openai_response_result json_str with
  | Error msg ->
    Alcotest.fail
      ("unexpected error: " ^ Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok resp ->
    (match resp.content with
     | [ Types.Thinking { content = t; _ }; Types.Text text ] ->
       check string "reasoning_content wins" "preferred field" t;
       check string "content text" "Answer." text;
       (* Text shape does not synthesize token counts. *)
       (match resp.telemetry with
        | Some tel ->
          check (option int) "reasoning_tokens absent" None tel.reasoning_tokens
        | None -> Alcotest.fail "telemetry should be present")
     | _ -> Alcotest.fail "expected [Thinking; Text]")
;;

(* ------------------------------------------------------------------ *)
(* message_to_json                                                      *)
(* ------------------------------------------------------------------ *)

let test_message_to_json () =
  let msg =
    { Types.role = Types.User
    ; content = [ Types.Text "hi" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "user" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  check int "1 block" 1 (List.length content)
;;

let test_message_to_json_ignores_metadata () =
  let msg =
    { Types.role = Types.Assistant
    ; content = [ Types.Text "visible" ]
    ; name = Some "assistant"
    ; tool_call_id = Some "call_1"
    ; metadata =
        [ ( "replay.namespace"
          , `Assoc
              [ "kind", `String "state_snapshot"
              ; "version", `Int 1
              ; "payload", `Assoc [ "goal", `String "finish" ]
              ] )
        ]
    }
  in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check
    bool
    "metadata omitted from provider payload"
    false
    (List.mem_assoc "metadata" assoc);
  check
    string
    "text still serialized"
    "visible"
    (json |> member "content" |> index 0 |> member "text" |> to_string)
;;

(* ------------------------------------------------------------------ *)
(* build_body_assoc: cache_system_prompt                                *)
(* ------------------------------------------------------------------ *)

let test_build_body_with_cache () =
  (* The caller's opt-in is serialized; Anthropic applies the model/platform
     minimum-token rule server-side. *)
  let long_prompt = "You are a cached helper. " ^ String.make 4100 'x' in
  let config =
    { (Types.default_config ~model:"test-model") with
      system_prompt = Some long_prompt
    ; max_tokens = Some 1024
    ; cache_system_prompt = true
    }
  in
  let state =
    { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }
  in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let system = json |> member "system" in
  let blocks = system |> to_list in
  check int "1 system block" 1 (List.length blocks);
  let block = List.hd blocks in
  check string "block type" "text" (block |> member "type" |> to_string);
  let cc = block |> member "cache_control" in
  check string "cache_control type" "ephemeral" (cc |> member "type" |> to_string)
;;

let test_build_body_tools_cache_control () =
  (* When cache_system_prompt is true, last tool gets cache_control *)
  let config =
    { (Types.default_config ~model:"test-model") with
      system_prompt = Some "You are helpful."
    ; max_tokens = Some 1024
    ; cache_system_prompt = true
    }
  in
  let state =
    { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }
  in
  let tool1 = `Assoc [ "name", `String "t1"; "description", `String "first" ] in
  let tool2 = `Assoc [ "name", `String "t2"; "description", `String "second" ] in
  let assoc =
    Api.build_body_assoc
      ~config:state
      ~messages:[]
      ~tools:[ tool1; tool2 ]
      ~stream:false
      ()
  in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  check int "2 tools" 2 (List.length tools);
  (* First tool has no cache_control *)
  let first = List.hd tools in
  (match first |> member "cache_control" with
   | `Null -> ()
   | _ -> fail "first tool should not have cache_control");
  (* Last tool has cache_control *)
  let last = List.nth tools 1 in
  let cc = last |> member "cache_control" in
  check string "cache_control type" "ephemeral" (cc |> member "type" |> to_string)
;;

let test_build_body_tools_no_cache_without_flag () =
  (* When cache_system_prompt is false, tools have no cache_control *)
  let state = make_state () in
  let tool1 = `Assoc [ "name", `String "t1"; "description", `String "first" ] in
  let assoc =
    Api.build_body_assoc ~config:state ~messages:[] ~tools:[ tool1 ] ~stream:false ()
  in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  let last = List.hd tools in
  match last |> member "cache_control" with
  | `Null -> ()
  | _ -> fail "tools should not have cache_control without flag"
;;

let test_build_body_no_system_prompt () =
  let state =
    { Types.config =
        { (Types.default_config ~model:"test-model") with max_tokens = Some 1024 }
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let has_system = List.exists (fun (k, _) -> k = "system") assoc in
  check bool "no system key" false has_system
;;

(* ------------------------------------------------------------------ *)
(* parse_response: cache tokens in usage                               *)
(* ------------------------------------------------------------------ *)

let test_parse_response_with_cache_tokens () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_cache",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "cached"}],
    "usage": {
      "input_tokens": 50,
      "output_tokens": 25,
      "cache_creation_input_tokens": 1000,
      "cache_read_input_tokens": 800
    }
  }|}
  in
  let resp = Api.parse_response json in
  match resp.usage with
  | Some u ->
    check int "input" 50 u.Types.input_tokens;
    check int "output" 25 u.output_tokens;
    check int "cache_creation" 1000 u.cache_creation_input_tokens;
    check int "cache_read" 800 u.cache_read_input_tokens
  | None -> fail "expected usage"
;;

(* ------------------------------------------------------------------ *)
(* parse_sse_event                                                      *)
(* ------------------------------------------------------------------ *)

let test_parse_sse_message_start () =
  let data =
    {|{"message":{"id":"msg_1","model":"claude-sonnet-4-6","usage":{"input_tokens":10}}}|}
  in
  match Streaming.parse_sse_event (Some "message_start") data with
  | Some (Types.MessageStart { id; model; usage }) ->
    check string "id" "msg_1" id;
    check string "model" "claude-sonnet-4-6" model;
    (match usage with
     | Some u -> check int "input" 10 u.Types.input_tokens
     | None -> fail "expected usage")
  | _ -> fail "expected MessageStart"
;;

let test_parse_sse_content_block_delta_text () =
  let data = {|{"index":0,"delta":{"type":"text_delta","text":"hello"}}|} in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { index; delta = Types.TextDelta t }) ->
    check int "index" 0 index;
    check string "text" "hello" t
  | _ -> fail "expected ContentBlockDelta TextDelta"
;;

let test_parse_sse_content_block_delta_thinking () =
  let data = {|{"index":0,"delta":{"type":"thinking_delta","thinking":"hmm"}}|} in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { delta = Types.ThinkingDelta t; _ }) ->
    check string "thinking" "hmm" t
  | _ -> fail "expected ThinkingDelta"
;;

let test_parse_sse_content_block_delta_input_json () =
  let data =
    {|{"index":1,"delta":{"type":"input_json_delta","partial_json":"{\"x\":1"}}|}
  in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { delta = Types.InputJsonDelta j; _ }) ->
    check string "partial json" {|{"x":1|} j
  | _ -> fail "expected InputJsonDelta"
;;

let test_parse_sse_content_block_start () =
  let data =
    {|{"index":0,"content_block":{"type":"tool_use","id":"tu_1","name":"calc"}}|}
  in
  match Streaming.parse_sse_event (Some "content_block_start") data with
  | Some (Types.ContentBlockStart { index; content_type; tool_id; tool_name }) ->
    check int "index" 0 index;
    check string "type" "tool_use" content_type;
    check (option string) "tool_id" (Some "tu_1") tool_id;
    check (option string) "tool_name" (Some "calc") tool_name
  | _ -> fail "expected ContentBlockStart"
;;

let test_parse_sse_message_delta () =
  let data = {|{"delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":42}}|} in
  match Streaming.parse_sse_event (Some "message_delta") data with
  | Some (Types.MessageDelta { stop_reason; usage }) ->
    (match stop_reason with
     | Some Types.EndTurn -> ()
     | _ -> fail "expected EndTurn");
    (match usage with
     | Some u -> check int "output" 42 u.Types.output_tokens
     | None -> fail "expected usage")
  | _ -> fail "expected MessageDelta"
;;

let test_parse_sse_message_stop () =
  match Streaming.parse_sse_event (Some "message_stop") "{}" with
  | Some Types.MessageStop -> ()
  | _ -> fail "expected MessageStop"
;;

let test_parse_sse_ping () =
  match Streaming.parse_sse_event (Some "ping") "{}" with
  | Some Types.Ping -> ()
  | _ -> fail "expected Ping"
;;

let test_parse_sse_error () =
  let data = {|{"error":{"message":"rate limited","type":"rate_limit_exceeded"}}|} in
  match Streaming.parse_sse_event (Some "error") data with
  | Some (Types.SSEError { message; error_type; _ }) ->
    check string "error msg" "rate limited" message;
    check (option string) "error type" (Some "rate_limit_exceeded") error_type
  | _ -> fail "expected SSEError"
;;

let test_parse_sse_unknown_type () =
  match Streaming.parse_sse_event (Some "future_event") "{}" with
  | Some (Types.SSEUnknownEventType { event_type; raw }) ->
    check string "event_type" "future_event" event_type;
    check string "raw" "{}" raw
  | _ -> fail "expected SSEUnknownEventType for unknown type"
;;

let test_parse_sse_malformed_json () =
  match Streaming.parse_sse_event (Some "message_start") "not json" with
  | Some (Types.SSEParseFailed { raw; reason }) ->
    check string "raw" "not json" raw;
    check bool "reason present" true (String.length reason > 0)
  | _ -> fail "expected SSEParseFailed for malformed JSON"
;;

(* ------------------------------------------------------------------ *)
(* message_to_json: assistant + mixed content                           *)
(* ------------------------------------------------------------------ *)

let test_message_to_json_assistant () =
  let msg =
    { Types.role = Types.Assistant
    ; content =
        [ Types.Text "hi"; Types.ToolUse { id = "t1"; name = "calc"; input = `Null } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "assistant" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  check int "2 blocks" 2 (List.length content)
;;

(* ------------------------------------------------------------------ *)
(* openai_messages_of_message: multimodal user content                  *)
(* ------------------------------------------------------------------ *)

let test_openai_messages_text_only () =
  let msg =
    { Types.role = Types.User
    ; content = [ Types.Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let msgs = Api.openai_messages_of_message msg in
  check int "1 message" 1 (List.length msgs);
  let open Yojson.Safe.Util in
  let content = List.hd msgs |> member "content" in
  (* text-only should be a plain string, not an array *)
  check string "plain string" "hello" (to_string content)
;;

let test_openai_messages_with_image () =
  let msg =
    { Types.role = Types.User
    ; content =
        [ Types.Text "describe this"
        ; Types.Image
            { media_type = "image/png"; data = "abc123"; source_type = Types.Base64 }
        ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let msgs = Api.openai_messages_of_message msg in
  check int "1 message" 1 (List.length msgs);
  let open Yojson.Safe.Util in
  let content = List.hd msgs |> member "content" |> to_list in
  check int "2 content parts" 2 (List.length content);
  let first_type = List.nth content 0 |> member "type" |> to_string in
  let second_type = List.nth content 1 |> member "type" |> to_string in
  check string "text part" "text" first_type;
  check string "image part" "image_url" second_type
;;

(* ------------------------------------------------------------------ *)
(* F1: Openai API error → Openai_api_error exception                    *)
(* ------------------------------------------------------------------ *)

let test_openai_api_error_returns_error () =
  let error_json =
    {|{"error":{"message":"Invalid API key","type":"invalid_request_error"}}|}
  in
  match Api.parse_openai_response_result error_json with
  | Error msg ->
    check
      string
      "error message"
      "Invalid API key"
      (Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok _ -> Alcotest.fail "expected Error on API error"
;;

let test_openai_api_error_unknown_message () =
  let error_json = {|{"error":{}}|} in
  match Api.parse_openai_response_result error_json with
  | Error msg ->
    check
      string
      "unknown error"
      "Unknown API error"
      (Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok _ -> Alcotest.fail "expected Error on empty error"
;;

(* ------------------------------------------------------------------ *)
(* F2: Openai error returns structured Error, not exception              *)
(* ------------------------------------------------------------------ *)

let test_openai_error_returns_result () =
  let error_json = {|{"error":{"message":"bad request"}}|} in
  match Api.parse_openai_response_result error_json with
  | Error msg ->
    check
      string
      "error (Llm_provider.Backend_openai_parse.parse_error_to_string msg)"
      "bad request"
      (Llm_provider.Backend_openai_parse.parse_error_to_string msg)
  | Ok _ -> Alcotest.fail "expected Error result"
;;

(* ------------------------------------------------------------------ *)
(* Phase 6: additional api_common helpers                               *)
(* ------------------------------------------------------------------ *)

let test_text_blocks_to_string () =
  let blocks =
    [ Types.Text "hello"
    ; Types.Thinking { signature = Some "s"; content = "hmm" }
    ; Types.RedactedThinking "r"
    ; Types.ToolUse { id = "t"; name = "n"; input = `Null }
    ; Types.ToolResult
        { tool_use_id = "t"
        ; content = "ok"
        ; outcome = Tool_succeeded
        ; json = None
        ; content_blocks = None
        }
    ; Types.Image { media_type = "image/png"; data = ""; source_type = Types.Base64 }
    ; Types.Text "world"
    ]
  in
  let result = Api.text_blocks_to_string blocks in
  check string "text+thinking" "hello\nhmm\nworld" result
;;

let test_string_is_blank () =
  check bool "empty is blank" true (Api.string_is_blank "");
  check bool "spaces is blank" true (Api.string_is_blank "   ");
  check bool "text not blank" false (Api.string_is_blank "hi");
  check bool "tabs blank" true (Api.string_is_blank "\t\n ")
;;

let test_json_of_string_or_raw_valid () =
  let result = Api.json_of_string_or_raw {|{"key":"value"}|} in
  let open Yojson.Safe.Util in
  check string "parsed" "value" (result |> member "key" |> to_string)
;;

let test_json_of_string_or_raw_invalid () =
  let result = Api.json_of_string_or_raw "not json" in
  let open Yojson.Safe.Util in
  check string "fallback raw" "not json" (result |> member "raw" |> to_string)
;;

let test_build_body_disable_parallel () =
  let config =
    { (Types.default_config ~model:"test-model") with
      tool_choice = Some Types.Auto
    ; max_tokens = Some 1024
    ; disable_parallel_tool_use = true
    }
  in
  let state =
    { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }
  in
  let tool = `Assoc [ "name", `String "calculator" ] in
  let assoc =
    Api.build_body_assoc ~config:state ~messages:[] ~tools:[ tool ] ~stream:false ()
  in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tc = json |> member "tool_choice" in
  check bool "disable_parallel" true (tc |> member "disable_parallel_tool_use" |> to_bool)
;;

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  install_repo_model_catalog ();
  run
    "Api"
    [ ( "content_block_round_trip"
      , [ test_case "text" `Quick test_text_round_trip
        ; test_case "thinking" `Quick test_thinking_round_trip
        ; test_case "redacted_thinking" `Quick test_redacted_thinking_round_trip
        ; test_case "tool_use" `Quick test_tool_use_round_trip
        ; test_case "tool_result" `Quick test_tool_result_round_trip
        ; test_case "tool_result_error" `Quick test_tool_result_error_round_trip
        ; test_case "image" `Quick test_image_round_trip
        ; test_case "document" `Quick test_document_round_trip
        ; test_case "unknown type" `Quick test_unknown_type_returns_none
        ; test_case
            "reasoning_details requires details"
            `Quick
            test_reasoning_details_requires_details_list
        ] )
    ; ( "build_body_assoc"
      , [ test_case "basic" `Quick test_build_body_basic
        ; test_case
            "Anthropic catalog output cap"
            `Quick
            test_build_body_uses_catalog_output_cap
        ; test_case
            "OpenAI unknown-provider output fallback"
            `Quick
            test_build_openai_body_uses_unknown_model_fallback
        ; test_case "with thinking_budget" `Quick test_build_body_with_thinking_budget
        ; test_case
            "enable_thinking rejects unspecified budget"
            `Quick
            test_build_body_with_enable_thinking_rejects_unspecified_budget
        ; test_case
            "disabled thinking rejects reasoning effort"
            `Quick
            test_build_body_disabled_thinking_rejects_reasoning_effort
        ; test_case "adaptive thinking" `Quick test_build_body_adaptive_thinking
        ; test_case
            "adaptive output_config preserves format"
            `Quick
            test_build_body_adaptive_output_config_preserves_format
        ; test_case
            "adaptive numeric budget rejected"
            `Quick
            test_build_body_rejects_adaptive_numeric_budget
        ; test_case "without thinking" `Quick test_build_body_without_thinking
        ; test_case "with tool_choice" `Quick test_build_body_with_tool_choice
        ; test_case "with tools" `Quick test_build_body_with_tools
        ; test_case "openai json schema" `Quick test_build_openai_body_with_json_schema
        ; test_case
            "anthropic sampling params serialized"
            `Quick
            test_build_body_sampling_params_anthropic
        ; test_case
            "anthropic sampling params omitted when None"
            `Quick
            test_build_body_sampling_params_omitted_when_none
        ; test_case
            "with dashscope sampling"
            `Quick
            test_build_openai_body_with_provider_m_sampling
        ; test_case
            "deepseek thinking drops sampling"
            `Quick
            test_build_openai_body_deepseek_thinking_drops_sampling
        ; test_case
            "deepseek disabled thinking keeps sampling"
            `Quick
            test_build_openai_body_deepseek_disabled_thinking_keeps_sampling
        ; test_case
            "tool_choice none serializes for capable provider"
            `Quick
            test_build_openai_body_tool_choice_none_serializes
        ; test_case
            "tool_choice none drops tools without capability"
            `Quick
            test_build_openai_body_tool_choice_none_without_capability_drops_tools
        ; test_case
            "qwen preserve thinking"
            `Quick
            test_build_openai_body_with_qwen_preserve_thinking
        ; test_case
            "qwen preserve replays reasoning"
            `Quick
            test_build_openai_body_qwen_preserve_replays_reasoning
        ; test_case
            "kimi latest omits thinking param"
            `Quick
            test_build_openai_body_kimi_latest_omits_thinking_param
        ; test_case
            "kimi latest replays reasoning"
            `Quick
            test_build_openai_body_kimi_latest_replays_reasoning
        ; test_case
            "deepseek dialect controls"
            `Quick
            test_build_openai_body_deepseek_uses_dialect_controls
        ; test_case
            "minimax public projection dialect controls"
            `Quick
            test_build_openai_body_minimax_uses_public_projection_dialect_controls
        ; test_case
            "deepseek tool reasoning replay"
            `Quick
            test_build_openai_body_deepseek_replays_tool_reasoning_only
        ; test_case
            "generic compat omits dashscope-only fields"
            `Quick
            test_build_openai_body_omits_provider_m_only_fields_for_generic_compat
        ; test_case
            "glm rejects unsupported forced tool choice"
            `Quick
            test_build_openai_body_rejects_glm_forced_tool_choice
        ; test_case
            "bare glm gets no glm dialect"
            `Quick
            test_build_openai_body_bare_glm_gets_no_glm_dialect
        ; test_case
            "bare glm none tool_choice is generic"
            `Quick
            test_build_openai_body_bare_glm_tool_choice_none_is_generic
        ; test_case
            "glm default clear_thinking skips replay"
            `Quick
            test_build_openai_body_glm_default_clear_thinking_skips_replay
        ; test_case
            "glm preserved reasoning replay"
            `Quick
            test_build_openai_body_glm_preserves_reasoning_content
        ; test_case
            "glm clears thinking when not preserving"
            `Quick
            test_build_openai_body_glm_clears_thinking_when_not_preserving
        ; test_case
            "openai compat glm uses openai wire dialect"
            `Quick
            test_build_openai_body_openai_compat_glm_uses_openai_wire_dialect
        ; test_case
            "api thinking builder ssot"
            `Quick
            test_openai_api_uses_backend_thinking_builder_ssot
        ; test_case
            "glm none tool_choice omits tools"
            `Quick
            test_build_openai_body_glm_tool_choice_none_omits_tools
        ; test_case
            "registered glm gets glm dialect"
            `Quick
            test_build_openai_body_registered_glm_gets_glm_dialect
        ; test_case
            "registered glm-coding none tool_choice omits tools"
            `Quick
            test_build_openai_body_registered_glm_coding_tool_choice_none_omits_tools
        ; test_case
            "unknown registered provider fails closed"
            `Quick
            test_build_openai_body_unknown_registered_provider_fails_closed
        ; test_case "with cache_system_prompt" `Quick test_build_body_with_cache
        ; test_case
            "tools cache_control with flag"
            `Quick
            test_build_body_tools_cache_control
        ; test_case
            "tools no cache_control without flag"
            `Quick
            test_build_body_tools_no_cache_without_flag
        ; test_case "no system prompt" `Quick test_build_body_no_system_prompt
        ] )
    ; ( "parse_response"
      , [ test_case "complete response" `Quick test_parse_response_complete
        ; test_case "tool_use response" `Quick test_parse_response_tool_use
        ; test_case "unknown stop_reason" `Quick test_parse_response_unknown_stop
        ; test_case
            "strip fenced json"
            `Quick
            test_parse_openai_response_strips_fenced_json
        ; test_case "cache tokens in usage" `Quick test_parse_response_with_cache_tokens
        ; test_case
            "reasoning_content"
            `Quick
            test_parse_openai_response_reasoning_content
        ; test_case
            "reasoning_content with tools"
            `Quick
            test_parse_openai_response_reasoning_with_tools
        ; test_case
            "blank reasoning_content"
            `Quick
            test_parse_openai_response_blank_reasoning
        ; test_case "no reasoning_content" `Quick test_parse_openai_response_no_reasoning
        ; test_case
            "ollama reasoning field"
            `Quick
            test_parse_openai_response_ollama_reasoning
        ; test_case
            "reasoning_content preferred over reasoning"
            `Quick
            test_parse_openai_response_reasoning_content_preferred
        ] )
    ; ( "error_handling"
      , [ test_case
            "openai api error returns Error"
            `Quick
            test_openai_api_error_returns_error
        ; test_case
            "openai api error unknown message"
            `Quick
            test_openai_api_error_unknown_message
        ; test_case "openai error returns result" `Quick test_openai_error_returns_result
        ] )
    ; ( "parse_sse_event"
      , [ test_case "message_start" `Quick test_parse_sse_message_start
        ; test_case
            "content_block_delta text"
            `Quick
            test_parse_sse_content_block_delta_text
        ; test_case
            "content_block_delta thinking"
            `Quick
            test_parse_sse_content_block_delta_thinking
        ; test_case
            "content_block_delta input_json"
            `Quick
            test_parse_sse_content_block_delta_input_json
        ; test_case "content_block_start" `Quick test_parse_sse_content_block_start
        ; test_case "message_delta" `Quick test_parse_sse_message_delta
        ; test_case "message_stop" `Quick test_parse_sse_message_stop
        ; test_case "ping" `Quick test_parse_sse_ping
        ; test_case "error" `Quick test_parse_sse_error
        ; test_case "unknown event type" `Quick test_parse_sse_unknown_type
        ; test_case "malformed JSON" `Quick test_parse_sse_malformed_json
        ] )
    ; ( "message_to_json"
      , [ test_case "user message" `Quick test_message_to_json
        ; test_case "ignores metadata" `Quick test_message_to_json_ignores_metadata
        ; test_case "assistant mixed content" `Quick test_message_to_json_assistant
        ] )
    ; ( "openai_messages"
      , [ test_case "text only user" `Quick test_openai_messages_text_only
        ; test_case "user with image" `Quick test_openai_messages_with_image
        ] )
    ; ( "api_common_helpers"
      , [ test_case "text_blocks_to_string" `Quick test_text_blocks_to_string
        ; test_case "string_is_blank" `Quick test_string_is_blank
        ; test_case "json_of_string_or_raw valid" `Quick test_json_of_string_or_raw_valid
        ; test_case
            "json_of_string_or_raw invalid"
            `Quick
            test_json_of_string_or_raw_invalid
        ; test_case
            "kimi tool_result uses text blocks"
            `Quick
            test_kimi_message_to_json_tool_result_uses_text_blocks
        ; test_case "disable_parallel_tool_use" `Quick test_build_body_disable_parallel
        ] )
    ]
;;
