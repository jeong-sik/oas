(** Tests for Llm_provider.Complete — request JSON verification. *)

module PC = Llm_provider.Provider_config
module BA = Llm_provider.Backend_anthropic
module BO = Llm_provider.Backend_openai
module BOR = Llm_provider.Backend_openai_responses
module BGlm = Llm_provider.Backend_glm
module BOL = Llm_provider.Backend_ollama
module BGemini = Llm_provider.Backend_gemini
module Artifact = Llm_provider.Provider_request_artifact
open Llm_provider.Types

let () =
  let candidates = [ "models.toml"; "../models.toml" ] in
  match List.find_opt Sys.file_exists candidates with
  | None -> Alcotest.fail "models.toml not found for provider_complete tests"
  | Some path ->
    (match Llm_provider.Model_catalog.load_file path with
     | Ok catalog -> Llm_provider.Model_catalog.set_global catalog
     | Error msg -> Alcotest.failf "failed to load %s: %s" path msg)
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

let catalog_capabilities model_id =
  match Llm_provider.Capabilities.for_model_id model_id with
  | Some caps -> caps
  | None -> Alcotest.failf "expected catalog capabilities for %s" model_id
;;

let capabilities_with_max_output_tokens max_output_tokens =
  { Llm_provider.Capabilities.default_capabilities with max_output_tokens }
;;

let check_receipt
      ~requested
      ~effective
      ~policy
      ~ceiling
      ~ceiling_source
      ~envelope
      (receipt : output_token_receipt)
  =
  Alcotest.(check (option int))
    "receipt requested"
    requested
    (output_token_receipt_requested receipt);
  Alcotest.(check (option int))
    "receipt effective"
    effective
    (output_token_receipt_effective receipt);
  Alcotest.(check bool)
    "receipt policy"
    true
    (output_token_receipt_policy receipt = policy);
  Alcotest.(check (option int))
    "receipt ceiling"
    ceiling
    (output_token_receipt_ceiling receipt);
  Alcotest.(check bool)
    "receipt ceiling source"
    true
    (output_token_receipt_ceiling_source receipt = ceiling_source);
  let json = output_token_receipt_to_yojson receipt in
  let decoded =
    match output_token_receipt_of_yojson json with
    | Ok decoded -> decoded
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check bool)
    "receipt envelope"
    true
    (output_token_receipt_to_yojson decoded = json
     && Yojson.Safe.Util.member "envelope" json = output_token_envelope_to_yojson envelope
    )
;;

let test_output_token_receipt_optional_omission () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"receipt-optional-omission"
      ~base_url:""
      ~model_capabilities_override:(capabilities_with_max_output_tokens (Some 100))
      ()
  in
  let artifact = BO.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check bool)
    "optional max_tokens omitted"
    true
    (Yojson.Safe.Util.member "max_tokens" json = `Null);
  check_receipt
    ~requested:None
    ~effective:None
    ~policy:Omitted
    ~ceiling:(Some 100)
    ~ceiling_source:(Some Declared_capability_override)
    ~envelope:Openai_chat_max_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_explicit_exact () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"receipt-explicit-exact"
      ~base_url:""
      ~max_tokens:80
      ~model_capabilities_override:(capabilities_with_max_output_tokens (Some 100))
      ()
  in
  let artifact = BOR.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "exact max_output_tokens"
    80
    Yojson.Safe.Util.(json |> member "max_output_tokens" |> to_int);
  check_receipt
    ~requested:(Some 80)
    ~effective:(Some 80)
    ~policy:Explicit
    ~ceiling:(Some 100)
    ~ceiling_source:(Some Declared_capability_override)
    ~envelope:Openai_responses_max_output_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_explicit_clamp () =
  let config =
    PC.make
      ~kind:Gemini
      ~model_id:"receipt-explicit-clamp"
      ~base_url:""
      ~max_tokens:200
      ~model_capabilities_override:(capabilities_with_max_output_tokens (Some 100))
      ()
  in
  let artifact =
    BGemini.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] ()
  in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "clamped maxOutputTokens"
    100
    Yojson.Safe.Util.(
      json |> member "generationConfig" |> member "maxOutputTokens" |> to_int);
  check_receipt
    ~requested:(Some 200)
    ~effective:(Some 100)
    ~policy:Explicit_clamped
    ~ceiling:(Some 100)
    ~ceiling_source:(Some Declared_capability_override)
    ~envelope:Gemini_generation_config_max_output_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_anthropic_required_fallback () =
  let model_id = "claude-sonnet-4-6" in
  let ceiling =
    match (catalog_capabilities model_id).max_output_tokens with
    | Some value -> value
    | None -> Alcotest.fail "catalog model must declare max_output_tokens"
  in
  let config = PC.make ~kind:Anthropic ~model_id ~base_url:"" () in
  let artifact = BA.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "required max_tokens fallback"
    ceiling
    Yojson.Safe.Util.(json |> member "max_tokens" |> to_int);
  check_receipt
    ~requested:None
    ~effective:(Some ceiling)
    ~policy:Required_catalog_fallback
    ~ceiling:(Some ceiling)
    ~ceiling_source:(Some Catalog_model)
    ~envelope:Anthropic_messages_max_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_anthropic_zero_cache_prewarm () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~max_tokens:0
      ~cache_system_prompt:true
      ~system_prompt:"cache this system prompt"
      ()
  in
  let artifact =
    BA.build_request_with_receipt ~config ~messages:[ user_msg "warmup" ] ()
  in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "Anthropic cache prewarm max_tokens"
    0
    Yojson.Safe.Util.(json |> member "max_tokens" |> to_int);
  check_receipt
    ~requested:(Some 0)
    ~effective:(Some 0)
    ~policy:Explicit
    ~ceiling:
      (Some
         (match (catalog_capabilities "claude-sonnet-4-6").max_output_tokens with
          | Some value -> value
          | None -> Alcotest.fail "catalog model must declare max_output_tokens"))
    ~ceiling_source:(Some Catalog_model)
    ~envelope:Anthropic_messages_max_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_rejects_negative_requested_value () =
  Alcotest.check_raises
    "negative requested tokens rejected at construction"
    (Invalid_argument
       "optional_output_token_receipt: requested value must be non-negative")
    (fun () ->
       ignore
         (optional_output_token_receipt
            ~envelope:Openai_chat_max_tokens
            ~requested:(Some (-1))
            ~ceiling:None))
;;

let test_output_token_receipt_ollama_envelope () =
  let config =
    PC.make
      ~kind:Ollama
      ~model_id:"receipt-ollama-envelope"
      ~base_url:""
      ~max_tokens:60
      ~model_capabilities_override:(capabilities_with_max_output_tokens (Some 100))
      ()
  in
  let artifact = BOL.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "Ollama num_predict"
    60
    Yojson.Safe.Util.(json |> member "options" |> member "num_predict" |> to_int);
  check_receipt
    ~requested:(Some 60)
    ~effective:(Some 60)
    ~policy:Explicit
    ~ceiling:(Some 100)
    ~ceiling_source:(Some Declared_capability_override)
    ~envelope:Ollama_options_num_predict
    (Artifact.output_token_receipt artifact)
;;

let test_output_token_receipt_anthropic_missing_required_ceiling () =
  let config =
    PC.make ~kind:Anthropic ~model_id:"receipt-required-missing" ~base_url:"" ()
  in
  Alcotest.(check bool)
    "typed missing required ceiling"
    true
    (BA.required_output_token_receipt config
     = Error Required_output_token_catalog_ceiling_missing);
  Alcotest.check_raises
    "builder rejects missing required ceiling"
    (Invalid_argument
       "Backend_anthropic.required_max_output_tokens: model receipt-required-missing \
        declares no max_output_tokens and the caller passed none; the Anthropic Messages \
        API requires max_tokens — declare max_output_tokens in the model catalog or pass \
        ~max_tokens")
    (fun () -> ignore (BA.build_request ~config ~messages:[ user_msg "hi" ] ()))
;;

let test_provider_default_ceiling_does_not_clamp_unknown_model () =
  let config =
    PC.make
      ~kind:Gemini
      ~model_id:"receipt-unknown-gemini"
      ~base_url:""
      ~max_tokens:70_000
      ()
  in
  let artifact =
    BGemini.build_request_with_receipt ~config ~messages:[ user_msg "hi" ] ()
  in
  let json = Yojson.Safe.from_string (Artifact.payload artifact) in
  Alcotest.(check int)
    "unknown model keeps explicit caller value"
    70_000
    Yojson.Safe.Util.(
      json |> member "generationConfig" |> member "maxOutputTokens" |> to_int);
  check_receipt
    ~requested:(Some 70_000)
    ~effective:(Some 70_000)
    ~policy:Explicit
    ~ceiling:None
    ~ceiling_source:None
    ~envelope:Gemini_generation_config_max_output_tokens
    (Artifact.output_token_receipt artifact)
;;

let test_declared_override_is_not_anthropic_catalog_fallback () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"receipt-declared-override-no-fallback"
      ~base_url:""
      ~model_capabilities_override:(capabilities_with_max_output_tokens (Some 100))
      ()
  in
  Alcotest.(check bool)
    "declared override cannot impersonate catalog fallback"
    true
    (BA.required_output_token_receipt config
     = Error Required_output_token_catalog_ceiling_missing)
;;

(* ── Anthropic build_request ─────────────────────────── *)

let test_anthropic_basic_body () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:"https://api.anthropic.com"
      ~max_tokens:1024
      ()
  in
  let msgs = [ user_msg "hello" ] in
  let body = BA.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "claude-sonnet-4-6" (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 1024 (json |> member "max_tokens" |> to_int);
  Alcotest.(check bool) "stream false" false (json |> member "stream" |> to_bool);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)
;;

let test_anthropic_with_system () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~system_prompt:"You are helpful."
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "system"
    "You are helpful."
    (json |> member "system" |> to_string)
;;

let test_anthropic_with_thinking () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~enable_thinking:true
      ~thinking_budget:5000
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "think" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  Alcotest.(check string)
    "thinking type"
    "adaptive"
    (thinking |> member "type" |> to_string);
  Alcotest.(check bool)
    "budget_tokens omitted"
    true
    (thinking |> member "budget_tokens" = `Null);
  Alcotest.(check string)
    "effort"
    "medium"
    (json |> member "output_config" |> member "effort" |> to_string)
;;

let test_anthropic_disabled_thinking_omits_adaptive_effort () =
  (* A turn hook may disable thinking while a thinking_budget is still set. The
     adaptive effort must be gated on thinking being enabled, otherwise
     output_config.effort leaks without an accompanying thinking block. *)
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~enable_thinking:false
      ~thinking_budget:5000
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "thinking omitted" true (json |> member "thinking" = `Null);
  (* With thinking disabled and no output schema, output_config carries no
     fields, so the whole object is omitted rather than leaking a bare effort. *)
  Alcotest.(check bool)
    "output_config omitted (no leaked effort)"
    true
    (json |> member "output_config" = `Null)
;;

let test_anthropic_sonnet5_explicit_disable_is_serialized () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-5"
      ~base_url:""
      ~enable_thinking:false
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "Sonnet 5 explicit disable uses the provider disabled mode"
    "disabled"
    (json |> member "thinking" |> member "type" |> to_string)
;;

let test_anthropic_thinking_forced_tool_choice_rejected_before_request () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:"https://api.anthropic.com"
      ~enable_thinking:true
      ~tool_choice:Any
      ()
  in
  Alcotest.check_raises
    "thinking + forced tool_choice fails before JSON body serialization"
    (Invalid_argument
       "Backend_anthropic.build_request: anthropic model \"claude-sonnet-4-6\" does not \
        support required forced tool_choice when thinking is enabled; use auto/none or \
        disable thinking")
    (fun () ->
       ignore (BA.build_request ~config ~messages:[ user_msg "think with a tool" ] ()))
;;

let test_anthropic_stream_flag () =
  let config = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" ~max_tokens:128 () in
  let body = BA.build_request ~stream:true ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true (json |> member "stream" |> to_bool)
;;

let test_anthropic_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~output_schema:schema
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "output_config type"
    "json_schema"
    (json |> member "output_config" |> member "format" |> member "type" |> to_string);
  Alcotest.(check bool)
    "schema copied"
    true
    (json |> member "output_config" |> member "format" |> member "schema" = schema)
;;

let test_anthropic_json_schema_response_format_without_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    { (PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6" ~base_url:"" ()) with
      response_format = JsonSchema schema
    ; output_schema = None
    }
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "schema copied from response_format"
    true
    (json |> member "output_config" |> member "format" |> member "schema" = schema)
;;

let test_anthropic_build_request_preserves_multiturn_thinking_tool_order () =
  let msg role content = make_message ~role content in
  let signed_thinking signature content =
    Thinking { signature = Some signature; content }
  in
  let tool_call id city =
    ToolUse { id; name = "lookup_weather"; input = `Assoc [ "city", `String city ] }
  in
  let tool_result id content =
    ToolResult
      { tool_use_id = id
      ; content
      ; is_error = false
      ; json = Some (`Assoc [ "content", `String content ])
      ; content_blocks = None
      }
  in
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:"https://api.anthropic.com"
      ~max_tokens:1024
      ()
  in
  let body =
    BA.build_request
      ~config
      ~messages:
        [ msg User [ Text "User message 1" ]
        ; msg
            Assistant
            [ signed_thinking "sig_1_1" "Thinking 1.1"; tool_call "call_1_1" "Seoul" ]
        ; msg Tool [ tool_result "call_1_1" "Tool result 1.1" ]
        ; msg
            Assistant
            [ signed_thinking "sig_1_2" "Thinking 1.2"; tool_call "call_1_2" "Busan" ]
        ; msg Tool [ tool_result "call_1_2" "Tool result 1.2" ]
        ; msg Assistant [ signed_thinking "sig_1_3" "Thinking 1.3"; Text "Answer 1" ]
        ; msg User [ Text "User message 2" ]
        ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let require_string_field key json =
    match member key json with
    | `String value -> value
    | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
      Alcotest.failf "expected string field %s in %s" key (Yojson.Safe.to_string json)
  in
  let marker message =
    let role = require_string_field "role" message in
    message
    |> member "content"
    |> to_list
    |> List.map (fun block ->
      match require_string_field "type" block with
      | "text" -> role ^ ":text:" ^ require_string_field "text" block
      | "thinking" ->
        role
        ^ ":thinking:"
        ^ require_string_field "signature" block
        ^ ":"
        ^ require_string_field "thinking" block
      | "tool_use" -> role ^ ":tool_use:" ^ require_string_field "id" block
      | "tool_result" -> role ^ ":tool_result:" ^ require_string_field "tool_use_id" block
      | other -> role ^ ":" ^ other)
  in
  let markers = body |> member "messages" |> to_list |> List.concat_map marker in
  Alcotest.(check (list string))
    "Anthropic Turn 2.1 input keeps signed thinking/tool groups in order"
    [ "user:text:User message 1"
    ; "assistant:thinking:sig_1_1:Thinking 1.1"
    ; "assistant:tool_use:call_1_1"
    ; "user:tool_result:call_1_1"
    ; "assistant:thinking:sig_1_2:Thinking 1.2"
    ; "assistant:tool_use:call_1_2"
    ; "user:tool_result:call_1_2"
    ; "assistant:thinking:sig_1_3:Thinking 1.3"
    ; "assistant:text:Answer 1"
    ; "user:text:User message 2"
    ]
    markers
;;

let test_anthropic_parse_response_initializes_telemetry () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_test",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|}
  in
  let resp = BA.parse_response json in
  match resp.telemetry with
  | Some t ->
    Alcotest.(check (option int))
      "request_latency_ms defaults to unknown"
      None
      t.request_latency_ms;
    let provider_kind_t = Alcotest.testable PC.pp_provider_kind ( = ) in
    Alcotest.(check (option provider_kind_t))
      "provider_kind placeholder"
      None
      t.provider_kind;
    Alcotest.(check (option string))
      "canonical model placeholder"
      None
      t.canonical_model_id
  | None -> Alcotest.fail "expected telemetry placeholder"
;;

let test_anthropic_parse_response_rejects_unknown_content_block () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_future",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "future_block", "payload": {"text": "do not drop me"}}
    ],
    "usage": {"input_tokens": 1, "output_tokens": 1}
  }|}
  in
  Alcotest.check_raises
    "unknown Anthropic content block fails closed"
    (Invalid_argument
       "Backend_anthropic.parse_response: unsupported_content_block_type:future_block")
    (fun () -> ignore (BA.parse_response json))
;;

let test_anthropic_parse_response_rejects_unknown_media_source_kind () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_future_media",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {
        "type": "image",
        "source": {
          "type": "bytes",
          "media_type": "image/png",
          "data": "abc"
        }
      }
    ],
    "usage": {"input_tokens": 1, "output_tokens": 1}
  }|}
  in
  Alcotest.check_raises
    "unknown Anthropic media source kind fails closed"
    (Invalid_argument
       "Backend_anthropic.parse_response: unsupported_media_source_kind:image:bytes")
    (fun () -> ignore (BA.parse_response json))
;;

(* ── Openai build_request ────────────────────────────── *)

let test_openai_basic_body () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4"
      ~base_url:"https://api.openai.com/v1"
      ~max_tokens:2048
      ()
  in
  let msgs = [ user_msg "hello" ] in
  let body = BO.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "gpt-4" (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 2048 (json |> member "max_tokens" |> to_int);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)
;;

let test_openai_with_system () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4"
      ~base_url:""
      ~system_prompt:"Be helpful."
      ()
  in
  let body = BO.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let msgs = json |> member "messages" |> to_list in
  (* First message should be system *)
  let first = List.hd msgs in
  Alcotest.(check string) "system role" "system" (first |> member "role" |> to_string);
  Alcotest.(check string)
    "system content"
    "Be helpful."
    (first |> member "content" |> to_string)
;;

let test_openai_with_tools () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"gpt-4" ~base_url:"" () in
  let tool =
    `Assoc
      [ "name", `String "calc"
      ; "description", `String "calculator"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let body =
    BO.build_request ~config ~messages:[ user_msg "add 1+1" ] ~tools:[ tool ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "1 tool" 1 (List.length tools)
;;

let openai_responses_config () =
  PC.make
    ~kind:OpenAI_compat
    ~model_id:"gpt-5.5"
    ~base_url:"https://api.openai.com/v1"
    ~request_path:"/v1/responses"
    ()
;;

let test_openai_responses_replays_assistant_phase_metadata () =
  let config = openai_responses_config () in
  let messages =
    [ { role = User
      ; content = [ Text "continue" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = [ BOR.response_phase_metadata BOR.Final_answer ]
      }
    ; { role = Assistant
      ; content = [ Text "I will check." ]
      ; name = None
      ; tool_call_id = None
      ; metadata = [ BOR.response_phase_metadata BOR.Commentary ]
      }
    ]
  in
  let body = BOR.build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let input = json |> member "input" |> to_list in
  let user_item = List.nth input 0 in
  let assistant_item = List.nth input 1 in
  Alcotest.(check bool) "user phase omitted" true (user_item |> member "phase" = `Null);
  Alcotest.(check string)
    "assistant type"
    "message"
    (assistant_item |> member "type" |> to_string);
  Alcotest.(check string)
    "assistant phase"
    "commentary"
    (assistant_item |> member "phase" |> to_string)
;;

let unsupported_openai_responses_phase_message phase =
  Printf.sprintf
    "Backend_openai_responses.phase: unsupported %s=%S"
    BOR.response_phase_metadata_key
    phase
;;

let check_openai_responses_phase_metadata_rejected ~name ~metadata ~message =
  let config = openai_responses_config () in
  let messages =
    [ { role = Assistant
      ; content = [ Text "drafting" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = [ metadata ]
      }
    ]
  in
  Alcotest.check_raises name (Invalid_argument message) (fun () ->
    ignore (BOR.build_request ~config ~messages ()))
;;

let check_openai_responses_phase_rejected ~name ~phase =
  check_openai_responses_phase_metadata_rejected
    ~name
    ~metadata:(BOR.response_phase_metadata_key, `String phase)
    ~message:(unsupported_openai_responses_phase_message phase)
;;

let test_openai_responses_rejects_unknown_assistant_phase () =
  check_openai_responses_phase_rejected
    ~name:"unknown Responses phase rejected"
    ~phase:"analysis"
;;

let test_openai_responses_rejects_padded_assistant_phase () =
  check_openai_responses_phase_rejected
    ~name:"padded Responses phase rejected"
    ~phase:" commentary "
;;

let test_openai_responses_rejects_blank_assistant_phase () =
  check_openai_responses_phase_rejected
    ~name:"blank Responses phase rejected"
    ~phase:"   "
;;

let test_openai_responses_rejects_non_string_assistant_phase () =
  check_openai_responses_phase_metadata_rejected
    ~name:"non-string Responses phase rejected"
    ~metadata:(BOR.response_phase_metadata_key, `Int 1)
    ~message:"Backend_openai_responses.phase: openai.responses.phase must be a string"
;;

let test_openai_stream_flag () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  let body = BO.build_request ~stream:true ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true (json |> member "stream" |> to_bool)
;;

let test_ollama_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Ollama
      ~model_id:"dashscope-3.5:9b"
      ~base_url:"http://localhost:11434"
      ~output_schema:schema
      ()
  in
  let body = BOL.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "format copied" true (json |> member "format" = schema)
;;

let test_ollama_gemma4_thinking_uses_template_token () =
  let config =
    PC.make
      ~kind:Ollama
      ~model_id:"hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL"
      ~base_url:"http://localhost:11434"
      ~system_prompt:"You are a helpful assistant."
      ~enable_thinking:true
      ()
  in
  let body = BOL.build_request ~config ~messages:[ user_msg "solve 19*21" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "omits top-level think for Gemma 4"
    true
    (json |> member "think" = `Null);
  let first_message = json |> member "messages" |> index 0 in
  Alcotest.(check string)
    "system role"
    "system"
    (first_message |> member "role" |> to_string);
  Alcotest.(check bool)
    "system prompt starts with Gemma 4 think token"
    true
    (String.starts_with
       ~prefix:"<|think|>\n"
       (first_message |> member "content" |> to_string))
;;

let test_ollama_native_multimodal_request_body () =
  (* Ollama native /api/chat rejects an array-valued content field and expects
     images in a separate images array of base64 payloads. *)
  let config =
    PC.make ~kind:Ollama ~model_id:"gemma4:9b" ~base_url:"http://localhost:11434" ()
  in
  let messages =
    [ { role = User
      ; content =
          [ Text "What is in this image?"
          ; Image { media_type = "image/png"; data = "base64img"; source_type = Base64 }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = BOL.build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let user_msg = json |> member "messages" |> index 0 in
  Alcotest.(check string) "role" "user" (user_msg |> member "role" |> to_string);
  Alcotest.(check string)
    "content is plain string"
    "What is in this image?"
    (user_msg |> member "content" |> to_string);
  let images = user_msg |> member "images" |> to_list in
  Alcotest.(check int) "images count" 1 (List.length images);
  Alcotest.(check string) "image payload" "base64img" (List.nth images 0 |> to_string);
  (* Verify the overall body still carries model/stream/keep_alive as before. *)
  Alcotest.(check string) "model" "gemma4:9b" (json |> member "model" |> to_string);
  Alcotest.(check bool) "stream" false (json |> member "stream" |> to_bool)
;;

let test_ollama_parse_parallel_tool_calls_object_arguments () =
  let body =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"function":{"index":0,"name":"get_temperature","arguments":{"city":"New York"}}},
           {"function":{"index":1,"name":"get_conditions","arguments":{"city":"London"}}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg -> Alcotest.fail msg
  | Ok resp ->
    Alcotest.(check bool) "stop tool use" true (resp.stop_reason = StopToolUse);
    (match resp.content with
     | [ ToolUse first; ToolUse second ] ->
       Alcotest.(check string) "first name" "get_temperature" first.name;
       Alcotest.(check string) "second name" "get_conditions" second.name;
       Alcotest.(check bool) "distinct synthetic ids" true (first.id <> second.id);
       Alcotest.(check bool)
         "first input object"
         true
         (first.input = `Assoc [ "city", `String "New York" ]);
       Alcotest.(check bool)
         "second input object"
         true
         (second.input = `Assoc [ "city", `String "London" ])
     | _ -> Alcotest.fail "expected two ToolUse blocks")
;;

let test_ollama_parse_tool_call_preserves_explicit_id_and_string_arguments () =
  let body =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"id":"call_explicit","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg -> Alcotest.fail msg
  | Ok resp ->
    (match resp.content with
     | [ ToolUse tool_use ] ->
       Alcotest.(check string) "id" "call_explicit" tool_use.id;
       Alcotest.(check string) "name" "get_weather" tool_use.name;
       Alcotest.(check bool)
         "input"
         true
         (tool_use.input = `Assoc [ "city", `String "Seoul" ])
     | _ -> Alcotest.fail "expected one ToolUse block")
;;

let test_ollama_parse_rejects_malformed_tool_call () =
  let body =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"function":{"name":"ok_tool","arguments":{"city":"Seoul"}}},
           {"function":{"arguments":{"city":"Missing name"}}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg ->
    Alcotest.(check string)
      "malformed tool call rejected"
      "malformed_ollama_tool_call:index:1:missing_name"
      msg
  | Ok _ -> Alcotest.fail "expected malformed Ollama tool_call to fail closed"
;;

let test_ollama_parse_rejects_non_object_tool_arguments () =
  let body =
    {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"function":{"name":"get_weather","arguments":"42"}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg ->
    Alcotest.(check string)
      "non-object arguments rejected"
      "malformed_ollama_tool_call_arguments:index:0:not_object"
      msg
  | Ok _ -> Alcotest.fail "expected non-object Ollama tool arguments to fail closed"
;;

let test_openai_with_json_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-mini"
      ~base_url:"https://api.openai.com/v1"
      ~response_format:(JsonSchema schema)
      ()
  in
  let body = BO.build_request ~config ~messages:[ user_msg "Return JSON." ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let response_format = json |> member "response_format" in
  Alcotest.(check string)
    "response_format.type"
    "json_schema"
    (response_format |> member "type" |> to_string);
  Alcotest.(check string)
    "json_schema.name"
    "structured_output"
    (response_format |> member "json_schema" |> member "name" |> to_string);
  Alcotest.(check string)
    "json_schema.schema.type"
    "object"
    (response_format
     |> member "json_schema"
     |> member "schema"
     |> member "type"
     |> to_string)
;;

let test_gemini_with_json_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Gemini
      ~model_id:"gemini-2.5-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ~api_key:"test-key"
      ~response_format:(JsonSchema schema)
      ()
  in
  let body = BGemini.build_request ~config ~messages:[ user_msg "Return JSON." ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let generation_config = json |> member "generationConfig" in
  Alcotest.(check string)
    "responseMimeType"
    "application/json"
    (generation_config |> member "responseMimeType" |> to_string);
  Alcotest.(check string)
    "responseJsonSchema.type"
    "object"
    (generation_config |> member "responseJsonSchema" |> member "type" |> to_string);
  Alcotest.(check string)
    "responseJsonSchema.required[0]"
    "answer"
    (generation_config
     |> member "responseJsonSchema"
     |> member "required"
     |> to_list
     |> List.hd
     |> to_string)
;;

let test_kimi_direct_with_tools_and_thinking () =
  let config =
    PC.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ~enable_thinking:true
      ()
  in
  let tool =
    `Assoc
      [ "name", `String "shell"
      ; "description", `String "run shell command"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let body =
    BA.build_request ~config ~messages:[ user_msg "inspect repo" ] ~tools:[ tool ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  let thinking = json |> member "thinking" in
  Alcotest.(check string) "model" "kimi-for-coding" (json |> member "model" |> to_string);
  Alcotest.(check int) "tool count" 1 (List.length tools);
  Alcotest.(check string)
    "thinking type"
    "enabled"
    (thinking |> member "type" |> to_string)
;;

let test_kimi_direct_tool_result_uses_text_blocks () =
  let config =
    PC.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking
              { signature = Some "sig_1"; content = "I should call the calculator." }
          ; ToolUse
              { id = "tool_1"
              ; name = "calculator"
              ; input = `Assoc [ "a", `Int 2; "b", `Int 3 ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "tool_1"
              ; content = "5"
              ; is_error = false
              ; json = Some (`Int 5)
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = BA.build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let replay = json |> member "messages" |> index 1 in
  let block = replay |> member "content" |> index 0 in
  Alcotest.(check string)
    "tool result role serialized as user"
    "user"
    (replay |> member "role" |> to_string);
  Alcotest.(check string)
    "tool_result type"
    "tool_result"
    (block |> member "type" |> to_string);
  Alcotest.(check string)
    "tool_use id preserved"
    "tool_1"
    (block |> member "tool_use_id" |> to_string);
  Alcotest.(check string)
    "tool_result content"
    "5"
    (block |> member "content" |> to_string)
;;

let test_glm_preserved_reasoning_replay_and_preserves_auto_tool_choice () =
  let config =
    PC.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ~clear_thinking:false
      ~tool_stream:true
      ~tool_choice:Auto
      ~supports_tool_choice_override:true
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking { signature = None; content = "I need the calculator result." }
          ; ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "call_1"
              ; content = "{\"value\":4}"
              ; is_error = false
              ; json = Some (`Assoc [ "value", `Int 4 ])
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = BGlm.build_request ~stream:true ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  Alcotest.(check string)
    "glm auto tool_choice preserved"
    "auto"
    (json |> member "tool_choice" |> to_string);
  Alcotest.(check string)
    "assistant content remains text channel"
    ""
    (assistant |> member "content" |> to_string);
  Alcotest.(check string)
    "reasoning replayed separately"
    "I need the calculator result."
    (assistant |> member "reasoning_content" |> to_string);
  Alcotest.(check bool)
    "clear_thinking false preserved"
    true
    (json |> member "thinking" |> member "clear_thinking" |> to_bool = false);
  Alcotest.(check bool)
    "tool_stream enabled"
    true
    (json |> member "tool_stream" |> to_bool)
;;

let test_complete_rejects_glm_named_forced_tool_choice_before_request () =
  let config =
    PC.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ~tool_choice:(Tool "calculator")
      ()
  in
  let assert_rejected label result =
    match result with
    | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
      Alcotest.(check bool)
        label
        true
        (contains_substring ~sub:"tool_choice" reason
         && contains_substring ~sub:"calculator" reason)
    | Ok _ -> Alcotest.failf "%s: expected AcceptRejected" label
    | Error _ -> Alcotest.failf "%s: expected AcceptRejected" label
  in
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  assert_rejected
    "sync completion rejects before request"
    (Llm_provider.Complete.complete ~sw ~net ~config ~messages:[] ());
  assert_rejected
    "stream completion rejects before request"
    (Llm_provider.Complete.complete_stream
       ~sw
       ~net
       ~config
       ~messages:[]
       ~on_event:(fun _ -> ())
       ())
;;

(* ── Provider_config.make ────────────────────────────── *)

let test_config_default_paths () =
  let anth = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "anthropic path" "/v1/messages" anth.request_path;
  let kimi = PC.make ~kind:Kimi ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "kimi path" "/v1/messages" kimi.request_path;
  let oai = PC.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "openai path" "/v1/chat/completions" oai.request_path
;;

let test_config_custom_path () =
  let cfg =
    PC.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" ~request_path:"/custom" ()
  in
  Alcotest.(check string) "custom path" "/custom" cfg.request_path
;;

(* ── Retry config ────────────────────────────────────── *)

let test_default_retry_config () =
  let cfg = Llm_provider.Complete.default_retry_config in
  Alcotest.(check int) "max_retries" 3 cfg.max_retries;
  Alcotest.(check (float 0.01)) "initial_delay" 1.0 cfg.initial_delay_sec;
  Alcotest.(check (float 0.01)) "max_delay" 30.0 cfg.max_delay_sec;
  Alcotest.(check (float 0.01)) "backoff" 2.0 cfg.backoff_multiplier
;;

let test_is_retryable () =
  let open Llm_provider in
  (* Retryable status codes *)
  Alcotest.(check bool)
    "429 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }));
  Alcotest.(check bool)
    "503 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }));
  Alcotest.(check bool)
    "529 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }));
  (* Network errors *)
  Alcotest.(check bool)
    "network retryable"
    true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "timeout"; kind = Unknown }));
  (* Non-retryable *)
  Alcotest.(check bool)
    "400 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }));
  Alcotest.(check bool)
    "401 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }));
  Alcotest.(check bool)
    "404 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }))
;;

let usage =
  Some
    { input_tokens = 1
    ; output_tokens = 1
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
;;

let fake_transport response : Llm_provider.Llm_transport.t =
  { complete_sync = (fun _request -> { response = Ok response; latency_ms = Some 1 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _request -> Ok response)
  }
;;

let complete_with_captured_diag ~config ~response =
  let entries = ref [] in
  let run () =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let transport = fake_transport response in
    match
      Llm_provider.Complete.complete
        ~sw
        ~net
        ~config
        ~messages:[ user_msg "hi" ]
        ~transport
        ()
    with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "fake completion should succeed"
  in
  Llm_provider.Diag.with_sink
    (fun level ~ctx message -> entries := (level, ctx, message) :: !entries)
    run;
  List.rev !entries
;;

let response_with_thinking =
  { id = "resp-thinking"
  ; model = "auto"
  ; stop_reason = EndTurn
  ; content = [ Thinking { signature = None; content = "reasoning" } ]
  ; usage
  ; telemetry = None
  }
;;

let test_provider_default_thinking_drift_is_info () =
  let config =
    PC.make ~kind:OpenAI_compat ~model_id:"auto" ~base_url:"https://example.invalid/v1" ()
  in
  let entries = complete_with_captured_diag ~config ~response:response_with_thinking in
  Alcotest.(check bool)
    "no warn for provider-default thinking observation"
    false
    (List.exists (fun (level, _, _) -> level = Llm_provider.Diag.Warn) entries);
  Alcotest.(check bool)
    "low-confidence info is recorded"
    true
    (List.exists
       (fun (level, ctx, message) ->
          level = Llm_provider.Diag.Info
          && ctx = "complete"
          && contains_substring ~sub:"capability_observation" message
          && contains_substring ~sub:"provider_default" message
          && contains_substring ~sub:"low" message)
       entries)
;;

let test_model_capability_thinking_drift_remains_warn () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"glm-4-flash"
      ~base_url:"https://declared-openai-compat.example/v1"
      ~model_capabilities_override:(catalog_capabilities "glm-4-flash")
      ()
  in
  let entries = complete_with_captured_diag ~config ~response:response_with_thinking in
  Alcotest.(check bool)
    "model-specific mismatch remains warn"
    true
    (List.exists
       (fun (level, ctx, message) ->
          level = Llm_provider.Diag.Warn
          && ctx = "complete"
          && contains_substring ~sub:"capability_drift" message
          && contains_substring ~sub:"model" message
          && contains_substring ~sub:"high" message)
       entries)
;;

let test_declared_glm_model_thinking_uses_model_capability () =
  let config =
    PC.make
      ~kind:OpenAI_compat
      ~model_id:"glm-5"
      ~base_url:"https://declared-zai-openai-compat.example/v1"
      ~model_capabilities_override:(catalog_capabilities "glm-5")
      ()
  in
  let entries = complete_with_captured_diag ~config ~response:response_with_thinking in
  Alcotest.(check bool)
    "declared glm-5 thinking does not emit capability drift"
    false
    (List.exists
       (fun (_level, ctx, message) ->
          ctx = "complete"
          && (contains_substring ~sub:"capability_observation" message
              || contains_substring ~sub:"capability_drift" message))
       entries)
;;

let test_complete_rejects_output_schema_for_glm () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let config =
    PC.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match
    Llm_provider.Complete.complete ~sw ~net ~config ~messages:[ user_msg "hi" ] ()
  with
  | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
    Alcotest.(check bool)
      "mentions glm json mode"
      true
      (contains_substring ~sub:"json mode" (String.lowercase_ascii reason))
  | Ok _ -> Alcotest.fail "expected AcceptRejected for glm output_schema"
  | Error _ -> Alcotest.fail "expected AcceptRejected for glm output_schema"
;;

let test_annotate_response_cost () =
  let response : api_response =
    { id = "resp-1"
    ; model = "claude-sonnet-4-6"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage =
        Some
          { input_tokens = 1_000
          ; output_tokens = 500
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    ; telemetry = None
    }
  in
  match Llm_provider.Pricing.annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } ->
    Alcotest.(check bool) "annotated cost" true (cost > 0.0)
  | _ -> Alcotest.fail "expected annotated response cost"
;;

let test_annotate_response_cost_gpt55 () =
  let response : api_response =
    { id = "resp-gpt55"
    ; model = "gpt-5.5"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage =
        Some
          { input_tokens = 1_000_000
          ; output_tokens = 1_000_000
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    ; telemetry = None
    }
  in
  match Llm_provider.Pricing.annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } ->
    Alcotest.(check (float 0.001)) "gpt-5.5 cost" 35.0 cost
  | _ -> Alcotest.fail "expected gpt-5.5 annotated response cost"
;;

(* ── Stream accumulator ──────────────────────────────── *)

let test_stream_acc_text () =
  (* Simulate a minimal Anthropic SSE event sequence *)
  let events =
    [ MessageStart
        { id = "msg_123"
        ; model = "claude-sonnet-4-6"
        ; usage =
            Some
              { input_tokens = 10
              ; output_tokens = 0
              ; cache_creation_input_tokens = 0
              ; cache_read_input_tokens = 0
              ; cost_usd = None
              }
        }
    ; ContentBlockStart
        { index = 0; content_type = "text"; tool_id = None; tool_name = None }
    ; ContentBlockDelta { index = 0; delta = TextDelta "Hello " }
    ; ContentBlockDelta { index = 0; delta = TextDelta "world" }
    ; ContentBlockStop { index = 0 }
    ; MessageDelta
        { stop_reason = Some EndTurn
        ; usage =
            Some
              { input_tokens = 0
              ; output_tokens = 5
              ; cache_creation_input_tokens = 0
              ; cache_read_input_tokens = 0
              ; cost_usd = None
              }
        }
    ; MessageStop
    ]
  in
  (* Use the internal accumulator via a module alias *)
  let module C = Llm_provider.Complete in
  ignore C.default_retry_config;
  (* force link *)
  (* We can't call the internal functions directly, but we can test
     that the event types compose correctly *)
  Alcotest.(check int) "7 events" 7 (List.length events)
;;

let test_stream_acc_tool_use () =
  let events =
    [ MessageStart { id = "msg_456"; model = "gpt-4"; usage = None }
    ; ContentBlockStart
        { index = 0
        ; content_type = "tool_use"
        ; tool_id = Some "tu_1"
        ; tool_name = Some "calc"
        }
    ; ContentBlockDelta { index = 0; delta = InputJsonDelta "{\"x\":1}" }
    ; ContentBlockStop { index = 0 }
    ; MessageDelta { stop_reason = Some StopToolUse; usage = None }
    ; MessageStop
    ]
  in
  Alcotest.(check int) "6 events" 6 (List.length events)
;;

(* ── Prompt caching ───────────────────────────────── *)

(* A long prompt exercises the explicit cache breakpoint serialization. *)
let long_prompt =
  String.concat
    ""
    (List.init 200 (fun i ->
       Printf.sprintf "Rule %d: follow this guideline carefully. " i))
;;

let test_cache_system_prompt () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:""
      ~system_prompt:long_prompt
      ~cache_system_prompt:true
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let system = json |> member "system" |> to_list in
  Alcotest.(check int) "1 system block" 1 (List.length system);
  let block = List.hd system in
  Alcotest.(check string) "type" "text" (block |> member "type" |> to_string);
  let cc = block |> member "cache_control" in
  Alcotest.(check string)
    "cache_control type"
    "ephemeral"
    (cc |> member "type" |> to_string)
;;

let test_cache_short_prompt_preserves_opt_in () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:""
      ~max_tokens:128
      ~system_prompt:"Short."
      ~cache_system_prompt:true
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let system = json |> member "system" |> to_list in
  Alcotest.(check int) "1 system block" 1 (List.length system);
  let block = List.hd system in
  Alcotest.(check string) "type" "text" (block |> member "type" |> to_string);
  Alcotest.(check string)
    "cache_control type"
    "ephemeral"
    (block |> member "cache_control" |> member "type" |> to_string)
;;

let test_cache_no_system_no_cache () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:""
      ~max_tokens:128
      ~system_prompt:"Hello."
      ~cache_system_prompt:false
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  (* system should be a plain string when caching disabled *)
  Alcotest.(check string)
    "system is string"
    "Hello."
    (json |> member "system" |> to_string)
;;

let test_cache_tools () =
  let config =
    PC.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:""
      ~max_tokens:128
      ~cache_system_prompt:true
      ()
  in
  let tool1 = `Assoc [ "name", `String "a"; "description", `String "tool a" ] in
  let tool2 = `Assoc [ "name", `String "b"; "description", `String "tool b" ] in
  let body =
    BA.build_request ~config ~messages:[ user_msg "hi" ] ~tools:[ tool1; tool2 ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "2 tools" 2 (List.length tools);
  (* First tool should NOT have cache_control *)
  let first = List.hd tools in
  Alcotest.(check bool)
    "first tool no cache"
    true
    (first |> member "cache_control" = `Null);
  (* Last tool SHOULD have cache_control *)
  let last = List.nth tools 1 in
  let cc = last |> member "cache_control" in
  Alcotest.(check string)
    "last tool cache_control"
    "ephemeral"
    (cc |> member "type" |> to_string)
;;

let test_cache_default_false () =
  let cfg = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  Alcotest.(check bool) "default cache off" false cfg.cache_system_prompt
;;

let () =
  let open Alcotest in
  run
    "provider_complete"
    [ ( "output_token_receipt"
      , [ test_case "optional omission" `Quick test_output_token_receipt_optional_omission
        ; test_case "explicit exact" `Quick test_output_token_receipt_explicit_exact
        ; test_case "explicit clamp" `Quick test_output_token_receipt_explicit_clamp
        ; test_case "Ollama envelope" `Quick test_output_token_receipt_ollama_envelope
        ; test_case
            "Anthropic required fallback"
            `Quick
            test_output_token_receipt_anthropic_required_fallback
        ; test_case
            "Anthropic zero-token cache prewarm"
            `Quick
            test_output_token_receipt_anthropic_zero_cache_prewarm
        ; test_case
            "negative requested tokens rejected"
            `Quick
            test_output_token_receipt_rejects_negative_requested_value
        ; test_case
            "Anthropic missing required ceiling"
            `Quick
            test_output_token_receipt_anthropic_missing_required_ceiling
        ; test_case
            "provider default does not clamp unknown model"
            `Quick
            test_provider_default_ceiling_does_not_clamp_unknown_model
        ; test_case
            "declared override is not catalog fallback"
            `Quick
            test_declared_override_is_not_anthropic_catalog_fallback
        ] )
    ; ( "anthropic_build_request"
      , [ test_case "basic body" `Quick test_anthropic_basic_body
        ; test_case "with system" `Quick test_anthropic_with_system
        ; test_case "with thinking" `Quick test_anthropic_with_thinking
        ; test_case
            "disabled thinking omits adaptive effort"
            `Quick
            test_anthropic_disabled_thinking_omits_adaptive_effort
        ; test_case
            "Sonnet 5 explicit disable is serialized"
            `Quick
            test_anthropic_sonnet5_explicit_disable_is_serialized
        ; test_case
            "thinking forced tool_choice rejected before request"
            `Quick
            test_anthropic_thinking_forced_tool_choice_rejected_before_request
        ; test_case "with output schema" `Quick test_anthropic_output_schema
        ; test_case
            "with json schema response_format"
            `Quick
            test_anthropic_json_schema_response_format_without_output_schema
        ; test_case
            "multi-turn signed thinking/tool order"
            `Quick
            test_anthropic_build_request_preserves_multiturn_thinking_tool_order
        ; test_case "stream flag" `Quick test_anthropic_stream_flag
        ; test_case
            "parse response initializes telemetry"
            `Quick
            test_anthropic_parse_response_initializes_telemetry
        ; test_case
            "parse response rejects unknown content block"
            `Quick
            test_anthropic_parse_response_rejects_unknown_content_block
        ; test_case
            "parse response rejects unknown media source kind"
            `Quick
            test_anthropic_parse_response_rejects_unknown_media_source_kind
        ] )
    ; ( "openai_build_request"
      , [ test_case "basic body" `Quick test_openai_basic_body
        ; test_case "with system" `Quick test_openai_with_system
        ; test_case "with tools" `Quick test_openai_with_tools
        ; test_case
            "kimi direct tools + thinking"
            `Quick
            test_kimi_direct_with_tools_and_thinking
        ; test_case
            "kimi direct tool_result uses scalar text"
            `Quick
            test_kimi_direct_tool_result_uses_text_blocks
        ; test_case "stream flag" `Quick test_openai_stream_flag
        ; test_case "with json schema" `Quick test_openai_with_json_schema
        ; test_case
            "Responses assistant phase metadata"
            `Quick
            test_openai_responses_replays_assistant_phase_metadata
        ; test_case
            "Responses unknown phase rejected"
            `Quick
            test_openai_responses_rejects_unknown_assistant_phase
        ; test_case
            "Responses padded phase rejected"
            `Quick
            test_openai_responses_rejects_padded_assistant_phase
        ; test_case
            "Responses blank phase rejected"
            `Quick
            test_openai_responses_rejects_blank_assistant_phase
        ; test_case
            "Responses non-string phase rejected"
            `Quick
            test_openai_responses_rejects_non_string_assistant_phase
        ; test_case "ollama output schema" `Quick test_ollama_output_schema
        ; test_case
            "ollama gemma4 thinking uses template token"
            `Quick
            test_ollama_gemma4_thinking_uses_template_token
        ; test_case
            "ollama native multimodal request body"
            `Quick
            test_ollama_native_multimodal_request_body
        ; test_case
            "ollama parse parallel tool calls object args"
            `Quick
            test_ollama_parse_parallel_tool_calls_object_arguments
        ; test_case
            "ollama parse explicit id string args"
            `Quick
            test_ollama_parse_tool_call_preserves_explicit_id_and_string_arguments
        ; test_case
            "ollama malformed tool call rejected"
            `Quick
            test_ollama_parse_rejects_malformed_tool_call
        ; test_case
            "ollama non-object tool arguments rejected"
            `Quick
            test_ollama_parse_rejects_non_object_tool_arguments
        ; test_case
            "glm preserved reasoning replay"
            `Quick
            test_glm_preserved_reasoning_replay_and_preserves_auto_tool_choice
        ; test_case
            "glm named forced tool_choice rejected at completion boundary"
            `Quick
            test_complete_rejects_glm_named_forced_tool_choice_before_request
        ] )
    ; ( "gemini_build_request"
      , [ test_case "with json schema" `Quick test_gemini_with_json_schema ] )
    ; ( "provider_config"
      , [ test_case "default paths" `Quick test_config_default_paths
        ; test_case "custom path" `Quick test_config_custom_path
        ] )
    ; ( "retry"
      , [ test_case "default config" `Quick test_default_retry_config
        ; test_case "is_retryable" `Quick test_is_retryable
        ] )
    ; ( "cli_transport_guard"
      , [ test_case
            "glm output schema rejected before request"
            `Quick
            test_complete_rejects_output_schema_for_glm
        ] )
    ; ( "capability_drift"
      , [ test_case
            "provider-default thinking observation is info"
            `Quick
            test_provider_default_thinking_drift_is_info
        ; test_case
            "model-specific thinking drift remains warn"
            `Quick
            test_model_capability_thinking_drift_remains_warn
        ; test_case
            "declared glm thinking uses model capability"
            `Quick
            test_declared_glm_model_thinking_uses_model_capability
        ] )
    ; ( "cost"
      , [ test_case "annotate response cost" `Quick test_annotate_response_cost
        ; test_case
            "annotate gpt-5.5 response cost"
            `Quick
            test_annotate_response_cost_gpt55
        ] )
    ; ( "stream_acc"
      , [ test_case "text events" `Quick test_stream_acc_text
        ; test_case "tool_use events" `Quick test_stream_acc_tool_use
        ] )
    ; ( "prompt_caching"
      , [ test_case "system block with cache_control" `Quick test_cache_system_prompt
        ; test_case "no cache when disabled" `Quick test_cache_no_system_no_cache
        ; test_case "last tool gets cache_control" `Quick test_cache_tools
        ; test_case "default cache off" `Quick test_cache_default_false
        ; test_case
            "short prompt preserves opt-in"
            `Quick
            test_cache_short_prompt_preserves_opt_in
        ] )
    ]
;;
