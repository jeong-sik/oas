(** Tests for Provider_config — lightweight provider configuration. *)

open Llm_provider

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let getenv_from pairs name = List.assoc_opt name pairs

let reasoning_effort_option_to_string =
  Option.map Provider_config.reasoning_effort_to_string
;;

(* ── make: defaults ───────────────────────────────────── *)

let test_make_defaults () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"test"
      ~base_url:"http://localhost:8080"
      ()
  in
  check_string "model_id" "test" cfg.model_id;
  check_string "base_url" "http://localhost:8080" cfg.base_url;
  check_string "api_key default empty" "" (cfg.api_key :> string);
  check_bool "max_tokens default None" true (cfg.max_tokens = None);
  check_bool "temperature None" true (cfg.temperature = None);
  check_bool "top_p None" true (cfg.top_p = None);
  check_bool "top_k None" true (cfg.top_k = None);
  check_bool "min_p None" true (cfg.min_p = None);
  check_bool "system_prompt None" true (cfg.system_prompt = None);
  check_bool "enable_thinking None" true (cfg.enable_thinking = None);
  check_bool "preserve_thinking None" true (cfg.preserve_thinking = None);
  check_bool "thinking_budget None" true (cfg.thinking_budget = None);
  check_bool "clear_thinking None" true (cfg.clear_thinking = None);
  check_bool "tool_stream false" false cfg.tool_stream;
  check_bool "tool_choice None" true (cfg.tool_choice = None);
  check_bool "no parallel tool use" false cfg.disable_parallel_tool_use;
  check_bool "response format off" true (cfg.response_format = Types.Off);
  check_bool "no output schema" true (Option.is_none cfg.output_schema);
  check_bool "no cache system prompt" false cfg.cache_system_prompt
;;

(* ── make: request_path per kind ──────────────────────── *)

let test_request_path_anthropic () =
  let cfg = Provider_config.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  check_string "anthropic path" "/v1/messages" cfg.request_path
;;

let test_request_path_provider_c () =
  let cfg = Provider_config.make ~kind:Kimi ~model_id:"m" ~base_url:"" () in
  check_string "kimi path" "/v1/chat/completions" cfg.request_path
;;

let test_request_path_openai () =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  check_string "openai path" "/v1/chat/completions" cfg.request_path
;;

let test_request_path_gemini () =
  let cfg = Provider_config.make ~kind:Gemini ~model_id:"m" ~base_url:"" () in
  check_string "gemini path" "" cfg.request_path
;;

let test_request_path_glm () =
  let cfg = Provider_config.make ~kind:Glm ~model_id:"m" ~base_url:"" () in
  check_string "glm path" "/chat/completions" cfg.request_path
;;

let test_request_path_ollama () =
  let cfg = Provider_config.make ~kind:Ollama ~model_id:"m" ~base_url:"" () in
  check_string "ollama path" "/api/chat" cfg.request_path
;;

let test_request_path_dashscope () =
  let cfg = Provider_config.make ~kind:DashScope ~model_id:"m" ~base_url:"" () in
  check_string "dashscope path" "/chat/completions" cfg.request_path
;;

let test_request_path_override () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:""
      ~request_path:"/custom/path"
      ()
  in
  check_string "custom path" "/custom/path" cfg.request_path
;;

(* ── auth headers ────────────────────────────────────── *)

let check_headers = Alcotest.(check (list (pair string string)))

let test_auth_headers_for_kind_and_key_matches_config () =
  List.iter
    (fun kind ->
       let cfg =
         Provider_config.make
           ~kind
           ~model_id:"auth-model"
           ~base_url:"https://provider.example"
           ~api_key:"provider-key"
           ()
       in
       check_headers
         (Provider_config.string_of_provider_kind kind)
         (Provider_config.auth_headers_for_config cfg)
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:"provider-key"))
    Provider_config.all_provider_kinds
;;

let expected_auth_headers_for_kind = function
  | Provider_config.Anthropic | Provider_config.Kimi -> [ "x-api-key", "provider-key" ]
  | Provider_config.Gemini -> [ "x-goog-api-key", "provider-key" ]
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Glm
  | Provider_config.DashScope -> [ "Authorization", "Bearer provider-key" ]
;;

let test_auth_headers_for_kind_and_key_wire_headers () =
  List.iter
    (fun kind ->
       check_headers
         (Provider_config.string_of_provider_kind kind)
         (expected_auth_headers_for_kind kind)
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:"provider-key"))
    Provider_config.all_provider_kinds
;;

let test_auth_headers_for_kind_and_key_omits_empty_secret () =
  List.iter
    (fun kind ->
       check_headers
         (Provider_config.string_of_provider_kind kind)
         []
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:""))
    Provider_config.all_provider_kinds
;;

(* ── make: explicit values ────────────────────────────── *)

let test_make_with_all_options () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-opus"
      ~base_url:"https://api.anthropic.com"
      ~api_key:"sk-test"
      ~headers:[ "X-Custom", "val" ]
      ~max_tokens:2048
      ~temperature:0.7
      ~top_p:0.9
      ~top_k:40
      ~min_p:0.05
      ~system_prompt:"system"
      ~enable_thinking:true
      ~preserve_thinking:true
      ~thinking_budget:1000
      ~clear_thinking:false
      ~tool_stream:true
      ~disable_parallel_tool_use:true
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~cache_system_prompt:true
      ()
  in
  check_string "api_key" "sk-test" (cfg.api_key :> string);
  check_bool "max_tokens" true (cfg.max_tokens = Some 2048);
  check_bool "temperature" true (cfg.temperature = Some 0.7);
  check_bool "top_p" true (cfg.top_p = Some 0.9);
  check_bool "top_k" true (cfg.top_k = Some 40);
  check_bool "min_p" true (cfg.min_p = Some 0.05);
  check_bool "system_prompt" true (cfg.system_prompt = Some "system");
  check_bool "enable_thinking" true (cfg.enable_thinking = Some true);
  check_bool "preserve_thinking" true (cfg.preserve_thinking = Some true);
  check_bool "thinking_budget" true (cfg.thinking_budget = Some 1000);
  check_bool "clear_thinking" true (cfg.clear_thinking = Some false);
  check_bool "tool_stream" true cfg.tool_stream;
  check_bool "disable_parallel" true cfg.disable_parallel_tool_use;
  let expected_schema = `Assoc [ "type", `String "object" ] in
  check_bool
    "json schema mode"
    true
    (cfg.response_format = Types.JsonSchema expected_schema);
  check_bool "has output schema" true (Option.is_some cfg.output_schema);
  check_bool "cache prompt" true cfg.cache_system_prompt
;;

let test_make_response_format_json_mode () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt"
      ~base_url:"https://api.openai.com/v1"
      ~response_format_json:true
      ()
  in
  check_bool "json mode" true (cfg.response_format = Types.JsonMode);
  check_bool "no json schema" true (Option.is_none cfg.output_schema)
;;

let test_output_schema_of_response_format () =
  let schema = `Assoc [ "type", `String "object" ] in
  check_bool
    "schema derived"
    true
    (Option.equal
       Yojson.Safe.equal
       (Some schema)
       (Provider_config.output_schema_of_response_format (Types.JsonSchema schema)));
  check_bool
    "json mode has no schema"
    true
    (Option.is_none (Provider_config.output_schema_of_response_format Types.JsonMode));
  check_bool
    "off has no schema"
    true
    (Option.is_none (Provider_config.output_schema_of_response_format Types.Off));
  check_bool
    "override wins"
    true
    (Option.equal
       Yojson.Safe.equal
       (Some schema)
       (Provider_config.output_schema_of_response_format ~override:schema Types.JsonMode))
;;

let test_validate_output_schema_openai_official () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt"
      ~base_url:"https://api.openai.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "official openai accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_openai_compat_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt"
      ~base_url:"https://openrouter.ai/api/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "generic compat rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_glm_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "glm rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_dashscope_accepted () =
  let cfg =
    Provider_config.make
      ~kind:DashScope
      ~model_id:"dashscope-max"
      ~base_url:"https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "dashscope accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_kimi_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "kimi rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_unrequested_ok () =
  let cfg =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ()
  in
  check_bool
    "no schema request bypasses provider restriction"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_direct_response_format_record () =
  let schema = `Assoc [ "type", `String "object" ] in
  let cfg =
    { (Provider_config.make
         ~kind:OpenAI_compat
         ~model_id:"gpt"
         ~base_url:"https://openrouter.ai/api/v1"
         ())
      with
      response_format = Types.JsonSchema schema
    ; output_schema = None
    }
  in
  check_bool
    "response_format JsonSchema is validated even without output_schema"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_supported_non_openai () =
  let schema = `Assoc [ "type", `String "object" ] in
  List.iter
    (fun kind ->
       let cfg =
         Provider_config.make
           ~kind
           ~model_id:"m"
           ~base_url:"https://api.example.test"
           ~output_schema:schema
           ()
       in
       check_bool
         (Provider_config.string_of_provider_kind kind ^ " accepts schema")
         true
         (Result.is_ok (Provider_config.validate_output_schema_request cfg)))
    [ Anthropic; Gemini; Ollama; DashScope ]
;;

let test_validate_output_schema_capability_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"unknown-model-without-schema-capability"
      ~base_url:"https://api.openai.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg -> check_bool "returns explanatory error" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "expected model capability rejection"
;;

let test_validate_responses_request_path_allows_structured_output () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.5"
      ~base_url:"https://api.openai.com/v1"
      ~request_path:"/v1/responses"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "responses structured output accepted at path layer"
    true
    (Result.is_ok (Provider_config.validate_request_path cfg))
;;

let test_validate_responses_request_path_allows_json_mode () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.5"
      ~base_url:"https://api.openai.com/v1"
      ~request_path:"/v1/responses"
      ~response_format_json:true
      ()
  in
  check_bool
    "responses json mode accepted at path layer"
    true
    (Result.is_ok (Provider_config.validate_request_path cfg))
;;

(* ── make: headers default ────────────────────────────── *)

let test_default_headers () =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  check_int "1 default header" 1 (List.length cfg.headers);
  let k, v = List.hd cfg.headers in
  check_string "Content-Type key" "Content-Type" k;
  check_string "Content-Type val" "application/json" v
;;

let test_custom_headers () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~headers:[ "Auth", "Bearer x"; "X-Custom", "val" ]
      ()
  in
  check_int "2 custom headers" 2 (List.length cfg.headers)
;;

(* ── locality ────────────────────────────────────────── *)

let test_is_local_loopback_ip () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_bool "loopback ip is local" true (Provider_config.is_local cfg)
;;

let test_is_local_localhost () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhost/v1"
      ()
  in
  check_bool "localhost is local" true (Provider_config.is_local cfg)
;;

let test_is_local_remote_false () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"https://api.example.com"
      ()
  in
  check_bool "remote is not local" false (Provider_config.is_local cfg)
;;

let test_is_local_host_boundary_false () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhostevil.com"
      ()
  in
  check_bool "hostname boundary respected" false (Provider_config.is_local cfg)
;;

let test_is_local_localhost_query_true () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhost?foo=bar"
      ()
  in
  check_bool "localhost query is local" true (Provider_config.is_local cfg)
;;

let test_default_attempt_timeout_s () =
  let check_timeout label expected kind =
    Alcotest.(check (option (float 0.001)))
      label
      expected
      (Provider_config.default_attempt_timeout_s kind)
  in
  check_timeout "ollama has no default hard attempt timeout" None Ollama;
  check_timeout "openai_compat has no default hard attempt timeout" None OpenAI_compat
;;

let test_max_turns_hard_cap_and_clamp () =
  Alcotest.(check (option int))
    "anthropic no hard cap"
    None
    (Provider_config.max_turns_hard_cap Anthropic);
  check_int
    "anthropic preserves request"
    99
    (Provider_config.clamp_max_turns Anthropic 99)
;;

let test_reasoning_effort_of_thinking_config () =
  let check_effort label expected enable_thinking thinking_budget =
    check_string
      label
      expected
      (Provider_config.effort_of_thinking_config ~enable_thinking ~thinking_budget)
  in
  check_effort "disabled" "none" (Some false) (Some 4096);
  check_effort "missing flag" "none" None (Some 4096);
  check_effort "zero budget" "none" (Some true) (Some 0);
  check_effort
    "low budget"
    "low"
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_effort
    "medium budget"
    "medium"
    (Some true)
    (Some Reasoning_effort.medium_budget_max_tokens);
  check_effort
    "high budget"
    "high"
    (Some true)
    (Some (Reasoning_effort.medium_budget_max_tokens + 1))
;;

let test_reasoning_effort_top_tier_budget_mapping () =
  let check_effort label expected budget =
    Alcotest.(check (option string))
      label
      (Some expected)
      (reasoning_effort_option_to_string (Reasoning_effort.of_budget_with_xhigh budget))
  in
  check_effort "low top-tier mapping" "low" Reasoning_effort.low_budget_max_tokens;
  check_effort
    "medium top-tier mapping"
    "medium"
    Reasoning_effort.medium_budget_max_tokens;
  check_effort "high top-tier mapping" "high" Reasoning_effort.high_budget_max_tokens;
  check_effort
    "xhigh top-tier mapping"
    "xhigh"
    (Reasoning_effort.high_budget_max_tokens + 1);
  Alcotest.(check (option string))
    "non-positive budget omits effort"
    None
    (reasoning_effort_option_to_string (Reasoning_effort.of_budget_with_xhigh 0))
;;

let test_reasoning_effort_typed_roundtrip () =
  let cases =
    [ Provider_config.Minimal, "minimal"
    ; Provider_config.Low, "low"
    ; Provider_config.Medium, "medium"
    ; Provider_config.High, "high"
    ; Provider_config.XHigh, "xhigh"
    ]
  in
  List.iter
    (fun (value, wire) ->
       check_string "to wire" wire (Provider_config.reasoning_effort_to_string value);
       Alcotest.(check (option string))
         "from wire"
         (Some wire)
         (reasoning_effort_option_to_string
            (Provider_config.reasoning_effort_of_string wire)))
    cases;
  Alcotest.(check (option string))
    "unknown wire"
    None
    (reasoning_effort_option_to_string
       (Provider_config.reasoning_effort_of_string "urgent"));
  Alcotest.(check (option string))
    "trimmed case-insensitive wire"
    (Some "low")
    (reasoning_effort_option_to_string
       (Provider_config.reasoning_effort_of_string " LOW "))
;;

let test_reasoning_effort_typed_config_value () =
  let check_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (reasoning_effort_option_to_string
         (Provider_config.effort_of_thinking_config_value
            ~enable_thinking
            ~thinking_budget
            ()))
  in
  check_value "disabled typed" None (Some false) (Some 4096);
  check_value "missing flag typed" None None (Some 4096);
  check_value "zero budget typed" None (Some true) (Some 0);
  check_value
    "low typed"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_value
    "medium typed"
    (Some "medium")
    (Some true)
    (Some Reasoning_effort.medium_budget_max_tokens);
  check_value
    "high typed"
    (Some "high")
    (Some true)
    (Some (Reasoning_effort.medium_budget_max_tokens + 1));
  let getenv = getenv_from [ "OAS_DEFAULT_REASONING_EFFORT", "xhigh" ] in
  Alcotest.(check (option string))
    "env default typed"
    (Some "xhigh")
    (reasoning_effort_option_to_string
       (Provider_config.effort_of_thinking_config_value
          ~getenv
          ~enable_thinking:(Some true)
          ~thinking_budget:None
          ()));
  let invalid_getenv = getenv_from [ "OAS_DEFAULT_REASONING_EFFORT", "urgent" ] in
  Alcotest.(check string)
    "invalid env defaults medium"
    "medium"
    (Provider_config.reasoning_effort_to_string
       (Provider_config.default_reasoning_effort_value ~getenv:invalid_getenv ()))
;;

let test_reasoning_effort_of_config () =
  let ollama =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama"
      ~base_url:"http://127.0.0.1:11434"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  let anthropic =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet"
      ~base_url:"https://api.anthropic.com"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  Alcotest.(check (option string))
    "ollama exposes effort"
    (Some "low")
    (Provider_config.reasoning_effort_of_config ollama);
  Alcotest.(check (option string))
    "non-ollama has no effort"
    None
    (Provider_config.reasoning_effort_of_config anthropic)
;;

let test_reasoning_effort_request_value () =
  let check_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (Provider_config.reasoning_effort_request_value ~enable_thinking ~thinking_budget)
  in
  let check_typed_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (reasoning_effort_option_to_string
         (Provider_config.reasoning_effort_request_value_typed
            ~enable_thinking
            ~thinking_budget))
  in
  check_value "unset omits field" None None (Some 4096);
  check_value "disabled omits field" None (Some false) (Some 4096);
  check_value "zero budget omits field" None (Some true) (Some 0);
  check_value
    "enabled maps effort"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_typed_value
    "enabled maps typed effort"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens)
;;

let test_structured_output_name_of_schema () =
  let check_name label expected schema =
    check_string label expected (Provider_config.structured_output_name_of_schema schema)
  in
  check_name "normalizes title" "invoice_v2" (`Assoc [ "title", `String " Invoice V2! " ]);
  check_name
    "keeps hyphen underscore"
    "my-schema_v2"
    (`Assoc [ "title", `String "My-Schema_v2" ]);
  check_name
    "blank title uses default"
    "structured_output"
    (`Assoc [ "title", `String "   " ]);
  check_name "missing title uses default" "structured_output" (`Assoc []);
  check_name "non-object uses default" "structured_output" (`List [])
;;

(* ── provider_name_of_config ─────────────────────────── *)

let test_provider_name_of_config_glm_general () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  check_string "glm general" "glm" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_glm_coding () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:Zai_catalog.coding_base_url
      ()
  in
  check_string "glm coding" "glm-coding" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_local_openai_compat () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"local-model"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_string
    "local openai compat resolves to llama"
    "nous"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_openrouter () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"openai/gpt-oss-20b"
      ~base_url:"https://openrouter.ai/api/v1"
      ~request_path:"/chat/completions"
      ()
  in
  check_string "openrouter" "openrouter" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_unmatched_openai_compat () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"unlisted-model"
      ~base_url:"https://unlisted.example/v1"
      ~request_path:"/chat/completions"
      ()
  in
  check_string
    "unmatched openai compat"
    "openai_compat"
    (Provider_registry.provider_name_of_config cfg)
;;

(* ── provider_kind_of_string ─────────────────────────── *)

(** Check a raw string parses to the expected variant. Compared via
    [string_of_provider_kind] to avoid needing a derived [equal_provider_kind]. *)
let check_parse label input expected =
  match Provider_config.provider_kind_of_string input with
  | None -> Alcotest.failf "%s: expected Some _, got None for %S" label input
  | Some k ->
    let got = Provider_config.string_of_provider_kind k in
    let want = Provider_config.string_of_provider_kind expected in
    check_string label want got
;;

(* SSOT: pull the canonical list from the type's own module so adding a
   new variant without updating [Provider_kind.all] is caught by the
   [test_all_is_exhaustive] property below rather than silently skipping
   the new variant in every iterative test. *)
let all_kinds : Provider_config.provider_kind list = Provider_config.all_provider_kinds

let test_kind_roundtrip () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       check_parse ("roundtrip " ^ s) s k)
    all_kinds
;;

let test_kind_aliases_rejected () =
  List.iter
    (fun input ->
       check_bool
         ("alias rejected " ^ input)
         true
         (Option.is_none (Provider_config.provider_kind_of_string input)))
    [ "claude"; "openai"; "nous"; "claude"; "openai"; "llama"; "zhipu" ]
;;

let test_kind_case_insensitive () =
  check_parse "ANTHROPIC" "ANTHROPIC" Anthropic;
  check_parse "OpenAI_Compat" "OpenAI_Compat" OpenAI_compat;
  check_parse "Glm" "Glm" Glm
;;

let test_kind_whitespace () =
  check_parse "leading ws" "  anthropic" Anthropic;
  check_parse "trailing ws" "ollama  " Ollama;
  check_parse "both ws" "\topenai_compat\n" OpenAI_compat
;;

let test_kind_unknown_returns_none () =
  check_bool
    "empty string"
    true
    (Option.is_none (Provider_config.provider_kind_of_string ""));
  check_bool
    "misspelling"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "anthrpic"));
  check_bool
    "bare openrouter"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "openrouter"));
  check_bool
    "json-ish"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "\"claude\""))
;;

(* ── provider_kind serializers ───────────────────────── *)

let test_show_matches_string_of () =
  List.iter
    (fun k ->
       check_string
         "show = string_of"
         (Provider_config.string_of_provider_kind k)
         (Provider_config.show_provider_kind k))
    all_kinds
;;

let test_pp_uses_lowercase () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  Provider_config.pp_provider_kind fmt Anthropic;
  Format.pp_print_flush fmt ();
  check_string "pp Anthropic" "anthropic" (Buffer.contents buf)
;;

let test_to_yojson_roundtrip () =
  List.iter
    (fun k ->
       let json = Provider_config.provider_kind_to_yojson k in
       match json with
       | `String s ->
         check_string "to_yojson wire form" (Provider_config.string_of_provider_kind k) s
       | _ -> Alcotest.fail "to_yojson must produce `String")
    all_kinds
;;

let test_of_yojson_accepts_canonical () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       let json : Yojson.Safe.t = `String s in
       match Provider_config.provider_kind_of_yojson json with
       | Ok k' ->
         check_string "of_yojson roundtrip" s (Provider_config.string_of_provider_kind k')
       | Error msg -> Alcotest.failf "of_yojson failed for %s: %s" s msg)
    all_kinds
;;

let test_of_yojson_rejects_aliases () =
  List.iter
    (fun input ->
       let json : Yojson.Safe.t = `String input in
       match Provider_config.provider_kind_of_yojson json with
       | Ok _ -> Alcotest.failf "of_yojson alias %S should fail" input
       | Error _ -> ())
    [ "claude"; "openai"; "nous" ]
;;

let test_of_yojson_rejects_unknown_string () =
  let json : Yojson.Safe.t = `String "nopenope" in
  match Provider_config.provider_kind_of_yojson json with
  | Ok _ -> Alcotest.fail "expected Error for unknown string"
  | Error _ -> ()
;;

let test_of_yojson_rejects_non_string () =
  let cases : (string * Yojson.Safe.t) list =
    [ "null", `Null; "int", `Int 1; "assoc", `Assoc [ "kind", `String "anthropic" ] ]
  in
  List.iter
    (fun (label, json) ->
       match Provider_config.provider_kind_of_yojson json with
       | Ok _ -> Alcotest.failf "expected Error for non-string %s" label
       | Error _ -> ())
    cases
;;

(* ── telemetry wire-format regression ─────────────────── *)

(** Build a throwaway inference_telemetry with only provider_kind varying.
    Other fields carry placeholder values so the serialised payload is stable. *)
let telemetry_with_kind (pk : Provider_config.provider_kind option)
  : Types.inference_telemetry
  =
  { Types.default_inference_telemetry with provider_kind = pk }
;;

(** Substring search helper local to this module. *)
let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop i =
    if i + sub_len > text_len
    then false
    else if String.sub text i sub_len = sub
    then true
    else loop (i + 1)
  in
  sub_len = 0 || loop 0
;;

let test_wire_kind_lowercase () =
  let cases =
    [ Provider_config.Anthropic, "\"provider_kind\":\"anthropic\""
    ; Provider_config.OpenAI_compat, "\"provider_kind\":\"openai_compat\""
    ; Provider_config.Ollama, "\"provider_kind\":\"ollama\""
    ; Provider_config.Gemini, "\"provider_kind\":\"gemini\""
    ; Provider_config.Glm, "\"provider_kind\":\"glm\""
    ]
  in
  List.iter
    (fun (kind, expected_substring) ->
       let json = Types.inference_telemetry_to_yojson (telemetry_with_kind (Some kind)) in
       let encoded = Yojson.Safe.to_string json in
       Alcotest.(check bool)
         (Printf.sprintf
            "wire for %s contains %s"
            (Provider_config.string_of_provider_kind kind)
            expected_substring)
         true
         (contains_substring ~sub:expected_substring encoded))
    cases
;;

let test_wire_kind_none_roundtrip () =
  let t = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson t) in
  (* None should not produce "anthropic" / "ollama" / any kind string. *)
  List.iter
    (fun s ->
       Alcotest.(check bool)
         (Printf.sprintf "None telemetry must not contain %S" s)
         false
         (contains_substring ~sub:s encoded))
    [ "\"anthropic\""; "\"ollama\""; "\"openai_compat\"" ]
;;

let test_wire_unknown_latency_is_null () =
  let original = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "unknown latency encoded as JSON null"
    true
    (contains_substring ~sub:"\"request_latency_ms\":null" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "unknown latency roundtrips"
    None
    decoded.request_latency_ms
;;

let test_wire_measured_zero_latency_is_distinct () =
  let original = { (telemetry_with_kind None) with request_latency_ms = Some 0 } in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "measured zero encoded as JSON zero"
    true
    (contains_substring ~sub:"\"request_latency_ms\":0" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "measured zero roundtrips"
    (Some 0)
    decoded.request_latency_ms
;;

(* ── enumeration & default_api_key_env ────────────────── *)

(** [all_provider_kinds] must contain every variant exactly once. The
    property guards against adding a variant to the sum type without
    extending {!Provider_kind.all}; subsequent iterative tests would
    silently skip the new kind otherwise. *)
let test_all_is_exhaustive () =
  let xs = Provider_config.all_provider_kinds in
  Alcotest.(check int) "seven canonical variants" 7 (List.length xs);
  Alcotest.(check bool)
    "no duplicate canonical strings"
    true
    (let strs = List.map Provider_config.string_of_provider_kind xs in
     List.length strs = List.length (List.sort_uniq compare strs));
  (* Exhaustive match: any missing or extra variant produces a compile
     error here — the check is the compiler, not the runtime. *)
  List.iter
    (fun k ->
       match (k : Provider_config.provider_kind) with
       | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | DashScope | Glm -> ())
    xs
;;

let test_all_drives_parse_roundtrip () =
  (* Property: [of_string (to_string k) = Some k] for every variant in
     [all_provider_kinds]. Stronger than the spot-check roundtrip
     because the driver is the canonical enumeration — new variants
     are tested automatically. *)
  List.iter
    (fun k ->
       let encoded = Provider_config.string_of_provider_kind k in
       match Provider_config.provider_kind_of_string encoded with
       | Some k' ->
         Alcotest.(check string)
           ("parse " ^ encoded)
           encoded
           (Provider_config.string_of_provider_kind k')
       | None -> Alcotest.failf "of_string %S returned None for a canonical form" encoded)
    Provider_config.all_provider_kinds
;;

let test_default_api_key_env_known () =
  Alcotest.(check (option string))
    "anthropic"
    (Some "ANTHROPIC_API_KEY")
    (Provider_config.default_api_key_env Anthropic);
  Alcotest.(check (option string))
    "gemini"
    (Some "GEMINI_API_KEY")
    (Provider_config.default_api_key_env Gemini);
  Alcotest.(check (option string))
    "glm"
    (Some "ZAI_API_KEY")
    (Provider_config.default_api_key_env Glm);
  Alcotest.(check (option string))
    "kimi"
    (Some "KIMI_API_KEY")
    (Provider_config.default_api_key_env Kimi)
;;

let test_default_api_key_env_none_for_others () =
  (* Local / transport-mediated / OpenAI-compatible share: OAS does not
     dictate a single env var; callers supply their own. *)
  List.iter
    (fun (label, k) ->
       Alcotest.(check (option string)) label None (Provider_config.default_api_key_env k))
    [ "openai_compat", Provider_config.OpenAI_compat; "ollama", Provider_config.Ollama ]
;;

let test_wire_kind_roundtrip_via_yojson () =
  (* End-to-end: record -> JSON string -> JSON tree -> record; the
     provider_kind survives as the same typed constructor. *)
  let original = telemetry_with_kind (Some Provider_config.Ollama) in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  match decoded.provider_kind with
  | Some Ollama -> ()
  | Some other ->
    Alcotest.failf
      "roundtrip produced wrong variant: %s"
      (Provider_config.string_of_provider_kind other)
  | None -> Alcotest.fail "roundtrip produced None"
;;

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "provider_config"
    [ ( "defaults"
      , [ Alcotest.test_case "make defaults" `Quick test_make_defaults
        ; Alcotest.test_case "default headers" `Quick test_default_headers
        ] )
    ; ( "request_path"
      , [ Alcotest.test_case "anthropic" `Quick test_request_path_anthropic
        ; Alcotest.test_case "kimi" `Quick test_request_path_provider_c
        ; Alcotest.test_case "openai" `Quick test_request_path_openai
        ; Alcotest.test_case "gemini" `Quick test_request_path_gemini
        ; Alcotest.test_case "glm" `Quick test_request_path_glm
        ; Alcotest.test_case "ollama" `Quick test_request_path_ollama
        ; Alcotest.test_case "dashscope" `Quick test_request_path_dashscope
        ; Alcotest.test_case "override" `Quick test_request_path_override
        ] )
    ; ( "auth_headers"
      , [ Alcotest.test_case
            "kind/key API matches config API"
            `Quick
            test_auth_headers_for_kind_and_key_matches_config
        ; Alcotest.test_case
            "kind/key API emits provider wire headers"
            `Quick
            test_auth_headers_for_kind_and_key_wire_headers
        ; Alcotest.test_case
            "empty secret omits headers"
            `Quick
            test_auth_headers_for_kind_and_key_omits_empty_secret
        ] )
    ; ( "explicit_values"
      , [ Alcotest.test_case "all options" `Quick test_make_with_all_options
        ; Alcotest.test_case "custom headers" `Quick test_custom_headers
        ; Alcotest.test_case
            "response_format_json mode"
            `Quick
            test_make_response_format_json_mode
        ; Alcotest.test_case
            "output schema derivation"
            `Quick
            test_output_schema_of_response_format
        ] )
    ; ( "output_schema"
      , [ Alcotest.test_case
            "official openai"
            `Quick
            test_validate_output_schema_openai_official
        ; Alcotest.test_case
            "generic compat rejected"
            `Quick
            test_validate_output_schema_openai_compat_rejected
        ; Alcotest.test_case
            "glm rejected"
            `Quick
            test_validate_output_schema_glm_rejected
        ; Alcotest.test_case
            "kimi rejected"
            `Quick
            test_validate_output_schema_kimi_rejected
        ; Alcotest.test_case
            "dashscope accepted"
            `Quick
            test_validate_output_schema_dashscope_accepted
        ; Alcotest.test_case
            "unrequested schema bypasses restrictions"
            `Quick
            test_validate_output_schema_unrequested_ok
        ; Alcotest.test_case
            "direct JsonSchema record is validated"
            `Quick
            test_validate_output_schema_direct_response_format_record
        ; Alcotest.test_case
            "supported non-openai providers"
            `Quick
            test_validate_output_schema_supported_non_openai
        ; Alcotest.test_case
            "openai capability rejection"
            `Quick
            test_validate_output_schema_capability_rejected
        ; Alcotest.test_case
            "responses structured path accepted"
            `Quick
            test_validate_responses_request_path_allows_structured_output
        ; Alcotest.test_case
            "responses json mode path accepted"
            `Quick
            test_validate_responses_request_path_allows_json_mode
        ] )
    ; ( "locality"
      , [ Alcotest.test_case "loopback ip" `Quick test_is_local_loopback_ip
        ; Alcotest.test_case "localhost" `Quick test_is_local_localhost
        ; Alcotest.test_case "remote false" `Quick test_is_local_remote_false
        ; Alcotest.test_case
            "host boundary false"
            `Quick
            test_is_local_host_boundary_false
        ; Alcotest.test_case
            "localhost query true"
            `Quick
            test_is_local_localhost_query_true
        ; Alcotest.test_case
            "default attempt timeout hints"
            `Quick
            test_default_attempt_timeout_s
        ; Alcotest.test_case
            "turn hard caps and clamp"
            `Quick
            test_max_turns_hard_cap_and_clamp
        ; Alcotest.test_case
            "reasoning effort typed roundtrip"
            `Quick
            test_reasoning_effort_typed_roundtrip
        ; Alcotest.test_case
            "reasoning effort typed config value"
            `Quick
            test_reasoning_effort_typed_config_value
        ; Alcotest.test_case
            "thinking effort thresholds"
            `Quick
            test_reasoning_effort_of_thinking_config
        ; Alcotest.test_case
            "thinking effort top-tier thresholds"
            `Quick
            test_reasoning_effort_top_tier_budget_mapping
        ; Alcotest.test_case
            "reasoning effort by config"
            `Quick
            test_reasoning_effort_of_config
        ; Alcotest.test_case
            "reasoning effort request value"
            `Quick
            test_reasoning_effort_request_value
        ; Alcotest.test_case
            "structured output names"
            `Quick
            test_structured_output_name_of_schema
        ] )
    ; ( "provider_name"
      , [ Alcotest.test_case "glm general" `Quick test_provider_name_of_config_glm_general
        ; Alcotest.test_case "glm coding" `Quick test_provider_name_of_config_glm_coding
        ; Alcotest.test_case
            "local openai compat"
            `Quick
            test_provider_name_of_config_local_openai_compat
        ; Alcotest.test_case "openrouter" `Quick test_provider_name_of_config_openrouter
        ; Alcotest.test_case
            "unmatched openai_compat"
            `Quick
            test_provider_name_of_config_unmatched_openai_compat
        ] )
    ; ( "kind_of_string"
      , [ Alcotest.test_case "roundtrip all variants" `Quick test_kind_roundtrip
        ; Alcotest.test_case "aliases rejected" `Quick test_kind_aliases_rejected
        ; Alcotest.test_case "case insensitive" `Quick test_kind_case_insensitive
        ; Alcotest.test_case "whitespace trimmed" `Quick test_kind_whitespace
        ; Alcotest.test_case "unknown returns None" `Quick test_kind_unknown_returns_none
        ] )
    ; ( "kind_serializers"
      , [ Alcotest.test_case "show matches string_of" `Quick test_show_matches_string_of
        ; Alcotest.test_case "pp uses lowercase" `Quick test_pp_uses_lowercase
        ; Alcotest.test_case "to_yojson roundtrip" `Quick test_to_yojson_roundtrip
        ; Alcotest.test_case "of_yojson canonical" `Quick test_of_yojson_accepts_canonical
        ; Alcotest.test_case
            "of_yojson aliases rejected"
            `Quick
            test_of_yojson_rejects_aliases
        ; Alcotest.test_case
            "of_yojson unknown rejected"
            `Quick
            test_of_yojson_rejects_unknown_string
        ; Alcotest.test_case
            "of_yojson non-string rejected"
            `Quick
            test_of_yojson_rejects_non_string
        ] )
    ; ( "kind_enumeration"
      , [ Alcotest.test_case
            "all_provider_kinds is exhaustive"
            `Quick
            test_all_is_exhaustive
        ; Alcotest.test_case
            "all drives parse roundtrip"
            `Quick
            test_all_drives_parse_roundtrip
        ; Alcotest.test_case
            "default_api_key_env known"
            `Quick
            test_default_api_key_env_known
        ; Alcotest.test_case
            "default_api_key_env None for others"
            `Quick
            test_default_api_key_env_none_for_others
        ] )
    ; ( "telemetry_wire_format"
      , [ Alcotest.test_case
            "kind emitted as lowercase canonical string"
            `Quick
            test_wire_kind_lowercase
        ; Alcotest.test_case
            "None kind stays absent / no kind leaks"
            `Quick
            test_wire_kind_none_roundtrip
        ; Alcotest.test_case
            "unknown latency is encoded as null"
            `Quick
            test_wire_unknown_latency_is_null
        ; Alcotest.test_case
            "measured zero latency remains zero"
            `Quick
            test_wire_measured_zero_latency_is_distinct
        ; Alcotest.test_case
            "record JSON roundtrip preserves variant"
            `Quick
            test_wire_kind_roundtrip_via_yojson
        ] )
    ]
;;
