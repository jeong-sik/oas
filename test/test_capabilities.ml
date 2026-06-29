(** Tests for expanded capabilities — model lookup, limits, feature flags. *)

open Alcotest
open Llm_provider

let string_contains_sub s sub =
  let s_len = String.length s in
  let sub_len = String.length sub in
  let rec loop i =
    if sub_len = 0
    then true
    else if i + sub_len > s_len
    then false
    else if String.sub s i sub_len = sub
    then true
    else loop (i + 1)
  in
  loop 0
;;

let check_contains label s sub =
  if not (string_contains_sub s sub)
  then Alcotest.failf "%s: expected %S to contain %S" label s sub
;;

let check_thinking_control label expected actual =
  check bool label true (actual = expected)
;;

let with_temp_manifest contents f =
  let path = Filename.temp_file "oas-capability-manifest" ".json" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents);
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () -> f path)
;;

let with_temp_model_catalog contents f =
  let path = Filename.temp_file "oas-model-catalog" ".toml" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents);
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () -> f path)
;;

(* ── Default capabilities ────────────────────────────── *)

let test_default_no_limits () =
  let c = Capabilities.default_capabilities in
  check bool "no context limit" true (c.max_context_tokens = None);
  check bool "no output limit" true (c.max_output_tokens = None);
  check bool "no tools" false c.supports_tools;
  check bool "system prompt default true" true c.supports_system_prompt
;;

let test_default_new_fields_false () =
  let c = Capabilities.default_capabilities in
  check bool "no parallel tools" false c.supports_parallel_tool_calls;
  check bool "no extended thinking" false c.supports_extended_thinking;
  check bool "no structured output" false c.supports_structured_output;
  check bool "no image input" false c.supports_image_input;
  check bool "no audio input" false c.supports_audio_input;
  check bool "no video input" false c.supports_video_input;
  check bool "no caching" false c.supports_caching;
  check bool "no computer use" false c.supports_computer_use;
  check bool "no code execution" false c.supports_code_execution
;;

(* ── Preset capabilities ─────────────────────────────── *)

let test_anthropic_capabilities () =
  let c = Capabilities.anthropic_capabilities in
  check bool "has tools" true c.supports_tools;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "has extended thinking" true c.supports_extended_thinking;
  check bool "has reasoning budget" true c.supports_reasoning_budget;
  check bool "has image" true c.supports_image_input;
  check bool "has caching" true c.supports_caching;
  check bool "has computer use" true c.supports_computer_use;
  check bool "has structured output" true c.supports_structured_output;
  check bool "no audio" false c.supports_audio_input;
  (* Anthropic Messages API accepts top_k per its documented body
     params; pin the field so the #830/#831 capability-gated
     serializer paths do not silently drop it for claude configs. *)
  check bool "supports top_k" true c.supports_top_k;
  check bool "no min_p" false c.supports_min_p;
  check bool "context 200K" true (c.max_context_tokens = Some 200_000)
;;

let test_openai_capabilities () =
  let c = Capabilities.openai_compat_chat_capabilities in
  check bool "has structured output" true c.supports_structured_output;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "no reasoning" false c.supports_reasoning;
  check bool "context 128K" true (c.max_context_tokens = Some 128_000)
;;

let test_openai_extended () =
  let c = Capabilities.openai_compat_chat_extended_capabilities in
  check bool "has reasoning" true c.supports_reasoning;
  check_thinking_control
    "uses reasoning_effort"
    Capabilities.Reasoning_effort
    c.thinking_control_format;
  check bool "has top_k" true c.supports_top_k;
  check bool "has min_p" true c.supports_min_p
;;

let test_lookup_mimo_v25_pro () =
  match Capabilities.for_model_id "mimo-v2.5-pro" with
  | Some c ->
    check bool "has reasoning" true c.supports_reasoning;
    check_thinking_control
      "uses thinking object only"
      Capabilities.Thinking_object_only
      c.thinking_control_format;
    check bool "has tools" true c.supports_tools
  | None -> fail "should match mimo-v2.5-pro"
;;

(* ── Model lookup ────────────────────────────────────── *)

let test_lookup_claude_opus () =
  match Capabilities.for_model_id "claude-opus-4-6" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match claude-opus"
;;

let test_lookup_claude_sonnet () =
  match Capabilities.for_model_id "claude-sonnet-4-6" with
  | Some c ->
    check (option int) "output 64K" (Some 64_000) c.max_output_tokens;
    check bool "parallel tools" true c.supports_parallel_tool_calls
  | None -> fail "should match claude-sonnet"
;;

let test_lookup_gpt5 () =
  match Capabilities.for_model_id "gpt-5.4" with
  | Some c ->
    check (option int) "context 1.05M" (Some 1_050_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "structured output" true c.supports_structured_output;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match gpt-5"
;;

let test_lookup_gemini () =
  match Capabilities.for_model_id "gemini-3.1-pro" with
  | Some c ->
    check bool "audio" true c.supports_audio_input;
    check bool "video" true c.supports_video_input;
    check bool "code execution" true c.supports_code_execution;
    check bool "structured output" true c.supports_structured_output
  | None -> fail "should match gemini"
;;

(* ── Typed gemini_family classifier (root-fix for #968) ─────── *)

let pp_gemini_family ppf = function
  | Capabilities.Gemini_3_1 -> Format.fprintf ppf "Gemini_3_1"
  | Capabilities.Gemini_3 -> Format.fprintf ppf "Gemini_3"
  | Capabilities.Gemini_2_5 -> Format.fprintf ppf "Gemini_2_5"
  | Capabilities.Gemini_other s -> Format.fprintf ppf "Gemini_other(%s)" s
;;

let gemini_family_testable = Alcotest.testable pp_gemini_family ( = )

let test_gemini_family_3_1 () =
  check
    gemini_family_testable
    "gemini-3.1-pro-preview classifies as Gemini_3_1"
    Capabilities.Gemini_3_1
    (Capabilities.gemini_family_of_id "gemini-3.1-pro-preview")
;;

let test_gemini_family_3_1_flash_lite () =
  check
    gemini_family_testable
    "gemini-3.1-flash-lite-preview classifies as Gemini_3_1"
    Capabilities.Gemini_3_1
    (Capabilities.gemini_family_of_id "gemini-3.1-flash-lite-preview")
;;

let test_gemini_family_3 () =
  check
    gemini_family_testable
    "gemini-3-flash-preview classifies as Gemini_3 (not 3.1)"
    Capabilities.Gemini_3
    (Capabilities.gemini_family_of_id "gemini-3-flash-preview")
;;

let test_gemini_family_2_5 () =
  check
    gemini_family_testable
    "gemini-2.5-flash classifies as Gemini_2_5"
    Capabilities.Gemini_2_5
    (Capabilities.gemini_family_of_id "gemini-2.5-flash")
;;

let test_gemini_family_other_non_gemini () =
  check
    gemini_family_testable
    "non-gemini id falls into Gemini_other with literal retained"
    (Capabilities.Gemini_other "claude-opus-4")
    (Capabilities.gemini_family_of_id "claude-opus-4")
;;

let test_gemini_family_other_unknown_gemini () =
  (* A future gemini line not yet classified should land in Gemini_other —
     not be silently absorbed into an existing arm. *)
  check
    gemini_family_testable
    "gemini-4-foo lands in Gemini_other (no silent fallback)"
    (Capabilities.Gemini_other "gemini-4-foo")
    (Capabilities.gemini_family_of_id "gemini-4-foo")
;;

let test_gemini_family_drives_capabilities () =
  (* Behavioural cross-check: all three live variants resolve to
     gemini_capabilities (1M context). This is the property the #968 drift
     gate was trying to assert via string-grep; now it is enforced by the
     type system at the dispatch site and by this test. *)
  let ctx id =
    match Capabilities.for_model_id id with
    | Some c -> c.max_context_tokens
    | None -> None
  in
  check
    (option int)
    "gemini-3-flash-preview ctx"
    (Some 1_000_000)
    (ctx "gemini-3-flash-preview");
  check
    (option int)
    "gemini-3.1-pro-preview ctx"
    (Some 1_000_000)
    (ctx "gemini-3.1-pro-preview");
  check (option int) "gemini-2.5-flash ctx" (Some 1_000_000) (ctx "gemini-2.5-flash")
;;

let test_lookup_kimi_k2_native_cloud_suffix () =
  let check_visual_first label caps =
    match caps.Capabilities.modality_priority with
    | Modality.Visual_first -> ()
    | Modality.Preserve_input_order -> fail (label ^ " should be visual_first")
  in
  let check_preserve_order label caps =
    match caps.Capabilities.modality_priority with
    | Modality.Preserve_input_order -> ()
    | Modality.Visual_first -> fail (label ^ " should preserve input order")
  in
  match Capabilities.for_model_id "kimi-k2.7-code" with
  | Some native ->
    (* Kimi built-in semantics track the latest K2 code model. Older K2
       variants can still be expressed by external catalog entries through the
       separate preserve_thinking_control_format axis. *)
    check (option int) "native Kimi context 256K" (Some 256_000) native.max_context_tokens;
    check (option int) "native Kimi output 32K" (Some 32_768) native.max_output_tokens;
    check bool "native Kimi tools" true native.supports_tools;
    check bool "native Kimi reasoning" true native.supports_reasoning;
    check_thinking_control
      "native latest Kimi has no thinking request toggle"
      Capabilities.No_thinking_control
      native.thinking_control_format;
    check
      bool
      "native latest Kimi always preserves reasoning"
      true
      (native.preserve_thinking_control_format = Capabilities.Always_preserved_thinking);
    check_preserve_order "native Kimi latest" native;
    check bool "native Kimi code execution" true native.supports_code_execution;
    (match Capabilities.for_model_id "kimi-k2" with
     | Some bare_native ->
       check
         (option int)
         "bare native Kimi context 256K"
         (Some 256_000)
         bare_native.max_context_tokens;
       check
         (option int)
         "bare native Kimi output 32K"
         (Some 32_768)
         bare_native.max_output_tokens;
       check bool "bare native Kimi tools" true bare_native.supports_tools;
       check bool "bare native Kimi reasoning" true bare_native.supports_reasoning;
       check_thinking_control
         "bare native latest Kimi has no thinking request toggle"
         Capabilities.No_thinking_control
         bare_native.thinking_control_format;
       check
         bool
         "bare native latest Kimi always preserves reasoning"
         true
         (bare_native.preserve_thinking_control_format
          = Capabilities.Always_preserved_thinking);
       check_preserve_order "bare native Kimi" bare_native;
       check
         bool
         "bare native Kimi code execution"
         true
         bare_native.supports_code_execution
     | None -> fail "should match native bare Kimi route");
    (match
       Capabilities.for_provider_model_id
         ~provider_label:"ollama_cloud"
         ~model_id:"kimi-k2.7-code"
     with
     | Some cloud ->
       check
         (option int)
         "provider-qualified Ollama Cloud Kimi context"
         (Some 262_144)
         cloud.max_context_tokens;
       check_thinking_control
         "provider-qualified Ollama Cloud Kimi uses native think"
         Capabilities.Ollama_think
         cloud.thinking_control_format;
       check_visual_first "provider-qualified Ollama Cloud Kimi" cloud
     | None -> fail "should match provider-qualified Ollama Cloud Kimi route")
  | None -> fail "should match kimi-k2 cloud route"
;;

let test_lookup_provider_m () =
  match Capabilities.for_model_id "dashscope-3.5-35b-a3b" with
  | Some c ->
    check (option int) "context 262K" (Some 262_144) c.max_context_tokens;
    check bool "tools" true c.supports_tools;
    check bool "thinking" true c.supports_extended_thinking;
    check bool "reasoning budget" true c.supports_reasoning_budget;
    check
      bool
      "chat_template_kwargs thinking control"
      true
      (c.thinking_control_format = Capabilities.Chat_template_kwargs);
    check bool "top_k" true c.supports_top_k
  | None -> fail "should match qwen3"
;;

let test_lookup_provider_m_runpod_name () =
  match Capabilities.for_model_id "DashScope_3.6-35B-A3B-UD-Q4_K_XL.gguf" with
  | Some c ->
    check
      bool
      "runpod qwen3.6 uses chat_template_kwargs"
      true
      (c.thinking_control_format = Capabilities.Chat_template_kwargs)
  | None -> fail "should match qwen3.6 runpod model id"
;;

let test_lookup_deepseek_v4_flash () =
  match Capabilities.for_model_id "deepseek-v4-flash" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    (* thinking mode (default) 400s on forced tool_choice; auto stays valid *)
    check bool "accepts auto tool_choice" true c.supports_tool_choice;
    check bool "rejects named forced tool_choice" false c.supports_named_tool_choice;
    check bool "reasoning" true c.supports_reasoning;
    check_thinking_control
      "uses thinking object"
      Capabilities.Thinking_object
      c.thinking_control_format;
    check bool "caching" true c.supports_caching
  | None -> fail "should match deepseek-v4-flash"
;;

let test_lookup_deepseek_v4_pro () =
  match Capabilities.for_model_id "deepseek-v4-pro" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    (* thinking mode (default) 400s on forced tool_choice; auto stays valid *)
    check bool "accepts auto tool_choice" true c.supports_tool_choice;
    check bool "rejects named forced tool_choice" false c.supports_named_tool_choice;
    check bool "reasoning" true c.supports_reasoning;
    check_thinking_control
      "uses thinking object"
      Capabilities.Thinking_object
      c.thinking_control_format;
    check bool "caching" true c.supports_caching
  | None -> fail "should match deepseek-v4-pro"
;;

let test_lookup_grok () =
  match Capabilities.for_model_id "grok-4.3" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check bool "structured" true c.supports_structured_output;
    check bool "reasoning" true c.supports_reasoning;
    check bool "image" true c.supports_image_input
  | None -> fail "should match grok"
;;

let test_lookup_qwen3_thinking_control () =
  (* RFC-OAS-023: self-served Qwen3 (vLLM / llama-server, OpenAI_compat kind)
     toggles reasoning on the wire via
     {"chat_template_kwargs":{"enable_thinking":b}}. Without an explicit
     thinking_control_format the Qwen_3 record defaulted to
     No_thinking_control and [supports_extended_thinking=true] never reached
     the wire.

     This asserts the built-in static table. [for_model_id] consults the
     ambient OAS_CAPABILITY_MANIFEST first, and that manifest may carry a
     generic [qwen] entry that the [apply_manifest_entry] codec resolves
     WITHOUT a thinking_control_format (the manifest schema has no such
     field). We pin the static fallback by routing through
     [for_model_id_with_manifest] with a non-matching manifest, mirroring
     [test_manifest_fallback_to_static]. *)
  let non_matching =
    match
      Capability_manifest.of_json
        (Yojson.Safe.from_string {|{"schema_version":1,"models":[]}|})
    with
    | Ok m -> m
    | Error e -> Alcotest.failf "manifest parse error: %s" e
  in
  match Capabilities.for_model_id_with_manifest non_matching "qwen3-32b" with
  | Some c ->
    check bool "supports reasoning" true c.supports_reasoning;
    check_thinking_control
      "qwen3 uses chat_template_kwargs"
      Capabilities.Chat_template_kwargs
      c.thinking_control_format
  | None -> fail "should match qwen3"
;;

let test_lookup_unknown () =
  check
    bool
    "unknown returns None"
    true
    (Capabilities.for_model_id "totally-unknown-model" = None)
;;

let test_lookup_case_insensitive () =
  check bool "uppercase matches" true (Capabilities.for_model_id "Claude-Opus-4-6" <> None)
;;

let test_lookup_glm5_text_only () =
  match Capabilities.for_model_id "glm-5" with
  | Some c ->
    check bool "no image input" false c.supports_image_input;
    check bool "reasoning" true c.supports_reasoning;
    check bool "structured output disabled" false c.supports_structured_output
  | None -> fail "should match glm-5"
;;

let test_lookup_glm5v_vision () =
  match Capabilities.for_model_id "glm-5v-turbo" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs
  | None -> fail "should match glm-5v"
;;

let test_lookup_glm46v_vision () =
  match Capabilities.for_model_id "glm-4.6v-flashx" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs;
    check bool "reasoning" true c.supports_reasoning
  | None -> fail "should match glm-4.6v"
;;

let test_lookup_glm_ocr () =
  match Capabilities.for_model_id "glm-ocr" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs;
    check bool "no tools" false c.supports_tools
  | None -> fail "should match glm-ocr"
;;

let test_ollama_cloud_current_catalog_resolves () =
  let cases =
    [ "deepseek-v4-pro", 524_288, false
    ; "minimax-m2.1", 204_800, false
    ; "minimax-m2.5", 196_608, false
    ; "qwen3.5:397b", 262_144, true
    ; "deepseek-v3.1:671b", 163_840, false
    ; "nemotron-3-nano:30b", 262_144, false
    ; "devstral-2:123b", 262_144, false
    ; "gemma3:12b", 131_072, true
    ; "rnj-1:8b", 32_768, false
    ; "nemotron-3-ultra", 262_144, false
    ; "qwen3-coder:480b", 262_144, false
    ; "devstral-small-2:24b", 262_144, true
    ; "gemini-3-flash-preview", 1_048_576, true
    ; "gemma4:31b", 262_144, true
    ; "kimi-k2.5", 262_144, true
    ; "kimi-k2.7-code", 262_144, true
    ; "gpt-oss:20b", 131_072, false
    ; "gemma3:27b", 131_072, true
    ; "kimi-k2.6", 262_144, true
    ; "deepseek-v3.2", 163_840, false
    ; "mistral-large-3:675b", 262_144, true
    ; "glm-5.1", 202_752, false
    ; "glm-5.2", 1_000_000, false
    ; "gpt-oss:120b", 131_072, false
    ; "minimax-m3", 524_288, true
    ; "ministral-3:3b", 262_144, true
    ; "glm-5", 202_752, false
    ; "qwen3-coder-next", 262_144, false
    ; "minimax-m2.7", 196_608, false
    ; "ministral-3:8b", 262_144, true
    ; "deepseek-v4-flash", 1_048_576, false
    ; "ministral-3:14b", 262_144, true
    ; "gemma3:4b", 131_072, true
    ; "nemotron-3-super", 262_144, false
    ; "glm-4.7", 202_752, false
    ]
  in
  List.iter
    (fun (model_id, context, vision) ->
       match
         Capabilities.for_provider_model_id ~provider_label:"ollama_cloud" ~model_id
       with
       | None -> failf "ollama_cloud/%s should resolve" model_id
       | Some c ->
         check (option int) (model_id ^ " context") (Some context) c.max_context_tokens;
         check bool (model_id ^ " vision") vision c.supports_image_input;
         check bool (model_id ^ " multimodal") vision c.supports_multimodal_inputs;
         check
           bool
           (model_id ^ " reasoning visibility")
           true
           (c.reasoning_visibility_override = Capabilities.Force_visible_text))
    cases
;;

let test_ollama_cloud_provider_qualified_preserves_shared_bare_family () =
  let open Capabilities in
  let bare_glm =
    match for_model_id "glm-5.1" with
    | Some c -> c
    | None -> fail "bare glm-5.1 should resolve"
  in
  let cloud_glm =
    match for_provider_model_id ~provider_label:"ollama_cloud" ~model_id:"glm-5.1" with
    | Some c -> c
    | None -> fail "ollama_cloud/glm-5.1 should resolve"
  in
  check (option int) "bare GLM context" (Some 200_000) bare_glm.max_context_tokens;
  check (option int) "cloud GLM context" (Some 202_752) cloud_glm.max_context_tokens;
  check_thinking_control
    "cloud GLM uses Ollama native think"
    Ollama_think
    cloud_glm.thinking_control_format;
  let bare_glm52 =
    match for_model_id "glm-5.2" with
    | Some c -> c
    | None -> fail "bare glm-5.2 should resolve"
  in
  let cloud_glm52 =
    match for_provider_model_id ~provider_label:"ollama_cloud" ~model_id:"glm-5.2" with
    | Some c -> c
    | None -> fail "ollama_cloud/glm-5.2 should resolve"
  in
  check_thinking_control
    "bare GLM-5.2 keeps ZAI GLM thinking control"
    No_thinking_control
    bare_glm52.thinking_control_format;
  check_thinking_control
    "cloud GLM-5.2 uses Ollama native think"
    Ollama_think
    cloud_glm52.thinking_control_format;
  let bare_kimi =
    match for_model_id "kimi-k2.7-code" with
    | Some c -> c
    | None -> fail "bare kimi-k2.7-code should resolve"
  in
  let cloud_kimi =
    match
      for_provider_model_id ~provider_label:"ollama_cloud" ~model_id:"kimi-k2.7-code"
    with
    | Some c -> c
    | None -> fail "ollama_cloud/kimi-k2.7-code should resolve"
  in
  check_thinking_control
    "bare latest Kimi has no thinking request toggle"
    No_thinking_control
    bare_kimi.thinking_control_format;
  check
    bool
    "bare latest Kimi always preserves reasoning"
    true
    (bare_kimi.preserve_thinking_control_format = Always_preserved_thinking);
  check_thinking_control
    "cloud Kimi uses Ollama native think"
    Ollama_think
    cloud_kimi.thinking_control_format;
  check
    (option int)
    "bare Kimi native context"
    (Some 256_000)
    bare_kimi.max_context_tokens;
  check
    (option int)
    "cloud Kimi official context"
    (Some 262_144)
    cloud_kimi.max_context_tokens;
  check
    bool
    "bare/cloud Kimi contexts intentionally differ"
    true
    (bare_kimi.max_context_tokens <> cloud_kimi.max_context_tokens);
  check bool "bare Kimi vision" true bare_kimi.supports_image_input;
  check bool "cloud Kimi vision" true cloud_kimi.supports_image_input
;;

(* ── with_context_size ───────────────────────────────── *)

let test_with_context_size () =
  let c = Capabilities.default_capabilities in
  let c2 = Capabilities.with_context_size c ~ctx_size:131072 in
  check (option int) "ctx_size set" (Some 131072) c2.max_context_tokens;
  check bool "other fields unchanged" false c2.supports_tools
;;

(* ── Capability manifest ─────────────────────────────── *)

let make_manifest_json ?(base = "default_capabilities") ?(extra_fields = []) prefix =
  let fields =
    [ "id_prefix", Printf.sprintf {|"%s"|} prefix ]
    @ (if base = "default_capabilities"
       then []
       else [ "base", Printf.sprintf {|"%s"|} base ])
    @ extra_fields
  in
  let inner =
    fields |> List.map (fun (k, v) -> Printf.sprintf {|"%s":%s|} k v) |> String.concat ","
  in
  Printf.sprintf {|{"schema_version":1,"models":[{%s}]}|} inner
;;

let make_manifest ?(base = "default_capabilities") ?(extra_fields = []) prefix =
  let json = Yojson.Safe.from_string (make_manifest_json ~base ~extra_fields prefix) in
  match Capability_manifest.of_json json with
  | Ok m -> m
  | Error e -> Alcotest.failf "manifest parse error: %s" e
;;

let test_manifest_overrides_static_table () =
  (* Build a manifest that declares a model with same prefix as claude-opus
     but different capabilities — manifest must win. *)
  let m =
    make_manifest
      ~base:"openai_chat"
      ~extra_fields:
        [ "max_context_tokens", "999999"
        ; "supports_computer_use", "false"
        ; "supports_tools", "true"
        ]
      "claude-opus-4"
  in
  match Capabilities.for_model_id_with_manifest m "claude-opus-4-6" with
  | Some c ->
    check (option int) "manifest overrides ctx" (Some 999999) c.max_context_tokens;
    check bool "manifest overrides computer_use" false c.supports_computer_use;
    check bool "manifest keeps tools" true c.supports_tools
  | None -> fail "expected Some from manifest"
;;

let test_manifest_fallback_to_static () =
  (* Manifest has no entry for claude-opus — should fall through to static table. *)
  let m = make_manifest "totally-other-model" in
  match Capabilities.for_model_id_with_manifest m "claude-opus-4-6" with
  | Some c ->
    check (option int) "fallback ctx 1M" (Some 1_000_000) c.max_context_tokens;
    check bool "fallback computer_use" true c.supports_computer_use
  | None -> fail "should fall through to static table"
;;

let test_manifest_unknown_model_still_none () =
  (* Neither manifest nor static table knows this model. *)
  let m = make_manifest "known-prefix" in
  check
    bool
    "unknown → None"
    true
    (Capabilities.for_model_id_with_manifest m "totally-unknown-xyz" = None)
;;

let test_manifest_base_label_openai_chat () =
  let m =
    make_manifest
      ~base:"openai_chat"
      ~extra_fields:[ "max_context_tokens", "65536" ]
      "custom-gpt"
  in
  match Capabilities.for_model_id_with_manifest m "custom-gpt-v2" with
  | Some c ->
    check (option int) "custom ctx" (Some 65536) c.max_context_tokens;
    check bool "openai_chat base: tools" true c.supports_tools;
    check bool "openai_chat base: streaming" true c.supports_native_streaming
  | None -> fail "expected Some"
;;

let test_manifest_base_label_anthropic () =
  let m =
    make_manifest
      ~base:"anthropic"
      ~extra_fields:[ "max_context_tokens", "512000" ]
      "my-claude"
  in
  match Capabilities.for_model_id_with_manifest m "my-claude-custom" with
  | Some c ->
    check (option int) "custom ctx 512K" (Some 512000) c.max_context_tokens;
    check bool "anthropic base: caching" true c.supports_caching;
    check bool "anthropic base: extended thinking" true c.supports_extended_thinking
  | None -> fail "expected Some"
;;

let test_manifest_base_absent_uses_default () =
  (* No base label — should use default_capabilities as base. *)
  let m =
    make_manifest
      ~extra_fields:[ "supports_tools", "true"; "max_context_tokens", "32768" ]
      "my-special-model"
  in
  match Capabilities.for_model_id_with_manifest m "my-special-model-q4" with
  | Some c ->
    check bool "tools overridden true" true c.supports_tools;
    check (option int) "ctx overridden" (Some 32768) c.max_context_tokens;
    (* default has supports_reasoning=false — not overridden *)
    check bool "reasoning unchanged from default" false c.supports_reasoning
  | None -> fail "expected Some"
;;

let test_example_manifest_base_labels_are_canonical () =
  let path =
    [ "docs/example-capability-manifest.json"
    ; "../docs/example-capability-manifest.json"
    ; "../../docs/example-capability-manifest.json"
    ]
    |> List.find_opt Sys.file_exists
    |> Option.value ~default:"docs/example-capability-manifest.json"
  in
  match Capability_manifest.load_file path with
  | Error msg -> failf "example manifest should parse: %s" msg
  | Ok entries ->
    List.iter
      (fun (entry : Capability_manifest.entry) ->
         match entry.base_label with
         | None ->
           (* The Codex Spark entry intentionally does not inherit a public
              chat-completions preset; every other example entry must specify a
              base so the manifest actually applies the intended preset. *)
           if entry.id_prefix <> "gpt-5.3-codex-spark"
           then
             check bool (Printf.sprintf "expected base for %s" entry.id_prefix) true false
         | Some label ->
           check
             bool
             (Printf.sprintf "base label resolves for %s: %s" entry.id_prefix label)
             true
             (Option.is_some (Capabilities.capabilities_for_provider_label label)))
      entries
;;

let test_manifest_prefix_wins_over_longer_static_prefix () =
  (* Manifest entry "dashscope-3" must win over static table "dashscope-3" prefix too,
     letting operator override even well-known models. *)
  let m =
    make_manifest
      ~base:"openai_chat"
      ~extra_fields:[ "supports_reasoning", "false" ]
      "dashscope-3"
  in
  match Capabilities.for_model_id_with_manifest m "dashscope-3.5-35b-a3b-q4" with
  | Some c ->
    check bool "manifest disables reasoning" false c.supports_reasoning;
    check bool "base openai_chat: tools" true c.supports_tools
  | None -> fail "expected Some"
;;

let test_apply_manifest_entry_all_none_uses_base () =
  (* Entry with only id_prefix set — should be identical to base. *)
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"x","base":"anthropic"}]}|}
  in
  let manifest = Capability_manifest.of_json json |> Result.get_ok in
  let entry = List.hd manifest in
  let caps = Capabilities.apply_manifest_entry entry in
  let base = Capabilities.anthropic_capabilities in
  check bool "tools matches base" base.supports_tools caps.supports_tools;
  check (option int) "ctx matches base" base.max_context_tokens caps.max_context_tokens;
  check bool "caching matches base" base.supports_caching caps.supports_caching
;;

let test_manifest_wrong_type_feature_fields_warn_and_ignore () =
  let warnings = ref [] in
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"typed","max_context_tokens":"131072","supports_tools":"yes"}]}|}
  in
  let manifest =
    Diag.with_sink
      (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
      (fun () -> Capability_manifest.of_json json)
  in
  let entry =
    match manifest with
    | Ok [ entry ] -> entry
    | Ok _ -> Alcotest.fail "expected one manifest entry"
    | Error msg -> Alcotest.failf "unexpected parse error: %s" msg
  in
  check (option int) "wrong-type int ignored" None entry.max_context_tokens;
  check (option bool) "wrong-type bool ignored" None entry.supports_tools;
  let has_warning field expected =
    List.exists
      (fun (level, ctx, msg) ->
         level = Diag.Warn
         && String.equal ctx "capability_manifest"
         && string_contains_sub msg (Printf.sprintf "field %S" field)
         && string_contains_sub msg (Printf.sprintf "expected %s" expected))
      !warnings
  in
  check bool "warned for max_context_tokens" true (has_warning "max_context_tokens" "int");
  check bool "warned for supports_tools" true (has_warning "supports_tools" "bool")
;;

let test_manifest_rejects_wrong_type_base () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"bad-base","base":17}]}|}
  in
  match Capability_manifest.of_json json with
  | Error msg ->
    check_contains "mentions field" msg "base";
    check_contains "mentions expected type" msg "expected string"
  | Ok _ -> Alcotest.fail "wrong-type base should reject"
;;

let test_manifest_rejects_wrong_type_policy_string () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"bad-policy","thinking_control_format":false}]}|}
  in
  match Capability_manifest.of_json json with
  | Error msg ->
    check_contains "mentions field" msg "thinking_control_format";
    check_contains "mentions expected type" msg "expected string"
  | Ok _ -> Alcotest.fail "wrong-type thinking_control_format should reject"
;;

let test_manifest_accepts_ollama_think_policy_string () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"ollama-manifest","thinking_control_format":"ollama_think"}]}|}
  in
  match Capability_manifest.of_json json with
  | Ok [ entry ] ->
    let caps = Capabilities.apply_manifest_entry entry in
    check_thinking_control
      "manifest ollama_think"
      Capabilities.Ollama_think
      caps.thinking_control_format
  | Ok _ -> Alcotest.fail "expected one manifest entry"
  | Error msg -> Alcotest.failf "unexpected parse error: %s" msg
;;

let test_manifest_rejects_unknown_preserve_thinking_control_format () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"bad-preserve","preserve_thinking_control_format":"memory_palace"}]}|}
  in
  match Capability_manifest.of_json json with
  | Error msg -> check_contains "mentions field" msg "preserve_thinking_control_format"
  | Ok _ -> Alcotest.fail "unknown preserve_thinking_control_format should reject"
;;

let test_manifest_rejects_unknown_reasoning_replay () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"bad-replay","reasoning_replay":"preserve-allways"}]}|}
  in
  match Capability_manifest.of_json json with
  | Error msg ->
    check_contains "mentions field" msg "reasoning_replay";
    check_contains "mentions value" msg "preserve-allways"
  | Ok _ -> Alcotest.fail "unknown reasoning_replay should reject"
;;

let test_manifest_rejects_unknown_reasoning_visibility () =
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"bad-visibility","reasoning_visibility":"translucent"}]}|}
  in
  match Capability_manifest.of_json json with
  | Error msg ->
    check_contains "mentions field" msg "reasoning_visibility";
    check_contains "mentions value" msg "translucent"
  | Ok _ -> Alcotest.fail "unknown reasoning_visibility should reject"
;;

let test_manifest_intlit_in_range_accepted () =
  (* Yojson.Safe represents large literals as `Intlit s. Build the JSON value
     directly to exercise the Intlit branch deterministically. *)
  let json =
    `Assoc
      [ "schema_version", `Int 1
      ; ( "models"
        , `List
            [ `Assoc
                [ "id_prefix", `String "intlit-ok"
                ; "max_context_tokens", `Intlit "131072"
                ]
            ] )
      ]
  in
  match Capability_manifest.of_json json with
  | Ok [ entry ] ->
    check (option int) "intlit accepted" (Some 131_072) entry.max_context_tokens
  | Ok _ -> Alcotest.fail "expected one manifest entry"
  | Error msg -> Alcotest.failf "unexpected parse error: %s" msg
;;

let test_manifest_intlit_out_of_range_warns () =
  let warnings = ref [] in
  let huge = "99999999999999999999999999" in
  let json =
    `Assoc
      [ "schema_version", `Int 1
      ; ( "models"
        , `List
            [ `Assoc
                [ "id_prefix", `String "intlit-overflow"
                ; "max_context_tokens", `Intlit huge
                ]
            ] )
      ]
  in
  let manifest =
    Diag.with_sink
      (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
      (fun () -> Capability_manifest.of_json json)
  in
  let entry =
    match manifest with
    | Ok [ entry ] -> entry
    | Ok _ -> Alcotest.fail "expected one manifest entry"
    | Error msg -> Alcotest.failf "unexpected parse error: %s" msg
  in
  check (option int) "out-of-range intlit ignored" None entry.max_context_tokens;
  let has_warning =
    List.exists
      (fun (level, ctx, msg) ->
         level = Diag.Warn
         && String.equal ctx "capability_manifest"
         && string_contains_sub msg "max_context_tokens"
         && string_contains_sub msg "out of native int range")
      !warnings
  in
  check bool "warned about overflow" true has_warning
;;

let test_manifest_load_file_missing_returns_error () =
  let path = Filename.temp_file "oas-capability-manifest-missing" ".json" in
  Sys.remove path;
  match Capability_manifest.load_file path with
  | Error msg ->
    check_contains "mentions cannot read" msg "cannot read capability manifest";
    check_contains "mentions path" msg path
  | Ok _ -> Alcotest.fail "expected missing manifest path to fail"
;;

let test_manifest_load_file_malformed_returns_error () =
  with_temp_manifest {|{"schema_version":1,"models":[|} (fun path ->
    match Capability_manifest.load_file path with
    | Error msg ->
      check_contains "mentions JSON parse" msg "capability manifest JSON parse error";
      check_contains "mentions path" msg path
    | Ok _ -> Alcotest.fail "expected malformed manifest JSON to fail")
;;

let test_manifest_load_runtime_file_success_logs_info () =
  let logs = ref [] in
  with_temp_manifest
    {|{"schema_version":1,"models":[{"id_prefix":"runtime-visible","supports_tools":true}] }|}
    (fun path ->
       let manifest =
         Diag.with_sink
           (fun level ~ctx msg -> logs := (level, ctx, msg) :: !logs)
           (fun () -> Capability_manifest.load_runtime_file path)
       in
       (match manifest with
        | Some [ entry ] ->
          check string "loaded id_prefix" "runtime-visible" entry.id_prefix
        | Some _ -> Alcotest.fail "expected one runtime manifest entry"
        | None -> Alcotest.fail "expected runtime manifest to load");
       let has_info =
         List.exists
           (fun (level, ctx, msg) ->
              level = Diag.Info
              && String.equal ctx "capability_manifest"
              && string_contains_sub msg "loaded 1 entries"
              && string_contains_sub msg path)
           !logs
       in
       check bool "logs info load success" true has_info)
;;

let test_model_catalog_rejects_unknown_reasoning_replay () =
  with_temp_model_catalog
    {|
[[models]]
id_prefix = "bad-replay"
reasoning_replay = "preserve-allways"
|}
    (fun path ->
       match Model_catalog.load_file path with
       | Error msg ->
         check_contains "mentions field" msg "reasoning_replay";
         check_contains "mentions value" msg "preserve-allways"
       | Ok _ -> Alcotest.fail "unknown model catalog reasoning_replay should reject")
;;

let test_model_catalog_rejects_wrong_type_policy_string () =
  with_temp_model_catalog
    {|
[[models]]
id_prefix = "bad-policy-type"
reasoning_replay = true
|}
    (fun path ->
       match Model_catalog.load_file path with
       | Error msg ->
         check_contains "mentions field" msg "reasoning_replay";
         check_contains "mentions expected type" msg "expected string"
       | Ok _ -> Alcotest.fail "wrong-type model catalog reasoning_replay should reject")
;;

let test_model_catalog_rejects_unknown_policy_strings () =
  let cases =
    [ "thinking_control_format", "mind_palace"
    ; "preserve_thinking_control_format", "memory_palace"
    ; "reasoning_visibility", "visible_ether"
    ; "modality_priority", "image_only"
    ]
  in
  List.iter
    (fun (field, value) ->
       with_temp_model_catalog
         (Printf.sprintf
            {|
[[models]]
id_prefix = "bad-%s"
%s = "%s"
|}
            field
            field
            value)
         (fun path ->
            match Model_catalog.load_file path with
            | Error msg ->
              check_contains (field ^ " mentions field") msg field;
              check_contains (field ^ " mentions value") msg value
            | Ok _ -> Alcotest.failf "unknown model catalog %s should reject" field))
    cases
;;

let test_model_catalog_rejects_wrong_type_base_label () =
  with_temp_model_catalog
    {|
[[models]]
id_prefix = "bad-base-type"
base = 17
|}
    (fun path ->
       match Model_catalog.load_file path with
       | Error msg ->
         check_contains "mentions base" msg "base";
         check_contains "mentions expected type" msg "expected string"
       | Ok _ -> Alcotest.fail "wrong-type model catalog base should reject")
;;

(* ── DashScope preset ────────────────────────────────── *)

let test_dashscope_capabilities () =
  let c = Capabilities.dashscope_capabilities in
  (* DashScope (DashScope) exposes response_format.json_schema on its OpenAI-compatible
     endpoint; native schema output is supported. Ref: DashScope structured output
     guide — checked 2026-05-05. *)
  check bool "has structured output" true c.supports_structured_output;
  check bool "has json mode" true c.supports_response_format_json;
  check bool "has tools" true c.supports_tools;
  check bool "has tool_choice" true c.supports_tool_choice;
  check bool "has reasoning" true c.supports_reasoning;
  check_thinking_control
    "enable_thinking control"
    Capabilities.Enable_thinking
    c.thinking_control_format;
  check bool "has min_p" true c.supports_min_p
;;

let test_openai_compat_reasoning_records_have_explicit_control () =
  let cases =
    [ "openai_chat_extended", Some Capabilities.openai_compat_chat_extended_capabilities
    ; "kimi", Some Capabilities.kimi_capabilities
    ; "dashscope", Some Capabilities.dashscope_capabilities
    ; "mimo-v2.5-pro", Capabilities.for_model_id "mimo-v2.5-pro"
    ; "dashscope-3.5", Capabilities.for_model_id "dashscope-3.5-35b-a3b"
    ; "deepseek-v4-flash", Capabilities.for_model_id "deepseek-v4-flash"
    ; "nvidia-ultra", Capabilities.for_model_id "nvidia-ultra-253b"
    ]
  in
  List.iter
    (fun (label, caps) ->
       match caps with
       | None -> fail (Printf.sprintf "%s should resolve capabilities" label)
       | Some (c : Capabilities.capabilities) ->
         check bool (label ^ " supports reasoning") true c.supports_reasoning;
         check
           bool
           (label ^ " has explicit reasoning control or preserve policy")
           true
           (c.thinking_control_format <> Capabilities.No_thinking_control
            || c.preserve_thinking_control_format
               <> Capabilities.No_preserve_thinking_control))
    cases
;;

(* ── Prefix ordering invariant (M01) ────────────────────── *)

(* [for_model_id] resolves capabilities via a sequential if-else chain of
   [starts_with] prefix checks.  Whenever prefix A is a string prefix of
   prefix B (every model-id starting with B also starts with A), the branch
   for B *must* be evaluated before the branch for A; otherwise any model-id
   that starts with B is permanently captured by A, silently returning wrong
   capabilities (e.g. tool_choice sent to a model that does not support it
   → 400 error, anti-pattern M01).

   Each case below uses a concrete model-id that begins with the *longer*
   (more-specific) prefix — and therefore also with the *shorter* one — and
   asserts the capability fingerprint that is unique to the longer branch.
   If the two branches were swapped the assertion would fail.

   When adding a new prefix to [for_model_id], check whether it creates a
   new shadow pair with an existing prefix and add a corresponding entry
   here.  The full ordered prefix list lives in
   [lib/llm_provider/capabilities.ml]. *)
let test_prefix_ordering_invariant () =
  (* Each entry: (model_id, label, discriminating_predicate).
     The predicate is true only when the more-specific (longer-prefix)
     branch wins. *)
  let cases =
    [ (* glm-5v-turbo must precede glm-5 (inside broad branch).
         Discriminator: supports_image_input (5v-turbo) vs not (broad glm-5). *)
      ( "glm-5v-turbo-x"
      , "glm-5v-turbo must precede broad glm-5"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input && c.max_output_tokens = Some 128_000 )
    ; (* glm-5-code must precede glm-5 (inside broad branch).
         Discriminator: 128K context (code branch) vs 200K (broad glm-5). *)
      ( "glm-5-code-x"
      , "glm-5-code must precede broad glm-5"
      , fun (c : Capabilities.capabilities) ->
          c.max_context_tokens = Some 128_000 && c.supports_extended_thinking )
    ; (* glm-4.6v must precede glm-4.6 (inside broad branch) *)
      ( "glm-4.6v-x"
      , "glm-4.6v must precede broad glm-4.6"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 32_768 )
    ; (* glm-4.5v must precede glm-4.5 (inside broad branch) *)
      ( "glm-4.5v-x"
      , "glm-4.5v must precede broad glm-4.5"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 16_384 )
    ; (* broad glm-4.5 branch must precede glm-4.
         Discriminator: supports_reasoning + 96K output (broad) vs neither (glm-4). *)
      ( "glm-4.5-latest"
      , "broad glm-4.5 branch must precede glm-4"
      , fun (c : Capabilities.capabilities) ->
          c.supports_reasoning && c.max_output_tokens = Some 96_000 )
    ; (* glm-4v must precede glm-4.
         Discriminator: supports_image_input (glm-4v) vs not (glm-4). *)
      ( "glm-4v-x"
      , "glm-4v must precede glm-4"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input && c.supports_multimodal_inputs )
    ]
  in
  List.iter
    (fun (model_id, label, ok) ->
       match Capabilities.for_model_id model_id with
       | None ->
         fail
           (Printf.sprintf
              "prefix ordering [%s]: for_model_id %S returned None"
              label
              model_id)
       | Some c -> check bool (Printf.sprintf "prefix ordering: %s" label) true (ok c))
    cases
;;

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run
    "Capabilities"
    [ ( "defaults"
      , [ test_case "no limits" `Quick test_default_no_limits
        ; test_case "new fields false" `Quick test_default_new_fields_false
        ] )
    ; ( "presets"
      , [ test_case "anthropic" `Quick test_anthropic_capabilities
        ; test_case "openai" `Quick test_openai_capabilities
        ; test_case "openai extended" `Quick test_openai_extended
        ; test_case "dashscope" `Quick test_dashscope_capabilities
        ; test_case
            "openai compat reasoning records have explicit control"
            `Quick
            test_openai_compat_reasoning_records_have_explicit_control
        ] )
    ; ( "model_lookup"
      , [ test_case "claude opus" `Quick test_lookup_claude_opus
        ; test_case "claude sonnet" `Quick test_lookup_claude_sonnet
        ; test_case "gpt-5" `Quick test_lookup_gpt5
        ; test_case "gemini" `Quick test_lookup_gemini
        ; test_case "gemini_family Gemini_3_1" `Quick test_gemini_family_3_1
        ; test_case
            "gemini_family Gemini_3_1 flash-lite"
            `Quick
            test_gemini_family_3_1_flash_lite
        ; test_case "gemini_family Gemini_3" `Quick test_gemini_family_3
        ; test_case "gemini_family Gemini_2_5" `Quick test_gemini_family_2_5
        ; test_case
            "gemini_family Gemini_other (non-gemini)"
            `Quick
            test_gemini_family_other_non_gemini
        ; test_case
            "gemini_family Gemini_other (unknown gemini)"
            `Quick
            test_gemini_family_other_unknown_gemini
        ; test_case
            "gemini_family drives 1M ctx capabilities"
            `Quick
            test_gemini_family_drives_capabilities
        ; test_case
            "kimi-k2 native cloud suffix vs Ollama Cloud"
            `Quick
            test_lookup_kimi_k2_native_cloud_suffix
        ; test_case "dashscope" `Quick test_lookup_provider_m
        ; test_case "dashscope runpod name" `Quick test_lookup_provider_m_runpod_name
        ; test_case "deepseek v4 flash" `Quick test_lookup_deepseek_v4_flash
        ; test_case "deepseek v4 pro" `Quick test_lookup_deepseek_v4_pro
        ; test_case "grok 4.3 1M context" `Quick test_lookup_grok
        ; test_case "glm-5 text only" `Quick test_lookup_glm5_text_only
        ; test_case "glm-5v vision" `Quick test_lookup_glm5v_vision
        ; test_case "glm-4.6v vision" `Quick test_lookup_glm46v_vision
        ; test_case "glm-ocr vision" `Quick test_lookup_glm_ocr
        ; test_case
            "ollama cloud current catalog"
            `Quick
            test_ollama_cloud_current_catalog_resolves
        ; test_case
            "ollama cloud preserves shared bare families"
            `Quick
            test_ollama_cloud_provider_qualified_preserves_shared_bare_family
        ; test_case "mimo-v2.5-pro" `Quick test_lookup_mimo_v25_pro
        ; test_case "qwen3 thinking control" `Quick test_lookup_qwen3_thinking_control
        ; test_case "unknown" `Quick test_lookup_unknown
        ; test_case "case insensitive" `Quick test_lookup_case_insensitive
        ] )
    ; "merge", [ test_case "with_context_size" `Quick test_with_context_size ]
    ; ( "manifest"
      , [ test_case "overrides static table" `Quick test_manifest_overrides_static_table
        ; test_case "fallback to static" `Quick test_manifest_fallback_to_static
        ; test_case "unknown model → None" `Quick test_manifest_unknown_model_still_none
        ; test_case "base openai_chat" `Quick test_manifest_base_label_openai_chat
        ; test_case "base anthropic" `Quick test_manifest_base_label_anthropic
        ; test_case "base absent = default" `Quick test_manifest_base_absent_uses_default
        ; test_case
            "example base labels are canonical"
            `Quick
            test_example_manifest_base_labels_are_canonical
        ; test_case
            "manifest prefix wins"
            `Quick
            test_manifest_prefix_wins_over_longer_static_prefix
        ; test_case
            "all-None entry matches base"
            `Quick
            test_apply_manifest_entry_all_none_uses_base
        ; test_case
            "wrong-type feature fields warn and ignore"
            `Quick
            test_manifest_wrong_type_feature_fields_warn_and_ignore
        ; test_case "wrong-type base rejects" `Quick test_manifest_rejects_wrong_type_base
        ; test_case
            "wrong-type policy string rejects"
            `Quick
            test_manifest_rejects_wrong_type_policy_string
        ; test_case
            "ollama_think policy string accepted"
            `Quick
            test_manifest_accepts_ollama_think_policy_string
        ; test_case
            "unknown preserve_thinking_control_format rejects"
            `Quick
            test_manifest_rejects_unknown_preserve_thinking_control_format
        ; test_case
            "unknown reasoning_replay rejects"
            `Quick
            test_manifest_rejects_unknown_reasoning_replay
        ; test_case
            "unknown reasoning_visibility rejects"
            `Quick
            test_manifest_rejects_unknown_reasoning_visibility
        ; test_case
            "intlit in range accepted"
            `Quick
            test_manifest_intlit_in_range_accepted
        ; test_case
            "intlit out of range warns"
            `Quick
            test_manifest_intlit_out_of_range_warns
        ; test_case
            "missing manifest file errors"
            `Quick
            test_manifest_load_file_missing_returns_error
        ; test_case
            "malformed manifest file errors"
            `Quick
            test_manifest_load_file_malformed_returns_error
        ; test_case
            "runtime manifest load logs success"
            `Quick
            test_manifest_load_runtime_file_success_logs_info
        ; test_case
            "model catalog rejects unknown reasoning_replay"
            `Quick
            test_model_catalog_rejects_unknown_reasoning_replay
        ; test_case
            "model catalog rejects wrong-type policy string"
            `Quick
            test_model_catalog_rejects_wrong_type_policy_string
        ; test_case
            "model catalog rejects unknown policy strings"
            `Quick
            test_model_catalog_rejects_unknown_policy_strings
        ; test_case
            "model catalog rejects wrong-type base"
            `Quick
            test_model_catalog_rejects_wrong_type_base_label
        ] )
    ; ( "prefix_ordering"
      , [ test_case
            "shadow pairs all resolve to specific branch (M01)"
            `Quick
            test_prefix_ordering_invariant
        ] )
    ]
;;
