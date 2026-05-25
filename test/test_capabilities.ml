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

let test_provider_a_capabilities () =
  let c = Capabilities.provider_a_capabilities in
  check bool "has tools" true c.supports_tools;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "has extended thinking" true c.supports_extended_thinking;
  check bool "has reasoning budget" true c.supports_reasoning_budget;
  check bool "has image" true c.supports_image_input;
  check bool "has caching" true c.supports_caching;
  check bool "has computer use" true c.supports_computer_use;
  check bool "has structured output" true c.supports_structured_output;
  check bool "no audio" false c.supports_audio_input;
  (* Provider_a Messages API accepts top_k per its documented body
     params; pin the field so the #830/#831 capability-gated
     serializer paths do not silently drop it for agent_llm_a configs. *)
  check bool "supports top_k" true c.supports_top_k;
  check bool "no min_p" false c.supports_min_p;
  check bool "context 200K" true (c.max_context_tokens = Some 200_000)
;;

let test_provider_d_capabilities () =
  let c = Capabilities.provider_d_chat_capabilities in
  check bool "has structured output" true c.supports_structured_output;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "no reasoning" false c.supports_reasoning;
  check bool "context 128K" true (c.max_context_tokens = Some 128_000)
;;

let test_provider_d_extended () =
  let c = Capabilities.provider_d_chat_extended_capabilities in
  check bool "has reasoning" true c.supports_reasoning;
  check_thinking_control
    "uses reasoning_effort"
    Capabilities.Reasoning_effort
    c.thinking_control_format;
  check bool "has top_k" true c.supports_top_k;
  check bool "has min_p" true c.supports_min_p
;;

(* ── Model lookup ────────────────────────────────────── *)

let test_lookup_agent_llm_a_opus () =
  match Capabilities.for_model_id "agent_llm_a-opus-4-6" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match agent_llm_a-opus"
;;

let test_lookup_agent_llm_a_sonnet () =
  match Capabilities.for_model_id "agent_llm_a-sonnet-4-6" with
  | Some c ->
    check (option int) "output 64K" (Some 64_000) c.max_output_tokens;
    check bool "parallel tools" true c.supports_parallel_tool_calls
  | None -> fail "should match agent_llm_a-sonnet"
;;

let test_lookup_gpt5 () =
  match Capabilities.for_model_id "model-d-5.4" with
  | Some c ->
    check (option int) "context 1.05M" (Some 1_050_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "structured output" true c.supports_structured_output;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match model-d-5"
;;

let test_lookup_provider_f () =
  match Capabilities.for_model_id "provider_f-3.1-pro" with
  | Some c ->
    check bool "audio" true c.supports_audio_input;
    check bool "video" true c.supports_video_input;
    check bool "code execution" true c.supports_code_execution;
    check bool "structured output" true c.supports_structured_output
  | None -> fail "should match provider_f"
;;

(* ── Typed provider_f_family classifier (root-fix for #968) ─────── *)

let pp_provider_f_family ppf = function
  | Capabilities.Provider_f_3_1 -> Format.fprintf ppf "Provider_f_3_1"
  | Capabilities.Provider_f_3 -> Format.fprintf ppf "Provider_f_3"
  | Capabilities.Provider_f_2_5 -> Format.fprintf ppf "Provider_f_2_5"
  | Capabilities.Provider_f_other s -> Format.fprintf ppf "Provider_f_other(%s)" s
;;

let provider_f_family_testable = Alcotest.testable pp_provider_f_family ( = )

let pp_static_model_route ppf = function
  | Capabilities.Agent_llm_a_opus_4 -> Format.fprintf ppf "Agent_llm_a_opus_4"
  | Capabilities.Agent_llm_a_sonnet_4 -> Format.fprintf ppf "Agent_llm_a_sonnet_4"
  | Capabilities.Agent_llm_a_haiku_4 -> Format.fprintf ppf "Agent_llm_a_haiku_4"
  | Capabilities.Provider_d_5 -> Format.fprintf ppf "Provider_d_5"
  | Capabilities.Provider_d_4_1 -> Format.fprintf ppf "Provider_d_4_1"
  | Capabilities.Provider_d_4o -> Format.fprintf ppf "Provider_d_4o"
  | Capabilities.Provider_f family ->
    Format.fprintf ppf "Provider_f(%a)" pp_provider_f_family family
  | Capabilities.Provider_c_for_coding -> Format.fprintf ppf "Provider_c_for_coding"
  | Capabilities.Provider_c_k2 -> Format.fprintf ppf "Provider_c_k2"
  | Capabilities.Provider_h_3 -> Format.fprintf ppf "Provider_h_3"
  | Capabilities.Provider_n_4 -> Format.fprintf ppf "Provider_n_4"
  | Capabilities.Provider_g_v4_flash -> Format.fprintf ppf "Provider_g_v4_flash"
  | Capabilities.Provider_g_v4_pro -> Format.fprintf ppf "Provider_g_v4_pro"
  | Capabilities.Provider_j_large -> Format.fprintf ppf "Provider_j_large"
  | Capabilities.Provider_j_small -> Format.fprintf ppf "Provider_j_small"
  | Capabilities.Provider_m_command -> Format.fprintf ppf "Provider_m_command"
  | Capabilities.Provider_e_grok -> Format.fprintf ppf "Provider_e_grok"
  | Capabilities.Provider_l { has_vision } ->
    Format.fprintf ppf "Provider_l(has_vision=%b)" has_vision
  | Capabilities.Provider_f_gemma_4 { has_large_audio } ->
    Format.fprintf ppf "Provider_f_gemma_4(has_large_audio=%b)" has_large_audio
  | Capabilities.Glm_4_7_flash -> Format.fprintf ppf "Glm_4_7_flash"
  | Capabilities.Glm_4_5_flash_air -> Format.fprintf ppf "Glm_4_5_flash_air"
  | Capabilities.Glm_5_turbo -> Format.fprintf ppf "Glm_5_turbo"
  | Capabilities.Glm_5v_turbo -> Format.fprintf ppf "Glm_5v_turbo"
  | Capabilities.Glm_ocr -> Format.fprintf ppf "Glm_ocr"
  | Capabilities.Glm_4_6_vision_reasoning -> Format.fprintf ppf "Glm_4_6_vision_reasoning"
  | Capabilities.Glm_4_5_vision_reasoning -> Format.fprintf ppf "Glm_4_5_vision_reasoning"
  | Capabilities.Glm_5_code -> Format.fprintf ppf "Glm_5_code"
  | Capabilities.Glm_4_5_text -> Format.fprintf ppf "Glm_4_5_text"
  | Capabilities.Glm_full_text -> Format.fprintf ppf "Glm_full_text"
  | Capabilities.Glm_4_flash -> Format.fprintf ppf "Glm_4_flash"
  | Capabilities.Glm_4v -> Format.fprintf ppf "Glm_4v"
  | Capabilities.Glm_4 -> Format.fprintf ppf "Glm_4"
;;

let static_model_route_testable = Alcotest.testable pp_static_model_route ( = )

let test_provider_f_family_3_1 () =
  check
    provider_f_family_testable
    "provider_f-3.1-pro-preview classifies as Provider_f_3_1"
    Capabilities.Provider_f_3_1
    (Capabilities.provider_f_family_of_id "provider_f-3.1-pro-preview")
;;

let test_provider_f_family_3 () =
  check
    provider_f_family_testable
    "provider_f-3-flash-preview classifies as Provider_f_3 (not 3.1)"
    Capabilities.Provider_f_3
    (Capabilities.provider_f_family_of_id "provider_f-3-flash-preview")
;;

let test_provider_f_family_2_5 () =
  check
    provider_f_family_testable
    "provider_f-2.5-flash classifies as Provider_f_2_5"
    Capabilities.Provider_f_2_5
    (Capabilities.provider_f_family_of_id "provider_f-2.5-flash")
;;

let test_provider_f_family_other_non_provider_f () =
  check
    provider_f_family_testable
    "non-provider_f id falls into Provider_f_other with literal retained"
    (Capabilities.Provider_f_other "agent_llm_a-opus-4")
    (Capabilities.provider_f_family_of_id "agent_llm_a-opus-4")
;;

let test_provider_f_family_other_unknown_provider_f () =
  (* A future provider_f line not yet classified should land in Provider_f_other —
     not be silently absorbed into an existing arm. *)
  check
    provider_f_family_testable
    "provider_f-4-foo lands in Provider_f_other (no silent fallback)"
    (Capabilities.Provider_f_other "provider_f-4-foo")
    (Capabilities.provider_f_family_of_id "provider_f-4-foo")
;;

let test_provider_f_family_drives_capabilities () =
  (* Behavioural cross-check: all three live variants resolve to
     provider_f_capabilities (1M context). This is the property the #968 drift
     gate was trying to assert via string-grep; now it is enforced by the
     type system at the dispatch site and by this test. *)
  let ctx id =
    match Capabilities.for_model_id id with
    | Some c -> c.max_context_tokens
    | None -> None
  in
  check
    (option int)
    "provider_f-3-flash-preview ctx"
    (Some 1_000_000)
    (ctx "provider_f-3-flash-preview");
  check
    (option int)
    "provider_f-3.1-pro-preview ctx"
    (Some 1_000_000)
    (ctx "provider_f-3.1-pro-preview");
  check
    (option int)
    "provider_f-2.5-flash ctx"
    (Some 1_000_000)
    (ctx "provider_f-2.5-flash")
;;

let test_static_model_route_normalizes_cloud_suffix () =
  check
    (option static_model_route_testable)
    "provider_c-k2 cloud route"
    (Some Capabilities.Provider_c_k2)
    (Capabilities.static_model_route_of_id " provider_c-k2.6:cloud ");
  check
    (option static_model_route_testable)
    "provider_g cloud route"
    (Some Capabilities.Provider_g_v4_pro)
    (Capabilities.static_model_route_of_id "provider_g-v4-pro:cloud");
  check
    (option static_model_route_testable)
    "provider_k cloud route"
    (Some Capabilities.Glm_full_text)
    (Capabilities.static_model_route_of_id "provider_k-5.1:cloud")
;;

let test_lookup_provider_c_k2_cloud () =
  match Capabilities.for_model_id "provider_c-k2.6:cloud" with
  | Some c ->
    check (option int) "context 262K" (Some 262_144) c.max_context_tokens;
    check (option int) "output 32K" (Some 32_768) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check_thinking_control
      "thinking object only"
      Capabilities.Thinking_object_only
      c.thinking_control_format;
    check bool "code execution" true c.supports_code_execution
  | None -> fail "should match provider_c-k2 cloud route"
;;

let test_lookup_provider_m () =
  match Capabilities.for_model_id "provider_h-3.5-35b-a3b" with
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
  | None -> fail "should match provider_h_3"
;;

let test_lookup_provider_m_runpod_name () =
  match Capabilities.for_model_id "Provider_h_3.6-35B-A3B-UD-Q4_K_XL.gguf" with
  | Some c ->
    check
      bool
      "runpod provider_h_3.6 uses chat_template_kwargs"
      true
      (c.thinking_control_format = Capabilities.Chat_template_kwargs)
  | None -> fail "should match provider_h_3.6 runpod model id"
;;

let test_lookup_provider_g_v4_flash () =
  match Capabilities.for_model_id "provider_g-v4-flash" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check bool "caching" true c.supports_caching
  | None -> fail "should match provider_g-v4-flash"
;;

let test_lookup_provider_g_v4_pro () =
  match Capabilities.for_model_id "provider_g-v4-pro" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check bool "caching" true c.supports_caching
  | None -> fail "should match provider_g-v4-pro"
;;

let test_lookup_grok () =
  match Capabilities.for_model_id "model-e" with
  | Some c ->
    check (option int) "context 2M" (Some 2_000_000) c.max_context_tokens;
    check bool "structured" true c.supports_structured_output
  | None -> fail "should match grok"
;;

let test_lookup_unknown () =
  check
    bool
    "unknown returns None"
    true
    (Capabilities.for_model_id "totally-unknown-model" = None)
;;

let test_lookup_case_insensitive () =
  check
    bool
    "uppercase matches"
    true
    (Capabilities.for_model_id "Agent_llm_a-Opus-4-6" <> None)
;;

let test_lookup_glm5_text_only () =
  match Capabilities.for_model_id "provider_k-5" with
  | Some c ->
    check bool "no image input" false c.supports_image_input;
    check bool "reasoning" true c.supports_reasoning;
    check bool "structured output disabled" false c.supports_structured_output
  | None -> fail "should match provider_k-5"
;;

let test_lookup_glm5v_vision () =
  match Capabilities.for_model_id "provider_k-5v-turbo" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs
  | None -> fail "should match provider_k-5v"
;;

let test_lookup_glm46v_vision () =
  match Capabilities.for_model_id "provider_k-4.6v-flashx" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs;
    check bool "reasoning" true c.supports_reasoning
  | None -> fail "should match provider_k-4.6v"
;;

let test_lookup_glm_ocr () =
  match Capabilities.for_model_id "provider_k-ocr" with
  | Some c ->
    check bool "has image input" true c.supports_image_input;
    check bool "multimodal" true c.supports_multimodal_inputs;
    check bool "no tools" false c.supports_tools
  | None -> fail "should match provider_k-ocr"
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
  (* Build a manifest that declares a model with same prefix as agent_llm_a-opus
     but different capabilities — manifest must win. *)
  let m =
    make_manifest
      ~base:"provider_d_chat"
      ~extra_fields:
        [ "max_context_tokens", "999999"
        ; "supports_computer_use", "false"
        ; "supports_tools", "true"
        ]
      "agent_llm_a-opus-4"
  in
  match Capabilities.for_model_id_with_manifest m "agent_llm_a-opus-4-6" with
  | Some c ->
    check (option int) "manifest overrides ctx" (Some 999999) c.max_context_tokens;
    check bool "manifest overrides computer_use" false c.supports_computer_use;
    check bool "manifest keeps tools" true c.supports_tools
  | None -> fail "expected Some from manifest"
;;

let test_manifest_fallback_to_static () =
  (* Manifest has no entry for agent_llm_a-opus — should fall through to static table. *)
  let m = make_manifest "totally-other-model" in
  match Capabilities.for_model_id_with_manifest m "agent_llm_a-opus-4-6" with
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

let test_manifest_base_label_provider_d_chat () =
  let m =
    make_manifest
      ~base:"provider_d_chat"
      ~extra_fields:[ "max_context_tokens", "65536" ]
      "custom-gpt"
  in
  match Capabilities.for_model_id_with_manifest m "custom-gpt-v2" with
  | Some c ->
    check (option int) "custom ctx" (Some 65536) c.max_context_tokens;
    check bool "provider_d_chat base: tools" true c.supports_tools;
    check bool "provider_d_chat base: streaming" true c.supports_native_streaming
  | None -> fail "expected Some"
;;

let test_manifest_base_label_provider_a () =
  let m =
    make_manifest
      ~base:"provider_a"
      ~extra_fields:[ "max_context_tokens", "512000" ]
      "my-agent_llm_a"
  in
  match Capabilities.for_model_id_with_manifest m "my-agent_llm_a-custom" with
  | Some c ->
    check (option int) "custom ctx 512K" (Some 512000) c.max_context_tokens;
    check bool "provider_a base: caching" true c.supports_caching;
    check bool "provider_a base: extended thinking" true c.supports_extended_thinking
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

let test_manifest_prefix_wins_over_longer_static_prefix () =
  (* Manifest entry "provider_h-3" must win over static table "provider_h-3" prefix too,
     letting operator override even well-known models. *)
  let m =
    make_manifest
      ~base:"provider_d_chat"
      ~extra_fields:[ "supports_reasoning", "false" ]
      "provider_h-3"
  in
  match Capabilities.for_model_id_with_manifest m "provider_h-3.5-35b-a3b-q4" with
  | Some c ->
    check bool "manifest disables reasoning" false c.supports_reasoning;
    check bool "base provider_d_chat: tools" true c.supports_tools
  | None -> fail "expected Some"
;;

let test_apply_manifest_entry_all_none_uses_base () =
  (* Entry with only id_prefix set — should be identical to base. *)
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"x","base":"provider_a"}]}|}
  in
  let manifest = Capability_manifest.of_json json |> Result.get_ok in
  let entry = List.hd manifest in
  let caps = Capabilities.apply_manifest_entry entry in
  let base = Capabilities.provider_a_capabilities in
  check bool "tools matches base" base.supports_tools caps.supports_tools;
  check (option int) "ctx matches base" base.max_context_tokens caps.max_context_tokens;
  check bool "caching matches base" base.supports_caching caps.supports_caching
;;

let test_manifest_wrong_type_fields_warn_and_ignore () =
  let warnings = ref [] in
  let json =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"typed","base":17,"max_context_tokens":"131072","supports_tools":"yes"}]}|}
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
  check (option string) "wrong-type base ignored" None entry.base_label;
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
  check bool "warned for base" true (has_warning "base" "string");
  check bool "warned for max_context_tokens" true (has_warning "max_context_tokens" "int");
  check bool "warned for supports_tools" true (has_warning "supports_tools" "bool")
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

(* ── Provider_h preset ────────────────────────────────── *)

let test_provider_h_capabilities () =
  let c = Capabilities.provider_h_capabilities in
  (* Provider_h (Provider_h) exposes response_format.json_schema on its Provider_d-compatible
     endpoint; native schema output is supported. Ref: Provider_h structured output
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

let test_provider_d_compat_reasoning_records_have_explicit_control () =
  let cases =
    [ "provider_d_chat_extended", Some Capabilities.provider_d_chat_extended_capabilities
    ; "provider_c", Some Capabilities.provider_c_capabilities
    ; "provider_h", Some Capabilities.provider_h_capabilities
    ; "provider_h-3.5", Capabilities.for_model_id "provider_h-3.5-35b-a3b"
    ; "provider_g-v4-flash", Capabilities.for_model_id "provider_g-v4-flash"
    ; "provider_l-ultra", Capabilities.for_model_id "provider_l-ultra-253b"
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
           (label ^ " has explicit thinking control")
           true
           (c.thinking_control_format <> Capabilities.No_thinking_control))
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
    [ (* provider_k-5v-turbo must precede provider_k-5 (inside broad branch).
         Discriminator: supports_image_input (5v-turbo) vs not (broad provider_k-5). *)
      ( "provider_k-5v-turbo-x"
      , "provider_k-5v-turbo must precede broad provider_k-5"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input && c.max_output_tokens = Some 128_000 )
    ; (* provider_k-5-code must precede provider_k-5 (inside broad branch).
         Discriminator: 128K context (code branch) vs 200K (broad provider_k-5). *)
      ( "provider_k-5-code-x"
      , "provider_k-5-code must precede broad provider_k-5"
      , fun (c : Capabilities.capabilities) ->
          c.max_context_tokens = Some 128_000 && c.supports_extended_thinking )
    ; (* provider_k-4.6v must precede provider_k-4.6 (inside broad branch) *)
      ( "provider_k-4.6v-x"
      , "provider_k-4.6v must precede broad provider_k-4.6"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 32_768 )
    ; (* provider_k-4.5v must precede provider_k-4.5 (inside broad branch) *)
      ( "provider_k-4.5v-x"
      , "provider_k-4.5v must precede broad provider_k-4.5"
      , fun (c : Capabilities.capabilities) ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 16_384 )
    ; (* broad provider_k-4.5 branch must precede provider_k-4.
         Discriminator: supports_reasoning + 96K output (broad) vs neither (provider_k-4). *)
      ( "provider_k-4.5-latest"
      , "broad provider_k-4.5 branch must precede provider_k-4"
      , fun (c : Capabilities.capabilities) ->
          c.supports_reasoning && c.max_output_tokens = Some 96_000 )
    ; (* provider_k-4v must precede provider_k-4.
         Discriminator: supports_image_input (provider_k-4v) vs not (provider_k-4). *)
      ( "provider_k-4v-x"
      , "provider_k-4v must precede provider_k-4"
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
      , [ test_case "provider_a" `Quick test_provider_a_capabilities
        ; test_case "provider_d" `Quick test_provider_d_capabilities
        ; test_case "provider_d extended" `Quick test_provider_d_extended
        ; test_case "provider_h" `Quick test_provider_h_capabilities
        ; test_case
            "provider_d compat reasoning records have explicit control"
            `Quick
            test_provider_d_compat_reasoning_records_have_explicit_control
        ] )
    ; ( "model_lookup"
      , [ test_case "agent_llm_a opus" `Quick test_lookup_agent_llm_a_opus
        ; test_case "agent_llm_a sonnet" `Quick test_lookup_agent_llm_a_sonnet
        ; test_case "model-d-5" `Quick test_lookup_gpt5
        ; test_case "provider_f" `Quick test_lookup_provider_f
        ; test_case "provider_f_family Provider_f_3_1" `Quick test_provider_f_family_3_1
        ; test_case "provider_f_family Provider_f_3" `Quick test_provider_f_family_3
        ; test_case "provider_f_family Provider_f_2_5" `Quick test_provider_f_family_2_5
        ; test_case
            "provider_f_family Provider_f_other (non-provider_f)"
            `Quick
            test_provider_f_family_other_non_provider_f
        ; test_case
            "provider_f_family Provider_f_other (unknown provider_f)"
            `Quick
            test_provider_f_family_other_unknown_provider_f
        ; test_case
            "provider_f_family drives 1M ctx capabilities"
            `Quick
            test_provider_f_family_drives_capabilities
        ; test_case
            "static route normalizes cloud suffix"
            `Quick
            test_static_model_route_normalizes_cloud_suffix
        ; test_case "provider_c-k2 cloud" `Quick test_lookup_provider_c_k2_cloud
        ; test_case "provider_h" `Quick test_lookup_provider_m
        ; test_case "provider_h runpod name" `Quick test_lookup_provider_m_runpod_name
        ; test_case "provider_g v4 flash" `Quick test_lookup_provider_g_v4_flash
        ; test_case "provider_g v4 pro" `Quick test_lookup_provider_g_v4_pro
        ; test_case "grok 2M context" `Quick test_lookup_grok
        ; test_case "provider_k-5 text only" `Quick test_lookup_glm5_text_only
        ; test_case "provider_k-5v vision" `Quick test_lookup_glm5v_vision
        ; test_case "provider_k-4.6v vision" `Quick test_lookup_glm46v_vision
        ; test_case "provider_k-ocr vision" `Quick test_lookup_glm_ocr
        ; test_case "unknown" `Quick test_lookup_unknown
        ; test_case "case insensitive" `Quick test_lookup_case_insensitive
        ] )
    ; "merge", [ test_case "with_context_size" `Quick test_with_context_size ]
    ; ( "manifest"
      , [ test_case "overrides static table" `Quick test_manifest_overrides_static_table
        ; test_case "fallback to static" `Quick test_manifest_fallback_to_static
        ; test_case "unknown model → None" `Quick test_manifest_unknown_model_still_none
        ; test_case "base provider_d_chat" `Quick test_manifest_base_label_provider_d_chat
        ; test_case "base provider_a" `Quick test_manifest_base_label_provider_a
        ; test_case "base absent = default" `Quick test_manifest_base_absent_uses_default
        ; test_case
            "manifest prefix wins"
            `Quick
            test_manifest_prefix_wins_over_longer_static_prefix
        ; test_case
            "all-None entry matches base"
            `Quick
            test_apply_manifest_entry_all_none_uses_base
        ; test_case
            "wrong-type fields warn and ignore"
            `Quick
            test_manifest_wrong_type_fields_warn_and_ignore
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
        ] )
    ; ( "prefix_ordering"
      , [ test_case
            "shadow pairs all resolve to specific branch (M01)"
            `Quick
            test_prefix_ordering_invariant
        ] )
    ]
;;
