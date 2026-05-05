(** Tests for expanded capabilities — model lookup, limits, feature flags. *)

open Alcotest
open Llm_provider

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
  let c = Capabilities.openai_chat_capabilities in
  check bool "has structured output" true c.supports_structured_output;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "no reasoning" false c.supports_reasoning;
  check bool "context 128K" true (c.max_context_tokens = Some 128_000)
;;

let test_openai_extended () =
  let c = Capabilities.openai_chat_extended_capabilities in
  check bool "has reasoning" true c.supports_reasoning;
  check bool "has top_k" true c.supports_top_k;
  check bool "has min_p" true c.supports_min_p
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

let test_lookup_qwen () =
  match Capabilities.for_model_id "qwen3.5-35b-a3b" with
  | Some c ->
    check (option int) "context 262K" (Some 262_144) c.max_context_tokens;
    check bool "tools" true c.supports_tools;
    check bool "thinking" true c.supports_extended_thinking;
    check bool "top_k" true c.supports_top_k
  | None -> fail "should match qwen3"
;;

let test_lookup_deepseek_v4_flash () =
  match Capabilities.for_model_id "deepseek-v4-flash" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check bool "caching" true c.supports_caching
  | None -> fail "should match deepseek-v4-flash"
;;

let test_lookup_deepseek_v4_pro () =
  match Capabilities.for_model_id "deepseek-v4-pro" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 384K" (Some 384_000) c.max_output_tokens;
    check bool "tools" true c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check bool "caching" true c.supports_caching
  | None -> fail "should match deepseek-v4-pro"
;;

let test_lookup_grok () =
  match Capabilities.for_model_id "grok-4" with
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

let test_manifest_prefix_wins_over_longer_static_prefix () =
  (* Manifest entry "qwen3" must win over static table "qwen3" prefix too,
     letting operator override even well-known models. *)
  let m =
    make_manifest
      ~base:"openai_chat"
      ~extra_fields:[ "supports_reasoning", "false" ]
      "qwen3"
  in
  match Capabilities.for_model_id_with_manifest m "qwen3.5-35b-a3b-q4" with
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

(* ── DashScope preset ────────────────────────────────── *)

let test_dashscope_capabilities () =
  let c = Capabilities.dashscope_capabilities in
  (* DashScope (Qwen) exposes response_format.json_schema on its OpenAI-compatible
     endpoint; native schema output is supported. Ref: DashScope structured output
     guide — checked 2026-05-05. *)
  check bool "has structured output" true c.supports_structured_output;
  check bool "has json mode" true c.supports_response_format_json;
  check bool "has tools" true c.supports_tools;
  check bool "has tool_choice" true c.supports_tool_choice;
  check bool "has min_p" true c.supports_min_p
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
    [ (* glm-4.7-flash must precede glm-4.7 (inside broad glm-4.5|4.6|4.7|5 branch) *)
      ( "glm-4.7-flash-x"
      , "glm-4.7-flash must precede broad glm-4.7"
      , fun (c : Capabilities.capabilities) ->
          (not c.supports_reasoning) && c.max_output_tokens = Some 16_384 )
    ; (* glm-4.5-flash must precede glm-4.5 (inside broad branch) *)
      ( "glm-4.5-flash-x"
      , "glm-4.5-flash must precede broad glm-4.5"
      , fun (c : Capabilities.capabilities) ->
          (not c.supports_reasoning) && c.max_output_tokens = Some 16_384 )
    ; (* glm-4.5-air must precede glm-4.5 (inside broad branch) *)
      ( "glm-4.5-air-x"
      , "glm-4.5-air must precede broad glm-4.5"
      , fun (c : Capabilities.capabilities) ->
          (not c.supports_reasoning) && c.max_output_tokens = Some 16_384 )
    ; (* glm-5-turbo must precede glm-5 (inside broad branch) *)
      ( "glm-5-turbo-x"
      , "glm-5-turbo must precede broad glm-5"
      , fun (c : Capabilities.capabilities) ->
          (not c.supports_extended_thinking) && c.max_output_tokens = Some 16_384 )
    ; (* glm-5v-turbo must precede glm-5 (inside broad branch).
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
          && c.max_output_tokens = Some 32_768 )
    ; (* broad glm-4.5 branch must precede glm-4.
         Discriminator: supports_reasoning + 128K output (broad) vs neither (glm-4). *)
      ( "glm-4.5-latest"
      , "broad glm-4.5 branch must precede glm-4"
      , fun (c : Capabilities.capabilities) ->
          c.supports_reasoning && c.max_output_tokens = Some 128_000 )
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
        ] )
    ; ( "model_lookup"
      , [ test_case "claude opus" `Quick test_lookup_claude_opus
        ; test_case "claude sonnet" `Quick test_lookup_claude_sonnet
        ; test_case "gpt-5" `Quick test_lookup_gpt5
        ; test_case "gemini" `Quick test_lookup_gemini
        ; test_case "qwen" `Quick test_lookup_qwen
        ; test_case "deepseek v4 flash" `Quick test_lookup_deepseek_v4_flash
        ; test_case "deepseek v4 pro" `Quick test_lookup_deepseek_v4_pro
        ; test_case "grok 2M context" `Quick test_lookup_grok
        ; test_case "glm-5 text only" `Quick test_lookup_glm5_text_only
        ; test_case "glm-5v vision" `Quick test_lookup_glm5v_vision
        ; test_case "glm-4.6v vision" `Quick test_lookup_glm46v_vision
        ; test_case "glm-ocr vision" `Quick test_lookup_glm_ocr
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
            "manifest prefix wins"
            `Quick
            test_manifest_prefix_wins_over_longer_static_prefix
        ; test_case
            "all-None entry matches base"
            `Quick
            test_apply_manifest_entry_all_none_uses_base
        ] )
    ; ( "prefix_ordering"
      , [ test_case
            "shadow pairs all resolve to specific branch (M01)"
            `Quick
            test_prefix_ordering_invariant
        ] )
    ]
;;
