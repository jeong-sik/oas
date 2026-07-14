(** Tests for capabilities wiring — discovery and exact capability filters. *)

open Alcotest
open Llm_provider

(* ── Capability filter: new predicates ───────────────── *)

let test_filter_parallel_tools () =
  let yes =
    { Capabilities.anthropic_capabilities with supports_parallel_tool_calls = true }
  in
  let no =
    { Capabilities.default_capabilities with supports_parallel_tool_calls = false }
  in
  check bool "anthropic has parallel" true (Capability_filter.requires_parallel_tools yes);
  check bool "default lacks parallel" false (Capability_filter.requires_parallel_tools no)
;;

let test_effective_disable_parallel_tool_use () =
  let effective =
    Capabilities.effective_disable_parallel_tool_use ~supports_parallel_tool_calls:false
  in
  check
    bool
    "explicit caller disable wins without tools"
    true
    (effective ~caller_disabled:true ~tools_present:false);
  check
    bool
    "capability disables when tools are present"
    true
    (effective ~caller_disabled:false ~tools_present:true);
  check
    bool
    "no tools does not force disable"
    false
    (effective ~caller_disabled:false ~tools_present:false);
  check
    bool
    "parallel-capable model stays enabled"
    false
    (Capabilities.effective_disable_parallel_tool_use
       ~caller_disabled:false
       ~supports_parallel_tool_calls:true
       ~tools_present:true)
;;

let test_filter_thinking () =
  let claude = Capabilities.anthropic_capabilities in
  let basic = Capabilities.openai_compat_chat_capabilities in
  check bool "claude has thinking" true (Capability_filter.requires_thinking claude);
  check bool "basic openai no thinking" false (Capability_filter.requires_thinking basic)
;;

let test_filter_fits_context () =
  let caps =
    { Capabilities.default_capabilities with max_context_tokens = Some 128_000 }
  in
  check bool "fits 100K" true (Capability_filter.fits_context ~tokens:100_000 caps);
  check bool "exceeds 128K" false (Capability_filter.fits_context ~tokens:200_000 caps);
  check
    bool
    "unknown = fail closed"
    false
    (Capability_filter.fits_context ~tokens:999_999 Capabilities.default_capabilities)
;;

let test_filter_fits_output () =
  let caps = { Capabilities.default_capabilities with max_output_tokens = Some 8_000 } in
  check bool "fits 4K" true (Capability_filter.fits_output ~tokens:4_000 caps);
  check bool "exceeds 8K" false (Capability_filter.fits_output ~tokens:16_000 caps)
;;

let test_filter_combined () =
  let caps = Capabilities.anthropic_capabilities in
  let need_all =
    Capability_filter.requires_all
      [ Capability_filter.requires_tools
      ; Capability_filter.requires_thinking
      ; Capability_filter.requires_vision
      ]
  in
  check bool "claude meets all" true (need_all caps);
  let need_audio =
    Capability_filter.requires_all
      [ Capability_filter.requires_tools; (fun c -> c.supports_audio_input) ]
  in
  check bool "claude lacks audio" false (need_audio caps)
;;

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run
    "Capabilities_Wiring"
    [ ( "filter"
      , [ test_case "parallel tools" `Quick test_filter_parallel_tools
        ; test_case
            "effective parallel tool disable"
            `Quick
            test_effective_disable_parallel_tool_use
        ; test_case "thinking" `Quick test_filter_thinking
        ; test_case "fits context" `Quick test_filter_fits_context
        ; test_case "fits output" `Quick test_filter_fits_output
        ; test_case "combined predicates" `Quick test_filter_combined
        ] )
    ]
;;
