(** Tests for capabilities wiring — discovery, context_reducer, filter. *)

open Alcotest
open Llm_provider

(* ── Discovery: model lookup integration ─────────────── *)

let test_discovery_infers_from_model_name () =
  (* Simulate: Discovery finds a "qwen3.5-35b" model *)
  let models : Discovery.model_info list = [
    { id = "qwen3.5-35b-a3b-q4"; owned_by = "local" }
  ] in
  let props : Discovery.server_props option = Some {
    total_slots = 4; ctx_size = 262144; model = "qwen3.5-35b"
  } in
  (* Discovery.infer_capabilities is internal, but we can test via
     the endpoint_status.capabilities after Discovery.discover.
     Here we verify the for_model_id lookup + with_context_size pattern. *)
  let caps = match Capabilities.for_model_id "qwen3.5-35b-a3b-q4" with
    | Some c -> Capabilities.with_context_size c ~ctx_size:262144
    | None -> Capabilities.default_capabilities
  in
  ignore models; ignore props;
  check (option int) "context from props" (Some 262144) caps.max_context_tokens;
  check bool "tools from lookup" true caps.supports_tools;
  check bool "thinking from lookup" true caps.supports_extended_thinking;
  check bool "top_k from lookup" true caps.supports_top_k

(* ── Capability filter: new predicates ───────────────── *)

let test_filter_parallel_tools () =
  let yes = { Capabilities.anthropic_capabilities with
              supports_parallel_tool_calls = true } in
  let no = { Capabilities.default_capabilities with
             supports_parallel_tool_calls = false } in
  check bool "anthropic has parallel" true
    (Capability_filter.requires_parallel_tools yes);
  check bool "default lacks parallel" false
    (Capability_filter.requires_parallel_tools no)

let test_filter_thinking () =
  let claude = Capabilities.anthropic_capabilities in
  let basic = Capabilities.openai_chat_capabilities in
  check bool "claude has thinking" true
    (Capability_filter.requires_thinking claude);
  check bool "basic openai no thinking" false
    (Capability_filter.requires_thinking basic)

let test_filter_fits_context () =
  let caps = { Capabilities.default_capabilities with
               max_context_tokens = Some 128_000 } in
  check bool "fits 100K" true
    (Capability_filter.fits_context ~tokens:100_000 caps);
  check bool "exceeds 128K" false
    (Capability_filter.fits_context ~tokens:200_000 caps);
  check bool "unknown = fail closed" false
    (Capability_filter.fits_context ~tokens:999_999
       Capabilities.default_capabilities)

let test_filter_fits_output () =
  let caps = { Capabilities.default_capabilities with
               max_output_tokens = Some 8_000 } in
  check bool "fits 4K" true
    (Capability_filter.fits_output ~tokens:4_000 caps);
  check bool "exceeds 8K" false
    (Capability_filter.fits_output ~tokens:16_000 caps)

let test_filter_combined () =
  let caps = Capabilities.anthropic_capabilities in
  let need_all = Capability_filter.requires_all [
    Capability_filter.requires_tools;
    Capability_filter.requires_thinking;
    Capability_filter.requires_vision;
  ] in
  check bool "claude meets all" true (need_all caps);
  let need_audio = Capability_filter.requires_all [
    Capability_filter.requires_tools;
    (fun c -> c.supports_audio_input);
  ] in
  check bool "claude lacks audio" false (need_audio caps)

(* ── Context reducer: from_capabilities ──────────────── *)

let test_reducer_from_caps_some () =
  let caps = { Capabilities.default_capabilities with
               max_context_tokens = Some 100_000 } in
  match Agent_sdk.Context_reducer.from_capabilities caps with
  | Some reducer ->
    (* Budget should be 80% of 100K = 80K tokens.
       Test with a message list that fits. *)
    let msgs = [
      { Types.role = Types.User; content = [Types.Text "hello"];
        name = None; tool_call_id = None }
    ] in
    let reduced = Agent_sdk.Context_reducer.reduce reducer msgs in
    check int "small message kept" 1 (List.length reduced)
  | None -> fail "expected Some reducer"

let test_reducer_from_caps_none () =
  let caps = Capabilities.default_capabilities in
  check bool "unknown ctx = None" true
    (Agent_sdk.Context_reducer.from_capabilities caps = None)

let test_reducer_custom_margin () =
  let caps = { Capabilities.default_capabilities with
               max_context_tokens = Some 10_000 } in
  match Agent_sdk.Context_reducer.from_capabilities ~margin:0.5 caps with
  | Some _ -> ()  (* 50% of 10K = 5K budget *)
  | None -> fail "expected Some with custom margin"

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run "Capabilities_Wiring" [
    "discovery", [
      test_case "infers from model name" `Quick
        test_discovery_infers_from_model_name;
    ];
    "filter", [
      test_case "parallel tools" `Quick test_filter_parallel_tools;
      test_case "thinking" `Quick test_filter_thinking;
      test_case "fits context" `Quick test_filter_fits_context;
      test_case "fits output" `Quick test_filter_fits_output;
      test_case "combined predicates" `Quick test_filter_combined;
    ];
    "context_reducer", [
      test_case "from_capabilities Some" `Quick test_reducer_from_caps_some;
      test_case "from_capabilities None" `Quick test_reducer_from_caps_none;
      test_case "custom margin" `Quick test_reducer_custom_margin;
    ];
  ]
