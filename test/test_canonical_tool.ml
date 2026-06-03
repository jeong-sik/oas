(** Tests for {!Llm_provider.Canonical_tool} — RFC-OAS-024 WP8 Increment 1.

    Covers the result projection (round-trip fidelity, non-coupling) and the
    call projection (extend-not-rebuild id, order_index correctness, 3-way
    reasoning distinguishability). The {e wiring} of [tool_result_of_block]
    into the turn pipeline is covered by the inline tests on
    [Pipeline_stage_prepare.last_tool_results_from]. *)

module Ct = Llm_provider.Canonical_tool
module Types = Llm_provider.Types
module Provider_kind = Llm_provider.Provider_kind
module Api_common = Llm_provider.Api_common

let json_eq = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

(* ── tool_result_of_block ─────────────────────────────────────── *)

let test_result_roundtrip_preserves_fields () =
  let json = `Assoc [ "rows", `Int 3 ] in
  let blocks = [ Types.Text "ignored" ] in
  let block =
    Types.ToolResult
      { tool_use_id = "call_abc"
      ; content = "3 rows"
      ; is_error = false
      ; json = Some json
      ; content_blocks = Some blocks
      }
  in
  match Ct.tool_result_of_block block with
  | None -> Alcotest.fail "expected Some projection for a ToolResult block"
  | Some proj ->
    Alcotest.(check string) "call_id" "call_abc" proj.call_id;
    Alcotest.(check string) "content" "3 rows" proj.content;
    Alcotest.(check bool) "is_error" false proj.is_error;
    (match proj.structured_content with
     | Some j -> Alcotest.check json_eq "structured_content == json" json j
     | None -> Alcotest.fail "structured_content dropped");
    (match proj.content_blocks with
     | Some bs -> Alcotest.(check int) "content_blocks length" 1 (List.length bs)
     | None -> Alcotest.fail "content_blocks dropped")
;;

let test_result_preserves_is_error () =
  let block =
    Types.ToolResult
      { tool_use_id = "call_err"
      ; content = "boom"
      ; is_error = true
      ; json = None
      ; content_blocks = None
      }
  in
  match Ct.tool_result_of_block block with
  | Some proj ->
    Alcotest.(check bool) "is_error preserved" true proj.is_error;
    Alcotest.(check (option json_eq))
      "structured_content None when json None"
      None
      proj.structured_content
  | None -> Alcotest.fail "expected Some projection"
;;

let test_result_none_for_non_toolresult () =
  let cases =
    [ Types.Text "hi"
    ; Types.Thinking { thinking_type = "thinking"; content = "..." }
    ; Types.RedactedThinking "redacted"
    ; Types.ToolUse { id = "call_x"; name = "t"; input = `Null }
    ; Types.Image { media_type = "image/png"; data = "AAAA"; source_type = "base64" }
    ]
  in
  List.iter
    (fun block ->
       Alcotest.(check bool)
         "non-ToolResult projects to None"
         true
         (Option.is_none (Ct.tool_result_of_block block)))
    cases
;;

(* ── tool_calls_of_response ───────────────────────────────────── *)

let mk_response content =
  { Types.id = "resp_1"
  ; model = "test-model"
  ; stop_reason = Types.StopToolUse
  ; content
  ; usage = None
  ; telemetry = None
  }
;;

(* Extend, not rebuild: for a synthesized-id provider (Gemini), the projected
   call_id must equal the existing Api_common.synthesize_tool_use_id output. *)
let test_call_id_matches_synthesize () =
  let args = `Assoc [ "q", `String "weather" ] in
  let expected = Api_common.synthesize_tool_use_id ~name:"get_weather" args in
  let block = Types.ToolUse { id = expected; name = "get_weather"; input = args } in
  match
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.Gemini
      ~reasoning_suppressed:false
      (mk_response [ block ])
  with
  | [ call ] ->
    Alcotest.(check string) "call_id == synthesize_tool_use_id" expected call.call_id;
    Alcotest.(check string) "name" "get_weather" call.name;
    Alcotest.(check int) "order_index 0" 0 call.order_index
  | calls -> Alcotest.failf "expected exactly 1 call, got %d" (List.length calls)
;;

(* order_index is the index among tool-use blocks only (filter+mapi),
   contiguous regardless of interleaved text/thinking blocks (RFC D3). *)
let test_order_index_filters_tool_blocks () =
  let content =
    [ Types.Text "intro"
    ; Types.ToolUse { id = "a"; name = "alpha"; input = `Null }
    ; Types.Text "between"
    ; Types.Thinking { thinking_type = "thinking"; content = "hmm" }
    ; Types.ToolUse { id = "b"; name = "beta"; input = `Null }
    ; Types.ToolUse { id = "c"; name = "gamma"; input = `Null }
    ]
  in
  let calls =
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.Anthropic
      ~reasoning_suppressed:false
      (mk_response content)
  in
  let indices = List.map (fun (c : Ct.provider_tool_call) -> c.order_index) calls in
  let ids = List.map (fun (c : Ct.provider_tool_call) -> c.call_id) calls in
  Alcotest.(check (list int)) "contiguous 0..2 over tool blocks only" [ 0; 1; 2 ] indices;
  Alcotest.(check (list string)) "appearance order preserved" [ "a"; "b"; "c" ] ids
;;

let test_no_tool_calls_empty () =
  let content = [ Types.Text "just text"; Types.RedactedThinking "x" ] in
  let calls =
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.OpenAI_compat
      ~reasoning_suppressed:false
      (mk_response content)
  in
  Alcotest.(check int) "no tool calls" 0 (List.length calls)
;;

(* ── reasoning_link 3-way distinguishability (RFC D6) ──────────── *)

let reasoning_label (r : Ct.reasoning_link) =
  match r with
  | No_reasoning -> "no_reasoning"
  | Suppressed -> "suppressed"
  | Available _ -> "available"
;;

let test_reasoning_no_reasoning () =
  let content = [ Types.ToolUse { id = "a"; name = "t"; input = `Null } ] in
  match
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.Anthropic
      ~reasoning_suppressed:false
      (mk_response content)
  with
  | [ call ] ->
    Alcotest.(check string) "no_reasoning" "no_reasoning" (reasoning_label call.reasoning)
  | _ -> Alcotest.fail "expected 1 call"
;;

let test_reasoning_suppressed () =
  let content = [ Types.ToolUse { id = "a"; name = "t"; input = `Null } ] in
  match
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.Anthropic
      ~reasoning_suppressed:true
      (mk_response content)
  with
  | [ call ] ->
    Alcotest.(check string) "suppressed" "suppressed" (reasoning_label call.reasoning)
  | _ -> Alcotest.fail "expected 1 call"
;;

let test_reasoning_available_from_adjacent_thinking () =
  let content =
    [ Types.Thinking { thinking_type = "thinking"; content = "let me think" }
    ; Types.ToolUse { id = "a"; name = "t"; input = `Null }
    ]
  in
  match
    Ct.tool_calls_of_response
      ~provider_kind:Provider_kind.Anthropic
      ~reasoning_suppressed:false
      (mk_response content)
  with
  | [ call ] ->
    Alcotest.(check string) "available" "available" (reasoning_label call.reasoning);
    (match call.reasoning with
     | Available state ->
       Alcotest.(check string) "reasoning content" "let me think" state.content;
       Alcotest.(check bool) "kind is Thinking" true (state.kind = Ct.Thinking)
     | _ -> Alcotest.fail "expected Available")
  | _ -> Alcotest.fail "expected 1 call"
;;

(* The three reasoning outcomes are genuinely distinct values — an option-based
   design (the rejected alternative) could not separate these. *)
let test_reasoning_three_distinct () =
  let labels =
    [ Ct.No_reasoning
    ; Ct.Suppressed
    ; Ct.Available { kind = Ct.Thinking; content = ""; tokens = None }
    ]
    |> List.map reasoning_label
  in
  Alcotest.(check (list string))
    "three distinct reasoning labels"
    [ "no_reasoning"; "suppressed"; "available" ]
    labels
;;

let () =
  Alcotest.run
    "canonical_tool"
    [ ( "tool_result_of_block"
      , [ Alcotest.test_case
            "roundtrip preserves fields"
            `Quick
            test_result_roundtrip_preserves_fields
        ; Alcotest.test_case
            "preserves is_error / None json"
            `Quick
            test_result_preserves_is_error
        ; Alcotest.test_case
            "None for non-ToolResult"
            `Quick
            test_result_none_for_non_toolresult
        ] )
    ; ( "tool_calls_of_response"
      , [ Alcotest.test_case
            "call_id == synthesize_tool_use_id"
            `Quick
            test_call_id_matches_synthesize
        ; Alcotest.test_case
            "order_index filters tool blocks"
            `Quick
            test_order_index_filters_tool_blocks
        ; Alcotest.test_case "no tool calls -> empty" `Quick test_no_tool_calls_empty
        ] )
    ; ( "reasoning_link"
      , [ Alcotest.test_case "No_reasoning" `Quick test_reasoning_no_reasoning
        ; Alcotest.test_case "Suppressed" `Quick test_reasoning_suppressed
        ; Alcotest.test_case
            "Available from adjacent thinking"
            `Quick
            test_reasoning_available_from_adjacent_thinking
        ; Alcotest.test_case "three distinct values" `Quick test_reasoning_three_distinct
        ] )
    ]
;;
