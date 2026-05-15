(** Tests for Completion_contract.tool_use_calls — lazy tool-index behavior.

    Regression coverage for the Codex P1 finding on PR #1592: the tool-name
    lookup index must only be constructed when at least one [ToolUse] block
    is present in the response content. Text-only responses must skip the
    O(|tools|) hash build entirely. *)

open Agent_sdk
module Lp = Llm_provider.Types

let noop_handler _ = Ok { Types.content = "ok" }

let make_tool name =
  Tool.create ~name ~description:("desc:" ^ name) ~parameters:[] noop_handler
;;

let make_response ~content : Lp.api_response =
  { id = "test"
  ; model = "test-model"
  ; stop_reason = Lp.EndTurn
  ; content
  ; usage = None
  ; telemetry = None
  }
;;

(* ── Text-only response: index must NOT be forced ─────────────────────── *)

let test_text_only_response_returns_empty_calls () =
  let tools = List.init 64 (fun i -> make_tool (Printf.sprintf "tool_%d" i)) in
  let response = make_response ~content:[ Lp.Text "hello" ] in
  let calls = Completion_contract.tool_use_calls ~tools response in
  Alcotest.(check int) "no tool calls extracted" 0 (List.length calls)
;;

let test_text_only_with_empty_tools () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  let calls = Completion_contract.tool_use_calls ~tools:[] response in
  Alcotest.(check int) "no tool calls extracted" 0 (List.length calls)
;;

(* ── ToolUse present: index must be forced and resolve tools ──────────── *)

let test_single_tool_use_resolves () =
  let tools = [ make_tool "alpha"; make_tool "beta" ] in
  let response =
    make_response
      ~content:
        [ Lp.ToolUse { id = "id-1"; name = "beta"; input = `Assoc [] } ]
  in
  let calls = Completion_contract.tool_use_calls ~tools response in
  match calls with
  | [ call ] ->
    Alcotest.(check string) "name forwarded" "beta" call.name;
    Alcotest.(check bool) "tool resolved" true (Option.is_some call.tool);
    (match call.tool with
     | Some t -> Alcotest.(check string) "resolved to beta" "beta" t.schema.name
     | None -> Alcotest.fail "expected tool to resolve")
  | _ -> Alcotest.fail "expected exactly one call"
;;

let test_unknown_tool_use_yields_none () =
  let tools = [ make_tool "alpha" ] in
  let response =
    make_response
      ~content:
        [ Lp.ToolUse { id = "id-1"; name = "missing"; input = `Assoc [] } ]
  in
  match Completion_contract.tool_use_calls ~tools response with
  | [ call ] ->
    Alcotest.(check string) "name forwarded" "missing" call.name;
    Alcotest.(check bool) "tool unresolved" true (Option.is_none call.tool)
  | _ -> Alcotest.fail "expected exactly one call"
;;

let test_mixed_blocks_only_tool_uses_returned () =
  let tools = [ make_tool "alpha" ] in
  let response =
    make_response
      ~content:
        [ Lp.Text "prelude"
        ; Lp.ToolUse { id = "id-1"; name = "alpha"; input = `Assoc [] }
        ; Lp.Text "trailing"
        ]
  in
  let calls = Completion_contract.tool_use_calls ~tools response in
  Alcotest.(check int) "exactly one call extracted" 1 (List.length calls)
;;

(* ── Laziness witness: with [tools = []] and a ToolUse block, lookup
       returns None but the call list still has length 1. This pins the
       semantic that the index is built on first ToolUse, not eagerly. *)

let test_tool_use_with_empty_tools_still_emits_call () =
  let response =
    make_response
      ~content:
        [ Lp.ToolUse { id = "id-1"; name = "anything"; input = `Assoc [] } ]
  in
  match Completion_contract.tool_use_calls ~tools:[] response with
  | [ call ] ->
    Alcotest.(check string) "name forwarded" "anything" call.name;
    Alcotest.(check bool) "tool unresolved" true (Option.is_none call.tool)
  | _ -> Alcotest.fail "expected exactly one call"
;;

let () =
  Alcotest.run
    "completion_contract_tool_use_calls"
    [ ( "lazy_index"
      , [ Alcotest.test_case
            "text-only response yields empty calls"
            `Quick
            test_text_only_response_returns_empty_calls
        ; Alcotest.test_case
            "text-only with empty tool catalog"
            `Quick
            test_text_only_with_empty_tools
        ; Alcotest.test_case
            "single ToolUse resolves against tool catalog"
            `Quick
            test_single_tool_use_resolves
        ; Alcotest.test_case
            "unknown ToolUse name yields call with tool=None"
            `Quick
            test_unknown_tool_use_yields_none
        ; Alcotest.test_case
            "mixed content blocks: only ToolUse extracted"
            `Quick
            test_mixed_blocks_only_tool_uses_returned
        ; Alcotest.test_case
            "ToolUse with empty catalog still emits unresolved call"
            `Quick
            test_tool_use_with_empty_tools_still_emits_call
        ] )
    ]
;;
