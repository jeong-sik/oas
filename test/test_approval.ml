(** Tests for approval callback (human-in-the-loop) in execute_tools. *)

open Alcotest
open Agent_sdk
open Types

(** Helper: create a simple tool that echoes its input as JSON string *)
let make_echo_tool name =
  Tool.create ~name ~description:"echo" ~parameters:[] (fun input ->
    Ok { Types.content = Yojson.Safe.to_string input })

(** Helper: create a minimal agent inside Eio with given hooks and approval.
    Returns execute_tools results for the given tool_uses. *)
let run_execute ~hooks ?approval tool_uses =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let tools = [make_echo_tool "safe"; make_echo_tool "dangerous"] in
  let options = { Agent.default_options with hooks; approval } in
  let agent = Agent.create ~net ~tools ~options () in
  let opts = Agent.options agent in
  Agent_tools.execute_tools
    ~context:(Agent.context agent) ~tools:(Tool_set.to_list (Agent.tools agent))
    ~hooks:opts.hooks ~event_bus:opts.event_bus
    ~tracer:opts.tracer ~agent_name:(Agent.state agent).config.name
    ~turn_count:(Agent.state agent).turn_count ~usage:(Agent.state agent).usage
    ~approval:opts.approval
    tool_uses

(* --- Test cases --- *)

let test_approval_required_no_callback () =
  (* ApprovalRequired with no callback registered: fail closed. *)
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) } in
  let results = run_execute ~hooks [ToolUse { id = "t1"; name = "safe"; input = `String "hello" }] in
  match results with
  | [(id, content, is_error)] ->
    check string "id" "t1" id;
    check string "content"
      "Tool 'safe' requires approval but no approval callback is configured"
      content;
    check bool "is error" true is_error
  | _ -> fail "expected exactly one result"

let test_approval_approve () =
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) } in
  let approval ~tool_name:_ ~input:_ = Hooks.Approve in
  let results = run_execute ~hooks ~approval
    [ToolUse { id = "t1"; name = "safe"; input = `String "data" }] in
  match results with
  | [(id, content, is_error)] ->
    check string "id" "t1" id;
    check string "content" {|"data"|} content;
    check bool "no error" false is_error
  | _ -> fail "expected exactly one result"

let test_approval_reject () =
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) } in
  let approval ~tool_name:_ ~input:_ = Hooks.Reject "too dangerous" in
  let results = run_execute ~hooks ~approval
    [ToolUse { id = "t1"; name = "dangerous"; input = `String "rm -rf" }] in
  match results with
  | [(id, content, is_error)] ->
    check string "id" "t1" id;
    check string "content" "Tool rejected: too dangerous" content;
    check bool "is error" true is_error
  | _ -> fail "expected exactly one result"

let test_approval_edit () =
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) } in
  let safe_input = `String "sanitized" in
  let approval ~tool_name:_ ~input:_ = Hooks.Edit safe_input in
  let results = run_execute ~hooks ~approval
    [ToolUse { id = "t1"; name = "dangerous"; input = `String "original" }] in
  match results with
  | [(id, content, is_error)] ->
    check string "id" "t1" id;
    check string "content uses edited input" {|"sanitized"|} content;
    check bool "no error" false is_error
  | _ -> fail "expected exactly one result"

let test_selective_approval () =
  (* Only "dangerous" requires approval; "safe" is auto-approved *)
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun event ->
      match event with
      | Hooks.PreToolUse { tool_name; _ } when tool_name = "dangerous" ->
        Hooks.ApprovalRequired
      | _ -> Hooks.Continue) } in
  let approval ~tool_name ~input:_ =
    if tool_name = "dangerous" then Hooks.Reject "blocked"
    else Hooks.Approve
  in
  let results = run_execute ~hooks ~approval [
    ToolUse { id = "t1"; name = "safe"; input = `String "ok" };
    ToolUse { id = "t2"; name = "dangerous"; input = `String "bad" };
  ] in
  (* Results may be in any order due to Eio.Fiber.List.map, so sort by id *)
  let sorted = List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) results in
  (match sorted with
  | [(id1, content1, err1); (id2, content2, err2)] ->
    check string "safe id" "t1" id1;
    check string "safe executed" {|"ok"|} content1;
    check bool "safe no error" false err1;
    check string "dangerous id" "t2" id2;
    check string "dangerous rejected" "Tool rejected: blocked" content2;
    check bool "dangerous is error" true err2
  | _ -> fail "expected exactly two results")

let test_skip_override_unaffected () =
  (* Skip and Override decisions still work when approval is configured *)
  let hooks = { Hooks.empty with
    pre_tool_use = Some (fun event ->
      match event with
      | Hooks.PreToolUse { tool_name = "safe"; _ } -> Hooks.Skip
      | Hooks.PreToolUse { tool_name = "dangerous"; _ } -> Hooks.Override "overridden"
      | _ -> Hooks.Continue) } in
  let approval_called = ref false in
  let approval ~tool_name:_ ~input:_ =
    approval_called := true;
    Hooks.Approve
  in
  let results = run_execute ~hooks ~approval [
    ToolUse { id = "t1"; name = "safe"; input = `Null };
    ToolUse { id = "t2"; name = "dangerous"; input = `Null };
  ] in
  let sorted = List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) results in
  check bool "approval callback not called" false !approval_called;
  (match sorted with
  | [(id1, content1, err1); (id2, content2, err2)] ->
    check string "skip id" "t1" id1;
    check string "skipped" "Tool execution skipped by hook" content1;
    check bool "skip not error" false err1;
    check string "override id" "t2" id2;
    check string "overridden" "overridden" content2;
    check bool "override not error" false err2
  | _ -> fail "expected exactly two results")

let test_non_tool_use_blocks_filtered () =
  (* Non-ToolUse blocks (Text, Thinking) must be filtered out, not produce
     bogus ("", "", false) triples. Regression test for issue #327. *)
  let hooks = Hooks.empty in
  let results = run_execute ~hooks [
    Text "some assistant text";
    ToolUse { id = "t1"; name = "safe"; input = `String "data" };
    Thinking { thinking_type = "thinking"; content = "reasoning" };
    ToolUse { id = "t2"; name = "safe"; input = `String "more" };
  ] in
  (* Only the 2 ToolUse blocks should produce results *)
  check int "result count" 2 (List.length results);
  let sorted = List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) results in
  (match sorted with
  | [(id1, _, _); (id2, _, _)] ->
    check string "first id" "t1" id1;
    check string "second id" "t2" id2
  | _ -> fail "expected exactly two results")

let test_only_non_tool_use_blocks () =
  (* When all blocks are non-ToolUse, result should be empty *)
  let hooks = Hooks.empty in
  let results = run_execute ~hooks [
    Text "just text";
    Thinking { thinking_type = "thinking"; content = "thoughts" };
  ] in
  check int "empty results" 0 (List.length results)

let () =
  run "Approval" [
    "approval_required", [
      test_case "no callback = error" `Quick test_approval_required_no_callback;
      test_case "Approve = normal execution" `Quick test_approval_approve;
      test_case "Reject with reason" `Quick test_approval_reject;
      test_case "Edit modifies input" `Quick test_approval_edit;
      test_case "selective by tool name" `Quick test_selective_approval;
      test_case "Skip/Override unaffected" `Quick test_skip_override_unaffected;
    ];
    "non_tool_use_filtering", [
      test_case "mixed blocks filtered (#327)" `Quick test_non_tool_use_blocks_filtered;
      test_case "only non-ToolUse = empty" `Quick test_only_non_tool_use_blocks;
    ];
  ]
