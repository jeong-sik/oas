(** Tests for hooks.ml — lifecycle events and hook decisions *)

open Alcotest
open Agent_sdk

let test_empty_hooks () =
  let hooks = Hooks.empty in
  check bool "before_turn is None" true (hooks.before_turn = None);
  check bool "after_turn is None" true (hooks.after_turn = None);
  check bool "pre_tool_use is None" true (hooks.pre_tool_use = None);
  check bool "post_tool_use is None" true (hooks.post_tool_use = None);
  check bool "post_tool_use_failure is None" true
    (hooks.post_tool_use_failure = None);
  check bool "on_stop is None" true (hooks.on_stop = None)

let test_invoke_none () =
  let result = Hooks.invoke None (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "invoke None returns Continue" true (result = Hooks.Continue)

let test_invoke_continue () =
  let hook _event = Hooks.Continue in
  let result = Hooks.invoke (Some hook) (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "hook returns Continue" true (result = Hooks.Continue)

let test_invoke_skip () =
  let hook _event = Hooks.Skip in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse { tool_name = "echo"; input = `Null }) in
  check bool "hook returns Skip" true (result = Hooks.Skip)

let test_invoke_override () =
  let hook _event = Hooks.Override "custom value" in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse { tool_name = "echo"; input = `Null }) in
  check bool "hook returns Override" true (result = Hooks.Override "custom value")

let test_hook_receives_event () =
  let received = ref "" in
  let hook = function
    | Hooks.PreToolUse { tool_name; _ } ->
      received := tool_name;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse { tool_name = "test_tool"; input = `Null }) in
  check string "hook received tool_name" "test_tool" !received

let test_post_tool_use_event () =
  let received_output = ref "" in
  let hook = function
    | Hooks.PostToolUse { output = Ok { content }; _ } ->
      received_output := content;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PostToolUse {
      tool_name = "echo";
      input = `Null;
      output = Ok { Types.content = "hello" }
    }) in
  check string "hook received output" "hello" !received_output

let test_post_tool_use_failure_event () =
  let received_error = ref "" in
  let hook = function
    | Hooks.PostToolUseFailure { error; _ } ->
        received_error := error;
        Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result =
    Hooks.invoke (Some hook)
      (Hooks.PostToolUseFailure
         { tool_name = "echo"; input = `Null; error = "boom" })
  in
  check string "hook received error" "boom" !received_error

let test_invoke_approval_required () =
  let hook _event = Hooks.ApprovalRequired in
  let result = Hooks.invoke (Some hook)
    (Hooks.PreToolUse { tool_name = "dangerous"; input = `Null }) in
  check bool "hook returns ApprovalRequired" true (result = Hooks.ApprovalRequired)

let () =
  run "Hooks" [
    "empty", [
      test_case "empty hooks" `Quick test_empty_hooks;
    ];
    "invoke", [
      test_case "invoke None" `Quick test_invoke_none;
      test_case "invoke Continue" `Quick test_invoke_continue;
      test_case "invoke Skip" `Quick test_invoke_skip;
      test_case "invoke Override" `Quick test_invoke_override;
      test_case "invoke ApprovalRequired" `Quick test_invoke_approval_required;
      test_case "receives event" `Quick test_hook_receives_event;
      test_case "post_tool_use event" `Quick test_post_tool_use_event;
      test_case "post_tool_use_failure event" `Quick
        test_post_tool_use_failure_event;
    ];
  ]
