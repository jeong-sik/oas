(** Tests for hooks.ml — lifecycle events and hook decisions *)

open Alcotest
open Agent_sdk

let test_empty_hooks () =
  let hooks = Hooks.empty in
  check bool "session_start is None" true (hooks.session_start = None);
  check bool "session_end is None" true (hooks.session_end = None);
  check bool "user_prompt_submit is None" true (hooks.user_prompt_submit = None);
  check bool "before_turn is None" true (hooks.before_turn = None);
  check bool "after_turn is None" true (hooks.after_turn = None);
  check bool "permission_request is None" true (hooks.permission_request = None);
  check bool "pre_tool_use is None" true (hooks.pre_tool_use = None);
  check bool "post_tool_use is None" true (hooks.post_tool_use = None);
  check bool "subagent_start is None" true (hooks.subagent_start = None);
  check bool "subagent_stop is None" true (hooks.subagent_stop = None);
  check bool "pre_compact is None" true (hooks.pre_compact = None);
  check bool "config_change is None" true (hooks.config_change = None);
  check bool "on_stop is None" true (hooks.on_stop = None)

let test_invoke_none () =
  let result = Hooks.invoke None (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "invoke None returns Continue" true (result = Hooks.Continue)

let test_invoke_continue () =
  let hook _event = Hooks.Continue in
  let result = Hooks.invoke (Some hook) (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "hook returns Continue" true (result = Hooks.Continue)

let test_invoke_allow () =
  let hook _event = Hooks.Allow in
  let result = Hooks.invoke (Some hook) (Hooks.BeforeTurn { turn = 0; messages = [] }) in
  check bool "hook returns Allow" true (result = Hooks.Allow)

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
    | Hooks.PostToolUse { output = Ok s; _ } ->
      received_output := s;
      Hooks.Continue
    | _ -> Hooks.Continue
  in
  let _result = Hooks.invoke (Some hook)
    (Hooks.PostToolUse {
      tool_name = "echo";
      input = `Null;
      output = Ok "hello"
    }) in
  check string "hook received output" "hello" !received_output

let test_permission_request_event () =
  let mode = ref Guardrails.Default in
  let hook = function
    | Hooks.PermissionRequest { permission_mode; _ } ->
      mode := permission_mode;
      Hooks.Allow
    | _ -> Hooks.Continue
  in
  let result =
    Hooks.invoke (Some hook)
      (Hooks.PermissionRequest {
         tool_name = "write_file";
         tool_kind = Types.File_edit;
         input = `Null;
         permission_mode = Guardrails.Default;
         reason = "edit";
       })
  in
  check bool "permission hook allows" true (result = Hooks.Allow);
  check bool "permission mode propagated" true (!mode = Guardrails.Default)

let () =
  run "Hooks" [
    "empty", [
      test_case "empty hooks" `Quick test_empty_hooks;
    ];
    "invoke", [
      test_case "invoke None" `Quick test_invoke_none;
      test_case "invoke Continue" `Quick test_invoke_continue;
      test_case "invoke Allow" `Quick test_invoke_allow;
      test_case "invoke Skip" `Quick test_invoke_skip;
      test_case "invoke Override" `Quick test_invoke_override;
      test_case "receives event" `Quick test_hook_receives_event;
      test_case "post_tool_use event" `Quick test_post_tool_use_event;
      test_case "permission_request event" `Quick test_permission_request_event;
    ];
  ]
