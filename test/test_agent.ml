(** Unit tests for Agent module — pure functions only (no Eio, no network). *)

open Agent_sdk
open Types

(* --- find_handoff_in_messages --- *)

let test_find_handoff_none () =
  let msgs = [
    { role = User; content = [Text "hello"] };
    { role = Assistant; content = [Text "world"] };
  ] in
  Alcotest.(check bool) "no handoff" true
    (Agent.find_handoff_in_messages msgs = None)

let test_find_handoff_normal_tool () =
  let msgs = [
    { role = User; content = [Text "use_tool"] };
    { role = Assistant; content = [ToolUse { id = "t1"; name = "calculator"; input = `Assoc [("a", `Int 1)] }] };
  ] in
  Alcotest.(check bool) "non-handoff tool" true
    (Agent.find_handoff_in_messages msgs = None)

let test_find_handoff_present () =
  let input = `Assoc [("prompt", `String "research this")] in
  let msgs = [
    { role = User; content = [Text "delegate"] };
    { role = Assistant; content = [ToolUse { id = "h1"; name = "transfer_to_researcher"; input }] };
  ] in
  match Agent.find_handoff_in_messages msgs with
  | Some (id, name, prompt) ->
      Alcotest.(check string) "tool id" "h1" id;
      Alcotest.(check string) "target name" "researcher" name;
      Alcotest.(check string) "prompt" "research this" prompt
  | None ->
      Alcotest.fail "expected Some handoff"

let test_find_handoff_no_prompt_field () =
  let input = `Assoc [("other", `Int 42)] in
  let msgs = [
    { role = Assistant; content = [ToolUse { id = "h2"; name = "transfer_to_coder"; input }] };
  ] in
  match Agent.find_handoff_in_messages msgs with
  | Some (_, _, prompt) ->
      Alcotest.(check string) "default prompt" "Continue the conversation." prompt
  | None ->
      Alcotest.fail "expected Some handoff"

let test_find_handoff_empty () =
  Alcotest.(check bool) "empty messages" true
    (Agent.find_handoff_in_messages [] = None)

let test_find_handoff_mixed_content () =
  let msgs = [
    { role = Assistant; content = [
        Text "I'll delegate";
        ToolUse { id = "h3"; name = "transfer_to_analyst"; input = `Assoc [("prompt", `String "analyze")] };
    ] };
  ] in
  match Agent.find_handoff_in_messages msgs with
  | Some (id, name, prompt) ->
      Alcotest.(check string) "id" "h3" id;
      Alcotest.(check string) "name" "analyst" name;
      Alcotest.(check string) "prompt" "analyze" prompt
  | None ->
      Alcotest.fail "expected handoff in mixed content"

(* --- replace_tool_result --- *)

let test_replace_existing () =
  let msgs = [
    { role = User; content = [Text "hello"] };
    { role = Assistant; content = [ToolUse { id = "t1"; name = "calc"; input = `Null }] };
    { role = User; content = [ToolResult { tool_use_id = "t1"; content = "old result"; is_error = false }] };
  ] in
  let updated = Agent.replace_tool_result msgs ~tool_id:"t1" ~content:"new result" ~is_error:false in
  let last = List.nth updated (List.length updated - 1) in
  match last.content with
  | [ToolResult { tool_use_id; content; is_error }] ->
      Alcotest.(check string) "id preserved" "t1" tool_use_id;
      Alcotest.(check string) "content replaced" "new result" content;
      Alcotest.(check bool) "not error" false is_error
  | _ ->
      Alcotest.fail "expected single ToolResult"

let test_replace_missing_appends () =
  let msgs = [
    { role = User; content = [Text "hello"] };
  ] in
  let updated = Agent.replace_tool_result msgs ~tool_id:"t99" ~content:"injected" ~is_error:true in
  let last = List.nth updated (List.length updated - 1) in
  match last.content with
  | [ToolResult { tool_use_id; content; is_error }] ->
      Alcotest.(check string) "id" "t99" tool_use_id;
      Alcotest.(check string) "content" "injected" content;
      Alcotest.(check bool) "is error" true is_error
  | _ ->
      Alcotest.fail "expected appended ToolResult"

let test_replace_preserves_other_results () =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "keep"; is_error = false };
        ToolResult { tool_use_id = "t2"; content = "replace me"; is_error = false };
    ] };
  ] in
  let updated = Agent.replace_tool_result msgs ~tool_id:"t2" ~content:"replaced" ~is_error:true in
  let last = List.nth updated (List.length updated - 1) in
  match last.content with
  | [ToolResult { tool_use_id = "t1"; content = c1; is_error = e1 }; ToolResult { tool_use_id = "t2"; content = c2; is_error = e2 }] ->
      Alcotest.(check string) "t1 unchanged" "keep" c1;
      Alcotest.(check bool) "t1 no error" false e1;
      Alcotest.(check string) "t2 replaced" "replaced" c2;
      Alcotest.(check bool) "t2 error" true e2
  | other ->
      Alcotest.fail (Printf.sprintf "unexpected content: %d blocks" (List.length other))

(* --- check_token_budget --- *)

let test_budget_no_limit () =
  let config = { default_config with max_input_tokens = None; max_total_tokens = None } in
  let usage = { empty_usage with total_input_tokens = 999999; total_output_tokens = 999999 } in
  Alcotest.(check bool) "no limit set" true
    (Agent.check_token_budget config usage = None)

let test_budget_input_within () =
  let config = { default_config with max_input_tokens = Some 1000 } in
  let usage = { empty_usage with total_input_tokens = 500 } in
  Alcotest.(check bool) "within budget" true
    (Agent.check_token_budget config usage = None)

let test_budget_input_exceeded () =
  let config = { default_config with max_input_tokens = Some 1000 } in
  let usage = { empty_usage with total_input_tokens = 1500 } in
  match Agent.check_token_budget config usage with
  | Some msg ->
    Alcotest.(check bool) "contains exceeded" true
      (String.length (Error.to_string msg) > 0)
  | None -> Alcotest.fail "expected budget exceeded"

let test_budget_total_exceeded () =
  let config = { default_config with max_total_tokens = Some 2000 } in
  let usage = { empty_usage with total_input_tokens = 1200; total_output_tokens = 1000 } in
  match Agent.check_token_budget config usage with
  | Some msg ->
    Alcotest.(check bool) "contains exceeded" true
      (String.length (Error.to_string msg) > 0)
  | None -> Alcotest.fail "expected total budget exceeded"

let test_budget_total_within () =
  let config = { default_config with max_total_tokens = Some 5000 } in
  let usage = { empty_usage with total_input_tokens = 1200; total_output_tokens = 1000 } in
  Alcotest.(check bool) "within total budget" true
    (Agent.check_token_budget config usage = None)

let test_budget_input_priority () =
  (* When both limits are set and input is exceeded, input error takes priority *)
  let config = { default_config with max_input_tokens = Some 100; max_total_tokens = Some 5000 } in
  let usage = { empty_usage with total_input_tokens = 200; total_output_tokens = 10 } in
  match Agent.check_token_budget config usage with
  | Some msg ->
    Alcotest.(check bool) "input mentioned" true
      (let s = Error.to_string msg in
       let len = String.length s in
       let rec find i =
         if i + 5 > len then false
         else if String.sub s i 5 = "Input" then true
         else find (i + 1)
       in find 0)
  | None -> Alcotest.fail "expected input budget exceeded"

let () =
  Alcotest.run "Agent" [
    "find_handoff", [
      Alcotest.test_case "no handoff" `Quick test_find_handoff_none;
      Alcotest.test_case "normal tool ignored" `Quick test_find_handoff_normal_tool;
      Alcotest.test_case "handoff present" `Quick test_find_handoff_present;
      Alcotest.test_case "no prompt field" `Quick test_find_handoff_no_prompt_field;
      Alcotest.test_case "empty messages" `Quick test_find_handoff_empty;
      Alcotest.test_case "mixed content" `Quick test_find_handoff_mixed_content;
    ];
    "replace_tool_result", [
      Alcotest.test_case "replace existing" `Quick test_replace_existing;
      Alcotest.test_case "missing appends" `Quick test_replace_missing_appends;
      Alcotest.test_case "preserves siblings" `Quick test_replace_preserves_other_results;
    ];
    "check_token_budget", [
      Alcotest.test_case "no limit" `Quick test_budget_no_limit;
      Alcotest.test_case "input within" `Quick test_budget_input_within;
      Alcotest.test_case "input exceeded" `Quick test_budget_input_exceeded;
      Alcotest.test_case "total exceeded" `Quick test_budget_total_exceeded;
      Alcotest.test_case "total within" `Quick test_budget_total_within;
      Alcotest.test_case "input priority" `Quick test_budget_input_priority;
    ];
  ]
