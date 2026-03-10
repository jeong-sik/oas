(** Test Types parsing, especially Unknown stop_reason variant. *)

open Agent_sdk

let test_known_stop_reasons () =
  Alcotest.(check string) "end_turn"
    "Types.EndTurn" (Types.show_stop_reason (Types.stop_reason_of_string "end_turn"));
  Alcotest.(check string) "tool_use"
    "Types.StopToolUse" (Types.show_stop_reason (Types.stop_reason_of_string "tool_use"));
  Alcotest.(check string) "max_tokens"
    "Types.MaxTokens" (Types.show_stop_reason (Types.stop_reason_of_string "max_tokens"));
  Alcotest.(check string) "stop_sequence"
    "Types.StopSequence" (Types.show_stop_reason (Types.stop_reason_of_string "stop_sequence"))

let test_unknown_stop_reason () =
  let sr = Types.stop_reason_of_string "some_new_reason" in
  let shown = Types.show_stop_reason sr in
  Alcotest.(check bool) "contains Unknown" true
    (String.length shown > 0 &&
     try let _ = String.index shown 'U' in true with Not_found -> false);
  match sr with
  | Types.Unknown s ->
    Alcotest.(check string) "preserves original string" "some_new_reason" s
  | _ ->
    Alcotest.fail (Printf.sprintf "expected Unknown variant, got %s" shown)

let test_empty_stop_reason () =
  let sr = Types.stop_reason_of_string "" in
  match sr with
  | Types.Unknown s ->
    Alcotest.(check string) "empty string preserved" "" s
  | _ ->
    Alcotest.fail
      (Printf.sprintf "expected Unknown for empty, got %s" (Types.show_stop_reason sr))

let test_model_to_string () =
  Alcotest.(check string) "opus 4.6" "claude-opus-4-6-20250514"
    (Types.model_to_string Types.Claude_opus_4_6);
  Alcotest.(check string) "sonnet 4.6" "claude-sonnet-4-6-20250514"
    (Types.model_to_string Types.Claude_sonnet_4_6);
  Alcotest.(check string) "haiku 4.5" "claude-haiku-4-5-20251001"
    (Types.model_to_string Types.Claude_haiku_4_5);
  Alcotest.(check string) "custom" "my-model"
    (Types.model_to_string (Types.Custom "my-model"))

let test_role_to_string () =
  Alcotest.(check string) "user" "user" (Types.role_to_string Types.User);
  Alcotest.(check string) "assistant" "assistant" (Types.role_to_string Types.Assistant)

let test_param_type_to_string () =
  Alcotest.(check string) "string" "string" (Types.param_type_to_string Types.String);
  Alcotest.(check string) "integer" "integer" (Types.param_type_to_string Types.Integer);
  Alcotest.(check string) "number" "number" (Types.param_type_to_string Types.Number);
  Alcotest.(check string) "boolean" "boolean" (Types.param_type_to_string Types.Boolean);
  Alcotest.(check string) "array" "array" (Types.param_type_to_string Types.Array);
  Alcotest.(check string) "object" "object" (Types.param_type_to_string Types.Object)

let test_tool_choice_auto () =
  let json = Types.tool_choice_to_json Types.Auto in
  match json with
  | `Assoc [("type", `String "auto")] -> ()
  | _ -> Alcotest.fail "expected auto"

let test_tool_choice_any () =
  let json = Types.tool_choice_to_json Types.Any in
  match json with
  | `Assoc [("type", `String "any")] -> ()
  | _ -> Alcotest.fail "expected any"

let test_tool_choice_tool () =
  let json = Types.tool_choice_to_json (Types.Tool "calculator") in
  match json with
  | `Assoc [("type", `String "tool"); ("name", `String "calculator")] -> ()
  | _ -> Alcotest.fail "expected tool with name"

let test_add_usage () =
  let stats = Types.empty_usage in
  let u : Types.api_usage = {
    input_tokens = 10; output_tokens = 20;
    cache_creation_input_tokens = 5; cache_read_input_tokens = 3;
  } in
  let result = Types.add_usage stats u in
  Alcotest.(check int) "input" 10 result.total_input_tokens;
  Alcotest.(check int) "output" 20 result.total_output_tokens;
  Alcotest.(check int) "cache_creation" 5 result.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 3 result.total_cache_read_input_tokens;
  Alcotest.(check int) "api_calls" 1 result.api_calls

let test_add_usage_accumulates () =
  let u1 : Types.api_usage = {
    input_tokens = 10; output_tokens = 5;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
  } in
  let u2 : Types.api_usage = {
    input_tokens = 20; output_tokens = 15;
    cache_creation_input_tokens = 3; cache_read_input_tokens = 7;
  } in
  let stats = Types.add_usage Types.empty_usage u1 in
  let stats = Types.add_usage stats u2 in
  Alcotest.(check int) "total input" 30 stats.total_input_tokens;
  Alcotest.(check int) "total output" 20 stats.total_output_tokens;
  Alcotest.(check int) "api_calls" 2 stats.api_calls

let test_default_config () =
  let c = Types.default_config in
  Alcotest.(check string) "name" "agent" c.name;
  Alcotest.(check int) "max_tokens" 4096 c.max_tokens;
  Alcotest.(check int) "max_turns" 10 c.max_turns;
  Alcotest.(check bool) "no system prompt" true (c.system_prompt = None);
  Alcotest.(check bool) "no thinking_budget" true (c.thinking_budget = None);
  Alcotest.(check bool) "cache off" false c.cache_system_prompt;
  Alcotest.(check bool) "no max_input_tokens" true (c.max_input_tokens = None);
  Alcotest.(check bool) "no max_total_tokens" true (c.max_total_tokens = None)

let () =
  Alcotest.run "Types" [
    "stop_reason", [
      Alcotest.test_case "known stop reasons" `Quick test_known_stop_reasons;
      Alcotest.test_case "unknown stop reason" `Quick test_unknown_stop_reason;
      Alcotest.test_case "empty stop reason" `Quick test_empty_stop_reason;
    ];
    "model", [
      Alcotest.test_case "model_to_string" `Quick test_model_to_string;
    ];
    "role", [
      Alcotest.test_case "role_to_string" `Quick test_role_to_string;
    ];
    "param_type", [
      Alcotest.test_case "param_type_to_string" `Quick test_param_type_to_string;
    ];
    "tool_choice", [
      Alcotest.test_case "auto" `Quick test_tool_choice_auto;
      Alcotest.test_case "any" `Quick test_tool_choice_any;
      Alcotest.test_case "tool" `Quick test_tool_choice_tool;
    ];
    "usage", [
      Alcotest.test_case "add_usage" `Quick test_add_usage;
      Alcotest.test_case "accumulates" `Quick test_add_usage_accumulates;
    ];
    "config", [
      Alcotest.test_case "default_config" `Quick test_default_config;
    ];
  ]
