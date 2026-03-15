(** Tests for Context_reducer — pure functions, no Eio needed. *)

open Agent_sdk

let user_msg text = Types.{ role = User; content = [Text text] }
let asst_msg text = Types.{ role = Assistant; content = [Text text] }
let tool_use_msg id name = Types.{ role = Assistant; content = [ToolUse { id; name; input = `Null }] }
let tool_result_msg id content = Types.{ role = User; content = [ToolResult { tool_use_id = id; content; is_error = false }] }

(* --- keep_last_n --- *)

let test_keep_last_n_identity () =
  let msgs = [user_msg "a"; asst_msg "b"] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 10) msgs in
  Alcotest.(check int) "all preserved" (List.length msgs) (List.length result)

let test_keep_last_n_trims () =
  let msgs = [
    user_msg "turn1"; asst_msg "r1";
    user_msg "turn2"; asst_msg "r2";
    user_msg "turn3"; asst_msg "r3";
  ] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 2) msgs in
  Alcotest.(check int) "trimmed to 2 turns (4 msgs)" 4 (List.length result);
  (* First message in result should be "turn2" *)
  (match result with
   | { Types.content = [Types.Text t]; _ } :: _ ->
     Alcotest.(check string) "first msg is turn2" "turn2" t
   | _ -> Alcotest.fail "unexpected first message")

let test_keep_last_n_tool_pairing () =
  (* Turn 1: user + assistant(ToolUse) + user(ToolResult) + assistant *)
  (* Turn 2: user + assistant *)
  let msgs = [
    user_msg "turn1";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";
    asst_msg "answer is 42";
    user_msg "turn2";
    asst_msg "done";
  ] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 1) msgs in
  (* Last turn is [user_msg "turn2"; asst_msg "done"] *)
  Alcotest.(check int) "last turn only" 2 (List.length result)

let test_keep_last_n_keeps_tool_pair_in_turn () =
  (* Keeping last 2 turns when turn1 has tool use/result *)
  let msgs = [
    user_msg "turn0"; asst_msg "r0";
    user_msg "turn1";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";
    asst_msg "answer";
    user_msg "turn2"; asst_msg "done";
  ] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 2) msgs in
  (* turn1 has 4 messages (user + tool_use + tool_result + asst), turn2 has 2 *)
  Alcotest.(check int) "2 turns with tool pair" 6 (List.length result);
  (* Verify tool_result is present *)
  let has_tool_result = List.exists (fun (m : Types.message) ->
    List.exists (function Types.ToolResult _ -> true | _ -> false) m.content
  ) result in
  Alcotest.(check bool) "tool result present" true has_tool_result

(* --- token_budget --- *)

let test_token_budget_within () =
  let msgs = [user_msg "hi"; asst_msg "hello"] in
  let result = Context_reducer.reduce (Context_reducer.token_budget 10000) msgs in
  Alcotest.(check int) "all preserved" (List.length msgs) (List.length result)

let test_token_budget_exceeds () =
  (* Each turn is ~1 token for short text, but let's use longer strings *)
  let long = String.make 400 'x' in (* 400 chars ~ 100 tokens *)
  let msgs = [
    user_msg long; asst_msg long;  (* turn1: ~200 tokens *)
    user_msg long; asst_msg long;  (* turn2: ~200 tokens *)
    user_msg long; asst_msg long;  (* turn3: ~200 tokens *)
  ] in
  let result = Context_reducer.reduce (Context_reducer.token_budget 250) msgs in
  (* Budget 250 fits 1 turn (~200 tokens), not 2 *)
  Alcotest.(check int) "only last turn" 2 (List.length result)

let test_token_budget_image_doc () =
  let msgs = [
    Types.{ role = User; content = [Image { media_type = "image/png"; data = "abc"; source_type = "base64" }] };
    asst_msg "nice image";
    Types.{ role = User; content = [Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" }] };
    asst_msg "nice doc";
  ] in
  (* Image turn ~ 1000+, Document turn ~ 2000+. Budget 1500 should keep only doc turn *)
  let result = Context_reducer.reduce (Context_reducer.token_budget 2100) msgs in
  Alcotest.(check int) "doc turn only" 2 (List.length result)

(* --- estimate_message_tokens --- *)

let test_estimate_text () =
  let msg = user_msg "hello world" in (* 11 chars -> (11+3)/4 = 3 *)
  let tokens = Context_reducer.estimate_message_tokens msg in
  Alcotest.(check int) "text estimation" 3 tokens

let test_estimate_tool_use () =
  let msg = Types.{ role = Assistant; content = [ToolUse { id = "id1"; name = "calc"; input = `String "input" }] } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  (* "calc" = 4 chars, `String "input" -> "\"input\"" = 7 chars, total 11, (11+3)/4 = 3 *)
  Alcotest.(check bool) "tool_use > 0" true (tokens > 0)

let test_estimate_tool_result () =
  let msg = Types.{ role = User; content = [ToolResult { tool_use_id = "id1"; content = "result text"; is_error = false }] } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  (* "result text" = 11 chars -> (11+3)/4 = 3 *)
  Alcotest.(check int) "tool_result estimation" 3 tokens

let test_estimate_image () =
  let msg = Types.{ role = User; content = [Image { media_type = "image/png"; data = "x"; source_type = "base64" }] } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  Alcotest.(check int) "image estimation" 1000 tokens

let test_estimate_document () =
  let msg = Types.{ role = User; content = [Document { media_type = "application/pdf"; data = "x"; source_type = "base64" }] } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  Alcotest.(check int) "document estimation" 2000 tokens

(* --- group_into_turns --- *)

let test_group_basic () =
  let msgs = [
    user_msg "a"; asst_msg "b";
    user_msg "c"; asst_msg "d";
  ] in
  let turns = Context_reducer.group_into_turns msgs in
  Alcotest.(check int) "2 turns" 2 (List.length turns);
  Alcotest.(check int) "turn1 has 2 msgs" 2 (List.length (List.nth turns 0));
  Alcotest.(check int) "turn2 has 2 msgs" 2 (List.length (List.nth turns 1))

let test_group_tool_result_stays () =
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";
    asst_msg "done";
  ] in
  let turns = Context_reducer.group_into_turns msgs in
  (* All 4 messages should be in one turn because ToolResult prevents split *)
  Alcotest.(check int) "1 turn" 1 (List.length turns);
  Alcotest.(check int) "4 msgs in turn" 4 (List.length (List.nth turns 0))

let test_group_assistant_first () =
  let msgs = [asst_msg "orphan"; user_msg "a"; asst_msg "b"] in
  let turns = Context_reducer.group_into_turns msgs in
  (* First turn: [orphan], second turn: [a, b] *)
  Alcotest.(check int) "2 turns" 2 (List.length turns)

(* --- custom strategy --- *)

let test_custom () =
  let called = ref false in
  let strategy = Context_reducer.custom (fun msgs ->
    called := true;
    List.rev msgs
  ) in
  let msgs = [user_msg "a"; asst_msg "b"] in
  let result = Context_reducer.reduce strategy msgs in
  Alcotest.(check bool) "custom fn called" true !called;
  Alcotest.(check int) "same length" 2 (List.length result);
  (* Result is reversed *)
  (match result with
   | { Types.role = Types.Assistant; _ } :: _ -> ()
   | _ -> Alcotest.fail "expected reversed order")

(* --- edge cases --- *)

let test_empty () =
  let result = Context_reducer.reduce (Context_reducer.keep_last 5) [] in
  Alcotest.(check int) "empty" 0 (List.length result)

let test_single_message () =
  let msgs = [user_msg "only"] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 5) msgs in
  Alcotest.(check int) "single" 1 (List.length result)

let () =
  Alcotest.run "Context_reducer" [
    "keep_last_n", [
      Alcotest.test_case "fewer turns than N" `Quick test_keep_last_n_identity;
      Alcotest.test_case "trims oldest turns" `Quick test_keep_last_n_trims;
      Alcotest.test_case "tool pairing: last turn only" `Quick test_keep_last_n_tool_pairing;
      Alcotest.test_case "tool pair kept in turn" `Quick test_keep_last_n_keeps_tool_pair_in_turn;
    ];
    "token_budget", [
      Alcotest.test_case "within budget" `Quick test_token_budget_within;
      Alcotest.test_case "exceeds budget" `Quick test_token_budget_exceeds;
      Alcotest.test_case "image/doc estimation" `Quick test_token_budget_image_doc;
    ];
    "estimate_tokens", [
      Alcotest.test_case "text" `Quick test_estimate_text;
      Alcotest.test_case "tool_use" `Quick test_estimate_tool_use;
      Alcotest.test_case "tool_result" `Quick test_estimate_tool_result;
      Alcotest.test_case "image" `Quick test_estimate_image;
      Alcotest.test_case "document" `Quick test_estimate_document;
    ];
    "group_into_turns", [
      Alcotest.test_case "basic grouping" `Quick test_group_basic;
      Alcotest.test_case "tool_result stays in turn" `Quick test_group_tool_result_stays;
      Alcotest.test_case "assistant-first" `Quick test_group_assistant_first;
    ];
    "custom", [
      Alcotest.test_case "custom fn called" `Quick test_custom;
    ];
    "edge_cases", [
      Alcotest.test_case "empty messages" `Quick test_empty;
      Alcotest.test_case "single message" `Quick test_single_message;
    ];
  ]
