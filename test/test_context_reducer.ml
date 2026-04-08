(** Tests for Context_reducer — pure functions, no Eio needed. *)

open Agent_sdk

let user_msg text = Types.{ role = User; content = [Text text]; name = None; tool_call_id = None }
let asst_msg text = Types.{ role = Assistant; content = [Text text]; name = None; tool_call_id = None }
let tool_use_msg id name = Types.{ role = Assistant; content = [ToolUse { id; name; input = `Null }]; name = None; tool_call_id = None }
let tool_result_msg id content = Types.{ role = User; content = [ToolResult { tool_use_id = id; content; is_error = false; json = None }]; name = None; tool_call_id = None }

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
  (* Use large data so token estimates are meaningful for budget testing *)
  let large_img = String.make 1_200_000 'A' in (* ~1200 tokens for image *)
  let large_doc = String.make 2_000_000 'A' in (* ~3000 tokens for document *)
  let msgs = [
    Types.{ role = User; content = [Image { media_type = "image/png"; data = large_img; source_type = "base64" }]; name = None; tool_call_id = None };
    asst_msg "nice image";
    Types.{ role = User; content = [Document { media_type = "application/pdf"; data = large_doc; source_type = "base64" }]; name = None; tool_call_id = None };
    asst_msg "nice doc";
  ] in
  (* Image turn: ~1200 + 3 tokens. Doc turn: ~3000 + 2 tokens. Budget 3100 should keep only doc turn *)
  let result = Context_reducer.reduce (Context_reducer.token_budget 3100) msgs in
  Alcotest.(check int) "doc turn only" 2 (List.length result)

(* --- estimate_message_tokens --- *)

let test_estimate_text () =
  let msg = user_msg "hello world" in (* 11 chars -> (11+3)/4 = 3 *)
  let tokens = Context_reducer.estimate_message_tokens msg in
  Alcotest.(check int) "text estimation" 3 tokens

let test_estimate_tool_use () =
  let msg = Types.{ role = Assistant; content = [ToolUse { id = "id1"; name = "calc"; input = `String "input" }]; name = None; tool_call_id = None } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  (* "calc" = 4 chars, `String "input" -> "\"input\"" = 7 chars, total 11, (11+3)/4 = 3 *)
  Alcotest.(check bool) "tool_use > 0" true (tokens > 0)

let test_estimate_tool_result () =
  let msg = Types.{ role = User; content = [ToolResult { tool_use_id = "id1"; content = "result text"; is_error = false; json = None }]; name = None; tool_call_id = None } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  (* "result text" = 11 chars -> (11+3)/4 = 3 *)
  Alcotest.(check int) "tool_result estimation" 3 tokens

let test_estimate_image () =
  (* Small data: 1 byte -> min((1*3/4/750)+1, 1600) = 1 *)
  let msg_small = Types.{ role = User; content = [Image { media_type = "image/png"; data = "x"; source_type = "base64" }]; name = None; tool_call_id = None } in
  let tokens_small = Context_reducer.estimate_message_tokens msg_small in
  Alcotest.(check int) "small image estimation" 1 tokens_small;
  (* Larger data: 100K chars -> min((100000*3/4/750)+1, 1600) = min(100+1, 1600) = 101 *)
  let msg_large = Types.{ role = User; content = [Image { media_type = "image/png"; data = String.make 100_000 'A'; source_type = "base64" }]; name = None; tool_call_id = None } in
  let tokens_large = Context_reducer.estimate_message_tokens msg_large in
  Alcotest.(check bool) "large image > 0" true (tokens_large > 0);
  Alcotest.(check bool) "large image capped" true (tokens_large <= 1600)

let test_estimate_document () =
  let msg_small = Types.{ role = User; content = [Document { media_type = "application/pdf"; data = "x"; source_type = "base64" }]; name = None; tool_call_id = None } in
  let tokens_small = Context_reducer.estimate_message_tokens msg_small in
  Alcotest.(check int) "small doc estimation" 1 tokens_small;
  let msg_large = Types.{ role = User; content = [Document { media_type = "application/pdf"; data = String.make 200_000 'A'; source_type = "base64" }]; name = None; tool_call_id = None } in
  let tokens_large = Context_reducer.estimate_message_tokens msg_large in
  Alcotest.(check bool) "large doc > 0" true (tokens_large > 0);
  Alcotest.(check bool) "large doc capped" true (tokens_large <= 3000)

let test_estimate_audio () =
  let msg = Types.{ role = User; content = [Audio { media_type = "audio/wav"; data = String.make 50_000 'A'; source_type = "base64" }]; name = None; tool_call_id = None } in
  let tokens = Context_reducer.estimate_message_tokens msg in
  Alcotest.(check bool) "audio > 0" true (tokens > 0);
  Alcotest.(check bool) "audio capped" true (tokens <= 5000)

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

let test_importance_scored_drops_low_scores () =
  let msgs = [
    user_msg "goal";
    asst_msg "low-value";
    asst_msg "keep-me";
  ] in
  let reducer =
    Context_reducer.importance_scored
      ~threshold:0.5
      ~scorer:(fun ~index ~total:_ _msg ->
        match index with
        | 0 -> 0.9
        | 1 -> 0.2
        | _ -> 0.8)
      ()
  in
  let result = Context_reducer.reduce reducer msgs in
  Alcotest.(check int) "drops one message" 2 (List.length result);
  match result with
  | [first; second] ->
    Alcotest.(check string) "first kept" "goal" (Types.text_of_message first);
    Alcotest.(check string) "second kept" "keep-me" (Types.text_of_message second)
  | _ -> Alcotest.fail "unexpected filtered result"

let test_importance_scored_boost_preserves_message () =
  let msgs = [
    user_msg "goal";
    asst_msg "[KEEP] anchor";
    asst_msg "low-value";
  ] in
  let reducer =
    Context_reducer.importance_scored
      ~threshold:0.5
      ~scorer:(fun ~index:_ ~total:_ _msg -> 0.2)
      ~boost:(fun msg ->
        if Util.string_contains ~needle:"[KEEP]" (Types.text_of_message msg) then
          Some 0.95
        else
          None)
      ()
  in
  let result = Context_reducer.reduce reducer msgs in
  Alcotest.(check int) "boost preserves one low score message" 1 (List.length result);
  match result with
  | [msg] ->
    Alcotest.(check string) "boosted text kept" "[KEEP] anchor" (Types.text_of_message msg)
  | _ -> Alcotest.fail "unexpected boosted result"

(* --- edge cases --- *)

let test_empty () =
  let result = Context_reducer.reduce (Context_reducer.keep_last 5) [] in
  Alcotest.(check int) "empty" 0 (List.length result)

let test_single_message () =
  let msgs = [user_msg "only"] in
  let result = Context_reducer.reduce (Context_reducer.keep_last 5) msgs in
  Alcotest.(check int) "single" 1 (List.length result)

(* --- prune_tool_outputs --- *)

let test_prune_short_outputs () =
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "short";
  ] in
  let result = Context_reducer.reduce (Context_reducer.prune_tool_outputs ~max_output_len:100) msgs in
  (* "short" is under 100, so no truncation *)
  let tool_content = match List.nth result 2 with
    | { Types.content = [Types.ToolResult { content; _ }]; _ } -> content
    | _ -> Alcotest.fail "expected tool result"
  in
  Alcotest.(check string) "no truncation" "short" tool_content

let test_prune_long_outputs () =
  let long_output = String.make 200 'x' in
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" long_output;
  ] in
  let result = Context_reducer.reduce (Context_reducer.prune_tool_outputs ~max_output_len:50) msgs in
  let tool_content = match List.nth result 2 with
    | { Types.content = [Types.ToolResult { content; _ }]; _ } -> content
    | _ -> Alcotest.fail "expected tool result"
  in
  Alcotest.(check bool) "truncated" true (String.length tool_content < 200);
  Alcotest.(check bool) "has marker" true (String.length tool_content > 50)

(* --- merge_contiguous --- *)

let test_merge_same_role () =
  let msgs = [
    user_msg "a"; user_msg "b"; asst_msg "c"
  ] in
  let result = Context_reducer.reduce Context_reducer.merge_contiguous msgs in
  Alcotest.(check int) "merged to 2" 2 (List.length result);
  (* First message should have 2 content blocks *)
  (match result with
   | { Types.content; _ } :: _ ->
     Alcotest.(check int) "2 blocks" 2 (List.length content)
   | _ -> Alcotest.fail "unexpected result")

let test_merge_preserves_tool_results () =
  let msgs = [
    tool_result_msg "t1" "42";
    tool_result_msg "t2" "43";
  ] in
  let result = Context_reducer.reduce Context_reducer.merge_contiguous msgs in
  (* Tool result messages should NOT be merged *)
  Alcotest.(check int) "not merged" 2 (List.length result)

(* --- drop_thinking --- *)

let test_drop_thinking_removes () =
  let msgs = [
    Types.{ role = Assistant; content = [
      Thinking { thinking_type = ""; content = "internal reasoning" };
      Text "answer"
    ]; name = None; tool_call_id = None};
    user_msg "follow up";
    asst_msg "response";
  ] in
  let result = Context_reducer.reduce Context_reducer.drop_thinking msgs in
  (* First message should have thinking stripped (not in last 2) *)
  (match result with
   | { Types.content; _ } :: _ ->
     let has_thinking = List.exists (function Types.Thinking _ -> true | _ -> false) content in
     Alcotest.(check bool) "thinking removed" false has_thinking
   | _ -> Alcotest.fail "unexpected result")

let test_drop_thinking_preserves_recent () =
  let msgs = [
    user_msg "q";
    Types.{ role = Assistant; content = [
      Thinking { thinking_type = ""; content = "recent thinking" };
      Text "answer"
    ]; name = None; tool_call_id = None};
  ] in
  let result = Context_reducer.reduce Context_reducer.drop_thinking msgs in
  (* Last 2 messages are preserved *)
  (match List.nth result 1 with
   | { Types.content; _ } ->
     let has_thinking = List.exists (function Types.Thinking _ -> true | _ -> false) content in
     Alcotest.(check bool) "thinking preserved in recent" true has_thinking)

(* --- compose --- *)

let test_compose () =
  let long_output = String.make 200 'x' in
  let msgs = [
    Types.{ role = Assistant; content = [
      Thinking { thinking_type = ""; content = "old thinking" };
      Text "old answer"
    ]; name = None; tool_call_id = None};
    user_msg "q2";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" long_output;
    asst_msg "final";
  ] in
  let reducer = Context_reducer.compose [
    Context_reducer.drop_thinking;
    Context_reducer.prune_tool_outputs ~max_output_len:50;
  ] in
  let result = Context_reducer.reduce reducer msgs in
  (* First message should have thinking removed *)
  (match result with
   | { Types.content; _ } :: _ ->
     let has_thinking = List.exists (function Types.Thinking _ -> true | _ -> false) content in
     Alcotest.(check bool) "thinking removed" false has_thinking
   | _ -> Alcotest.fail "unexpected result");
  (* Tool result should be truncated *)
  let tool_msg = List.find (fun (m : Types.message) ->
    List.exists (function Types.ToolResult _ -> true | _ -> false) m.content
  ) result in
  (match tool_msg.content with
   | [Types.ToolResult { content; _ }] ->
     Alcotest.(check bool) "truncated" true (String.length content < 200)
   | _ -> Alcotest.fail "expected tool result")

(* --- keep_first_and_last --- *)

let test_keep_first_and_last_identity () =
  let msgs = [
    user_msg "turn1"; asst_msg "r1";
    user_msg "turn2"; asst_msg "r2";
  ] in
  (* first_n=1 + last_n=1 = 2, total turns = 2, so all preserved *)
  let result = Context_reducer.reduce
    (Context_reducer.keep_first_and_last ~first_n:1 ~last_n:1) msgs in
  Alcotest.(check int) "all preserved" (List.length msgs) (List.length result)

let test_keep_first_and_last_drops_middle () =
  let msgs = [
    user_msg "turn1"; asst_msg "r1";
    user_msg "turn2"; asst_msg "r2";
    user_msg "turn3"; asst_msg "r3";
    user_msg "turn4"; asst_msg "r4";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.keep_first_and_last ~first_n:1 ~last_n:1) msgs in
  (* Should keep turn1 and turn4, dropping turn2 and turn3 *)
  Alcotest.(check int) "first + last = 4 msgs" 4 (List.length result);
  (match result with
   | { Types.content = [Types.Text t1]; _ } :: _ :: { Types.content = [Types.Text t3]; _ } :: _ ->
     Alcotest.(check string) "first is turn1" "turn1" t1;
     Alcotest.(check string) "last start is turn4" "turn4" t3
   | _ -> Alcotest.fail "unexpected structure")

(* --- prune_by_role --- *)

let test_prune_by_role_drops_user () =
  let msgs = [
    user_msg "q1"; asst_msg "a1";
    user_msg "q2"; asst_msg "a2";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.prune_by_role ~drop_roles:[Types.User]) msgs in
  (* Only assistant messages remain *)
  Alcotest.(check int) "only assistant" 2 (List.length result);
  List.iter (fun (m : Types.message) ->
    Alcotest.(check bool) "is assistant" true (m.role = Types.Assistant)
  ) result

let test_prune_by_role_preserves_tool_result () =
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";
    asst_msg "done";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.prune_by_role ~drop_roles:[Types.User]) msgs in
  (* The tool_result user message and tool_use assistant message must be preserved *)
  let has_tool_result = List.exists (fun (m : Types.message) ->
    List.exists (function Types.ToolResult _ -> true | _ -> false) m.content
  ) result in
  Alcotest.(check bool) "tool result preserved" true has_tool_result

(* --- summarize_old --- *)

let test_summarize_old_basic () =
  let msgs = [
    user_msg "turn1"; asst_msg "r1";
    user_msg "turn2"; asst_msg "r2";
    user_msg "turn3"; asst_msg "r3";
  ] in
  let summarizer msgs =
    Printf.sprintf "[Summary of %d messages]" (List.length msgs) in
  let result = Context_reducer.reduce
    (Context_reducer.summarize_old ~keep_recent:1 ~summarizer) msgs in
  (* Should have: summary_msg + turn3 (2 msgs) = 3 msgs *)
  Alcotest.(check int) "summary + recent" 3 (List.length result);
  (match result with
   | { Types.content = [Types.Text t]; _ } :: _ ->
     Alcotest.(check bool) "has summary prefix" true
       (String.length t > 0 && t.[0] = '[')
   | _ -> Alcotest.fail "expected summary first")

let test_summarize_old_all_recent () =
  let msgs = [user_msg "only"; asst_msg "one"] in
  let summarizer _ = "should not be called" in
  let result = Context_reducer.reduce
    (Context_reducer.summarize_old ~keep_recent:5 ~summarizer) msgs in
  (* All turns fit in keep_recent, no summarization *)
  Alcotest.(check int) "no change" 2 (List.length result)

(* --- clear_tool_results --- *)

let test_clear_tool_results_clears_old () =
  (* 3 turns: turn1 has tool result, turn2 has tool result, turn3 is recent *)
  let msgs = [
    user_msg "turn1";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" (String.make 200 'x');  (* long result *)
    asst_msg "r1";
    user_msg "turn2";
    tool_use_msg "t2" "search";
    tool_result_msg "t2" (String.make 300 'y');  (* long result *)
    asst_msg "r2";
    user_msg "turn3";
    asst_msg "r3";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.clear_tool_results ~keep_recent:1) msgs in
  (* Old turns' tool results should be cleared *)
  let old_tool_results = List.filter_map (fun (m : Types.message) ->
    List.find_map (function
      | Types.ToolResult { content; _ } -> Some content
      | _ -> None
    ) m.content
  ) result in
  let cleared = List.filter (fun c ->
    let len = String.length c in
    len < 100 (* cleared results are short markers *)
  ) old_tool_results in
  Alcotest.(check int) "2 old results cleared" 2 (List.length cleared)

let test_clear_tool_results_preserves_recent () =
  let msgs = [
    user_msg "turn1";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" (String.make 200 'x');
    asst_msg "r1";
  ] in
  (* keep_recent:1 means all turns are recent — nothing cleared *)
  let result = Context_reducer.reduce
    (Context_reducer.clear_tool_results ~keep_recent:5) msgs in
  let tool_results = List.filter_map (fun (m : Types.message) ->
    List.find_map (function
      | Types.ToolResult { content; _ } -> Some content
      | _ -> None
    ) m.content
  ) result in
  (match tool_results with
   | [content] -> Alcotest.(check int) "preserved full length" 200 (String.length content)
   | _ -> Alcotest.fail "expected 1 tool result")

let test_clear_tool_results_short_untouched () =
  (* Short tool results (<=50 chars) should NOT be cleared even in old turns *)
  let msgs = [
    user_msg "turn1";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";  (* 2 chars, below 50 threshold *)
    asst_msg "r1";
    user_msg "turn2";
    asst_msg "r2";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.clear_tool_results ~keep_recent:1) msgs in
  let tool_results = List.filter_map (fun (m : Types.message) ->
    List.find_map (function
      | Types.ToolResult { content; _ } -> Some content
      | _ -> None
    ) m.content
  ) result in
  (match tool_results with
   | [content] -> Alcotest.(check string) "short preserved" "42" content
   | _ -> Alcotest.fail "expected 1 tool result")

let test_clear_tool_results_error_marker () =
  (* Error tool results should get error-specific marker *)
  let msgs = [
    user_msg "turn1";
    Types.{ role = Assistant; content = [ToolUse { id = "t1"; name = "fail"; input = `Null }];
            name = None; tool_call_id = None };
    Types.{ role = User; content = [ToolResult { tool_use_id = "t1";
            content = String.make 100 'E'; is_error = true; json = None }];
            name = None; tool_call_id = None };
    asst_msg "r1";
    user_msg "turn2"; asst_msg "r2";
  ] in
  let result = Context_reducer.reduce
    (Context_reducer.clear_tool_results ~keep_recent:1) msgs in
  let has_error_marker = List.exists (fun (m : Types.message) ->
    List.exists (function
      | Types.ToolResult { content; _ } ->
        content = "[tool error result cleared]"
      | _ -> false
    ) m.content
  ) result in
  Alcotest.(check bool) "error marker present" true has_error_marker

(* --- prune_tool_args --- *)

(** Helper: build an assistant message with a ToolUse whose input has a long string arg. *)
let tool_use_msg_with_input id name input =
  Types.{ role = Assistant; content = [ToolUse { id; name; input }]; name = None; tool_call_id = None }

let test_prune_tool_args_short_unchanged () =
  let input = `Assoc [("content", `String "short")] in
  let msgs = [
    user_msg "turn1";
    tool_use_msg_with_input "t1" "write_file" input;
    tool_result_msg "t1" "ok";
    user_msg "turn2"; asst_msg "done";
  ] in
  let reducer = Context_reducer.prune_tool_args ~max_arg_len:2000 () in
  let result = Context_reducer.reduce reducer msgs in
  (* Short args unchanged *)
  (match List.nth result 1 with
   | { Types.content = [Types.ToolUse { input = `Assoc pairs; _ }]; _ } ->
     (match List.assoc "content" pairs with
      | `String s -> Alcotest.(check string) "unchanged" "short" s
      | _ -> Alcotest.fail "expected string")
   | _ -> Alcotest.fail "expected tool use")

let test_prune_tool_args_long_truncated () =
  let long_str = String.make 3000 'x' in
  let input = `Assoc [("content", `String long_str); ("path", `String "/file.txt")] in
  let msgs = [
    user_msg "turn1";
    tool_use_msg_with_input "t1" "write_file" input;
    tool_result_msg "t1" "ok";
    user_msg "turn2"; asst_msg "done";
  ] in
  (* keep_recent=1 so turn1 (with tool use) is in the old zone *)
  let reducer = Context_reducer.prune_tool_args ~max_arg_len:2000 ~keep_recent:1 () in
  let result = Context_reducer.reduce reducer msgs in
  (match List.nth result 1 with
   | { Types.content = [Types.ToolUse { input = `Assoc pairs; _ }]; _ } ->
     (* "content" should be truncated *)
     (match List.assoc "content" pairs with
      | `String s ->
        Alcotest.(check bool) "truncated" true (String.length s < 3000);
        let has_truncated_marker =
          try let _ = Str.search_forward (Str.regexp_string "(truncated") s 0 in true
          with Not_found -> false in
        Alcotest.(check bool) "has marker" true has_truncated_marker
      | _ -> Alcotest.fail "expected string");
     (* "path" should be unchanged (short) *)
     (match List.assoc "path" pairs with
      | `String s -> Alcotest.(check string) "path unchanged" "/file.txt" s
      | _ -> Alcotest.fail "expected string")
   | _ -> Alcotest.fail "expected tool use")

let test_prune_tool_args_preserves_recent () =
  let long_str = String.make 3000 'x' in
  let input = `Assoc [("content", `String long_str)] in
  let msgs = [
    user_msg "turn1";
    tool_use_msg_with_input "t1" "write_file" input;
    tool_result_msg "t1" "ok";
  ] in
  (* keep_recent=1 means all turns are recent *)
  let reducer = Context_reducer.prune_tool_args ~max_arg_len:100 ~keep_recent:1 () in
  let result = Context_reducer.reduce reducer msgs in
  (match List.nth result 1 with
   | { Types.content = [Types.ToolUse { input = `Assoc pairs; _ }]; _ } ->
     (match List.assoc "content" pairs with
      | `String s -> Alcotest.(check int) "not truncated" 3000 (String.length s)
      | _ -> Alcotest.fail "expected string")
   | _ -> Alcotest.fail "expected tool use")

let test_prune_tool_args_nested_json () =
  let long_str = String.make 3000 'y' in
  let input = `Assoc [("nested", `Assoc [("deep", `String long_str)])] in
  let msgs = [
    user_msg "old turn";
    tool_use_msg_with_input "t1" "edit_file" input;
    tool_result_msg "t1" "ok";
    user_msg "recent"; asst_msg "done";
  ] in
  let reducer = Context_reducer.prune_tool_args ~max_arg_len:100 ~keep_recent:1 () in
  let result = Context_reducer.reduce reducer msgs in
  (match List.nth result 1 with
   | { Types.content = [Types.ToolUse { input = `Assoc pairs; _ }]; _ } ->
     (match List.assoc "nested" pairs with
      | `Assoc nested_pairs ->
        (match List.assoc "deep" nested_pairs with
         | `String s ->
           Alcotest.(check bool) "nested truncated" true (String.length s < 3000)
         | _ -> Alcotest.fail "expected string")
      | _ -> Alcotest.fail "expected nested assoc")
   | _ -> Alcotest.fail "expected tool use")

let test_prune_tool_args_non_assistant_untouched () =
  (* User messages should never be modified, even with ToolResult *)
  let msgs = [
    user_msg "turn1";
    tool_use_msg_with_input "t1" "calc" (`Assoc [("x", `String (String.make 5000 'z'))]);
    tool_result_msg "t1" (String.make 5000 'r');
    user_msg "turn2"; asst_msg "done";
  ] in
  let reducer = Context_reducer.prune_tool_args ~max_arg_len:100 () in
  let result = Context_reducer.reduce reducer msgs in
  (* ToolResult (in User msg) should be unchanged *)
  (match List.nth result 2 with
   | { Types.content = [Types.ToolResult { content; _ }]; _ } ->
     Alcotest.(check int) "tool result unchanged" 5000 (String.length content)
   | _ -> Alcotest.fail "expected tool result")

(* --- repair_dangling_tool_calls --- *)

let test_repair_no_orphans () =
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    tool_result_msg "t1" "42";
    asst_msg "done";
  ] in
  let result = Context_reducer.reduce Context_reducer.repair_dangling_tool_calls msgs in
  Alcotest.(check int) "no change" (List.length msgs) (List.length result)

let test_repair_single_orphan () =
  (* ToolUse without matching ToolResult *)
  let msgs = [
    user_msg "q";
    tool_use_msg "t1" "calc";
    asst_msg "continuing";
  ] in
  let result = Context_reducer.reduce Context_reducer.repair_dangling_tool_calls msgs in
  (* Should insert a synthetic ToolResult after the ToolUse message *)
  Alcotest.(check int) "repaired (+1 msg)" 4 (List.length result);
  (* The inserted message should be a User message with ToolResult *)
  (match List.nth result 2 with
   | { Types.content = [Types.ToolResult { tool_use_id; is_error; _ }]; _ } ->
     Alcotest.(check string) "matches id" "t1" tool_use_id;
     Alcotest.(check bool) "is_error" true is_error
   | _ -> Alcotest.fail "expected synthetic tool result")

let test_repair_multiple_orphans () =
  let msgs = [
    user_msg "q";
    Types.{ role = Assistant; content = [
      ToolUse { id = "t1"; name = "read"; input = `Null };
      ToolUse { id = "t2"; name = "write"; input = `Null };
    ]; name = None; tool_call_id = None};
    asst_msg "done";
  ] in
  let result = Context_reducer.reduce Context_reducer.repair_dangling_tool_calls msgs in
  (* 2 orphan ToolUse → 2 synthetic ToolResult messages inserted *)
  Alcotest.(check int) "repaired (+2)" 5 (List.length result)

let test_repair_partial_orphan () =
  (* t1 has result, t2 does not *)
  let msgs = [
    user_msg "q";
    Types.{ role = Assistant; content = [
      ToolUse { id = "t1"; name = "read"; input = `Null };
      ToolUse { id = "t2"; name = "write"; input = `Null };
    ]; name = None; tool_call_id = None};
    tool_result_msg "t1" "ok";
    asst_msg "done";
  ] in
  let result = Context_reducer.reduce Context_reducer.repair_dangling_tool_calls msgs in
  (* Only t2 is orphan → 1 synthetic ToolResult *)
  Alcotest.(check int) "repaired (+1)" 5 (List.length result)

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
      Alcotest.test_case "audio" `Quick test_estimate_audio;
    ];
    "group_into_turns", [
      Alcotest.test_case "basic grouping" `Quick test_group_basic;
      Alcotest.test_case "tool_result stays in turn" `Quick test_group_tool_result_stays;
      Alcotest.test_case "assistant-first" `Quick test_group_assistant_first;
    ];
    "custom", [
      Alcotest.test_case "custom fn called" `Quick test_custom;
    ];
    "importance_scored", [
      Alcotest.test_case "drops low scores" `Quick test_importance_scored_drops_low_scores;
      Alcotest.test_case "boost preserves message" `Quick test_importance_scored_boost_preserves_message;
    ];
    "prune_tool_outputs", [
      Alcotest.test_case "short outputs unchanged" `Quick test_prune_short_outputs;
      Alcotest.test_case "long outputs truncated" `Quick test_prune_long_outputs;
    ];
    "merge_contiguous", [
      Alcotest.test_case "same role merged" `Quick test_merge_same_role;
      Alcotest.test_case "tool results not merged" `Quick test_merge_preserves_tool_results;
    ];
    "drop_thinking", [
      Alcotest.test_case "removes old thinking" `Quick test_drop_thinking_removes;
      Alcotest.test_case "preserves recent thinking" `Quick test_drop_thinking_preserves_recent;
    ];
    "compose", [
      Alcotest.test_case "compose strategies" `Quick test_compose;
    ];
    "keep_first_and_last", [
      Alcotest.test_case "identity when under limit" `Quick test_keep_first_and_last_identity;
      Alcotest.test_case "drops middle turns" `Quick test_keep_first_and_last_drops_middle;
    ];
    "prune_by_role", [
      Alcotest.test_case "drops user messages" `Quick test_prune_by_role_drops_user;
      Alcotest.test_case "preserves tool results" `Quick test_prune_by_role_preserves_tool_result;
    ];
    "summarize_old", [
      Alcotest.test_case "basic summarization" `Quick test_summarize_old_basic;
      Alcotest.test_case "all recent no-op" `Quick test_summarize_old_all_recent;
    ];
    "clear_tool_results", [
      Alcotest.test_case "clears old results" `Quick test_clear_tool_results_clears_old;
      Alcotest.test_case "preserves recent" `Quick test_clear_tool_results_preserves_recent;
      Alcotest.test_case "short untouched" `Quick test_clear_tool_results_short_untouched;
      Alcotest.test_case "error marker" `Quick test_clear_tool_results_error_marker;
    ];
    "prune_tool_args", [
      Alcotest.test_case "short args unchanged" `Quick test_prune_tool_args_short_unchanged;
      Alcotest.test_case "long args truncated" `Quick test_prune_tool_args_long_truncated;
      Alcotest.test_case "preserves recent turns" `Quick test_prune_tool_args_preserves_recent;
      Alcotest.test_case "nested JSON truncated" `Quick test_prune_tool_args_nested_json;
      Alcotest.test_case "non-assistant untouched" `Quick test_prune_tool_args_non_assistant_untouched;
    ];
    "repair_dangling_tool_calls", [
      Alcotest.test_case "no orphans unchanged" `Quick test_repair_no_orphans;
      Alcotest.test_case "single orphan repaired" `Quick test_repair_single_orphan;
      Alcotest.test_case "multiple orphans repaired" `Quick test_repair_multiple_orphans;
      Alcotest.test_case "partial orphan repaired" `Quick test_repair_partial_orphan;
    ];
    "edge_cases", [
      Alcotest.test_case "empty messages" `Quick test_empty;
      Alcotest.test_case "single message" `Quick test_single_message;
    ];
  ]
