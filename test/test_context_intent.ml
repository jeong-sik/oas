open Alcotest
open Agent_sdk

let test_intent_of_string_accepts_aliases () =
  match Context_intent.intent_of_string "status-check" with
  | Ok Context_intent.Status_check -> ()
  | Ok other ->
      failf "expected status_check, got %s"
        (Context_intent.show_intent other)
  | Error detail -> fail detail

let test_parse_model_json_valid () =
  let json =
    `Assoc
      [
        ("intent", `String "knowledge_query");
        ("confidence", `Float 0.82);
        ("rationale", `String "asks for explanation");
      ]
  in
  match Context_intent.parse_model_json json with
  | Ok parsed ->
      check string "intent" "knowledge_query"
        (Context_intent.intent_to_string parsed.intent);
      check bool "depth" true (parsed.depth = Context_intent.Full);
      check (float 0.0001) "confidence" 0.82 parsed.confidence
  | Error detail -> fail detail

let test_parse_model_json_rejects_out_of_range_confidence () =
  let json =
    `Assoc [ ("intent", `String "coordination"); ("confidence", `Float 1.5) ]
  in
  match Context_intent.parse_model_json json with
  | Ok _ -> fail "expected parse failure"
  | Error detail ->
      check bool "mentions confidence" true
        (Util.contains_substring_ci ~haystack:detail ~needle:"confidence")

let test_heuristic_conversational () =
  let classified = Context_intent.heuristic_classify "hello and thanks" in
  check string "intent" "conversational"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Skip)

let test_heuristic_task_command () =
  let classified =
    Context_intent.heuristic_classify "fix the failing test and open a PR"
  in
  check string "intent" "task_command"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Skip)

let test_heuristic_status_check () =
  let classified =
    Context_intent.heuristic_classify "what is the current status of issue 415?"
  in
  check string "intent" "status_check"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Light)

let test_heuristic_knowledge_query () =
  let classified =
    Context_intent.heuristic_classify
      "explain how context reduction works in the SDK"
  in
  check string "intent" "knowledge_query"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Full)

let test_heuristic_coordination () =
  let classified =
    Context_intent.heuristic_classify
      "delegate this to another agent and leave a handoff note"
  in
  check string "intent" "coordination"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Light)

let test_heuristic_coordination_generic () =
  let classified =
    Context_intent.heuristic_classify
      "notify the monitor group and reserve the next task for the team"
  in
  check string "intent" "coordination"
    (Context_intent.intent_to_string classified.intent)

let test_prompt_mentions_all_categories () =
  let prompt = Context_intent.prompt_for_query "status?" in
  List.iter
    (fun needle ->
      check bool needle true
        (Util.contains_substring_ci ~haystack:prompt ~needle))
    [
      "conversational";
      "task_command";
      "status_check";
      "knowledge_query";
      "coordination";
    ]

let () =
  run "Context_intent"
    [
      ( "parsing",
        [
          test_case "intent aliases" `Quick test_intent_of_string_accepts_aliases;
          test_case "valid model json" `Quick test_parse_model_json_valid;
          test_case "confidence bounds" `Quick
            test_parse_model_json_rejects_out_of_range_confidence;
        ] );
      ( "heuristics",
        [
          test_case "conversational" `Quick test_heuristic_conversational;
          test_case "task command" `Quick test_heuristic_task_command;
          test_case "status check" `Quick test_heuristic_status_check;
          test_case "knowledge query" `Quick test_heuristic_knowledge_query;
          test_case "coordination" `Quick test_heuristic_coordination;
          test_case "coordination generic" `Quick test_heuristic_coordination_generic;
        ] );
      ("prompt", [ test_case "mentions categories" `Quick test_prompt_mentions_all_categories ]);
    ]
