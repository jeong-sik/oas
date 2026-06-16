open Alcotest
open Agent_sdk

let test_intent_of_string_accepts_aliases () =
  match Context_intent.intent_of_string "status-check" with
  | Ok Context_intent.Status_check -> ()
  | Ok other -> failf "expected status_check, got %s" (Context_intent.show_intent other)
  | Error detail -> fail detail
;;

let test_intent_of_string_transfer_alias () =
  match Context_intent.intent_of_string "transfer" with
  | Ok Context_intent.Coordination -> ()
  | Ok other -> failf "expected coordination, got %s" (Context_intent.show_intent other)
  | Error detail -> fail detail
;;

let test_intent_and_classification_json_roundtrips () =
  List.iter
    (fun intent ->
       let json = Context_intent.intent_to_yojson intent in
       (match Context_intent.intent_of_yojson json with
        | Ok decoded -> check bool "intent roundtrip" true (decoded = intent)
        | Error detail -> fail detail);
       check
         bool
         "show intent"
         true
         (String.length (Context_intent.show_intent intent) > 0))
    [ Context_intent.Conversational
    ; Context_intent.Task_command
    ; Context_intent.Status_check
    ; Context_intent.Knowledge_query
    ; Context_intent.Coordination
    ];
  List.iter
    (fun depth ->
       let json = Context_intent.retrieval_depth_to_yojson depth in
       (match Context_intent.retrieval_depth_of_yojson json with
        | Ok decoded -> check bool "depth roundtrip" true (decoded = depth)
        | Error detail -> fail detail);
       check
         bool
         "show depth"
         true
         (String.length (Context_intent.show_retrieval_depth depth) > 0))
    [ Context_intent.Skip; Context_intent.Light; Context_intent.Full ];
  let classification : Context_intent.classification =
    { intent = Context_intent.Status_check
    ; depth = Context_intent.Light
    ; confidence = 0.91
    ; rationale = Some "json"
    }
  in
  let json = Context_intent.classification_to_yojson classification in
  (match Context_intent.classification_of_yojson json with
   | Ok decoded ->
     check
       string
       "classification intent"
       "status_check"
       (Context_intent.intent_to_string decoded.intent);
     check (float 0.0001) "classification confidence" 0.91 decoded.confidence
   | Error detail -> fail detail);
  match Context_intent.intent_of_string "conversation" with
  | Ok Context_intent.Conversational -> ()
  | Ok other -> failf "expected conversational, got %s" (Context_intent.show_intent other)
  | Error detail -> fail detail
;;

let test_intent_of_string_rejects_unknown () =
  match Context_intent.intent_of_string "ship-it" with
  | Ok other -> failf "expected unknown intent, got %s" (Context_intent.show_intent other)
  | Error detail ->
    check
      bool
      "mentions unknown"
      true
      (Util.contains_substring_ci ~haystack:detail ~needle:"unknown intent")
;;

let test_parse_model_json_valid () =
  let json =
    `Assoc
      [ "intent", `String "knowledge_query"
      ; "confidence", `Float 0.82
      ; "rationale", `String "asks for explanation"
      ]
  in
  match Context_intent.parse_model_json json with
  | Ok parsed ->
    check
      string
      "intent"
      "knowledge_query"
      (Context_intent.intent_to_string parsed.intent);
    check bool "depth" true (parsed.depth = Context_intent.Full);
    check (float 0.0001) "confidence" 0.82 parsed.confidence
  | Error detail -> fail detail
;;

let test_parse_model_json_alias_without_rationale () =
  let json = `Assoc [ "intent", `String "progress"; "confidence", `Float 0.64 ] in
  match Context_intent.parse_model_json json with
  | Ok parsed ->
    check string "intent" "status_check" (Context_intent.intent_to_string parsed.intent);
    check bool "depth" true (parsed.depth = Context_intent.Light);
    check (float 0.0001) "confidence" 0.64 parsed.confidence;
    check (option string) "no rationale" None parsed.rationale
  | Error detail -> fail detail
;;

let test_parse_model_json_requires_intent_and_confidence () =
  let missing_intent = `Assoc [ "confidence", `Float 0.5 ] in
  (match Context_intent.parse_model_json missing_intent with
   | Ok _ -> fail "expected missing intent failure"
   | Error detail ->
     check
       bool
       "missing intent"
       true
       (Util.contains_substring_ci ~haystack:detail ~needle:"intent"));
  let missing_confidence = `Assoc [ "intent", `String "task_command" ] in
  match Context_intent.parse_model_json missing_confidence with
  | Ok _ -> fail "expected missing confidence failure"
  | Error detail ->
    check
      bool
      "missing confidence"
      true
      (Util.contains_substring_ci ~haystack:detail ~needle:"confidence")
;;

let test_parse_model_json_rejects_out_of_range_confidence () =
  let json = `Assoc [ "intent", `String "coordination"; "confidence", `Float 1.5 ] in
  match Context_intent.parse_model_json json with
  | Ok _ -> fail "expected parse failure"
  | Error detail ->
    check
      bool
      "mentions confidence"
      true
      (Util.contains_substring_ci ~haystack:detail ~needle:"confidence")
;;

let test_heuristic_conversational () =
  let classified = Context_intent.heuristic_classify "hello and thanks" in
  check
    string
    "intent"
    "conversational"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Skip)
;;

let test_heuristic_task_command () =
  let classified =
    Context_intent.heuristic_classify "fix the failing test and open a PR"
  in
  check string "intent" "task_command" (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Skip)
;;

let test_heuristic_status_check () =
  let classified =
    Context_intent.heuristic_classify "what is the current status of issue 415?"
  in
  check string "intent" "status_check" (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Light)
;;

let test_heuristic_knowledge_query () =
  let classified =
    Context_intent.heuristic_classify "explain how context reduction works in the SDK"
  in
  check
    string
    "intent"
    "knowledge_query"
    (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Full)
;;

let test_heuristic_coordination () =
  let classified =
    Context_intent.heuristic_classify
      "route this to another actor and leave a transfer note"
  in
  check string "intent" "coordination" (Context_intent.intent_to_string classified.intent);
  check bool "depth" true (classified.depth = Context_intent.Light)
;;

let test_heuristic_coordination_generic () =
  let classified =
    Context_intent.heuristic_classify
      "notify the monitor group and reserve the next parallel task"
  in
  check string "intent" "coordination" (Context_intent.intent_to_string classified.intent)
;;

let test_heuristic_fallback_paths () =
  let question = Context_intent.heuristic_classify "who" in
  check
    string
    "question fallback intent"
    "knowledge_query"
    (Context_intent.intent_to_string question.intent);
  check
    (option string)
    "question rationale"
    (Some "knowledge_query: default fallback")
    question.rationale;
  let short = Context_intent.heuristic_classify "ok" in
  check
    string
    "short fallback intent"
    "conversational"
    (Context_intent.intent_to_string short.intent);
  check
    (option string)
    "short rationale"
    (Some "conversational: default fallback")
    short.rationale;
  let imperative =
    Context_intent.heuristic_classify
      "proceed through the next slice until verification succeeds"
  in
  check
    string
    "imperative fallback intent"
    "task_command"
    (Context_intent.intent_to_string imperative.intent);
  check
    (option string)
    "imperative rationale"
    (Some "task_command: default fallback")
    imperative.rationale
;;

let test_no_reserved_keywords_in_heuristic () =
  (* Verify that heuristic keywords do not contain domain-specific terms
     that would couple OAS to any particular downstream consumer. *)
  let reserved_terms = [ "delegate"; "handoff"; "agent"; "team" ] in
  let source =
    {|assign route transfer notify group actor monitor coordinate sync reserve parallel|}
  in
  List.iter
    (fun term ->
       check
         bool
         (Printf.sprintf "no '%s' in coordination keywords" term)
         true
         (not (Util.contains_substring_ci ~haystack:source ~needle:term)))
    reserved_terms
;;

let test_prompt_mentions_all_categories () =
  let prompt = Context_intent.prompt_for_query "status?" in
  List.iter
    (fun needle ->
       check bool needle true (Util.contains_substring_ci ~haystack:prompt ~needle))
    [ "conversational"
    ; "task_command"
    ; "status_check"
    ; "knowledge_query"
    ; "coordination"
    ]
;;

let test_classify_hybrid_requires_explicit_fallback () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let provider =
    { Provider.provider =
        Provider.OpenAICompat
          { base_url = "http://"
          ; auth_header = None
          ; path = "/v1/chat/completions"
          ; static_token = None
          }
    ; model_id = "gpt-4.1"
    ; api_key_env = ""
    }
  in
  let fallback _query =
    { Context_intent.intent = Context_intent.Coordination
    ; depth = Context_intent.Light
    ; confidence = 0.37
    ; rationale = Some "explicit fallback"
    }
  in
  match
    Context_intent.classify_hybrid
      ~sw
      ~net:env#net
      ~provider
      ~config:Types.default_config
      ~fallback
      "status?"
  with
  | Ok classified ->
    check
      string
      "intent"
      "coordination"
      (Context_intent.intent_to_string classified.intent);
    check (float 0.0001) "confidence" 0.37 classified.confidence;
    check (option string) "rationale" (Some "explicit fallback") classified.rationale
  | Error detail -> fail (Error.to_string detail)
;;

let () =
  run
    "Context_intent"
    [ ( "parsing"
      , [ test_case "intent aliases" `Quick test_intent_of_string_accepts_aliases
        ; test_case "transfer alias" `Quick test_intent_of_string_transfer_alias
        ; test_case
            "intent and classification json roundtrips"
            `Quick
            test_intent_and_classification_json_roundtrips
        ; test_case "unknown intent rejected" `Quick test_intent_of_string_rejects_unknown
        ; test_case "valid model json" `Quick test_parse_model_json_valid
        ; test_case
            "alias without rationale"
            `Quick
            test_parse_model_json_alias_without_rationale
        ; test_case
            "required model json fields"
            `Quick
            test_parse_model_json_requires_intent_and_confidence
        ; test_case
            "confidence bounds"
            `Quick
            test_parse_model_json_rejects_out_of_range_confidence
        ] )
    ; ( "heuristics"
      , [ test_case "conversational" `Quick test_heuristic_conversational
        ; test_case "task command" `Quick test_heuristic_task_command
        ; test_case "status check" `Quick test_heuristic_status_check
        ; test_case "knowledge query" `Quick test_heuristic_knowledge_query
        ; test_case "coordination" `Quick test_heuristic_coordination
        ; test_case "coordination generic" `Quick test_heuristic_coordination_generic
        ; test_case "fallback paths" `Quick test_heuristic_fallback_paths
        ; test_case "no reserved keywords" `Quick test_no_reserved_keywords_in_heuristic
        ] )
    ; ( "prompt"
      , [ test_case "mentions categories" `Quick test_prompt_mentions_all_categories
        ; test_case
            "explicit fallback required"
            `Quick
            test_classify_hybrid_requires_explicit_fallback
        ] )
    ]
;;
