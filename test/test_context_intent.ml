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
    ; ( "prompt"
      , [ test_case "mentions categories" `Quick test_prompt_mentions_all_categories
        ; test_case
            "explicit fallback required"
            `Quick
            test_classify_hybrid_requires_explicit_fallback
        ] )
    ]
;;
