open Agent_sdk

let call id name input = Types.ToolUse { id; name; input }

let result ?failure_kind ?error_class ?(is_error = true) id error =
  Types.ToolResult
    { tool_use_id = id
    ; content = error
    ; is_error
    ; failure_kind
    ; error_class
    ; json = None
    ; content_blocks = None
    }
;;

let message role content : Types.message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

let round call result =
  [ message Types.Assistant [ call ]; message Types.Tool [ result ] ]
;;

let typed_failure ?(error_class = Some Types.Deterministic) id error =
  result ~failure_kind:Types.Validation_error ?error_class id error
;;

let detect rounds = Tool_failure_episode.detect_latest (List.concat rounds)

let test_changed_input_and_error_text_detected () =
  let previous =
    round
      (call "p1" "Execute" (`Assoc [ "cmd", `String "gh pr list" ]))
      (typed_failure "p1" "working directory missing")
  in
  let current =
    round
      (call
         "c1"
         "Execute"
         (`Assoc [ "cmd", `String "gh pr list"; "cwd", `String "/repo" ]))
      (typed_failure "c1" "repository was not found")
  in
  match detect [ previous; current ] with
  | Ok [ episode ] ->
    Alcotest.(check string) "previous id" "p1" episode.previous.tool_use_id;
    Alcotest.(check string) "current id" "c1" episode.current.tool_use_id;
    Alcotest.(check bool)
      "input changed"
      false
      (Yojson.Safe.equal episode.previous.input episode.current.input)
  | Ok episodes -> Alcotest.failf "expected one episode, got %d" (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_history_error error)
;;

let test_different_failure_kind_not_detected () =
  let previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let current =
    round
      (call "c1" "Execute" `Null)
      (result
         ~failure_kind:Types.Recoverable_tool_error
         ~error_class:Types.Deterministic
         "c1"
         "second")
  in
  Alcotest.(check bool) "no episode" true (detect [ previous; current ] = Ok [])
;;

let test_different_error_class_not_detected () =
  let previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let current =
    round
      (call "c1" "Execute" `Null)
      (typed_failure ~error_class:(Some Types.Transient) "c1" "second")
  in
  Alcotest.(check bool) "no episode" true (detect [ previous; current ] = Ok [])
;;

let test_successful_intervening_round_breaks_adjacency () =
  let failed_previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let success = round (call "s1" "Execute" `Null) (result ~is_error:false "s1" "ok") in
  let failed_current = round (call "c1" "Execute" `Null) (typed_failure "c1" "second") in
  Alcotest.(check bool)
    "only adjacent rounds compared"
    true
    (detect [ failed_previous; success; failed_current ] = Ok [])
;;

let test_different_tool_not_detected () =
  let previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let current = round (call "c1" "Read" `Null) (typed_failure "c1" "second") in
  Alcotest.(check bool) "no episode" true (detect [ previous; current ] = Ok [])
;;

let test_parallel_failures_preserved () =
  let previous =
    [ message Types.Assistant [ call "p1" "Execute" `Null; call "p2" "Read" `Null ]
    ; message Types.Tool [ typed_failure "p1" "first-a"; typed_failure "p2" "first-b" ]
    ]
  in
  let current =
    [ message
        Types.Assistant
        [ call "c1" "Execute" (`String "changed"); call "c2" "Read" `Null ]
    ; message Types.Tool [ typed_failure "c1" "second-a"; typed_failure "c2" "second-b" ]
    ]
  in
  match detect [ previous; current ] with
  | Ok episodes -> Alcotest.(check int) "both episodes" 2 (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_history_error error)
;;

let test_ambiguous_previous_name_is_explicit () =
  let previous =
    [ message
        Types.Assistant
        [ call "p1" "Execute" `Null; call "p2" "Execute" (`String "other") ]
    ; message Types.Tool [ typed_failure "p1" "first-a"; typed_failure "p2" "first-b" ]
    ]
  in
  let current = round (call "c1" "Execute" `Null) (typed_failure "c1" "second") in
  match detect [ previous; current ] with
  | Error
      (Tool_failure_episode.Ambiguous_tool_name
         { position = Tool_failure_episode.Previous; _ }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_history_error error)
  | Ok _ -> Alcotest.fail "expected typed ambiguity"
;;

let test_incomplete_failure_metadata_is_explicit () =
  let previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let current =
    round
      (call "c1" "Execute" `Null)
      (result ~error_class:Types.Deterministic "c1" "second")
  in
  match detect [ previous; current ] with
  | Error (Tool_failure_episode.Failure_kind_missing { tool_use_id = "c1"; _ }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_history_error error)
  | Ok _ -> Alcotest.fail "expected incomplete metadata error"
;;

let test_unmatched_result_is_explicit () =
  let previous = round (call "p1" "Execute" `Null) (typed_failure "p1" "first") in
  let current =
    [ message Types.Assistant [ call "c1" "Execute" `Null ]
    ; message Types.Tool [ typed_failure "other" "second" ]
    ]
  in
  match detect [ previous; current ] with
  | Error (Tool_failure_episode.Missing_tool_result { tool_use_id = "c1"; _ }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_history_error error)
  | Ok _ -> Alcotest.fail "expected unmatched result error"
;;

let () =
  Alcotest.run
    "tool_failure_episode"
    [ ( "detect_latest"
      , [ Alcotest.test_case
            "changed input and error text"
            `Quick
            test_changed_input_and_error_text_detected
        ; Alcotest.test_case
            "different failure kind"
            `Quick
            test_different_failure_kind_not_detected
        ; Alcotest.test_case
            "different error class"
            `Quick
            test_different_error_class_not_detected
        ; Alcotest.test_case
            "successful intervening round"
            `Quick
            test_successful_intervening_round_breaks_adjacency
        ; Alcotest.test_case "different tool" `Quick test_different_tool_not_detected
        ; Alcotest.test_case
            "parallel failures preserved"
            `Quick
            test_parallel_failures_preserved
        ; Alcotest.test_case
            "ambiguous previous name"
            `Quick
            test_ambiguous_previous_name_is_explicit
        ; Alcotest.test_case
            "incomplete failure metadata"
            `Quick
            test_incomplete_failure_metadata_is_explicit
        ; Alcotest.test_case "unmatched result" `Quick test_unmatched_result_is_explicit
        ] )
    ]
;;
