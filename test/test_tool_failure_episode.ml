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

let typed_failure ?(error_class = Some Types.Deterministic) id error =
  result ~failure_kind:Types.Validation_error ?error_class id error
;;

let project calls results =
  match Tool_failure_episode.project ~tool_uses:calls ~tool_results:results with
  | Ok round -> round
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let detect previous current = Tool_failure_episode.detect ~previous ~current

let test_changed_input_and_error_text_detected () =
  let previous =
    project
      [ call "p1" "Execute" (`Assoc [ "cmd", `String "gh pr list" ]) ]
      [ typed_failure "p1" "working directory missing" ]
  in
  let current =
    project
      [ call
          "c1"
          "Execute"
          (`Assoc [ "cmd", `String "gh pr list"; "cwd", `String "/repo" ])
      ]
      [ typed_failure "c1" "repository was not found" ]
  in
  match detect previous current with
  | Ok [ episode ] ->
    Alcotest.(check string) "previous id" "p1" episode.previous.tool_use_id;
    Alcotest.(check string) "current id" "c1" episode.current.tool_use_id;
    Alcotest.(check bool)
      "input changed"
      false
      (Yojson.Safe.equal episode.previous.input episode.current.input)
  | Ok episodes -> Alcotest.failf "expected one episode, got %d" (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let test_different_failure_kind_not_detected () =
  let previous = project [ call "p1" "Execute" `Null ] [ typed_failure "p1" "first" ] in
  let current =
    project
      [ call "c1" "Execute" `Null ]
      [ result
          ~failure_kind:Types.Recoverable_tool_error
          ~error_class:Types.Deterministic
          "c1"
          "second"
      ]
  in
  Alcotest.(check bool) "no episode" true (detect previous current = Ok [])
;;

let test_different_error_class_not_detected () =
  let previous = project [ call "p1" "Execute" `Null ] [ typed_failure "p1" "first" ] in
  let current =
    project
      [ call "c1" "Execute" `Null ]
      [ typed_failure ~error_class:(Some Types.Transient) "c1" "second" ]
  in
  Alcotest.(check bool) "no episode" true (detect previous current = Ok [])
;;

let test_successful_intervening_round_breaks_adjacency () =
  let success =
    project [ call "s1" "Execute" `Null ] [ result ~is_error:false "s1" "ok" ]
  in
  let failed = project [ call "c1" "Execute" `Null ] [ typed_failure "c1" "second" ] in
  Alcotest.(check bool)
    "caller advances the adjacent boundary"
    true
    (detect success failed = Ok [])
;;

let test_parallel_failures_preserved () =
  let previous =
    project
      [ call "p1" "Execute" `Null; call "p2" "Read" `Null ]
      [ typed_failure "p1" "first-a"; typed_failure "p2" "first-b" ]
  in
  let current =
    project
      [ call "c1" "Execute" (`String "changed"); call "c2" "Read" `Null ]
      [ typed_failure "c1" "second-a"; typed_failure "c2" "second-b" ]
  in
  match detect previous current with
  | Ok episodes -> Alcotest.(check int) "both episodes" 2 (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let test_same_name_distinct_signatures_are_independent () =
  let recoverable id text =
    result
      ~failure_kind:Types.Recoverable_tool_error
      ~error_class:Types.Deterministic
      id
      text
  in
  let previous =
    project
      [ call "p1" "Execute" `Null; call "p2" "Execute" (`String "other") ]
      [ typed_failure "p1" "validation"; recoverable "p2" "recoverable" ]
  in
  let current =
    project
      [ call "c1" "Execute" `Null; call "c2" "Execute" (`String "changed") ]
      [ typed_failure "c1" "validation changed"; recoverable "c2" "recoverable changed" ]
  in
  match detect previous current with
  | Ok episodes -> Alcotest.(check int) "two signatures" 2 (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let test_success_and_matching_failure_same_name_are_unambiguous () =
  let previous =
    project
      [ call "p1" "Execute" `Null; call "p2" "Execute" (`String "other") ]
      [ result ~is_error:false "p1" "ok"; typed_failure "p2" "failed" ]
  in
  let current = project [ call "c1" "Execute" `Null ] [ typed_failure "c1" "again" ] in
  match detect previous current with
  | Ok [ _ ] -> ()
  | Ok episodes -> Alcotest.failf "expected one episode, got %d" (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let test_duplicate_failure_signature_is_explicit () =
  let previous =
    project
      [ call "p1" "Execute" `Null; call "p2" "Execute" (`String "other") ]
      [ typed_failure "p1" "first-a"; typed_failure "p2" "first-b" ]
  in
  let current = project [ call "c1" "Execute" `Null ] [ typed_failure "c1" "second" ] in
  match detect previous current with
  | Error
      (Tool_failure_episode.Ambiguous_failure_signature
         { previous_count = 2; current_count = 1; _ }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected typed ambiguity"
;;

let test_missing_failure_kind_is_explicit () =
  match
    Tool_failure_episode.project
      ~tool_uses:[ call "c1" "Execute" `Null ]
      ~tool_results:[ result "c1" "second" ]
  with
  | Error (Tool_failure_episode.Failure_kind_missing { tool_use_id = "c1" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected incomplete metadata error"
;;

let test_missing_result_is_explicit () =
  match
    Tool_failure_episode.project ~tool_uses:[ call "c1" "Execute" `Null ] ~tool_results:[]
  with
  | Error (Tool_failure_episode.Missing_tool_result { tool_use_id = "c1" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected missing result error"
;;

let test_unmatched_result_is_explicit () =
  match
    Tool_failure_episode.project
      ~tool_uses:[ call "c1" "Execute" `Null ]
      ~tool_results:[ typed_failure "c1" "matched"; typed_failure "other" "orphan" ]
  with
  | Error (Tool_failure_episode.Unmatched_tool_result { tool_use_id = "other" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected unmatched result error"
;;

let () =
  Alcotest.run
    "tool_failure_episode"
    [ ( "completed_rounds"
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
        ; Alcotest.test_case
            "parallel failures preserved"
            `Quick
            test_parallel_failures_preserved
        ; Alcotest.test_case
            "same name distinct signatures"
            `Quick
            test_same_name_distinct_signatures_are_independent
        ; Alcotest.test_case
            "success plus matching failure"
            `Quick
            test_success_and_matching_failure_same_name_are_unambiguous
        ; Alcotest.test_case
            "duplicate failure signature"
            `Quick
            test_duplicate_failure_signature_is_explicit
        ; Alcotest.test_case
            "missing failure kind"
            `Quick
            test_missing_failure_kind_is_explicit
        ; Alcotest.test_case "missing result" `Quick test_missing_result_is_explicit
        ; Alcotest.test_case "unmatched result" `Quick test_unmatched_result_is_explicit
        ] )
    ]
;;
