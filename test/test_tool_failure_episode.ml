open Agent_sdk

let call id tool_name input : Tool_failure_episode.executed_call =
  { tool_use_id = id; tool_name; input }
;;

let result outcome id content =
  Types.ToolResult
    { tool_use_id = id; content; outcome; json = None; content_blocks = None }
;;

let typed_failure
      ?(failure_kind = Types.Validation_error)
      ?(error_class = Some Types.Deterministic)
      id
      error
  =
  result (Types.Tool_failed { failure_kind; error_class }) id error
;;

let successful_result id content = result Types.Tool_succeeded id content

let project calls results =
  match Tool_failure_episode.project ~executions:calls ~tool_results:results with
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
      [ typed_failure ~failure_kind:Types.Recoverable_tool_error "c1" "second" ]
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
  let success = project [ call "s1" "Execute" `Null ] [ successful_result "s1" "ok" ] in
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
    typed_failure ~failure_kind:Types.Recoverable_tool_error id text
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
      [ successful_result "p1" "ok"; typed_failure "p2" "failed" ]
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

let test_unclassified_failure_is_explicit () =
  match
    Tool_failure_episode.project
      ~executions:[ call "c1" "Execute" `Null ]
      ~tool_results:[ result Types.Legacy_unclassified_failure "c1" "second" ]
  with
  | Error (Tool_failure_episode.Unclassified_failure { tool_use_id = "c1" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected unclassified failure error"
;;

let test_missing_result_is_explicit () =
  match
    Tool_failure_episode.project
      ~executions:[ call "c1" "Execute" `Null ]
      ~tool_results:[]
  with
  | Error (Tool_failure_episode.Missing_tool_result { tool_use_id = "c1" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected missing result error"
;;

let test_unmatched_result_is_explicit () =
  match
    Tool_failure_episode.project
      ~executions:[ call "c1" "Execute" `Null ]
      ~tool_results:[ typed_failure "c1" "matched"; typed_failure "other" "orphan" ]
  with
  | Error (Tool_failure_episode.Unmatched_tool_result { tool_use_id = "other" }) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected unmatched result error"
;;

let test_completed_round_metadata_preserves_canonical_execution () =
  let input = `Assoc [ "command", `String "git status"; "cwd", `String "/repo" ] in
  let tool_result = typed_failure "c1" "failed" in
  let message =
    Types.make_message
      ~metadata:
        [ Tool_failure_episode.completed_round_metadata [ call "c1" "Execute" input ] ]
      ~role:Types.Tool
      [ tool_result ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok [ restored ] ->
    (match detect restored restored with
     | Ok [ episode ] ->
       Alcotest.(check string) "canonical tool" "Execute" episode.current.tool_name;
       Alcotest.(check bool)
         "canonical input"
         true
         (Yojson.Safe.equal input episode.current.input)
     | Ok episodes ->
       Alcotest.failf "expected one restored episode, got %d" (List.length episodes)
     | Error error -> Alcotest.fail (Tool_failure_episode.show_error error))
  | Ok rounds -> Alcotest.failf "expected one restored round, got %d" (List.length rounds)
;;

let test_duplicate_completed_round_metadata_is_explicit () =
  let tool_result = typed_failure "c1" "failed" in
  let metadata =
    Tool_failure_episode.completed_round_metadata [ call "c1" "Execute" `Null ]
  in
  let message =
    Types.make_message ~metadata:[ metadata; metadata ] ~role:Types.Tool [ tool_result ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error Tool_failure_episode.Duplicate_completed_round_metadata -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected duplicate completed-round metadata error"
;;

let test_missing_completed_round_metadata_is_explicit () =
  let tool_result = typed_failure "c1" "failed" in
  let message = Types.make_message ~role:Types.Tool [ tool_result ] in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error
      (Tool_failure_episode.Missing_completed_round_metadata { tool_use_ids = [ "c1" ] })
    -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected missing completed-round metadata error"
;;

let test_invalid_completed_round_metadata_is_explicit () =
  let key, _ =
    Tool_failure_episode.completed_round_metadata [ call "c1" "Execute" `Null ]
  in
  let message =
    Types.make_message
      ~metadata:[ key, `Assoc [] ]
      ~role:Types.Tool
      [ typed_failure "c1" "failed" ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error (Tool_failure_episode.Invalid_completed_round_metadata _) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected invalid completed-round metadata error"
;;

let test_unexpected_execution_metadata_field_is_explicit () =
  let key, json =
    Tool_failure_episode.completed_round_metadata [ call "c1" "Execute" `Null ]
  in
  let json =
    match json with
    | `List [ `Assoc fields ] -> `List [ `Assoc (("unexpected", `Bool true) :: fields) ]
    | _ -> Alcotest.fail "expected one executed-call metadata object"
  in
  let message =
    Types.make_message
      ~metadata:[ key, json ]
      ~role:Types.Tool
      [ typed_failure "c1" "failed" ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error (Tool_failure_episode.Invalid_completed_round_metadata _) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected unexpected executed-call field error"
;;

let test_duplicate_execution_metadata_field_is_explicit () =
  let key, json =
    Tool_failure_episode.completed_round_metadata [ call "c1" "Execute" `Null ]
  in
  let json =
    match json with
    | `List [ `Assoc fields ] ->
      `List [ `Assoc (("tool_use_id", `String "duplicate") :: fields) ]
    | _ -> Alcotest.fail "expected one executed-call metadata object"
  in
  let message =
    Types.make_message
      ~metadata:[ key, json ]
      ~role:Types.Tool
      [ typed_failure "c1" "failed" ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error (Tool_failure_episode.Invalid_completed_round_metadata _) -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected duplicate executed-call field error"
;;

let test_tool_result_outcome_is_restore_ssot () =
  let execution = call "c1" "Execute" (`Assoc [ "cmd", `String "status" ]) in
  let metadata = Tool_failure_episode.completed_round_metadata [ execution ] in
  let restored_with result =
    let message = Types.make_message ~metadata:[ metadata ] ~role:Types.Tool [ result ] in
    match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
    | Ok [ round ] -> round
    | Ok rounds ->
      Alcotest.failf "expected one restored round, got %d" (List.length rounds)
    | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  in
  let failed = restored_with (typed_failure "c1" "failed") in
  let succeeded = restored_with (successful_result "c1" "ok") in
  (match detect failed failed with
   | Ok [ _ ] -> ()
   | Ok episodes ->
     Alcotest.failf "expected one failed episode, got %d" (List.length episodes)
   | Error error -> Alcotest.fail (Tool_failure_episode.show_error error));
  match detect succeeded succeeded with
  | Ok [] -> ()
  | Ok episodes ->
    Alcotest.failf
      "metadata overrode successful outcome (%d episodes)"
      (List.length episodes)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let test_invalid_run_boundary_is_explicit () =
  let key, _ = Types.Conversation_metadata.run_boundary_entry in
  let boundary =
    Types.make_message
      ~metadata:[ key, `Bool false ]
      ~role:Types.User
      [ Types.Text "new" ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ boundary ] with
  | Error Tool_failure_episode.Invalid_run_boundary_metadata -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected invalid run-boundary metadata error"
;;

let test_duplicate_run_boundary_is_explicit () =
  let boundary = Types.Conversation_metadata.run_boundary_entry in
  let message =
    Types.make_message
      ~metadata:[ boundary; boundary ]
      ~role:Types.User
      [ Types.Text "new" ]
  in
  match Tool_failure_episode.latest_completed_rounds ~count:1 [ message ] with
  | Error Tool_failure_episode.Duplicate_run_boundary_metadata -> ()
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok _ -> Alcotest.fail "expected duplicate run-boundary metadata error"
;;

let test_run_boundary_prevents_cross_request_pairing () =
  let old_result = typed_failure "old" "old failure" in
  let old_message =
    Types.make_message
      ~metadata:
        [ Tool_failure_episode.completed_round_metadata [ call "old" "Execute" `Null ] ]
      ~role:Types.Tool
      [ old_result ]
  in
  let boundary =
    Types.make_message
      ~metadata:Types.Conversation_metadata.run_boundary
      ~role:Types.User
      [ Types.Text "new request" ]
  in
  let current_result = typed_failure "current" "current failure" in
  let current_message =
    Types.make_message
      ~metadata:
        [ Tool_failure_episode.completed_round_metadata
            [ call "current" "Execute" (`String "new") ]
        ]
      ~role:Types.Tool
      [ current_result ]
  in
  match
    Tool_failure_episode.latest_completed_rounds
      ~count:2
      [ old_message; boundary; current_message ]
  with
  | Ok [ _ ] -> ()
  | Ok rounds ->
    Alcotest.failf "expected only the current request round, got %d" (List.length rounds)
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
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
            "unclassified failure"
            `Quick
            test_unclassified_failure_is_explicit
        ; Alcotest.test_case "missing result" `Quick test_missing_result_is_explicit
        ; Alcotest.test_case "unmatched result" `Quick test_unmatched_result_is_explicit
        ] )
    ; ( "durability"
      , [ Alcotest.test_case
            "canonical execution metadata"
            `Quick
            test_completed_round_metadata_preserves_canonical_execution
        ; Alcotest.test_case
            "duplicate metadata"
            `Quick
            test_duplicate_completed_round_metadata_is_explicit
        ; Alcotest.test_case
            "missing metadata"
            `Quick
            test_missing_completed_round_metadata_is_explicit
        ; Alcotest.test_case
            "invalid metadata"
            `Quick
            test_invalid_completed_round_metadata_is_explicit
        ; Alcotest.test_case
            "unexpected execution metadata field"
            `Quick
            test_unexpected_execution_metadata_field_is_explicit
        ; Alcotest.test_case
            "duplicate execution metadata field"
            `Quick
            test_duplicate_execution_metadata_field_is_explicit
        ; Alcotest.test_case
            "tool result outcome SSOT"
            `Quick
            test_tool_result_outcome_is_restore_ssot
        ; Alcotest.test_case
            "run boundary"
            `Quick
            test_run_boundary_prevents_cross_request_pairing
        ; Alcotest.test_case
            "invalid run boundary"
            `Quick
            test_invalid_run_boundary_is_explicit
        ; Alcotest.test_case
            "duplicate run boundary"
            `Quick
            test_duplicate_run_boundary_is_explicit
        ] )
    ]
;;
