(** Unit tests for Cognitive_event (RFC-0036 PR-B,
    Master Report transport schema). *)

open Agent_sdk.Cognitive_event

let test_name_is_stable () =
  Alcotest.(check string)
    "gravity_ranked label" "gravity_ranked"
    (name (Gravity_ranked { ranked_count = 0; query_terms = 0 }));
  Alcotest.(check string)
    "intent_predicted label" "intent_predicted"
    (name (Intent_predicted { intent_label = "x"; confidence = 0.5 }));
  Alcotest.(check string)
    "mode_transitioned label" "mode_transitioned"
    (name (Mode_transitioned { from_mode = "a"; to_mode = "b" }));
  Alcotest.(check string)
    "disclosure_level label" "disclosure_level"
    (name (Disclosure_level { level = 0 }))

let test_well_formed_accepts_valid () =
  let cases =
    [ Gravity_ranked { ranked_count = 0; query_terms = 0 }
    ; Gravity_ranked { ranked_count = 5; query_terms = 3 }
    ; Intent_predicted { intent_label = "task_command"; confidence = 0.95 }
    ; Intent_predicted { intent_label = "x"; confidence = 0.0 }
    ; Intent_predicted { intent_label = "x"; confidence = 1.0 }
    ; Mode_transitioned { from_mode = "focus"; to_mode = "scan" }
    ; Disclosure_level { level = 0 }
    ; Disclosure_level { level = 3 }
    ]
  in
  List.iter
    (fun ev ->
      match is_well_formed ev with
      | Ok () -> ()
      | Error msg ->
        Alcotest.failf "expected %s to be well-formed but got: %s"
          (name ev) msg)
    cases

let check_rejects label result =
  match result with
  | Ok () -> Alcotest.failf "%s should have been rejected" label
  | Error _ -> ()

let test_well_formed_rejects_invalid () =
  check_rejects "negative ranked_count"
    (is_well_formed (Gravity_ranked { ranked_count = -1; query_terms = 0 }));
  check_rejects "negative query_terms"
    (is_well_formed (Gravity_ranked { ranked_count = 0; query_terms = -1 }));
  check_rejects "empty intent_label"
    (is_well_formed (Intent_predicted { intent_label = ""; confidence = 0.5 }));
  check_rejects "confidence below 0"
    (is_well_formed
       (Intent_predicted { intent_label = "x"; confidence = -0.1 }));
  check_rejects "confidence above 1"
    (is_well_formed
       (Intent_predicted { intent_label = "x"; confidence = 1.01 }));
  check_rejects "confidence NaN"
    (is_well_formed
       (Intent_predicted { intent_label = "x"; confidence = Float.nan }));
  check_rejects "confidence +inf"
    (is_well_formed
       (Intent_predicted { intent_label = "x"; confidence = Float.infinity }));
  check_rejects "empty from_mode"
    (is_well_formed (Mode_transitioned { from_mode = ""; to_mode = "b" }));
  check_rejects "empty to_mode"
    (is_well_formed (Mode_transitioned { from_mode = "a"; to_mode = "" }));
  check_rejects "from_mode = to_mode"
    (is_well_formed (Mode_transitioned { from_mode = "a"; to_mode = "a" }));
  check_rejects "level below 0"
    (is_well_formed (Disclosure_level { level = -1 }));
  check_rejects "level above 3"
    (is_well_formed (Disclosure_level { level = 4 }))

let test_yojson_roundtrip () =
  let cases =
    [ Gravity_ranked { ranked_count = 7; query_terms = 2 }
    ; Intent_predicted
        { intent_label = "knowledge_query"; confidence = 0.84 }
    ; Mode_transitioned { from_mode = "focus"; to_mode = "scan" }
    ; Disclosure_level { level = 2 }
    ]
  in
  List.iter
    (fun ev ->
      let json = to_yojson ev in
      match of_yojson json with
      | Ok ev' ->
        Alcotest.(check string)
          (Printf.sprintf "roundtrip preserves name (%s)" (name ev))
          (name ev) (name ev');
        Alcotest.(check string)
          (Printf.sprintf "roundtrip preserves show (%s)" (name ev))
          (show ev) (show ev')
      | Error msg ->
        Alcotest.failf "yojson roundtrip failed for %s: %s" (name ev) msg)
    cases

let test_yojson_rejects_garbage () =
  let bad = Yojson.Safe.from_string {|{"unexpected":42}|} in
  match of_yojson bad with
  | Ok _ -> Alcotest.fail "of_yojson should have rejected unexpected JSON"
  | Error _ -> ()

let () =
  Alcotest.run "cognitive_event"
    [
      ( "labels",
        [ Alcotest.test_case "name is stable" `Quick test_name_is_stable ] );
      ( "well_formed",
        [
          Alcotest.test_case "accepts valid" `Quick
            test_well_formed_accepts_valid;
          Alcotest.test_case "rejects invalid" `Quick
            test_well_formed_rejects_invalid;
        ] );
      ( "yojson",
        [
          Alcotest.test_case "roundtrip" `Quick test_yojson_roundtrip;
          Alcotest.test_case "rejects garbage" `Quick
            test_yojson_rejects_garbage;
        ] );
    ]
