open Base
open Agent_sdk
open Alcotest

let mk_event seq =
  { Runtime.seq
  ; ts = float_of_int seq
  ; kind = Turn_recorded { actor = Some "agent"; message = Printf.sprintf "turn %d" seq }
  }
;;

let expect_ok label = function
  | Ok value -> value
  | Error _ -> failf "%s failed" label
;;

let test_make_window_sets_cursors () =
  let window =
    Runtime_sync.make_window
      ~stream_id:"session-1"
      ~after_seq:3
      [ mk_event 2; mk_event 7; mk_event 4 ]
  in
  check int "schema" Runtime_sync.schema_version_current window.schema_version;
  check string "stream_id" "session-1" window.stream_id;
  check int "cursor after_seq" 3 window.cursor.after_seq;
  check int "next_cursor after_seq" 7 window.next_cursor.after_seq;
  check int "event count" 2 (List.length window.events)
;;

let test_make_window_empty_keeps_cursor () =
  let window = Runtime_sync.make_window ~stream_id:"session-1" ~after_seq:9 [] in
  check int "next cursor unchanged" 9 window.next_cursor.after_seq;
  check int "event count" 0 (List.length window.events)
;;

let test_json_roundtrip () =
  let persistence =
    Runtime_sync.make_persistence_contract
      ~backend:Runtime_sync.Browser_indexeddb
      ~namespace:"runtime/session-1"
      ~max_window_events:1000
      ()
  in
  let window =
    Runtime_sync.make_window
      ~artifact_refs:[ "artifact://session-1/report.json" ]
      ~persistence
      ~merge_policy:Runtime_sync.Append_only
      ~stream_id:"session-1"
      ~after_seq:0
      [ mk_event 1 ]
  in
  let json = Runtime_sync.to_json window in
  let decoded = Runtime_sync.of_json json |> expect_ok "window decode" in
  check int "schema" window.schema_version decoded.schema_version;
  check string "stream_id" window.stream_id decoded.stream_id;
  check int "cursor" window.cursor.after_seq decoded.cursor.after_seq;
  check int "next cursor" window.next_cursor.after_seq decoded.next_cursor.after_seq;
  check int "events" 1 (List.length decoded.events);
  check (list string) "artifact refs" window.artifact_refs decoded.artifact_refs;
  check bool "persistence present" true (Option.is_some decoded.persistence);
  check
    bool
    "merge policy preserved"
    true
    (decoded.merge_policy = Runtime_sync.Append_only)
;;

let test_custom_envelope_preserved () =
  let envelope =
    Event_envelope.make
      ~event_id:"evt-1"
      ~correlation_id:"corr-1"
      ~run_id:"run-1"
      ~event_time:10.0
      ~observed_at:11.0
      ~seq:42
      ~source_clock:Event_envelope.Logical
      ()
  in
  let record =
    Runtime_sync.make_event_record ~envelope ~stream_id:"session-1" (mk_event 42)
  in
  let decoded =
    Runtime_sync.event_record_to_yojson record
    |> Runtime_sync.event_record_of_yojson
    |> expect_ok "event record decode"
  in
  check string "event_id" "evt-1" decoded.envelope.event_id;
  check (option int) "envelope seq" (Some 42) decoded.envelope.seq;
  check int "event seq" 42 decoded.event.seq
;;

let test_rejects_unsupported_schema () =
  let json =
    Runtime_sync.make_window ~stream_id:"session-1" ~after_seq:0 []
    |> Runtime_sync.to_json
  in
  let bad =
    match json with
    | `Assoc fields ->
      `Assoc (("schema_version", `Int 2) :: List.remove_assoc "schema_version" fields)
    | other -> other
  in
  match Runtime_sync.of_json bad with
  | Ok _ -> fail "expected unsupported schema error"
  | Error detail -> check bool "mentions schema_version" true (String.contains detail '2')
;;

let test_rejects_cursor_stream_mismatch () =
  let json =
    `Assoc
      [ "schema_version", `Int Runtime_sync.schema_version_current
      ; "stream_id", `String "session-1"
      ; "cursor", Runtime_sync.cursor_to_yojson { stream_id = "other"; after_seq = 0 }
      ; ( "next_cursor"
        , Runtime_sync.cursor_to_yojson { stream_id = "session-1"; after_seq = 0 } )
      ; "events", `List []
      ; "artifact_refs", `List []
      ]
  in
  match Runtime_sync.of_json json with
  | Ok _ -> fail "expected cursor stream mismatch"
  | Error detail -> check bool "mentions stream_id" true (String.contains detail '_')
;;

let test_rejects_non_monotonic_events () =
  let event_2 = Runtime_sync.make_event_record ~stream_id:"session-1" (mk_event 2) in
  let event_1 = Runtime_sync.make_event_record ~stream_id:"session-1" (mk_event 1) in
  let window =
    { Runtime_sync.schema_version = Runtime_sync.schema_version_current
    ; stream_id = "session-1"
    ; cursor = { stream_id = "session-1"; after_seq = 0 }
    ; next_cursor = { stream_id = "session-1"; after_seq = 2 }
    ; events = [ event_2; event_1 ]
    ; artifact_refs = []
    ; persistence = None
    ; merge_policy = Runtime_sync.Append_only
    }
  in
  match Runtime_sync.validate_window window with
  | Ok _ -> fail "expected non-monotonic event error"
  | Error detail -> check bool "mentions ordered seq" true (String.contains detail 'q')
;;

let test_rejects_invalid_persistence_contract () =
  let persistence =
    Runtime_sync.make_persistence_contract
      ~backend:Runtime_sync.Browser_indexeddb
      ~namespace:""
      ()
  in
  let window =
    Runtime_sync.make_window ~persistence ~stream_id:"session-1" ~after_seq:0 []
  in
  match Runtime_sync.validate_window window with
  | Ok _ -> fail "expected invalid persistence contract"
  | Error detail -> check bool "mentions namespace" true (String.contains detail 'n')
;;

let test_diff_events_filters_and_sorts () =
  let diff =
    Runtime_sync.diff_events ~after_seq:2 [ mk_event 4; mk_event 1; mk_event 3 ]
  in
  check (list int) "seqs" [ 3; 4 ] (List.map (fun event -> event.Runtime.seq) diff)
;;

let test_merge_offline_events_appends_after_committed_tail () =
  let merged =
    Runtime_sync.merge_offline_events
      ~stream_id:"session-1"
      ~committed:[ mk_event 1; mk_event 2 ]
      ~offline:[ mk_event 10; mk_event 11 ]
    |> expect_ok "offline merge"
  in
  check
    (list int)
    "renumbered seqs"
    [ 1; 2; 3; 4 ]
    (List.map (fun event -> event.Runtime.seq) merged)
;;

let test_merge_offline_events_reports_conflict () =
  match
    Runtime_sync.merge_offline_events
      ~stream_id:"session-1"
      ~committed:[ mk_event 1; mk_event 2 ]
      ~offline:[ mk_event 2; mk_event 3 ]
  with
  | Ok _ -> fail "expected offline merge conflict"
  | Error conflicts ->
    check int "conflict count" 1 (List.length conflicts);
    check int "conflict seq" 2 (List.hd conflicts).Runtime_sync.seq
;;

let () =
  run
    "Runtime_sync"
    [ ( "window"
      , [ test_case "sets cursors" `Quick test_make_window_sets_cursors
        ; test_case "empty window keeps cursor" `Quick test_make_window_empty_keeps_cursor
        ; test_case "json roundtrip" `Quick test_json_roundtrip
        ; test_case "rejects unsupported schema" `Quick test_rejects_unsupported_schema
        ; test_case
            "rejects cursor stream mismatch"
            `Quick
            test_rejects_cursor_stream_mismatch
        ; test_case
            "rejects non-monotonic events"
            `Quick
            test_rejects_non_monotonic_events
        ; test_case
            "rejects invalid persistence contract"
            `Quick
            test_rejects_invalid_persistence_contract
        ] )
    ; ( "event_record"
      , [ test_case "custom envelope preserved" `Quick test_custom_envelope_preserved ] )
    ; ( "delta"
      , [ test_case
            "filters and sorts events after cursor"
            `Quick
            test_diff_events_filters_and_sorts
        ] )
    ; ( "offline_merge"
      , [ test_case
            "appends offline events after committed tail"
            `Quick
            test_merge_offline_events_appends_after_committed_tail
        ; test_case
            "reports committed sequence conflicts"
            `Quick
            test_merge_offline_events_reports_conflict
        ] )
    ]
;;
