open Base
open Agent_sdk

let dummy_session : Runtime.session =
  { session_id = "s1"
  ; goal = "test"
  ; title = None
  ; tag = None
  ; permission_mode = None
  ; phase = Runtime.Running
  ; created_at = 0.0
  ; updated_at = 0.0
  ; provider = None
  ; model = None
  ; system_prompt = None
  ; max_turns = 10
  ; workdir = None
  ; planned_participants = []
  ; participants = []
  ; artifacts = []
  ; turn_count = 0
  ; last_seq = 5
  ; outcome = None
  }
;;

let test_extract_text_empty_content () =
  let resp =
    { Types.id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  Alcotest.(check string) "empty" "" (Runtime_server_worker.extract_text resp)
;;

let test_extract_text_single_text_block () =
  let resp =
    { Types.id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text "hello" ]
    ; usage = None
    ; telemetry = None
    }
  in
  Alcotest.(check string) "single" "hello" (Runtime_server_worker.extract_text resp)
;;

let test_extract_text_multiple_text_blocks () =
  let resp =
    { Types.id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text "hello"; Types.Text "world" ]
    ; usage = None
    ; telemetry = None
    }
  in
  Alcotest.(check string)
    "joined"
    "hello\nworld"
    (Runtime_server_worker.extract_text resp)
;;

let test_extract_text_filters_non_text_blocks () =
  let resp =
    { Types.id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content =
        [ Types.Text "before"
        ; Types.ToolUse { id = "t1"; name = "fn"; input = `Null }
        ; Types.Text "after"
        ]
    ; usage = None
    ; telemetry = None
    }
  in
  Alcotest.(check string)
    "filtered"
    "before\nafter"
    (Runtime_server_worker.extract_text resp)
;;

let test_extract_text_only_non_text_blocks () =
  let resp =
    { Types.id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content =
        [ Types.ToolUse { id = "t1"; name = "fn"; input = `Null }
        ; Types.Thinking { thinking_type = "thinking"; content = "hmm" }
        ]
    ; usage = None
    ; telemetry = None
    }
  in
  Alcotest.(check string) "empty" "" (Runtime_server_worker.extract_text resp)
;;

let test_make_event_seq_is_incremented () =
  let event =
    Runtime_server_worker.make_event
      dummy_session
      (Runtime.Session_started { goal = "test"; participants = [] })
  in
  Alcotest.(check int) "seq" 6 event.seq
;;

let test_make_event_ts_is_positive () =
  let event =
    Runtime_server_worker.make_event
      dummy_session
      (Runtime.Session_started { goal = "test"; participants = [] })
  in
  Alcotest.(check bool) "positive timestamp" true (event.ts > 0.0)
;;

let test_make_event_kind_is_preserved () =
  let kind = Runtime.Turn_recorded { actor = Some "alice"; message = "hi" } in
  let event = Runtime_server_worker.make_event dummy_session kind in
  Alcotest.(check bool) "kind preserved" true (event.kind = kind)
;;

let () =
  Alcotest.run
    "Runtime_server_worker"
    [ ( "extract_text"
      , [ Alcotest.test_case "empty content" `Quick test_extract_text_empty_content
        ; Alcotest.test_case
            "single Text block"
            `Quick
            test_extract_text_single_text_block
        ; Alcotest.test_case
            "multiple Text blocks joined by newline"
            `Quick
            test_extract_text_multiple_text_blocks
        ; Alcotest.test_case
            "non-Text blocks filtered out"
            `Quick
            test_extract_text_filters_non_text_blocks
        ; Alcotest.test_case
            "only non-Text blocks returns empty"
            `Quick
            test_extract_text_only_non_text_blocks
        ] )
    ; ( "make_event"
      , [ Alcotest.test_case
            "seq is last_seq + 1"
            `Quick
            test_make_event_seq_is_incremented
        ; Alcotest.test_case "ts is positive" `Quick test_make_event_ts_is_positive
        ; Alcotest.test_case "kind is preserved" `Quick test_make_event_kind_is_preserved
        ] )
    ]
;;
