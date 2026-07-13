(** Tests for Harness_case and Harness_dataset. *)

open Agent_sdk
open Alcotest
open Harness_case

let mk_record
      ~seq
      ~ts
      ~record_type
      ?prompt
      ?tool_use_id
      ?tool_name
      ?tool_input
      ?tool_execution_mode
      ?tool_result
      ?tool_error
      ?final_text
      ?error
      ()
  : Raw_trace.record
  =
  { trace_version = Raw_trace.trace_version
  ; worker_run_id = "wr-hcase"
  ; seq
  ; ts
  ; agent_name = "trace-agent"
  ; session_id = None
  ; record_type
  ; prompt
  ; model = None
  ; tool_choice = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; block_index = None
  ; block_kind = None
  ; assistant_block = None
  ; tool_use_id
  ; tool_name
  ; tool_input
  ; tool_planned_index = None
  ; tool_batch_index = None
  ; tool_batch_size = None
  ; tool_execution_mode
  ; tool_result
  ; tool_error
  ; hook_name = None
  ; hook_decision = None
  ; hook_detail = None
  ; final_text
  ; stop_reason = Some "end_turn"
  ; error
  }
;;

let test_dataset_roundtrip () =
  let case_ =
    Harness_case.make_fixture
      ~assertions:[ Harness_case.Response (Harness_case.Exact_text "hello") ]
      ~id:"fixture-1"
      ~prompt:"Say hello"
      ()
  in
  let path = Filename.temp_file "oas-harness-dataset" ".jsonl" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       (match Harness_dataset.save ~path [ case_ ] with
        | Ok () -> ()
        | Error e -> fail (Error.to_string e));
       match Harness_dataset.load ~path with
       | Error e -> fail (Error.to_string e)
       | Ok [ loaded ] ->
         check string "id" "fixture-1" loaded.id;
         check string "prompt" "Say hello" loaded.prompt
       | Ok _ -> fail "expected exactly one case")
;;

let test_trace_replay_of_records () =
  let records =
    [ mk_record ~seq:1 ~ts:1.0 ~record_type:Run_started ~prompt:"Replay me" ()
    ; mk_record ~seq:2 ~ts:2.0 ~record_type:Run_finished ~final_text:"done" ()
    ]
  in
  match
    Harness_case.trace_replay_of_records
      ~id:"trace-1"
      ~source_trace_path:"/tmp/trace-1.ndjson"
      records
  with
  | Error e -> fail (Error.to_string e)
  | Ok case_ ->
    check string "prompt" "Replay me" case_.prompt;
    check bool "is trace replay" true (case_.kind = Harness_case.Trace_replay);
    check bool "has assertions" true (List.length case_.assertions >= 2)
;;

let expect_case_ok json =
  match Harness_case.of_json json with
  | Ok case_ -> case_
  | Error e -> fail (Error.to_string e)
;;

let expect_case_error json =
  match Harness_case.of_json json with
  | Ok _ -> fail "expected Harness_case.of_json to fail"
  | Error _ -> ()
;;

let test_json_roundtrip_all_assertions () =
  let case_ =
    Harness_case.make_fixture
      ~tags:[ "smoke"; "json" ]
      ~artifacts:[ "/tmp/a.json" ]
      ~assertions:
        [ Response (Exact_text "done")
        ; Response (Contains_text "on")
        ; Response (Structural_json (`Assoc [ "ok", `Bool true ]))
        ; Response (Fuzzy_text { expected = "roughly done"; threshold = 0.8 })
        ; Trace Succeeds
        ; Trace (Tool_called "lookup")
        ; Trace (Tool_sequence [ "lookup"; "summarize" ])
        ; Trace (Tool_call_count 2)
        ; Metric
            { name = "accuracy"
            ; goal = Eval.Higher
            ; target = Eval.Float_val 0.9
            ; tolerance_pct = Some 2.5
            }
        ]
      ~id:"fixture-json"
      ~prompt:"Run assertions"
      ()
  in
  let parsed = expect_case_ok (Harness_case.to_json case_) in
  check string "id" case_.id parsed.id;
  check string "prompt" case_.prompt parsed.prompt;
  check (list string) "tags" case_.tags parsed.tags;
  check (list string) "artifacts" case_.artifacts parsed.artifacts;
  check int "assertions" (List.length case_.assertions) (List.length parsed.assertions)
;;

let test_of_json_accepts_null_optional_lists () =
  let json =
    `Assoc
      [ "id", `String "fixture-null"
      ; "kind", `String "fixture"
      ; "prompt", `String "No optional lists"
      ; "tags", `Null
      ; "assertions", `Null
      ; "artifacts", `Null
      ; "source_trace_path", `Null
      ]
  in
  let parsed = expect_case_ok json in
  check (list string) "tags default" [] parsed.tags;
  check int "assertions default" 0 (List.length parsed.assertions);
  check (list string) "artifacts default" [] parsed.artifacts;
  check (option string) "source trace" None parsed.source_trace_path
;;

let test_of_json_rejects_unknown_kind () =
  expect_case_error
    (`Assoc
        [ "id", `String "bad-kind"
        ; "kind", `String "future"
        ; "prompt", `String "x"
        ; "tags", `List []
        ; "assertions", `List []
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ])
;;

let test_of_json_rejects_bad_tags () =
  expect_case_error
    (`Assoc
        [ "id", `String "bad-tags"
        ; "kind", `String "fixture"
        ; "prompt", `String "x"
        ; "tags", `List [ `String "ok"; `Int 1 ]
        ; "assertions", `List []
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ])
;;

let test_of_json_rejects_unknown_assertions () =
  List.iter
    expect_case_error
    [ `Assoc
        [ "id", `String "bad-response"
        ; "kind", `String "fixture"
        ; "prompt", `String "x"
        ; "tags", `List []
        ; ( "assertions"
          , `List [ `Assoc [ "type", `String "response_future"; "value", `String "x" ] ] )
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ]
    ; `Assoc
        [ "id", `String "bad-trace"
        ; "kind", `String "fixture"
        ; "prompt", `String "x"
        ; "tags", `List []
        ; ( "assertions"
          , `List [ `Assoc [ "type", `String "trace_tool_sequence"; "value", `Int 1 ] ] )
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ]
    ; `Assoc
        [ "id", `String "bad-metric-goal"
        ; "kind", `String "fixture"
        ; "prompt", `String "x"
        ; "tags", `List []
        ; ( "assertions"
          , `List
              [ `Assoc
                  [ "type", `String "metric"
                  ; "name", `String "latency"
                  ; "goal", `String "sideways"
                  ; "target", `Int 1
                  ; "tolerance_pct", `Null
                  ]
              ] )
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ]
    ; `Assoc
        [ "id", `String "bad-assertion"
        ; "kind", `String "fixture"
        ; "prompt", `String "x"
        ; "tags", `List []
        ; "assertions", `List [ `Assoc [ "type", `String "future_assertion" ] ]
        ; "artifacts", `List []
        ; "source_trace_path", `Null
        ]
    ]
;;

let test_make_trace_replay_adds_source_metadata () =
  let case_ =
    Harness_case.make_trace_replay
      ~tags:[ "nightly" ]
      ~artifacts:[ "/tmp/extra.json" ]
      ~id:"trace-case"
      ~prompt:"Replay"
      ~source_trace_path:"/tmp/raw.jsonl"
      ()
  in
  check bool "trace replay" true (case_.kind = Harness_case.Trace_replay);
  check (list string) "tags" [ "trace-replay"; "nightly" ] case_.tags;
  check (list string) "artifacts" [ "/tmp/raw.jsonl"; "/tmp/extra.json" ] case_.artifacts;
  check (option string) "source" (Some "/tmp/raw.jsonl") case_.source_trace_path
;;

let test_trace_replay_requires_prompt () =
  match
    Harness_case.trace_replay_of_records
      ~id:"trace-missing-prompt"
      ~source_trace_path:"/tmp/trace.ndjson"
      [ mk_record ~seq:1 ~ts:1.0 ~record_type:Run_finished ~final_text:"done" () ]
  with
  | Ok _ -> fail "expected missing prompt to fail"
  | Error _ -> ()
;;

let test_trace_replay_extracts_tool_assertions () =
  let records =
    [ mk_record ~seq:1 ~ts:1.0 ~record_type:Run_started ~prompt:"Use a tool" ()
    ; mk_record
        ~seq:2
        ~ts:2.0
        ~record_type:Tool_execution_started
        ~tool_use_id:"tu-1"
        ~tool_name:"lookup"
        ~tool_input:(`Assoc [ "q", `String "oas" ])
        ~tool_execution_mode:Tool.Concurrent
        ()
    ; mk_record
        ~seq:3
        ~ts:3.0
        ~record_type:Tool_execution_finished
        ~tool_use_id:"tu-1"
        ~tool_result:"result"
        ~tool_error:false
        ()
    ; mk_record ~seq:4 ~ts:4.0 ~record_type:Run_finished ~final_text:"done" ()
    ]
  in
  match
    Harness_case.trace_replay_of_records
      ~id:"trace-tools"
      ~source_trace_path:"/tmp/trace-tools.ndjson"
      records
  with
  | Error e -> fail (Error.to_string e)
  | Ok case_ ->
    let has_sequence =
      List.exists
        (function
          | Harness_case.Trace (Tool_sequence [ "lookup" ]) -> true
          | _ -> false)
        case_.assertions
    in
    let has_count =
      List.exists
        (function
          | Harness_case.Trace (Tool_call_count 1) -> true
          | _ -> false)
        case_.assertions
    in
    check bool "tool sequence assertion" true has_sequence;
    check bool "tool count assertion" true has_count
;;

let () =
  run
    "harness_case"
    [ "dataset", [ test_case "roundtrip" `Quick test_dataset_roundtrip ]
    ; ( "json"
      , [ test_case "roundtrip all assertions" `Quick test_json_roundtrip_all_assertions
        ; test_case "null optional lists" `Quick test_of_json_accepts_null_optional_lists
        ; test_case "unknown kind rejected" `Quick test_of_json_rejects_unknown_kind
        ; test_case "bad tags rejected" `Quick test_of_json_rejects_bad_tags
        ; test_case
            "unknown assertions rejected"
            `Quick
            test_of_json_rejects_unknown_assertions
        ] )
    ; ( "trace_replay"
      , [ test_case "of_records" `Quick test_trace_replay_of_records
        ; test_case
            "make_trace_replay source metadata"
            `Quick
            test_make_trace_replay_adds_source_metadata
        ; test_case "requires prompt" `Quick test_trace_replay_requires_prompt
        ; test_case "tool assertions" `Quick test_trace_replay_extracts_tool_assertions
        ] )
    ]
;;
