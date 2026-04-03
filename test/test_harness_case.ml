(** Tests for Harness_case and Harness_dataset. *)

open Agent_sdk
open Alcotest

let mk_record ~seq ~ts ~record_type ?prompt ?final_text ?error () : Raw_trace.record =
  {
    trace_version = 1;
    worker_run_id = "wr-hcase";
    seq;
    ts;
    agent_name = "trace-agent";
    session_id = None;
    record_type;
    prompt;
    block_index = None;
    block_kind = None;
    assistant_block = None;
    tool_use_id = None;
    tool_name = None;
    tool_input = None;
    tool_planned_index = None;
    tool_batch_index = None;
    tool_batch_size = None;
    tool_concurrency_class = None;
    tool_result = None;
    tool_error = None;
    hook_name = None;
    hook_decision = None;
    hook_detail = None;
    final_text;
    stop_reason = Some "end_turn";
    error;
  }

let test_dataset_roundtrip () =
  let case_ =
    Harness_case.make_fixture
      ~assertions:[Harness_case.Response (Harness_case.Exact_text "hello")]
      ~id:"fixture-1"
      ~prompt:"Say hello"
      ()
  in
  let path = Filename.temp_file "oas-harness-dataset" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      (match Harness_dataset.save ~path [case_] with
       | Ok () -> ()
       | Error e -> fail (Error.to_string e));
      match Harness_dataset.load ~path with
      | Error e -> fail (Error.to_string e)
      | Ok [loaded] ->
        check string "id" "fixture-1" loaded.id;
        check string "prompt" "Say hello" loaded.prompt
      | Ok _ -> fail "expected exactly one case")

let test_trace_replay_of_records () =
  let records = [
    mk_record ~seq:1 ~ts:1.0 ~record_type:Run_started ~prompt:"Replay me" ();
    mk_record ~seq:2 ~ts:2.0 ~record_type:Run_finished ~final_text:"done" ();
  ] in
  match Harness_case.trace_replay_of_records
          ~id:"trace-1"
          ~source_trace_path:"/tmp/trace-1.ndjson"
          records with
  | Error e -> fail (Error.to_string e)
  | Ok case_ ->
    check string "prompt" "Replay me" case_.prompt;
    check bool "is trace replay" true (case_.kind = Harness_case.Trace_replay);
    check bool "has assertions" true (List.length case_.assertions >= 2)

let () =
  run "harness_case" [
    "dataset", [
      test_case "roundtrip" `Quick test_dataset_roundtrip;
    ];
    "trace_replay", [
      test_case "of_records" `Quick test_trace_replay_of_records;
    ];
  ]
