(** Tests for Durable_event — event-sourced agent loop journal. *)

open Alcotest
open Agent_sdk

let ts = 1711234567.0

(* ── Journal basics ───────────────────────────────── *)

let test_empty_journal () =
  let j = Durable_event.create () in
  check int "empty length" 0 (Durable_event.length j);
  check (list pass) "empty events" [] (Durable_event.events j);
  check bool "no last_timestamp" true
    (Option.is_none (Durable_event.last_timestamp j))

let test_append_and_events () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Turn_started { turn = 2; timestamp = ts +. 1.0 });
  check int "length 2" 2 (Durable_event.length j);
  let evts = Durable_event.events j in
  (* Chronological order *)
  (match evts with
   | [Durable_event.Turn_started { turn = 1; _ };
      Durable_event.Turn_started { turn = 2; _ }] -> ()
   | _ -> fail "expected chronological order")

let test_last_timestamp () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Checkpoint_saved { checkpoint_id = "cp1"; timestamp = ts +. 5.0 });
  (match Durable_event.last_timestamp j with
   | Some t -> check (float 0.01) "last ts" (ts +. 5.0) t
   | None -> fail "expected timestamp")

(* ── Idempotency ──────────────────────────────────── *)

let test_idempotency_key_deterministic () =
  let k1 = Durable_event.make_idempotency_key ~tool_name:"read" ~input:(`String "a") in
  let k2 = Durable_event.make_idempotency_key ~tool_name:"read" ~input:(`String "a") in
  check string "deterministic" k1 k2

let test_idempotency_key_unique () =
  let k1 = Durable_event.make_idempotency_key ~tool_name:"read" ~input:(`String "a") in
  let k2 = Durable_event.make_idempotency_key ~tool_name:"write" ~input:(`String "a") in
  check bool "different tools" true (k1 <> k2)

let test_find_completed_activity () =
  let j = Durable_event.create () in
  let key = Durable_event.make_idempotency_key ~tool_name:"calc" ~input:(`Int 42) in
  Durable_event.append j (Tool_called {
    turn = 1; tool_name = "calc"; idempotency_key = key;
    input_hash = "h"; timestamp = ts;
  });
  Durable_event.append j (Tool_completed {
    turn = 1; tool_name = "calc"; idempotency_key = key;
    output_json = `String "result"; is_error = false;
    duration_ms = 10.0; timestamp = ts +. 0.01;
  });
  (match Durable_event.find_completed_activity j key with
   | Some (`String "result") -> ()
   | _ -> fail "expected cached result");
  (* Non-existent key *)
  check bool "missing key" true
    (Option.is_none (Durable_event.find_completed_activity j "nonexistent"))

(* ── Replay summary ───────────────────────────────── *)

let test_replay_summary () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Llm_request { turn = 1; model = "test"; input_tokens = 100; timestamp = ts });
  Durable_event.append j (Llm_response { turn = 1; output_tokens = 50; stop_reason = "end_turn"; duration_ms = 200.0; timestamp = ts });
  Durable_event.append j (Tool_completed { turn = 1; tool_name = "read"; idempotency_key = "k1"; output_json = `Null; is_error = false; duration_ms = 5.0; timestamp = ts });
  Durable_event.append j (State_transition { from_state = "running"; to_state = "idle"; reason = "done"; timestamp = ts });
  Durable_event.append j (Error_occurred { turn = 1; error_domain = "Api"; detail = "timeout"; timestamp = ts });
  Durable_event.append j (Turn_started { turn = 2; timestamp = ts });
  let s = Durable_event.replay_summary j in
  check int "last_turn" 2 s.last_turn;
  check int "completed tools" 1 (List.length s.completed_tools);
  check string "last_state" "idle" s.last_state;
  check int "input_tokens" 100 s.total_input_tokens;
  check int "output_tokens" 50 s.total_output_tokens;
  check int "errors" 1 s.error_count

(* ── Events for turn ──────────────────────────────── *)

let test_events_for_turn () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Llm_request { turn = 1; model = "m"; input_tokens = 10; timestamp = ts });
  Durable_event.append j (Turn_started { turn = 2; timestamp = ts });
  Durable_event.append j (Llm_request { turn = 2; model = "m"; input_tokens = 20; timestamp = ts });
  let t1 = Durable_event.events_for_turn j 1 in
  check int "turn 1 events" 2 (List.length t1);
  let t2 = Durable_event.events_for_turn j 2 in
  check int "turn 2 events" 2 (List.length t2)

(* ── JSON round-trip ──────────────────────────────── *)

let test_json_roundtrip () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Tool_completed { turn = 1; tool_name = "read"; idempotency_key = "k"; output_json = `String "ok"; is_error = false; duration_ms = 5.0; timestamp = ts });
  Durable_event.append j (Error_occurred { turn = 1; error_domain = "Api"; detail = "err"; timestamp = ts });
  let json = Durable_event.journal_to_json j in
  match Durable_event.journal_of_json json with
  | Ok j2 ->
    check int "same length" (Durable_event.length j) (Durable_event.length j2)
  | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)

let test_event_json_roundtrip_all_types () =
  let events = [
    Durable_event.Turn_started { turn = 1; timestamp = ts };
    Llm_request { turn = 1; model = "m"; input_tokens = 10; timestamp = ts };
    Llm_response { turn = 1; output_tokens = 5; stop_reason = "end"; duration_ms = 1.0; timestamp = ts };
    Tool_called { turn = 1; tool_name = "t"; idempotency_key = "k"; input_hash = "h"; timestamp = ts };
    Tool_completed { turn = 1; tool_name = "t"; idempotency_key = "k"; output_json = `Null; is_error = false; duration_ms = 1.0; timestamp = ts };
    State_transition { from_state = "a"; to_state = "b"; reason = "r"; timestamp = ts };
    Checkpoint_saved { checkpoint_id = "c"; timestamp = ts };
    Error_occurred { turn = 1; error_domain = "d"; detail = "e"; timestamp = ts };
  ] in
  List.iter (fun evt ->
    let json = Durable_event.event_to_json evt in
    match Durable_event.event_of_json json with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
  ) events

(* ── Tool completions query ───────────────────────── *)

let test_tool_completions () =
  let j = Durable_event.create () in
  Durable_event.append j (Tool_completed { turn = 1; tool_name = "a"; idempotency_key = "k1"; output_json = `Int 1; is_error = false; duration_ms = 1.0; timestamp = ts });
  Durable_event.append j (Turn_started { turn = 2; timestamp = ts });
  Durable_event.append j (Tool_completed { turn = 2; tool_name = "b"; idempotency_key = "k2"; output_json = `Int 2; is_error = true; duration_ms = 2.0; timestamp = ts });
  let completions = Durable_event.tool_completions j in
  check int "2 completions" 2 (List.length completions)

(* ── on_append callback ───────────────────────────── *)

let test_on_append_fires () =
  let captured = ref [] in
  let j = Durable_event.create ~on_append:(fun evt ->
    captured := evt :: !captured) () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j (Llm_request { turn = 1; model = "m"; input_tokens = 10; timestamp = ts });
  check int "callback count" 2 (List.length !captured);
  check int "journal length" 2 (Durable_event.length j)

let test_no_callback_default () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  check int "still appends" 1 (Durable_event.length j)

(* ── Persistence ──────────────────────────────────── *)

let test_save_and_load_roundtrip () =
  let j = Durable_event.create () in
  Durable_event.append j (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append j
    (Llm_request { turn = 1; model = "q"; input_tokens = 42; timestamp = ts });
  Durable_event.append j
    (Error_occurred { turn = 1; error_domain = "Api"; detail = "boom"; timestamp = ts });
  let path = Filename.temp_file "durable_event" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      (match Durable_event.save_to_file j path with
       | Ok () -> ()
       | Error e -> fail (Printf.sprintf "save failed: %s" e));
      match Durable_event.load_from_file path with
      | Error e -> fail (Printf.sprintf "load failed: %s" e)
      | Ok j' ->
        check int "length" 3 (Durable_event.length j');
        let summary = Durable_event.replay_summary j' in
        check int "total input tokens" 42 summary.total_input_tokens;
        check int "error count" 1 summary.error_count)

let test_load_missing_file_is_empty () =
  let missing = "/tmp/definitely-does-not-exist-" ^ string_of_float (Unix.gettimeofday ()) in
  match Durable_event.load_from_file missing with
  | Ok j -> check int "empty journal" 0 (Durable_event.length j)
  | Error e -> fail (Printf.sprintf "expected Ok, got: %s" e)

let test_load_malformed_returns_error () =
  let path = Filename.temp_file "durable_bad" ".jsonl" in
  let oc = open_out path in
  output_string oc "not json\n";
  close_out oc;
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      match Durable_event.load_from_file path with
      | Ok _ -> fail "expected Error"
      | Error _ -> ())

(* ── Suite ────────────────────────────────────────── *)

let () =
  run "Durable_event" [
    "journal", [
      test_case "empty" `Quick test_empty_journal;
      test_case "append and events" `Quick test_append_and_events;
      test_case "last_timestamp" `Quick test_last_timestamp;
    ];
    "on_append", [
      test_case "callback fires" `Quick test_on_append_fires;
      test_case "no callback default" `Quick test_no_callback_default;
    ];
    "idempotency", [
      test_case "deterministic key" `Quick test_idempotency_key_deterministic;
      test_case "unique keys" `Quick test_idempotency_key_unique;
      test_case "find completed" `Quick test_find_completed_activity;
    ];
    "replay", [
      test_case "summary" `Quick test_replay_summary;
      test_case "events for turn" `Quick test_events_for_turn;
    ];
    "serialization", [
      test_case "journal roundtrip" `Quick test_json_roundtrip;
      test_case "all event types" `Quick test_event_json_roundtrip_all_types;
    ];
    "queries", [
      test_case "tool completions" `Quick test_tool_completions;
    ];
    "persistence", [
      test_case "save/load roundtrip" `Quick test_save_and_load_roundtrip;
      test_case "missing file empty" `Quick test_load_missing_file_is_empty;
      test_case "malformed returns error" `Quick test_load_malformed_returns_error;
    ];
  ]
