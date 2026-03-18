(** Tests for Traced_swarm — trace collection after swarm execution.

    Mock agent_entries write minimal JSONL traces during their run,
    then [Traced_swarm.collect_summaries] verifies collection.
    No LLM required. *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm

(* ── Helpers ─────────────────────────────────────────────── *)

(** Mock agent_entry that writes a minimal JSONL trace
    (run_started + tool executions + run_finished) alongside
    returning a text response. *)
let traced_mock_entry ~trace_dir ~name ~tool_names text =
  let trace_path = Filename.concat trace_dir (name ^ ".jsonl") in
  { Swarm_types.name;
    run = (fun ~sw:_ prompt ->
      (match Raw_trace.create ~path:trace_path () with
       | Error _ -> ()
       | Ok sink ->
         (match Raw_trace.start_run sink ~agent_name:name ~prompt with
          | Error _ -> ()
          | Ok active ->
            List.iter (fun tool_name ->
              ignore (Raw_trace.record_tool_execution_started active
                ~tool_use_id:("tu-" ^ tool_name) ~tool_name
                ~tool_input:(`Assoc []));
              ignore (Raw_trace.record_tool_execution_finished active
                ~tool_use_id:("tu-" ^ tool_name) ~tool_name
                ~tool_result:"ok" ~tool_error:false)
            ) tool_names;
            ignore (Raw_trace.finish_run active
              ~final_text:(Some text) ~stop_reason:(Some "EndTurn")
              ~error:None)));
      Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
           content = [Text text]; usage = None });
    role = Execute;
  }

(* ── Tests ────────────────────────────────────────────────── *)

(** 2 workers with different tools -> 2 summaries collected. *)
let test_collect_summaries () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let trace_dir = Filename.temp_dir "test_traced" "" in
  let entries = [
    traced_mock_entry ~trace_dir ~name:"w1"
      ~tool_names:["file_read"; "shell_exec"] "result-1";
    traced_mock_entry ~trace_dir ~name:"w2"
      ~tool_names:["web_search"; "api_call"] "result-2";
  ] in
  let config = Test_helpers.basic_config ~prompt:"test traced" entries in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~clock config with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "run error: %s" (Error.to_string e)));
  match Traced_swarm.collect_summaries ~trace_dir with
  | Error e -> fail (Printf.sprintf "collect error: %s" (Error.to_string e))
  | Ok summaries ->
    check int "2 summaries" 2 (List.length summaries);
    check bool "w1.jsonl exists" true
      (Sys.file_exists (Filename.concat trace_dir "w1.jsonl"));
    check bool "w2.jsonl exists" true
      (Sys.file_exists (Filename.concat trace_dir "w2.jsonl"))

(** summaries count == workers count. *)
let test_summaries_count_equals_workers () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let trace_dir = Filename.temp_dir "test_traced_n" "" in
  let n = 3 in
  let entries = List.init n (fun i ->
    let name = Printf.sprintf "w%d" (i + 1) in
    traced_mock_entry ~trace_dir ~name ~tool_names:["t1"] "ok"
  ) in
  let config = Test_helpers.basic_config ~prompt:"count test" entries in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~clock config with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e)));
  match Traced_swarm.collect_summaries ~trace_dir with
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))
  | Ok summaries ->
    check int "summaries == workers" n (List.length summaries)

(** trace_dir contains the expected .jsonl files. *)
let test_trace_dir_contains_jsonl () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let trace_dir = Filename.temp_dir "test_traced_files" "" in
  let entries = [
    traced_mock_entry ~trace_dir ~name:"agent-a"
      ~tool_names:["read"; "write"; "exec"] "done";
  ] in
  let config = Test_helpers.basic_config ~prompt:"file test" entries in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~clock config with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e)));
  let files = Sys.readdir trace_dir |> Array.to_list in
  let jsonl_files =
    List.filter (fun f -> Filename.check_suffix f ".jsonl") files
  in
  check int "1 jsonl file" 1 (List.length jsonl_files);
  check string "correct filename" "agent-a.jsonl" (List.hd jsonl_files)

(* ── Test suite ───────────────────────────────────────────── *)

let () =
  run "traced_swarm" [
    "collect", [
      test_case "summaries collected" `Quick test_collect_summaries;
      test_case "count equals workers" `Quick
        test_summaries_count_equals_workers;
      test_case "trace dir has jsonl" `Quick
        test_trace_dir_contains_jsonl;
    ];
  ]
