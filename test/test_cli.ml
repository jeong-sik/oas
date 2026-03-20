(** Tests for CLI binary — verifies the oas CLI builds and responds to commands.

    The binary path is resolved relative to the test executable location,
    which works both with [dune exec] and [dune runtest]. *)

open Alcotest
open Agent_sdk

(** Resolve the CLI binary path.
    Under dune, the test runs from _build/default/test/ and the binary
    is at _build/default/bin/oas_cli.exe. We walk up from the test
    executable's directory. *)
let cli_exe =
  let self = Sys.executable_name in
  let dir = Filename.dirname self in
  (* Try sibling bin directory first (dune layout) *)
  let candidate = Filename.concat (Filename.concat (Filename.dirname dir) "bin") "oas_cli.exe" in
  if Sys.file_exists candidate then candidate
  else
    (* Fallback: relative to cwd (dune exec) *)
    let cwd_candidate = Printf.sprintf "%s/_build/default/bin/oas_cli.exe" (Sys.getcwd ()) in
    if Sys.file_exists cwd_candidate then cwd_candidate
    else
      (* Last resort: just try the path and let it fail at runtime *)
      candidate

(* ── Version subcommand ─────────────────────────────────── *)

let test_version_output () =
  let cmd = Printf.sprintf "%s version" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  let trimmed = String.trim output in
  check bool "starts with oas" true
    (String.length trimmed >= 3 && String.sub trimmed 0 3 = "oas");
  check bool "contains version number" true
    (try ignore (Str.search_forward (Str.regexp "[0-9]+\\.[0-9]+\\.[0-9]+") trimmed 0); true
     with Not_found -> false)

(* ── Init subcommand ────────────────────────────────────── *)

let test_init_creates_config () =
  let dir = Printf.sprintf "/tmp/oas_test_init_%d" (Unix.getpid ()) in
  let cmd = Printf.sprintf "%s init %s" cli_exe dir in
  let exit_code = Sys.command cmd in
  check int "exit 0" 0 exit_code;
  let config_path = Filename.concat dir "oas.json" in
  check bool "config exists" true (Sys.file_exists config_path);
  let data = In_channel.with_open_text config_path In_channel.input_all in
  (try ignore (Yojson.Safe.from_string data)
   with _ -> fail "oas.json is not valid JSON");
  (try Sys.remove config_path; Sys.rmdir dir with _ -> ())

(* ── Run without config ─────────────────────────────────── *)

let test_run_no_config () =
  let cmd = Printf.sprintf "%s run 2>&1; echo $?" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0)

(* ── Card without config ────────────────────────────────── *)

let test_card_no_config () =
  let cmd = Printf.sprintf "%s card 2>&1; echo $?" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0)

(* ── Card with valid config ─────────────────────────────── *)

let test_card_with_config () =
  let config_path = Printf.sprintf "/tmp/oas_test_card_%d.json" (Unix.getpid ()) in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc {|{"name":"card-test","model":"claude-sonnet-4-6"}|});
  let cmd = Printf.sprintf "%s card --config %s 2>&1" cli_exe config_path in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "contains name" true
    (try ignore (Str.search_forward (Str.regexp_string "card-test") output 0); true
     with Not_found -> false);
  (try Sys.remove config_path with _ -> ())

(* ── Help output ────────────────────────────────────────── *)

let test_help () =
  let cmd = Printf.sprintf "%s --help 2>&1" cli_exe in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "contains COMMANDS" true
    (try ignore (Str.search_forward (Str.regexp_string_case_fold "command") output 0); true
     with Not_found -> false)
  ;
  check bool "contains eval" true
    (try ignore (Str.search_forward (Str.regexp_string_case_fold "eval") output 0); true
     with Not_found -> false)

(* ── Run with config but no prompt ──────────────────────── *)

let test_run_config_no_prompt () =
  let config_path = Printf.sprintf "/tmp/oas_test_noprompt_%d.json" (Unix.getpid ()) in
  Out_channel.with_open_text config_path (fun oc ->
    output_string oc {|{"name":"test"}|});
  let cmd = Printf.sprintf "%s run --config %s 2>&1; echo $?" cli_exe config_path in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  check bool "has output" true (String.length (String.trim output) > 0);
  (try Sys.remove config_path with _ -> ())

(* ── Eval record-trace ──────────────────────────────── *)

let test_eval_record_trace () =
  let trace_path = Printf.sprintf "/tmp/oas_cli_trace_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_dataset_%d.jsonl" (Unix.getpid ()) in
  let records = [
    {
      Raw_trace.trace_version = 1;
      worker_run_id = "wr-cli";
      seq = 1;
      ts = 1.0;
      agent_name = "cli-agent";
      session_id = None;
      record_type = Raw_trace.Run_started;
      prompt = Some "Replay me";
      block_index = None;
      block_kind = None;
      assistant_block = None;
      tool_use_id = None;
      tool_name = None;
      tool_input = None;
      tool_result = None;
      tool_error = None;
      hook_name = None;
      hook_decision = None;
      hook_detail = None;
      final_text = None;
      stop_reason = None;
      error = None;
    };
    {
      Raw_trace.trace_version = 1;
      worker_run_id = "wr-cli";
      seq = 2;
      ts = 2.0;
      agent_name = "cli-agent";
      session_id = None;
      record_type = Raw_trace.Run_finished;
      prompt = None;
      block_index = None;
      block_kind = None;
      assistant_block = None;
      tool_use_id = None;
      tool_name = None;
      tool_input = None;
      tool_result = None;
      tool_error = None;
      hook_name = None;
      hook_decision = None;
      hook_detail = None;
      final_text = Some "done";
      stop_reason = Some "end_turn";
      error = None;
    };
  ] in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove trace_path with _ -> ());
      (try Sys.remove dataset_path with _ -> ()))
    (fun () ->
      Out_channel.with_open_text trace_path (fun oc ->
        List.iter (fun record ->
          output_string oc (Yojson.Safe.to_string (Raw_trace.record_to_json record));
          output_char oc '\n'
        ) records);
      let cmd =
        Printf.sprintf "%s eval record-trace --session %s --out %s"
          cli_exe trace_path dataset_path
      in
      let exit_code = Sys.command cmd in
      check int "exit 0" 0 exit_code;
      check bool "dataset exists" true (Sys.file_exists dataset_path);
      match Harness_dataset.load ~path:dataset_path with
      | Error e -> fail (Error.to_string e)
      | Ok [case_] -> check string "prompt" "Replay me" case_.prompt
      | Ok _ -> fail "expected one replay case")

let test_eval_run_empty_dataset () =
  let dataset_path = Printf.sprintf "/tmp/oas_cli_empty_%d.jsonl" (Unix.getpid ()) in
  let config_path = Printf.sprintf "/tmp/oas_cli_eval_%d.json" (Unix.getpid ()) in
  let out_dir = Printf.sprintf "/tmp/oas_cli_eval_out_%d" (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove dataset_path with _ -> ());
      (try Sys.remove config_path with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.json") with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.md") with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.junit.xml") with _ -> ());
      (try Sys.rmdir out_dir with _ -> ()))
    (fun () ->
      Out_channel.with_open_text dataset_path (fun _ -> ());
      Out_channel.with_open_text config_path (fun oc ->
        output_string oc {|{"name":"eval-test","model":"claude-sonnet-4-6"}|});
      let cmd =
        Printf.sprintf "%s eval run --config %s --dataset %s --out %s"
          cli_exe config_path dataset_path out_dir
      in
      let exit_code = Sys.command cmd in
      check int "exit 0" 0 exit_code;
      check bool "report json exists" true
        (Sys.file_exists (Filename.concat out_dir "report.json")))

let test_eval_run_trace_replay_dataset () =
  let trace_path = Printf.sprintf "/tmp/oas_cli_eval_trace_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_eval_trace_%d.jsonl" (Unix.getpid ()) in
  let out_dir = Printf.sprintf "/tmp/oas_cli_eval_trace_out_%d" (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove trace_path with _ -> ());
      (try Sys.remove dataset_path with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.json") with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.md") with _ -> ());
      (try Sys.remove (Filename.concat out_dir "report.junit.xml") with _ -> ());
      (try Sys.rmdir out_dir with _ -> ()))
    (fun () ->
      Out_channel.with_open_text trace_path (fun oc ->
        output_string oc
          {|{"trace_version":1,"worker_run_id":"wr-cli","seq":1,"ts":1.0,"agent_name":"cli-agent","session_id":null,"record_type":"run_started","prompt":"Replay me","block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":null,"stop_reason":null,"error":null}|};
        output_char oc '\n';
        output_string oc
          {|{"trace_version":1,"worker_run_id":"wr-cli","seq":2,"ts":1.5,"agent_name":"cli-agent","session_id":null,"record_type":"run_finished","prompt":null,"block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":"done","stop_reason":"end_turn","error":null}|};
        output_char oc '\n');
      Out_channel.with_open_text dataset_path (fun oc ->
        output_string oc
          (Printf.sprintf
             {|{"id":"replay","kind":"trace_replay","prompt":"Replay me","tags":["test"],"assertions":[{"type":"response_exact_text","value":"done"},{"type":"trace_succeeds"}],"artifacts":["%s"],"source_trace_path":"%s"}|}
             trace_path trace_path);
        output_char oc '\n');
      let cmd =
        Printf.sprintf "%s eval run --dataset %s --out %s"
          cli_exe dataset_path out_dir
      in
      let exit_code = Sys.command cmd in
      check int "exit 0" 0 exit_code;
      check bool "report markdown exists" true
        (Sys.file_exists (Filename.concat out_dir "report.md")))

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run "CLI" [
    "version", [
      test_case "version output" `Quick test_version_output;
    ];
    "init", [
      test_case "creates config" `Quick test_init_creates_config;
    ];
    "run", [
      test_case "no config" `Quick test_run_no_config;
      test_case "config no prompt" `Quick test_run_config_no_prompt;
    ];
    "card", [
      test_case "no config" `Quick test_card_no_config;
      test_case "with config" `Quick test_card_with_config;
    ];
    "help", [
      test_case "help output" `Quick test_help;
    ];
    "eval", [
      test_case "record trace" `Quick test_eval_record_trace;
      test_case "run empty dataset" `Quick test_eval_run_empty_dataset;
      test_case "run trace replay dataset" `Quick test_eval_run_trace_replay_dataset;
    ];
  ]
