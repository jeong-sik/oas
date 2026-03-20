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

let quick_response text =
  Printf.sprintf
    {|{"id":"m","type":"message","role":"assistant","model":"m","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
    text

let start_mock ~sw ~net ~clock ~port response_text =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    ignore clock;
    Cohttp_eio.Server.respond_string
      ~status:`OK ~body:(quick_response response_text) ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let read_all_lines flow =
  let buf = Buffer.create 256 in
  let reader = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  try
    while true do
      Buffer.add_string buf (Eio.Buf_read.line reader);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file ->
    Buffer.contents buf

let run_cli_capture ~sw ~mgr args =
  let r_stdout, w_stdout = Eio_unix.pipe sw in
  let r_stderr, w_stderr = Eio_unix.pipe sw in
  let proc =
    Eio.Process.spawn ~sw mgr
      ~stdout:(w_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(w_stderr :> Eio.Flow.sink_ty Eio.Resource.t)
      (cli_exe :: args)
  in
  Eio.Flow.close w_stdout;
  Eio.Flow.close w_stderr;
  let stdout_buf = ref "" in
  let stderr_buf = ref "" in
  Eio.Fiber.both
    (fun () -> stdout_buf := read_all_lines (r_stdout :> _ Eio.Flow.source))
    (fun () -> stderr_buf := read_all_lines (r_stderr :> _ Eio.Flow.source));
  (Eio.Process.await proc, !stdout_buf, !stderr_buf)

let remove_path path =
  try Sys.remove path with _ -> ()

let remove_dir_if_empty path =
  try Sys.rmdir path with _ -> ()

let run_command_capture_output cmd =
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let status = Unix.close_process_in ic in
  (status, output)

let write_trace_replay_file ~path ~prompt ~final_text ~finished_ts =
  Out_channel.with_open_text path (fun oc ->
    output_string oc
      (Printf.sprintf
         {|{"trace_version":1,"worker_run_id":"wr-cli","seq":1,"ts":1.0,"agent_name":"cli-agent","session_id":null,"record_type":"run_started","prompt":"%s","block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":null,"stop_reason":null,"error":null}|}
         prompt);
    output_char oc '\n';
    output_string oc
      (Printf.sprintf
         {|{"trace_version":1,"worker_run_id":"wr-cli","seq":2,"ts":%.1f,"agent_name":"cli-agent","session_id":null,"record_type":"run_finished","prompt":null,"block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":"%s","stop_reason":"end_turn","error":null}|}
         finished_ts final_text);
    output_char oc '\n')

let replay_case_json ~id ~prompt ~expected_text ~trace_path =
  Printf.sprintf
    {|{"id":"%s","kind":"trace_replay","prompt":"%s","tags":["test"],"assertions":[{"type":"response_exact_text","value":"%s"},{"type":"trace_succeeds"}],"artifacts":["%s"],"source_trace_path":"%s"}|}
    id prompt expected_text trace_path trace_path

let pass_verdict : Harness.verdict =
  { passed = true; score = Some 1.0; evidence = []; detail = None }

let metric name value : Eval.metric =
  { name; value; unit_ = None; tags = [] }

let save_baseline_file ~path ~run_id ~elapsed_s =
  let run_metrics : Eval.run_metrics =
    {
      run_id;
      agent_name = "cli-agent";
      timestamp = 1000.0;
      metrics = [
        metric "turn_count" (Eval.Int_val 1);
        metric "tool_calls" (Eval.Int_val 0);
        metric "elapsed_s" (Eval.Float_val elapsed_s);
        metric "success" (Eval.Bool_val true);
      ];
      harness_verdicts = [pass_verdict];
      trace_summary = None;
    }
  in
  let baseline = Eval_baseline.create ~description:"test baseline" run_metrics in
  match Eval_baseline.save ~path baseline with
  | Ok () -> ()
  | Error e -> fail e

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

let test_eval_run_mixed_dataset () =
  let trace_path = Printf.sprintf "/tmp/oas_cli_mixed_trace_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_mixed_%d.jsonl" (Unix.getpid ()) in
  let config_path = Printf.sprintf "/tmp/oas_cli_mixed_%d.json" (Unix.getpid ()) in
  let out_dir = Printf.sprintf "/tmp/oas_cli_mixed_out_%d" (Unix.getpid ()) in
  let fixture_trace = Filename.concat out_dir "fixture-live.ndjson" in
  let port = 19150 + (Unix.getpid () mod 1000) in
  Fun.protect
    ~finally:(fun () ->
      remove_path trace_path;
      remove_path dataset_path;
      remove_path config_path;
      remove_path (Filename.concat out_dir "report.json");
      remove_path (Filename.concat out_dir "report.md");
      remove_path (Filename.concat out_dir "report.junit.xml");
      remove_path fixture_trace;
      remove_dir_if_empty out_dir)
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
          {|{"id":"fixture-live","kind":"fixture","prompt":"Say fixture ok","tags":["test"],"assertions":[{"type":"response_exact_text","value":"fixture ok"},{"type":"trace_succeeds"}],"artifacts":[],"source_trace_path":null}|};
        output_char oc '\n';
        output_string oc
          (Printf.sprintf
             {|{"id":"trace-offline","kind":"trace_replay","prompt":"Replay me","tags":["test"],"assertions":[{"type":"response_exact_text","value":"done"},{"type":"trace_succeeds"}],"artifacts":["%s"],"source_trace_path":"%s"}|}
             trace_path trace_path);
        output_char oc '\n');
      Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      let mgr = Eio.Stdenv.process_mgr env in
      try
        Eio.Switch.run @@ fun sw ->
        let url = start_mock ~sw ~net:env#net ~clock ~port "fixture ok" in
        Out_channel.with_open_text config_path (fun oc ->
          output_string oc
            (Printf.sprintf
               {|{"name":"eval-mixed","model":"mock-model","provider":"local","base_url":"%s","max_turns":1}|}
               url));
        let status, stdout, stderr =
          run_cli_capture ~sw ~mgr
            [
              "eval";
              "run";
              "--config"; config_path;
              "--dataset"; dataset_path;
              "--out"; out_dir;
            ]
        in
        (match status with
         | `Exited 0 -> ()
         | `Exited code ->
           fail (Printf.sprintf "cli exited %d\nstdout:\n%s\nstderr:\n%s" code stdout stderr)
         | _ ->
           fail (Printf.sprintf "cli terminated unexpectedly\nstdout:\n%s\nstderr:\n%s"
                   stdout stderr));
        check bool "report json exists" true
          (Sys.file_exists (Filename.concat out_dir "report.json"));
        let report_json =
          In_channel.with_open_text (Filename.concat out_dir "report.json")
            (fun ic -> Yojson.Safe.from_string (In_channel.input_all ic))
        in
        let open Yojson.Safe.Util in
        check int "total" 2 (report_json |> member "summary" |> member "total" |> to_int);
        check int "passed" 2 (report_json |> member "summary" |> member "passed" |> to_int);
        check bool "fixture trace exists" true (Sys.file_exists fixture_trace);
        let kinds =
          report_json
          |> member "results"
          |> to_list
          |> List.map (fun json -> json |> member "kind" |> to_string)
        in
        check bool "has fixture result" true (List.mem "fixture" kinds);
        check bool "has trace replay result" true (List.mem "trace_replay" kinds);
        Eio.Switch.fail sw Exit
      with Exit -> ())

let test_eval_save_baseline_trace_replay_dataset () =
  let trace_path = Printf.sprintf "/tmp/oas_cli_baseline_trace_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_baseline_dataset_%d.jsonl" (Unix.getpid ()) in
  let baseline_path = Printf.sprintf "/tmp/oas_cli_baseline_%d.json" (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      remove_path trace_path;
      remove_path dataset_path;
      remove_path baseline_path)
    (fun () ->
      write_trace_replay_file
        ~path:trace_path
        ~prompt:"Replay me"
        ~final_text:"done"
        ~finished_ts:1.5;
      Out_channel.with_open_text dataset_path (fun oc ->
        output_string oc
          (replay_case_json
             ~id:"replay"
             ~prompt:"Replay me"
             ~expected_text:"done"
             ~trace_path);
        output_char oc '\n');
      let cmd =
        Printf.sprintf "%s eval save-baseline --dataset %s --out %s"
          cli_exe dataset_path baseline_path
      in
      let exit_code = Sys.command cmd in
      check int "exit 0" 0 exit_code;
      check bool "baseline exists" true (Sys.file_exists baseline_path);
      match Eval_baseline.load ~path:baseline_path with
      | Error e -> fail e
      | Ok baseline ->
        check string "run id" "replay" baseline.run_metrics.run_id)

let test_eval_run_with_baseline_failure () =
  let trace_path = Printf.sprintf "/tmp/oas_cli_reg_trace_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_reg_dataset_%d.jsonl" (Unix.getpid ()) in
  let baseline_path = Printf.sprintf "/tmp/oas_cli_reg_baseline_%d.json" (Unix.getpid ()) in
  let out_dir = Printf.sprintf "/tmp/oas_cli_reg_out_%d" (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      remove_path trace_path;
      remove_path dataset_path;
      remove_path baseline_path;
      remove_path (Filename.concat out_dir "report.json");
      remove_path (Filename.concat out_dir "report.md");
      remove_path (Filename.concat out_dir "report.junit.xml");
      remove_path (Filename.concat out_dir "eval.json");
      remove_path (Filename.concat out_dir "eval.txt");
      remove_dir_if_empty out_dir)
    (fun () ->
      write_trace_replay_file
        ~path:trace_path
        ~prompt:"Replay me"
        ~final_text:"done"
        ~finished_ts:2.5;
      Out_channel.with_open_text dataset_path (fun oc ->
        output_string oc
          (replay_case_json
             ~id:"replay"
             ~prompt:"Replay me"
             ~expected_text:"done"
             ~trace_path);
        output_char oc '\n');
      save_baseline_file ~path:baseline_path ~run_id:"replay" ~elapsed_s:2.5;
      let cmd =
        Printf.sprintf "%s eval run --baseline %s --dataset %s --out %s"
          cli_exe baseline_path dataset_path out_dir
      in
      let exit_code = Sys.command cmd in
      check int "exit 1" 1 exit_code;
      check bool "eval json exists" true
        (Sys.file_exists (Filename.concat out_dir "eval.json"));
      let eval_json =
        In_channel.with_open_text (Filename.concat out_dir "eval.json")
          (fun ic -> Yojson.Safe.from_string (In_channel.input_all ic))
      in
      let open Yojson.Safe.Util in
      check string "verdict" "fail" (eval_json |> member "verdict" |> to_string);
      check int "has regression" 1 (eval_json |> member "regressions" |> to_int);
      let report_md =
        In_channel.with_open_text (Filename.concat out_dir "report.md") In_channel.input_all
      in
      check bool "report markdown includes regression metric" true
        (Util.string_contains ~needle:"elapsed_s" report_md))

let test_eval_save_baseline_requires_case_for_multi_run () =
  let trace_a = Printf.sprintf "/tmp/oas_cli_case_a_%d.ndjson" (Unix.getpid ()) in
  let trace_b = Printf.sprintf "/tmp/oas_cli_case_b_%d.ndjson" (Unix.getpid ()) in
  let dataset_path = Printf.sprintf "/tmp/oas_cli_case_dataset_%d.jsonl" (Unix.getpid ()) in
  let baseline_path = Printf.sprintf "/tmp/oas_cli_case_baseline_%d.json" (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      remove_path trace_a;
      remove_path trace_b;
      remove_path dataset_path;
      remove_path baseline_path)
    (fun () ->
      write_trace_replay_file
        ~path:trace_a
        ~prompt:"Replay A"
        ~final_text:"done-a"
        ~finished_ts:1.5;
      write_trace_replay_file
        ~path:trace_b
        ~prompt:"Replay B"
        ~final_text:"done-b"
        ~finished_ts:1.6;
      Out_channel.with_open_text dataset_path (fun oc ->
        output_string oc
          (replay_case_json
             ~id:"replay-a"
             ~prompt:"Replay A"
             ~expected_text:"done-a"
             ~trace_path:trace_a);
        output_char oc '\n';
        output_string oc
          (replay_case_json
             ~id:"replay-b"
             ~prompt:"Replay B"
             ~expected_text:"done-b"
             ~trace_path:trace_b);
        output_char oc '\n');
      let status, output =
        run_command_capture_output
          (Printf.sprintf
             "%s eval save-baseline --dataset %s --out %s 2>&1"
             cli_exe dataset_path baseline_path)
      in
      (match status with
       | Unix.WEXITED 0 -> fail "expected save-baseline to fail without --case"
       | Unix.WEXITED _ -> ()
       | _ -> fail "expected normal process exit");
      check bool "mentions --case" true
        (Util.string_contains ~needle:"--case" output))

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
      test_case "run mixed dataset" `Quick test_eval_run_mixed_dataset;
      test_case "save baseline trace replay dataset" `Quick
        test_eval_save_baseline_trace_replay_dataset;
      test_case "run with baseline failure" `Quick
        test_eval_run_with_baseline_failure;
      test_case "save baseline requires case for multi-run" `Quick
        test_eval_save_baseline_requires_case_for_multi_run;
    ];
  ]
