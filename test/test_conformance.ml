open Agent_sdk

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-conformance-test-%d-%06x" (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let unwrap = function
  | Ok value -> value
  | Error err -> Alcotest.fail (Error.to_string err)

let with_runtime_client ~session_root f =
  let client =
    unwrap
      (Runtime_client.connect
         ~options:
           {
             Runtime_client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
           }
         ())
  in
  Fun.protect ~finally:(fun () -> Runtime_client.close client) (fun () -> f client)

let wait_until_session ~timeout_s fetch =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let (session : Runtime.session) = fetch () in
    let has_terminal_participant =
      session.Runtime.participants
      |> List.exists (fun (participant : Runtime.participant) ->
             participant.state = Runtime.Done
             || participant.state = Runtime.Failed_participant)
    in
    if has_terminal_participant then session
    else if Unix.gettimeofday () >= deadline then session
    else (
      Thread.delay 0.02;
      loop ())
  in
  loop ()

let start_mock_runtime_session client ~session_id =
  let start_request =
    Runtime.
      {
        session_id = Some session_id;
        goal = "Check harness conformance";
        participants = [ "reviewer" ];
        provider = Some "mock";
        model = Some "qwen3.5";
        permission_mode = Some "default";
        system_prompt = None;
        max_turns = Some 2;
        workdir = None;
      }
  in
  let session = unwrap (Runtime_client.start_session client start_request) in
  ignore
    (unwrap
       (Runtime_client.apply_command client ~session_id:session.session_id
          (Runtime.Spawn_agent
             {
               participant_name = "reviewer";
               role = Some "reviewer";
               prompt = "Review the change.";
               provider = Some "mock";
               model = Some "qwen3.5";
               system_prompt = None;
               max_turns = Some 1;
             })));
  ignore
    (wait_until_session ~timeout_s:1.0 (fun () ->
         unwrap (Runtime_client.status client ~session_id)))

let finalize_runtime_session client ~session_id =
  unwrap (Runtime_client.finalize client ~session_id ())

let test_conformance_run_reports_ok () =
  with_temp_dir @@ fun session_root ->
  let session_id = "sess-conformance" in
  with_runtime_client ~session_root @@ fun client ->
  start_mock_runtime_session client ~session_id;
  ignore (finalize_runtime_session client ~session_id);
  let report = unwrap (Conformance.run ~session_root ~session_id ()) in
  Alcotest.(check bool) "report ok" true report.ok;
  Alcotest.(check int) "worker run count" 1 report.summary.worker_run_count;
  Alcotest.(check int) "raw trace run count" 1 report.summary.raw_trace_run_count;
  Alcotest.(check (option string)) "latest worker agent"
    (Some "reviewer") report.summary.latest_worker_agent_name;
  Alcotest.(check (option string)) "latest worker status"
    (Some "completed") report.summary.latest_worker_status;
  Alcotest.(check (option string)) "latest resolved provider"
    (Some "mock") report.summary.latest_resolved_provider

let test_conformance_report_detects_inconsistent_bundle () =
  with_temp_dir @@ fun session_root ->
  let session_id = "sess-conformance-bad" in
  with_runtime_client ~session_root @@ fun client ->
  start_mock_runtime_session client ~session_id;
  ignore (finalize_runtime_session client ~session_id);
  let bundle = unwrap (Sessions.get_proof_bundle ~session_root ~session_id ()) in
  let broken =
    { bundle with raw_trace_run_count = bundle.raw_trace_run_count + 1 }
  in
  let report = Conformance.report broken in
  Alcotest.(check bool) "report not ok" false report.ok;
  Alcotest.(check bool) "raw trace shape check failed" true
    (List.exists
       (fun (check : Conformance.check) ->
         check.name = "raw_trace_shapes_consistent"
         && String.equal check.code "raw_trace_shape_mismatch"
         && not check.passed)
       report.checks);
  Alcotest.(check bool) "direct evidence code present" true
    (List.exists
       (fun (check : Conformance.check) ->
         String.equal check.code "direct_evidence_incomplete")
       report.checks)

let () =
  let open Alcotest in
  run "Conformance"
    [
      ( "session",
        [
          test_case "run reports ok" `Quick test_conformance_run_reports_ok;
          test_case "detects inconsistent bundle" `Quick
            test_conformance_report_detects_inconsistent_bundle;
        ] );
    ]
