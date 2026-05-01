open Base
open Agent_sdk

let default_runtime_path () =
  Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"
;;

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> default_runtime_path ()
;;

let session_root () =
  match Sys.getenv_opt "OAS_CONFORMANCE_SESSION_ROOT" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-conformance-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
;;

let unwrap_result = function
  | Ok value -> value
  | Error err -> failwith (Error.to_string err)
;;

let gather_until_terminal client =
  let rec loop acc remaining =
    let batch = Client.wait_for_messages ~timeout:0.1 client in
    let acc = acc @ batch in
    let terminal =
      List.exists
        (function
          | Client.Session_events events ->
            List.exists
              (function
                | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ }
                  -> true
                | _ -> false)
              events
          | _ -> false)
        acc
    in
    if terminal || remaining <= 0 then acc else loop acc (remaining - 1)
  in
  loop [] 30
;;

let trace_capability_to_json capability =
  `String
    (match capability with
     | Sessions.Raw -> "raw"
     | Sessions.Summary_only -> "summary_only"
     | Sessions.No_trace -> "none")
;;

let check_to_json (check : Conformance.check) =
  `Assoc
    [ "code", `String check.code
    ; "name", `String check.name
    ; "passed", `Bool check.passed
    ; ( "detail"
      , match check.detail with
        | Some value -> `String value
        | None -> `Null )
    ]
;;

let report_to_json (report : Conformance.report) =
  `Assoc
    [ "ok", `Bool report.ok
    ; ( "summary"
      , `Assoc
          [ "session_id", `String report.summary.session_id
          ; "generated_at", `Float report.summary.generated_at
          ; "worker_run_count", `Int report.summary.worker_run_count
          ; "raw_trace_run_count", `Int report.summary.raw_trace_run_count
          ; "validated_worker_run_count", `Int report.summary.validated_worker_run_count
          ; ( "latest_accepted_worker_run_id"
            , match report.summary.latest_accepted_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_ready_worker_run_id"
            , match report.summary.latest_ready_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_running_worker_run_id"
            , match report.summary.latest_running_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_worker_run_id"
            , match report.summary.latest_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_completed_worker_run_id"
            , match report.summary.latest_completed_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_worker_agent_name"
            , match report.summary.latest_worker_agent_name with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_worker_validated"
            , match report.summary.latest_worker_validated with
              | Some value -> `Bool value
              | None -> `Null )
          ; ( "latest_failed_worker_run_id"
            , match report.summary.latest_failed_worker_run_id with
              | Some value -> `String value
              | None -> `Null )
          ; ( "latest_failure_reason"
            , match report.summary.latest_failure_reason with
              | Some value -> `String value
              | None -> `Null )
          ; ( "trace_capabilities"
            , `List (List.map trace_capability_to_json report.summary.trace_capabilities)
            )
          ] )
    ; "checks", `List (List.map check_to_json report.checks)
    ]
;;

let () =
  Random.self_init ();
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let root = session_root () in
  let client =
    unwrap_result
      (Client.connect
         ~sw
         ~mgr
         ~options:
           { Client.default_options with
             runtime_path = Some (runtime_path ())
           ; session_root = Some root
           ; provider = Some "mock"
           ; include_partial_messages = true
           ; agents =
               [ ( "conformance-worker"
                 , { Client.description = "conformance-worker"
                   ; prompt = "Produce a short deterministic summary."
                   ; tools = None
                   ; model = None
                   } )
               ]
           }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Client.close client)
    (fun () ->
       unwrap_result (Client.query client "Create conformance materials.");
       ignore (gather_until_terminal client);
       unwrap_result (Client.finalize client ());
       let session_id =
         match Client.current_session_id client with
         | Some value -> value
         | None ->
           (match Sessions.list_sessions ~session_root:root () |> unwrap_result with
            | info :: _ -> info.Sessions.session_id
            | [] -> failwith "No persisted sessions found")
       in
       let report = unwrap_result (Conformance.run ~session_root:root ~session_id ()) in
       let output =
         `Assoc
           [ "session_root", `String root
           ; "session_id", `String session_id
           ; "conformance", report_to_json report
           ]
       in
       print_endline (Yojson.Safe.pretty_to_string output))
;;
