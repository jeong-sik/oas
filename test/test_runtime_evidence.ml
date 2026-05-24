open Agent_sdk
open Alcotest

let mk_session ?(artifacts = []) () : Runtime.session =
  { session_id = "sess-evidence"
  ; goal = "collect telemetry"
  ; title = Some "Evidence"
  ; tag = Some "test"
  ; permission_mode = Some "default"
  ; phase = Runtime.Running
  ; created_at = 1.0
  ; updated_at = 2.0
  ; provider = Some "anthropic"
  ; model = Some "claude"
  ; system_prompt = None
  ; max_turns = 10
  ; workdir = Some "/tmp/work"
  ; planned_participants = [ "alice" ]
  ; participants = []
  ; artifacts
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let event seq kind : Runtime.event = { seq; ts = float_of_int seq; kind }

let participant_event
      ?summary
      ?provider
      ?model
      ?error
      ?raw_trace_run_id
      ?stop_reason
      ?completion_anomaly
      ?failure_cause
      participant_name
  : Runtime.participant_event
  =
  { participant_name
  ; summary
  ; provider
  ; model
  ; error
  ; raw_trace_run_id
  ; stop_reason
  ; completion_anomaly
  ; failure_cause
  }
;;

let all_event_kinds () =
  let open Runtime in
  let input_request : Runtime.input_request =
    { request_id = "input-1"
    ; participant_name = Some "alice"
    ; question = "Continue?"
    ; schema = Some (`Assoc [ "type", `String "string" ])
    ; timeout_s = Some 30.0
    ; created_at = 3.0
    }
  in
  [ Session_started { goal = "collect telemetry"; participants = [ "alice" ] }
  ; Session_settings_updated { model = Some "claude-opus"; permission_mode = Some "safe" }
  ; Turn_recorded { actor = Some "user"; message = "start" }
  ; Input_required input_request
  ; Input_provided
      { request_id = "input-1"
      ; participant_name = Some "alice"
      ; response = Input_answer (`String "yes")
      }
  ; Agent_spawn_requested
      { participant_name = "alice"
      ; role = Some "worker"
      ; prompt = "do work"
      ; provider = Some "anthropic"
      ; model = Some "claude"
      ; permission_mode = Some "safe"
      }
  ; Agent_became_live
      (participant_event
         ~summary:"ready"
         ~provider:"anthropic"
         ~model:"claude"
         ~raw_trace_run_id:"raw-1"
         ~stop_reason:"end_turn"
         "alice")
  ; Agent_output_delta { participant_name = "alice"; delta = "partial" }
  ; Agent_completed
      (participant_event
         ~summary:
           (Runtime_evidence.append_dropped_output_deltas_summary
              ~summary:"done"
              ~dropped_output_deltas:2)
         ~provider:"anthropic"
         ~model:"claude"
         ~raw_trace_run_id:"raw-2"
         ~stop_reason:"stop"
         "alice")
  ; Agent_completed
      (participant_event
         ~summary:"done with typed anomaly"
         ~completion_anomaly:(Dropped_output_deltas { count = 3 })
         "bob")
  ; Agent_failed
      (participant_event
         ~error:
           (Runtime_evidence.encode_persist_failure_detail
              ~phase:"append_event"
              "disk full")
         "alice")
  ; Agent_failed
      (participant_event
         ~failure_cause:
           (Persistence_failure { phase = "save_session"; detail = "denied" })
         "bob")
  ; Artifact_attached
      { artifact_id = "art-1"
      ; name = "report.json"
      ; kind = "json"
      ; mime_type = "application/json"
      ; path = "/tmp/report.json"
      ; size_bytes = 42
      }
  ; Checkpoint_saved { label = Some "cp1"; path = "/tmp/cp.json" }
  ; Finalize_requested { reason = Some "done" }
  ; Session_completed { outcome = Some "ok" }
  ; Session_failed { outcome = Some "failed" }
  ; Agent_failed
      (participant_event ~failure_cause:(Execution_error "tool failed") "charlie")
  ]
;;

let test_telemetry_report_covers_event_shapes () =
  let events =
    all_event_kinds () |> List.mapi (fun index kind -> event (index + 1) kind)
  in
  let report = Runtime_evidence.build_telemetry_report (mk_session ()) events in
  check int "step count" (List.length events) report.step_count;
  check int "dropped deltas" 5 report.dropped_output_deltas;
  check int "persistence failures" 2 report.persistence_failure_count;
  check
    (Alcotest.list Alcotest.string)
    "dropped participants"
    [ "alice"; "bob" ]
    report.participants_with_dropped_output_deltas;
  check
    (Alcotest.list Alcotest.string)
    "persistence participants"
    [ "alice"; "bob" ]
    report.participants_with_persistence_failures;
  check int "agent_failed count" 3 (List.assoc "agent_failed" report.event_name_counts);
  check
    int
    "agent_completed count"
    2
    (List.assoc "agent_completed" report.event_name_counts);
  let json = Runtime_evidence.telemetry_report_to_json report in
  let markdown = Runtime_evidence.telemetry_report_to_markdown report in
  check bool "json has steps" true Yojson.Safe.Util.(json |> member "steps" <> `Null);
  check bool "markdown mentions anomalies" true (String.contains markdown 'A')
;;

let test_evidence_bundle_collects_files_and_missing () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-runtime-evidence-%d" (Unix.getpid ()))
  in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" dir)))
    (fun () ->
       let present = Filename.concat dir "present.txt" in
       Runtime_store.save_text present "payload" |> Result.get_ok;
       let missing = Filename.concat dir "missing.txt" in
       let bundle =
         Runtime_evidence.build_evidence_bundle
           ~session_id:"sess-evidence"
           [ "present", present; "missing", missing ]
       in
       check int "files" 1 (List.length bundle.files);
       check int "missing" 1 (List.length bundle.missing_files);
       let file = List.hd bundle.files in
       check string "label" "present" file.label;
       check int "size" 7 file.size_bytes;
       check bool "md5" true (String.length file.md5 = 32);
       let json = Runtime_evidence.evidence_bundle_to_json bundle in
       check
         string
         "session"
         "sess-evidence"
         Yojson.Safe.Util.(json |> member "session_id" |> to_string))
;;

let test_file_specs_and_artifact_event () =
  let store = { Runtime_store.root = "/tmp/oas-store" } in
  let specs = Runtime_evidence.base_evidence_file_specs store "sess-1" in
  check int "base spec count" 6 (List.length specs);
  let artifact : Runtime.artifact =
    { artifact_id = "art-2"
    ; name = "out.txt"
    ; kind = "text"
    ; mime_type = "text/plain"
    ; path = None
    ; inline_content = Some "inline"
    ; size_bytes = 6
    ; created_at = 5.0
    }
  in
  match Runtime_evidence.artifact_attached_event artifact with
  | Runtime.Artifact_attached detail ->
    check string "artifact path default" "" detail.path;
    check string "artifact name" "out.txt" detail.name
  | _ -> Alcotest.fail "expected artifact event"
;;

let test_raw_trace_manifest_json () =
  let manifest =
    Runtime_evidence.build_raw_trace_manifest
      ~session_id:"sess-evidence"
      ~latest_raw_trace_run:None
      ~raw_trace_runs:[]
      ~raw_trace_summaries:[]
      ~raw_trace_validations:[]
  in
  let json = Runtime_evidence.raw_trace_manifest_to_json manifest in
  check
    string
    "manifest session"
    "sess-evidence"
    Yojson.Safe.Util.(json |> member "session_id" |> to_string)
;;

let write_artifact store session_id ~artifact_id ~name ~kind ~created_at content =
  let path =
    Runtime_store.save_artifact_text store session_id ~name ~kind ~content
    |> Result.get_ok
  in
  ({ Runtime.artifact_id
   ; name
   ; kind
   ; mime_type = Artifact_service.mime_type_of_kind kind
   ; path = Some path
   ; inline_content = None
   ; size_bytes = String.length content
   ; created_at
   }
   : Runtime.artifact)
;;

let test_sessions_store_decodes_runtime_artifacts () =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-sessions-parsers-%d" (Unix.getpid ()))
  in
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () ->
       let store = Runtime_store.create ~root () |> Result.get_ok in
       let session_id = "sess-evidence" in
       let telemetry_json =
         {|{
  "session_id":"sess-evidence",
  "generated_at":10.0,
  "step_count":1,
  "event_counts":{"Agent_completed":1},
  "event_name_counts":[{"event_name":"agent_completed","count":1}],
  "steps":[{"seq":1,"ts":10.1,"kind":"Agent_completed","participant":"alice","detail":"done","actor":"agent","role":"worker","provider":"openai","model":"gpt","raw_trace_run_id":"run-1","stop_reason":"stop","artifact_id":"art-telemetry","artifact_name":"runtime-telemetry-json","artifact_kind":"json","checkpoint_label":"cp","outcome":"ok","dropped_output_deltas":2,"persistence_failure_phase":"append_event"}]
}|}
       in
       let evidence_json =
         {|{"session_id":"sess-evidence","generated_at":11.0,"files":[{"label":"report","path":"/tmp/report.json","size_bytes":12,"md5":"0123456789abcdef0123456789abcdef"}],"missing_files":[{"label":"proof","path":"/tmp/proof.json"}]}|}
       in
       let manifest =
         Runtime_evidence.build_raw_trace_manifest
           ~session_id
           ~latest_raw_trace_run:None
           ~raw_trace_runs:[]
           ~raw_trace_summaries:[]
           ~raw_trace_validations:[]
       in
       let raw_trace_json =
         Runtime_evidence.raw_trace_manifest_to_json manifest
         |> Yojson.Safe.pretty_to_string
       in
       let tool_catalog_json =
         {|[
  {
    "name":"shell",
    "description":"Run a command",
    "origin":"runtime",
    "kind":"local",
    "shell":{"single_command_only":true,"shell_metacharacters_allowed":false,"chaining_allowed":false,"redirection_allowed":false,"pipes_allowed":true,"workdir_policy":"required"},
    "notes":["audit"],
    "examples":["ls"]
  },
  {
    "name":"plain",
    "description":"No shell constraints",
    "notes":[],
    "examples":[]
  }
]|}
       in
       let artifacts =
         [ write_artifact
             store
             session_id
             ~artifact_id:"art-telemetry"
             ~name:"runtime-telemetry-json"
             ~kind:"json"
             ~created_at:1.0
             telemetry_json
         ; write_artifact
             store
             session_id
             ~artifact_id:"art-evidence"
             ~name:"runtime-evidence"
             ~kind:"json"
             ~created_at:2.0
             evidence_json
         ; write_artifact
             store
             session_id
             ~artifact_id:"art-raw-trace"
             ~name:"runtime-raw-trace-json"
             ~kind:"json"
             ~created_at:3.0
             raw_trace_json
         ; write_artifact
             store
             session_id
             ~artifact_id:"art-tool-catalog"
             ~name:"tool-catalog"
             ~kind:"json"
             ~created_at:4.0
             tool_catalog_json
         ]
       in
       Runtime_store.save_session store (mk_session ~artifacts ()) |> Result.get_ok;
       let telemetry =
         Sessions_store.get_telemetry ~session_root:root ~session_id () |> Result.get_ok
       in
       check int "legacy event counts" 1 (List.length telemetry.event_counts);
       let structured =
         Sessions_store.get_telemetry_structured ~session_root:root ~session_id ()
         |> Result.get_ok
       in
       check
         string
         "structured event name"
         "agent_completed"
         (List.hd structured.event_counts).event_name;
       let step = List.hd structured.steps in
       check (option string) "step provider" (Some "openai") step.provider;
       check (option int) "dropped deltas" (Some 2) step.dropped_output_deltas;
       let evidence =
         Sessions_store.get_evidence ~session_root:root ~session_id () |> Result.get_ok
       in
       check int "evidence files" 1 (List.length evidence.files);
       check int "missing files" 1 (List.length evidence.missing_files);
       let raw_trace =
         Sessions_store.get_raw_trace_manifest ~session_root:root ~session_id ()
         |> Result.get_ok
       in
       check string "raw trace manifest" session_id raw_trace.session_id;
       let tools =
         Sessions_store.get_tool_catalog ~session_root:root ~session_id ()
         |> Result.get_ok
       in
       check int "tool catalog" 2 (List.length tools);
       let shell_tool = List.hd tools in
       check string "tool name" "shell" shell_tool.name;
       match shell_tool.shell with
       | Some shell ->
         check bool "single command" true shell.Tool.single_command_only;
         check bool "workdir policy" true (shell.Tool.workdir_policy = Some Tool.Required)
       | None -> fail "expected shell constraints")
;;

let () =
  Alcotest.run
    "runtime_evidence"
    [ ( "telemetry"
      , [ Alcotest.test_case
            "event shapes and anomalies"
            `Quick
            test_telemetry_report_covers_event_shapes
        ] )
    ; ( "bundle"
      , [ Alcotest.test_case
            "files and missing files"
            `Quick
            test_evidence_bundle_collects_files_and_missing
        ; Alcotest.test_case
            "file specs and artifact event"
            `Quick
            test_file_specs_and_artifact_event
        ; Alcotest.test_case "raw trace manifest json" `Quick test_raw_trace_manifest_json
        ; Alcotest.test_case
            "sessions store artifact decoders"
            `Quick
            test_sessions_store_decodes_runtime_artifacts
        ] )
    ]
;;
