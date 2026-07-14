open Agent_sdk
open Alcotest

let mk_session
      ?(session_id = "sess-evidence")
      ?(goal = "collect telemetry")
      ?(title = Some "Evidence")
      ?(tag = Some "test")
      ?(updated_at = 2.0)
      ?(participants = [])
      ?(artifacts = [])
      ()
  : Runtime.session
  =
  { session_id
  ; goal
  ; title
  ; tag
  ; phase = Runtime.Running
  ; created_at = 1.0
  ; updated_at
  ; provider = Some "anthropic"
  ; model = Some "claude"
  ; system_prompt = None
  ; workdir = Some "/tmp/work"
  ; planned_participants = [ "alice" ]
  ; participants
  ; artifacts
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let event seq kind : Runtime.event = { seq; ts = float_of_int seq; kind }

let participant_common ?summary ?provider ?model ?raw_trace_run_id participant_name
  : Runtime.participant_event_common
  =
  { participant_name; summary; provider; model; raw_trace_run_id }
;;

let participant_live ?summary ?provider ?model ?raw_trace_run_id participant_name
  : Runtime.participant_live_event
  =
  { participant =
      participant_common ?summary ?provider ?model ?raw_trace_run_id participant_name
  }
;;

let participant_completed
      ?summary
      ?provider
      ?model
      ?raw_trace_run_id
      ?stop_reason
      ?completion_anomaly
      participant_name
  : Runtime.participant_completed_event
  =
  { participant =
      participant_common ?summary ?provider ?model ?raw_trace_run_id participant_name
  ; stop_reason
  ; completion_anomaly
  }
;;

let participant_failed
      ?summary
      ?provider
      ?model
      ?raw_trace_run_id
      ~failure_cause
      participant_name
  : Runtime.participant_failed_event
  =
  { participant =
      participant_common ?summary ?provider ?model ?raw_trace_run_id participant_name
  ; failure_cause
  }
;;

let valid_dropped_output_deltas count =
  match Runtime.dropped_output_deltas ~count with
  | Ok anomaly -> anomaly
  | Error error -> fail (Runtime.show_completion_anomaly_error error)
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
  ; Session_settings_updated { model = Some "claude-opus" }
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
      }
  ; Agent_became_live
      (participant_live
         ~summary:"ready"
         ~provider:"anthropic"
         ~model:"claude"
         ~raw_trace_run_id:"raw-1"
         "alice")
  ; Agent_output_delta
      { participant_name = "alice"; delta = "partial"; raw_trace_run_id = Some "raw-1" }
  ; Agent_completed
      (participant_completed
         ~summary:"done"
         ~completion_anomaly:(valid_dropped_output_deltas 2)
         ~provider:"anthropic"
         ~model:"claude"
         ~raw_trace_run_id:"raw-2"
         ~stop_reason:"stop"
         "alice")
  ; Agent_completed
      (participant_completed
         ~summary:"done with typed anomaly"
         ~completion_anomaly:(valid_dropped_output_deltas 3)
         "bob")
  ; Agent_failed
      (participant_failed
         ~failure_cause:
           (Persistence_failure { phase = "append_event"; detail = "disk full" })
         "alice")
  ; Agent_failed
      (participant_failed
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
      (participant_failed ~failure_cause:(Execution_error "tool failed") "charlie")
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
  let execution_failure_step =
    report.steps
    |> List.find_opt (fun step ->
      Option.equal String.equal step.Runtime_evidence.participant (Some "charlie"))
  in
  (match execution_failure_step with
   | Some step ->
     check (option string) "typed failure detail" (Some "tool failed") step.detail
   | None -> fail "missing execution failure step");
  let json = Runtime_evidence.telemetry_report_to_json report in
  let markdown = Runtime_evidence.telemetry_report_to_markdown report in
  check bool "json has steps" true Yojson.Safe.Util.(json |> member "steps" <> `Null);
  let output_delta_step =
    report.steps
    |> List.find_opt (fun step ->
      String.equal step.Runtime_evidence.event_name "agent_output_delta")
  in
  (match output_delta_step with
   | Some step ->
     check (option string) "delta raw trace run id" (Some "raw-1") step.raw_trace_run_id
   | None -> fail "missing output delta step");
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

let with_temp_root prefix f =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)
;;

let participant ?(aliases = []) name : Runtime.participant =
  { name
  ; role = Some "worker"
  ; aliases
  ; worker_id = None
  ; runtime_actor = None
  ; requested_provider = None
  ; requested_model = None
  ; provider = None
  ; model = None
  ; resolved_provider = None
  ; resolved_model = None
  ; state = Runtime.Planned
  ; summary = None
  ; accepted_at = None
  ; ready_at = None
  ; first_progress_at = None
  ; started_at = None
  ; finished_at = None
  ; last_progress_at = None
  ; last_error = None
  }
;;

let check_error label result =
  match result with
  | Ok _ -> fail (label ^ ": expected error")
  | Error _ -> ()
;;

let test_sessions_store_helpers_listing_and_mutation () =
  with_temp_root "oas-sessions-store"
  @@ fun root ->
  let older : Runtime.artifact =
    { artifact_id = "old"
    ; name = "report"
    ; kind = "json"
    ; mime_type = "application/json"
    ; path = None
    ; inline_content = Some "{}"
    ; size_bytes = 2
    ; created_at = 1.0
    }
  in
  let newer = { older with artifact_id = "new"; created_at = 3.0 } in
  let other =
    { older with artifact_id = "other"; name = "telemetry"; created_at = 5.0 }
  in
  check (option string) "primary alias empty" None (Sessions_store.primary_alias []);
  check
    (option string)
    "primary alias blank"
    None
    (Sessions_store.primary_alias [ "  "; "later" ]);
  check
    (option string)
    "primary alias first"
    (Some " coder ")
    (Sessions_store.primary_alias [ " coder "; "fallback" ]);
  (match Sessions_store.latest_named_artifact [ older; other; newer ] "report" with
   | Some artifact -> check string "latest artifact" "new" artifact.artifact_id
   | None -> fail "missing latest artifact");
  check
    (option string)
    "missing latest artifact"
    None
    (Option.map
       (fun (artifact : Runtime.artifact) -> artifact.artifact_id)
       (Sessions_store.latest_named_artifact [ older; newer ] "missing"));
  let store = Runtime_store.create ~root () |> Result.get_ok in
  let alice = participant ~aliases:[ "a"; "coder" ] "alice" in
  Runtime_store.save_session
    store
    (mk_session
       ~session_id:"sess-a"
       ~goal:"first"
       ~title:(Some "First")
       ~tag:(Some "alpha")
       ~updated_at:10.0
       ~participants:[ alice ]
       ())
  |> Result.get_ok;
  Runtime_store.save_session
    store
    (mk_session
       ~session_id:"sess-b"
       ~goal:"second"
       ~title:None
       ~tag:None
       ~updated_at:11.0
       ())
  |> Result.get_ok;
  let corrupt_dir = Runtime_store.session_dir store "sess-corrupt" in
  Runtime_store.ensure_dir corrupt_dir |> Result.get_ok;
  Runtime_store.save_text (Runtime_store.session_path store "sess-corrupt") "{broken"
  |> Result.get_ok;
  Runtime_store.save_text
    (Filename.concat (Runtime_store.sessions_dir store) "not-a-dir")
    ""
  |> Result.get_ok;
  let sessions = Sessions_store.list_sessions ~session_root:root () |> Result.get_ok in
  check
    (list string)
    "valid sessions only"
    [ "sess-a"; "sess-b" ]
    (List.map (fun (info : Sessions.session_info) -> info.session_id) sessions);
  let first = List.hd sessions in
  check int "participant count" 1 first.participant_count;
  check string "session path" (Runtime_store.session_path store "sess-a") first.path;
  Sessions_store.rename_session ~session_root:root ~session_id:"sess-a" ~title:"   " ()
  |> Result.get_ok;
  Sessions_store.tag_session
    ~session_root:root
    ~session_id:"sess-a"
    ~tag:(Some "  stable  ")
    ()
  |> Result.get_ok;
  let renamed = Runtime_store.load_session store "sess-a" |> Result.get_ok in
  check (option string) "blank title clears" None renamed.title;
  check (option string) "trimmed tag" (Some "stable") renamed.tag;
  Sessions_store.tag_session ~session_root:root ~session_id:"sess-a" ~tag:None ()
  |> Result.get_ok;
  let untagged = Runtime_store.load_session store "sess-a" |> Result.get_ok in
  check (option string) "none tag clears" None untagged.tag;
  check
    int
    "events missing"
    0
    (Sessions_store.get_session_events ~session_root:root "sess-a"
     |> Result.get_ok
     |> List.length);
  check_error
    "missing named artifact"
    (Sessions_store.get_named_artifact
       ~session_root:root
       ~session_id:"sess-a"
       ~name:"missing"
       ());
  check
    int
    "tool catalog absent"
    0
    (Sessions_store.get_tool_catalog ~session_root:root ~session_id:"sess-a" ()
     |> Result.get_ok
     |> List.length)
;;

let test_sessions_store_raw_trace_files_and_hooks () =
  with_temp_root "oas-sessions-store-raw"
  @@ fun root ->
  check
    (list string)
    "missing raw trace dir"
    []
    (Sessions_store.get_raw_trace_files ~session_root:root ~session_id:"sess-hooks" ()
     |> Result.get_ok);
  let store = Runtime_store.create ~root () |> Result.get_ok in
  Runtime_store.ensure_tree store "sess-hooks" |> Result.get_ok;
  let raw_dir =
    Sessions_store.get_raw_trace_dir ~session_root:root ~session_id:"sess-hooks" ()
    |> Result.get_ok
  in
  let trace_path = Filename.concat raw_dir "hook_worker.jsonl" in
  let older_trace_path = Filename.concat raw_dir "zz_older_hook_worker.jsonl" in
  let record
        ?prompt
        ?model
        ?hook_name
        ?hook_decision
        ?hook_detail
        ?final_text
        ?stop_reason
        seq
        record_type
    : Raw_trace.record
    =
    { trace_version = Raw_trace.trace_version
    ; worker_run_id = "wr-hooks"
    ; seq
    ; ts = float_of_int seq
    ; agent_name = "hook worker"
    ; session_id = Some "sess-hooks"
    ; record_type
    ; prompt
    ; model
    ; tool_choice = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; reasoning_effort = None
    ; block_index = None
    ; block_kind = None
    ; assistant_block = None
    ; tool_use_id = None
    ; tool_name = None
    ; tool_input = None
    ; tool_planned_index = None
    ; tool_batch_index = None
    ; tool_batch_size = None
    ; tool_execution_mode = None
    ; tool_result = None
    ; tool_error = None
    ; hook_name
    ; hook_decision
    ; hook_detail
    ; final_text
    ; stop_reason
    ; error = None
    }
  in
  [ record ~prompt:"collect hooks" ~model:"model-hook" 1 Raw_trace.Run_started
  ; record
      ~hook_name:"pre_tool"
      ~hook_decision:"allow"
      ~hook_detail:"initial"
      2
      Raw_trace.Hook_invoked
  ; record
      ~hook_name:"pre_tool"
      ~hook_decision:"deny"
      ~hook_detail:"latest"
      3
      Raw_trace.Hook_invoked
  ; record ~hook_name:"post_tool" ~hook_decision:"allow" 4 Raw_trace.Hook_invoked
  ; record ~final_text:"done" ~stop_reason:"stop" 5 Raw_trace.Run_finished
  ]
  |> List.map (fun record -> Raw_trace.record_to_json record |> Yojson.Safe.to_string)
  |> String.concat "\n"
  |> fun raw ->
  Runtime_store.save_text trace_path (raw ^ "\n") |> Result.get_ok;
  [ record
      ~hook_name:"pre_tool"
      ~hook_decision:"allow"
      ~hook_detail:"older-other-file"
      1
      Raw_trace.Hook_invoked
  ]
  |> List.map (fun record -> Raw_trace.record_to_json record |> Yojson.Safe.to_string)
  |> String.concat "\n"
  |> fun raw ->
  Runtime_store.save_text older_trace_path (raw ^ "\n") |> Result.get_ok;
  Runtime_store.save_text (Filename.concat raw_dir "ignore.txt") "ignored"
  |> Result.get_ok;
  let files =
    Sessions_store.get_raw_trace_files ~session_root:root ~session_id:"sess-hooks" ()
    |> Result.get_ok
  in
  check int "jsonl files only" 2 (List.length files);
  check string "trace file path" trace_path (List.hd files);
  let summaries =
    Sessions_store.get_hook_summary ~session_root:root ~session_id:"sess-hooks" ()
    |> Result.get_ok
  in
  check
    (list string)
    "hook summary sorted"
    [ "post_tool"; "pre_tool" ]
    (List.map (fun (summary : Sessions.hook_summary) -> summary.hook_name) summaries);
  let pre_tool =
    List.find
      (fun (summary : Sessions.hook_summary) -> String.equal summary.hook_name "pre_tool")
      summaries
  in
  check int "pre_tool count" 3 pre_tool.count;
  check (option string) "latest decision" (Some "deny") pre_tool.latest_decision;
  check (option string) "latest detail" (Some "latest") pre_tool.latest_detail;
  check bool "latest timestamp" true (Option.is_some pre_tool.latest_ts);
  let latest =
    Sessions_store.get_latest_raw_trace_run ~session_root:root ~session_id:"sess-hooks" ()
    |> Result.get_ok
  in
  check
    (option string)
    "latest run"
    (Some "wr-hooks")
    (Option.map (fun (run : Raw_trace.run_ref) -> run.worker_run_id) latest);
  check_error
    "missing raw trace run"
    (Sessions_store.get_raw_trace_run
       ~session_root:root
       ~session_id:"sess-hooks"
       ~worker_run_id:"missing"
       ());
  check
    int
    "summarize empty"
    0
    (Sessions_store.summarize_runs [] |> Result.get_ok |> List.length);
  check
    int
    "validate empty"
    0
    (Sessions_store.validate_runs [] |> Result.get_ok |> List.length)
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
    "execution_mode":"serial"
  },
  {
    "name":"plain",
    "description":"A concurrently callable tool",
    "origin":null,
    "execution_mode":"concurrent"
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
       check bool "serial" true (shell_tool.execution_mode = Tool.Serial);
       let plain_tool = List.nth tools 1 in
       check bool "concurrent" true (plain_tool.execution_mode = Tool.Concurrent))
;;

let test_get_tool_catalog_malformed_returns_error () =
  with_temp_root "oas-tool-catalog-malformed"
  @@ fun root ->
  let store = Runtime_store.create ~root () |> Result.get_ok in
  let session_id = "sess-tc" in
  (* Well-formed JSON that is NOT a list: [to_list] (and [tool_contract_of_json])
     raise Type_error. get_tool_catalog returns a result, so the decode failure
     must surface as Error rather than escaping as an uncaught exception. The
     artifact must be attached to the session (via save_session) so the named
     lookup reaches the decode path rather than failing earlier. *)
  let artifact =
    write_artifact
      store
      session_id
      ~artifact_id:"art-tool-catalog"
      ~name:"tool-catalog"
      ~kind:"json"
      ~created_at:1.0
      {|{"not":"a list"}|}
  in
  Runtime_store.save_session store (mk_session ~session_id ~artifacts:[ artifact ] ())
  |> Result.get_ok;
  check_error
    "malformed tool catalog returns Error (not raise)"
    (Sessions_store.get_tool_catalog ~session_root:root ~session_id ())
;;

let test_get_tool_catalog_rejects_removed_fields () =
  with_temp_root "oas-tool-catalog-removed-field"
  @@ fun root ->
  let store = Runtime_store.create ~root () |> Result.get_ok in
  let session_id = "sess-tc-removed" in
  let artifact =
    write_artifact
      store
      session_id
      ~artifact_id:"art-tool-catalog"
      ~name:"tool-catalog"
      ~kind:"json"
      ~created_at:1.0
      {|[{"name":"shell","description":"Run","origin":null,"execution_mode":"serial","kind":"local"}]|}
  in
  Runtime_store.save_session store (mk_session ~session_id ~artifacts:[ artifact ] ())
  |> Result.get_ok;
  match Sessions_store.get_tool_catalog ~session_root:root ~session_id () with
  | Ok _ -> fail "removed tool contract field must be rejected"
  | Error error ->
    check
      bool
      "error names removed field"
      true
      (Util.string_contains ~needle:"kind" (Error.to_string error))
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
            "sessions store helpers listing mutation"
            `Quick
            test_sessions_store_helpers_listing_and_mutation
        ; Alcotest.test_case
            "sessions store raw trace hooks"
            `Quick
            test_sessions_store_raw_trace_files_and_hooks
        ; Alcotest.test_case
            "sessions store artifact decoders"
            `Quick
            test_sessions_store_decodes_runtime_artifacts
        ; Alcotest.test_case
            "tool catalog malformed returns error"
            `Quick
            test_get_tool_catalog_malformed_returns_error
        ; Alcotest.test_case
            "tool catalog removed fields rejected"
            `Quick
            test_get_tool_catalog_rejects_removed_fields
        ] )
    ]
;;
