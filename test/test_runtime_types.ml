(** Roundtrip tests for Runtime ppx-generated yojson/show functions.
    Targets the ~270 uncovered points in runtime.ml from ppx codegen. *)

open Agent_sdk

let roundtrip
    (type a)
    ~(to_yojson : a -> Yojson.Safe.t)
    ~(of_yojson : Yojson.Safe.t -> a Ppx_deriving_yojson_runtime.error_or)
    ~(show : a -> string)
    ~name
    (value : a) =
  let json = to_yojson value in
  match of_yojson json with
  | Ok decoded ->
    Alcotest.(check string) (name ^ " roundtrip")
      (show value) (show decoded)
  | Error msg ->
    Alcotest.fail (Printf.sprintf "%s: of_yojson failed: %s" name msg)

let test_phase () =
  List.iter (fun v ->
    roundtrip
      ~to_yojson:Runtime.phase_to_yojson
      ~of_yojson:Runtime.phase_of_yojson
      ~show:Runtime.show_phase
      ~name:(Runtime.show_phase v) v
  ) Runtime.[Bootstrapping; Running; Waiting_on_workers;
             Finalizing; Completed; Failed; Cancelled]

let test_participant_state () =
  List.iter (fun v ->
    roundtrip
      ~to_yojson:Runtime.participant_state_to_yojson
      ~of_yojson:Runtime.participant_state_of_yojson
      ~show:Runtime.show_participant_state
      ~name:(Runtime.show_participant_state v) v
  ) Runtime.[Planned; Starting; Live; Idle; Done; Failed_participant; Detached]

let test_participant () =
  let v : Runtime.participant = {
    name = "worker-1"; role = Some "executor";
    aliases = ["w1"; "exec"]; worker_id = Some "wid-1";
    runtime_actor = Some "ra-1";
    requested_provider = Some "anthropic";
    requested_model = Some "sonnet-4-6";
    requested_policy = Some "best";
    provider = Some "anthropic"; model = Some "sonnet-4-6";
    resolved_provider = Some "anthropic";
    resolved_model = Some "sonnet-4-6-20250514";
    state = Runtime.Live;
    summary = Some "running well";
    accepted_at = Some 1.7e9; ready_at = Some 1.7e9;
    first_progress_at = Some 1.7e9; started_at = Some 1.7e9;
    finished_at = None; last_progress_at = Some 1.7e9;
    last_error = None;
  } in
  roundtrip
    ~to_yojson:Runtime.participant_to_yojson
    ~of_yojson:Runtime.participant_of_yojson
    ~show:Runtime.show_participant
    ~name:"participant" v

let test_artifact () =
  let v : Runtime.artifact = {
    artifact_id = "art-1"; name = "result.json";
    kind = "json"; mime_type = "application/json";
    path = Some "/tmp/result.json";
    inline_content = Some "{}";
    size_bytes = 2; created_at = 1.7e9;
  } in
  roundtrip
    ~to_yojson:Runtime.artifact_to_yojson
    ~of_yojson:Runtime.artifact_of_yojson
    ~show:Runtime.show_artifact
    ~name:"artifact" v

let test_session () =
  let v : Runtime.session = {
    session_id = "sess-rt"; goal = "test runtime";
    title = Some "Runtime Test"; tag = Some "test";
    permission_mode = Some "default";
    phase = Runtime.Running;
    created_at = 1.7e9; updated_at = 1.7e9;
    provider = Some "anthropic"; model = Some "sonnet-4-6";
    system_prompt = Some "You are helpful.";
    max_turns = 10; workdir = Some "/tmp/work";
    planned_participants = ["agent-1"];
    participants = [{
      name = "agent-1"; role = None; aliases = [];
      worker_id = None; runtime_actor = None;
      requested_provider = None; requested_model = None;
      requested_policy = None; provider = None; model = None;
      resolved_provider = None; resolved_model = None;
      state = Runtime.Planned; summary = None;
      accepted_at = None; ready_at = None;
      first_progress_at = None; started_at = None;
      finished_at = None; last_progress_at = None;
      last_error = None;
    }];
    artifacts = [];
    turn_count = 0; last_seq = 0;
    outcome = None;
  } in
  roundtrip
    ~to_yojson:Runtime.session_to_yojson
    ~of_yojson:Runtime.session_of_yojson
    ~show:Runtime.show_session
    ~name:"session" v

let test_init_request () =
  let v : Runtime.init_request = {
    session_root = Some "/tmp/sessions";
    provider = Some "anthropic"; model = Some "sonnet-4-6";
    permission_mode = Some "default";
    include_partial_messages = true;
    setting_sources = ["env"; "config"];
    resume_session = None; cwd = Some "/workspace";
  } in
  roundtrip
    ~to_yojson:Runtime.init_request_to_yojson
    ~of_yojson:Runtime.init_request_of_yojson
    ~show:Runtime.show_init_request
    ~name:"init_request" v

let test_init_response () =
  let v : Runtime.init_response = {
    sdk_name = "oas"; sdk_version = Agent_sdk.Sdk_version.version;
    runtime_version = "1.0"; protocol_version = "1";
    capabilities = ["streaming"; "tools"];
  } in
  roundtrip
    ~to_yojson:Runtime.init_response_to_yojson
    ~of_yojson:Runtime.init_response_of_yojson
    ~show:Runtime.show_init_response
    ~name:"init_response" v

let test_report () =
  let v : Runtime.report = {
    session_id = "sess-rpt";
    summary = ["did things"; "completed"];
    markdown = "# Report\nDone.";
    generated_at = 1.7e9;
  } in
  roundtrip
    ~to_yojson:Runtime.report_to_yojson
    ~of_yojson:Runtime.report_of_yojson
    ~show:Runtime.show_report
    ~name:"report" v

let test_proof () =
  let v : Runtime.proof = {
    session_id = "sess-proof";
    ok = true;
    checks = [{ name = "all_tools_paired"; passed = true }];
    evidence = ["trace.jsonl"];
    generated_at = 1.7e9;
  } in
  roundtrip
    ~to_yojson:Runtime.proof_to_yojson
    ~of_yojson:Runtime.proof_of_yojson
    ~show:Runtime.show_proof
    ~name:"proof" v

let test_permission_request () =
  let v : Runtime.permission_request = {
    action = "execute"; subject = "bash";
    payload = `Assoc [("command", `String "ls")];
  } in
  roundtrip
    ~to_yojson:Runtime.permission_request_to_yojson
    ~of_yojson:Runtime.permission_request_of_yojson
    ~show:Runtime.show_permission_request
    ~name:"permission_request" v

let test_hook_request () =
  let v : Runtime.hook_request = {
    hook_name = "pre_tool";
    payload = `Assoc [("tool", `String "bash")];
  } in
  roundtrip
    ~to_yojson:Runtime.hook_request_to_yojson
    ~of_yojson:Runtime.hook_request_of_yojson
    ~show:Runtime.show_hook_request
    ~name:"hook_request" v

(* ── Additional protocol types ────────────────────────────────── *)

let test_start_request () =
  let v : Runtime.start_request = {
    session_id = Some "s1"; goal = "test"; participants = ["a1"];
    provider = Some "anthropic"; model = Some "sonnet";
    permission_mode = Some "default"; system_prompt = Some "be helpful";
    max_turns = Some 5; workdir = Some "/work";
  } in
  roundtrip ~to_yojson:Runtime.start_request_to_yojson
    ~of_yojson:Runtime.start_request_of_yojson
    ~show:Runtime.show_start_request ~name:"start_request" v

let test_spawn_agent_request () =
  let v : Runtime.spawn_agent_request = {
    participant_name = "sub"; role = Some "helper"; prompt = "help me";
    provider = Some "local"; model = Some "qwen";
    system_prompt = None; max_turns = Some 3;
  } in
  roundtrip ~to_yojson:Runtime.spawn_agent_request_to_yojson
    ~of_yojson:Runtime.spawn_agent_request_of_yojson
    ~show:Runtime.show_spawn_agent_request ~name:"spawn_agent" v

let test_update_settings () =
  let v : Runtime.update_settings_request = {
    model = Some "opus"; permission_mode = Some "strict";
  } in
  roundtrip ~to_yojson:Runtime.update_settings_request_to_yojson
    ~of_yojson:Runtime.update_settings_request_of_yojson
    ~show:Runtime.show_update_settings_request ~name:"update_settings" v

let test_attach_artifact_request () =
  let v : Runtime.attach_artifact_request = {
    name = "log.txt"; kind = "text"; content = "log data";
  } in
  roundtrip ~to_yojson:Runtime.attach_artifact_request_to_yojson
    ~of_yojson:Runtime.attach_artifact_request_of_yojson
    ~show:Runtime.show_attach_artifact_request ~name:"attach_artifact" v

let test_command () =
  let variants = [
    Runtime.Record_turn { actor = Some "user"; message = "hello" };
    Runtime.Spawn_agent {
      participant_name = "sub"; role = None; prompt = "p";
      provider = None; model = None; system_prompt = None; max_turns = None;
    };
    Runtime.Update_session_settings { model = Some "opus"; permission_mode = None };
    Runtime.Attach_artifact { name = "r.txt"; kind = "text"; content = "data" };
    Runtime.Checkpoint { label = Some "mid" };
    Runtime.Request_finalize { reason = Some "done" };
  ] in
  List.iter (fun v ->
    roundtrip ~to_yojson:Runtime.command_to_yojson
      ~of_yojson:Runtime.command_of_yojson
      ~show:Runtime.show_command ~name:"command" v
  ) variants

let test_event_kind () =
  let events = [
    Runtime.Session_started { goal = "test"; participants = ["a1"] };
    Runtime.Turn_recorded { actor = Some "user"; message = "hi" };
    Runtime.Agent_spawn_requested {
      participant_name = "sub"; role = None; prompt = "p";
      provider = None; model = None; permission_mode = None;
    };
    Runtime.Agent_became_live {
      participant_name = "sub"; summary = Some "ready";
      provider = Some "local"; model = Some "qwen"; error = None;
      raw_trace_run_id = Some "wr-1"; stop_reason = None;
      completion_anomaly = None; failure_cause = None;
    };
    Runtime.Agent_output_delta { participant_name = "sub"; delta = "..." };
    Runtime.Artifact_attached {
      artifact_id = "a1"; name = "r.json"; kind = "json";
      mime_type = "application/json"; path = "/tmp/r.json"; size_bytes = 10;
    };
    Runtime.Checkpoint_saved { label = Some "mid"; path = "/tmp/cp" };
    Runtime.Session_completed { outcome = Some "success" };
    Runtime.Session_failed { outcome = Some "error" };
  ] in
  List.iter (fun ek ->
    roundtrip ~to_yojson:Runtime.event_kind_to_yojson
      ~of_yojson:Runtime.event_kind_of_yojson
      ~show:Runtime.show_event_kind
      ~name:"event_kind" ek
  ) events

let test_event () =
  let v : Runtime.event = {
    seq = 1; ts = 1.7e9;
    kind = Runtime.Session_started { goal = "x"; participants = [] };
  } in
  roundtrip ~to_yojson:Runtime.event_to_yojson
    ~of_yojson:Runtime.event_of_yojson
    ~show:Runtime.show_event ~name:"event" v

let test_permission_response () =
  let v : Runtime.permission_response = {
    allow = true; message = Some "ok"; interrupt = false;
  } in
  roundtrip ~to_yojson:Runtime.permission_response_to_yojson
    ~of_yojson:Runtime.permission_response_of_yojson
    ~show:Runtime.show_permission_response ~name:"perm_resp" v

let test_hook_response () =
  let v : Runtime.hook_response = {
    continue_ = true; message = Some "proceed";
  } in
  roundtrip ~to_yojson:Runtime.hook_response_to_yojson
    ~of_yojson:Runtime.hook_response_of_yojson
    ~show:Runtime.show_hook_response ~name:"hook_resp" v

let () =
  Alcotest.run "Runtime_types" [
    "phase", [
      Alcotest.test_case "all variants" `Quick test_phase;
    ];
    "participant_state", [
      Alcotest.test_case "all variants" `Quick test_participant_state;
    ];
    "records", [
      Alcotest.test_case "participant" `Quick test_participant;
      Alcotest.test_case "artifact" `Quick test_artifact;
      Alcotest.test_case "session" `Quick test_session;
      Alcotest.test_case "report" `Quick test_report;
      Alcotest.test_case "proof" `Quick test_proof;
    ];
    "protocol", [
      Alcotest.test_case "init_request" `Quick test_init_request;
      Alcotest.test_case "init_response" `Quick test_init_response;
      Alcotest.test_case "permission_request" `Quick test_permission_request;
      Alcotest.test_case "hook_request" `Quick test_hook_request;
      Alcotest.test_case "start_request" `Quick test_start_request;
      Alcotest.test_case "spawn_agent" `Quick test_spawn_agent_request;
      Alcotest.test_case "update_settings" `Quick test_update_settings;
      Alcotest.test_case "attach_artifact" `Quick test_attach_artifact_request;
      Alcotest.test_case "permission_response" `Quick test_permission_response;
      Alcotest.test_case "hook_response" `Quick test_hook_response;
    ];
    "command", [
      Alcotest.test_case "variants" `Quick test_command;
    ];
    "events", [
      Alcotest.test_case "event_kind all" `Quick test_event_kind;
      Alcotest.test_case "event" `Quick test_event;
    ];
  ]
