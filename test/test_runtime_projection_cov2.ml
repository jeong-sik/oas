(** Extended coverage for runtime_projection.ml — targeting remaining uncovered paths.

    This file targets uncovered paths in:
    - apply_event: all 14 event_kind branches
    - initial_session: construction from start_request
    - update_participant: new participant creation (not found path)
    - ensure_active_phase: terminal phase errors
    - count_by_state: all state variants
    - build_report: all fields, participant summary None/Some
    - build_proof: all proof checks (seq_contiguous, artifact uniqueness, etc.)
    - make_session_id
    - make_planned_participant *)

open Agent_sdk

(* ── Helpers ────────────────────────────────────────────── *)

let mk_event ?(seq = 1) ?(ts = 100.0) kind : Runtime.event =
  { seq; ts; kind }

let mk_start_request ?(goal = "test") ?(participants = []) () : Runtime.start_request =
  { session_id = None; goal; participants;
    provider = Some "anthropic"; model = Some "sonnet-4-6";
    permission_mode = None; system_prompt = Some "sys prompt";
    max_turns = Some 5; workdir = Some "/tmp" }

let mk_session
    ?(session_id = "s-001")
    ?(goal = "test")
    ?(phase = Runtime.Running)
    ?(participants = [])
    ?(artifacts = [])
    ?(votes = [])
    ?(outcome = None)
    ?(turn_count = 0)
    ?(last_seq = 0)
    () : Runtime.session =
  {
    session_id; goal; title = None; tag = None;
    permission_mode = None; phase;
    created_at = 100.0; updated_at = 200.0;
    provider = Some "anthropic"; model = Some "sonnet-4-6";
    system_prompt = Some "sys"; max_turns = 10;
    workdir = Some "/tmp";
    planned_participants = List.map (fun (p : Runtime.participant) -> p.name) participants;
    participants; artifacts; votes;
    turn_count; last_seq; outcome;
  }

let mk_participant
    ?(name = "alice")
    ?(state = Runtime.Planned)
    ?(started_at = None)
    () : Runtime.participant =
  {
    name; role = None; aliases = []; worker_id = None;
    runtime_actor = None; requested_provider = None;
    requested_model = None; requested_policy = None;
    provider = None; model = None;
    resolved_provider = None; resolved_model = None;
    state; summary = None;
    accepted_at = None; ready_at = None;
    first_progress_at = None; started_at;
    finished_at = None; last_progress_at = None;
    last_error = None;
  }

(* ── initial_session ──────────────────────────────────── *)

let test_initial_session_basic () =
  let req = mk_start_request ~goal:"deploy" ~participants:["alice"; "bob"] () in
  let session = Runtime_projection.initial_session req in
  Alcotest.(check string) "goal" "deploy" session.goal;
  Alcotest.(check int) "2 participants" 2 (List.length session.participants);
  Alcotest.(check bool) "phase Bootstrapping" true (session.phase = Runtime.Bootstrapping);
  Alcotest.(check int) "max_turns" 5 session.max_turns;
  Alcotest.(check (option string)) "provider" (Some "anthropic") session.provider;
  Alcotest.(check int) "turn_count 0" 0 session.turn_count;
  Alcotest.(check int) "last_seq 0" 0 session.last_seq

let test_initial_session_defaults () =
  let req : Runtime.start_request = {
    session_id = None; goal = "g"; participants = [];
    provider = None; model = None; permission_mode = None;
    system_prompt = None; max_turns = None; workdir = None;
  } in
  let session = Runtime_projection.initial_session req in
  Alcotest.(check int) "default max_turns=8" 8 session.max_turns;
  Alcotest.(check bool) "session_id non-empty" true
    (String.length session.session_id > 0)

let test_initial_session_explicit_id () =
  let req = { (mk_start_request ()) with session_id = Some "custom-id" } in
  let session = Runtime_projection.initial_session req in
  Alcotest.(check string) "custom id" "custom-id" session.session_id

(* ── ensure_active_phase (tested via apply_event on terminal phases) ── *)

let test_turn_on_running_ok () =
  let session = mk_session ~phase:Running () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "Running should accept turns"

let test_turn_on_bootstrapping_ok () =
  let session = mk_session ~phase:Bootstrapping () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "Bootstrapping should accept turns"

let test_turn_on_waiting_ok () =
  let session = mk_session ~phase:Waiting_on_workers () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "Waiting should accept turns"

let test_turn_on_finalizing_ok () =
  let session = mk_session ~phase:Finalizing () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "Finalizing should accept turns"

let test_turn_on_completed_fails () =
  let session = mk_session ~phase:Completed () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Completed should reject turns"

let test_turn_on_failed_fails () =
  let session = mk_session ~phase:Failed () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Failed should reject turns"

let test_turn_on_cancelled_fails () =
  let session = mk_session ~phase:Cancelled () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Cancelled should reject turns"

(* ── apply_event: Session_started ─────────────────────── *)

let test_apply_session_started () =
  let session = mk_session ~phase:Bootstrapping () in
  let event = mk_event (Session_started { goal = "test"; participants = [] }) in
  match Runtime_projection.apply_event session event with
  | Ok s -> Alcotest.(check bool) "Running" true (s.phase = Runtime.Running)
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Session_settings_updated ────────────── *)

let test_apply_settings_updated () =
  let session = mk_session () in
  let event = mk_event (Session_settings_updated {
    model = Some "new-model"; permission_mode = Some "auto";
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check (option string)) "model" (Some "new-model") s.model;
    Alcotest.(check (option string)) "perm" (Some "auto") s.permission_mode
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Turn_recorded ───────────────────────── *)

let test_apply_turn_recorded () =
  let session = mk_session ~turn_count:2 () in
  let event = mk_event (Turn_recorded { actor = Some "alice"; message = "hello" }) in
  match Runtime_projection.apply_event session event with
  | Ok s -> Alcotest.(check int) "turn_count" 3 s.turn_count
  | Error e -> Alcotest.fail (Error.to_string e)

let test_apply_turn_recorded_terminal_fails () =
  let session = mk_session ~phase:Completed () in
  let event = mk_event (Turn_recorded { actor = None; message = "x" }) in
  match Runtime_projection.apply_event session event with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on terminal phase"

(* ── apply_event: Agent_spawn_requested ───────────────── *)

let test_apply_agent_spawn () =
  let session = mk_session () in
  let event = mk_event (Agent_spawn_requested {
    participant_name = "bob"; role = Some "reviewer";
    prompt = "review"; provider = Some "openai";
    model = Some "gpt-4"; permission_mode = None;
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    let bob = List.find (fun (p : Runtime.participant) -> p.name = "bob") s.participants in
    Alcotest.(check bool) "state Starting" true (bob.state = Starting);
    Alcotest.(check (option string)) "role" (Some "reviewer") bob.role;
    Alcotest.(check (option string)) "req_provider" (Some "openai") bob.requested_provider
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Agent_became_live ───────────────────── *)

let test_apply_agent_became_live () =
  let p = mk_participant ~name:"alice" ~state:Starting () in
  let session = mk_session ~participants:[p] () in
  let event = mk_event (Agent_became_live {
    participant_name = "alice"; summary = Some "ready";
    provider = Some "anthropic"; model = Some "haiku"; error = None;
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    let alice = List.hd s.participants in
    Alcotest.(check bool) "Live" true (alice.state = Live);
    Alcotest.(check (option string)) "summary" (Some "ready") alice.summary
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Agent_output_delta ──────────────────── *)

let test_apply_agent_output_delta () =
  let p = mk_participant ~name:"alice" ~state:Live () in
  let session = mk_session ~participants:[p] () in
  let event = mk_event (Agent_output_delta {
    participant_name = "alice"; delta = "some output";
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    let alice = List.hd s.participants in
    Alcotest.(check bool) "still Live" true (alice.state = Live)
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Agent_completed ─────────────────────── *)

let test_apply_agent_completed () =
  let p = mk_participant ~name:"alice" ~state:Live ~started_at:(Some 90.0) () in
  let session = mk_session ~participants:[p] () in
  let event = mk_event ~ts:200.0 (Agent_completed {
    participant_name = "alice"; summary = Some "done";
    provider = None; model = None; error = None;
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    let alice = List.hd s.participants in
    Alcotest.(check bool) "Done" true (alice.state = Done);
    Alcotest.(check (option string)) "summary" (Some "done") alice.summary;
    Alcotest.(check bool) "finished_at" true (alice.finished_at = Some 200.0)
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Agent_failed ────────────────────────── *)

let test_apply_agent_failed () =
  let p = mk_participant ~name:"alice" ~state:Live () in
  let session = mk_session ~participants:[p] () in
  let event = mk_event ~ts:200.0 (Agent_failed {
    participant_name = "alice"; summary = Some "crashed";
    provider = None; model = None; error = Some "timeout";
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    let alice = List.hd s.participants in
    Alcotest.(check bool) "Failed_participant" true (alice.state = Failed_participant);
    Alcotest.(check (option string)) "error" (Some "timeout") alice.last_error
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Artifact_attached ───────────────────── *)

let test_apply_artifact_attached () =
  let session = mk_session () in
  let event = mk_event (Artifact_attached {
    artifact_id = "art-1"; name = "report.md"; kind = "document";
    mime_type = "text/markdown"; path = "/tmp/report.md"; size_bytes = 1024;
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check int) "1 artifact" 1 (List.length s.artifacts);
    let a = List.hd s.artifacts in
    Alcotest.(check string) "id" "art-1" a.artifact_id
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Vote_recorded ───────────────────────── *)

let test_apply_vote_recorded () =
  let session = mk_session () in
  let vote : Runtime.vote = {
    topic = "ready"; options = ["yes"; "no"]; choice = "yes";
    actor = Some "alice"; created_at = 100.0;
  } in
  let event = mk_event (Vote_recorded vote) in
  match Runtime_projection.apply_event session event with
  | Ok s -> Alcotest.(check int) "turn_count unchanged" 0 s.turn_count
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Checkpoint_saved ────────────────────── *)

let test_apply_checkpoint_saved () =
  let session = mk_session () in
  let event = mk_event (Checkpoint_saved { label = Some "cp1"; path = "/tmp/cp" }) in
  match Runtime_projection.apply_event session event with
  | Ok s -> Alcotest.(check string) "session unchanged" "s-001" s.session_id
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Finalize_requested ──────────────────── *)

let test_apply_finalize_requested () =
  let session = mk_session () in
  let event = mk_event (Finalize_requested { reason = Some "done" }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check bool) "Finalizing" true (s.phase = Finalizing);
    Alcotest.(check (option string)) "outcome" (Some "done") s.outcome
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Session_completed ───────────────────── *)

let test_apply_session_completed () =
  let session = mk_session () in
  let event = mk_event ~ts:300.0 (Session_completed { outcome = Some "merged" }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check bool) "Completed" true (s.phase = Completed);
    Alcotest.(check (option string)) "outcome" (Some "merged") s.outcome;
    Alcotest.(check (float 0.01)) "updated_at" 300.0 s.updated_at
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── apply_event: Session_failed ──────────────────────── *)

let test_apply_session_failed () =
  let session = mk_session () in
  let event = mk_event ~ts:300.0 (Session_failed { outcome = Some "error" }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check bool) "Failed" true (s.phase = Failed);
    Alcotest.(check (option string)) "outcome" (Some "error") s.outcome
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── update_participant: new participant (not found) ──── *)

let test_update_participant_new () =
  let session = mk_session () in
  let event = mk_event (Agent_spawn_requested {
    participant_name = "new-agent"; role = Some "worker";
    prompt = "do stuff"; provider = None; model = None; permission_mode = None;
  }) in
  match Runtime_projection.apply_event session event with
  | Ok s ->
    Alcotest.(check int) "1 participant added" 1 (List.length s.participants);
    let p = List.hd s.participants in
    Alcotest.(check string) "name" "new-agent" p.name
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── count_by_state (tested indirectly via build_report) ── *)

let test_count_by_state_via_report () =
  let p1 = mk_participant ~name:"a" ~state:Done () in
  let p2 = mk_participant ~name:"b" ~state:Done () in
  let p3 = mk_participant ~name:"c" ~state:Failed_participant () in
  let session = mk_session ~phase:Completed ~participants:[p1; p2; p3] () in
  let report = Runtime_projection.build_report session [] in
  (* Report summary includes "Participants done: N" and "Participants failed: N" *)
  let has needle = List.exists (fun line -> Util.string_contains ~needle line) report.summary in
  Alcotest.(check bool) "done count 2" true (has "done: 2");
  Alcotest.(check bool) "failed count 1" true (has "failed: 1")

(* ── build_report ─────────────────────────────────────── *)

let test_build_report_basic () =
  let p1 = { (mk_participant ~name:"a" ~state:Done ()) with summary = Some "finished" } in
  let p2 = { (mk_participant ~name:"b" ~state:Live ()) with summary = None } in
  let session = mk_session ~session_id:"s-report" ~goal:"deploy"
    ~phase:Completed ~participants:[p1; p2] ~turn_count:3 () in
  let events = [
    mk_event ~seq:1 (Session_started { goal = "deploy"; participants = [] });
    mk_event ~seq:2 (Turn_recorded { actor = Some "a"; message = "hi" });
  ] in
  let report = Runtime_projection.build_report session events in
  Alcotest.(check string) "session_id" "s-report" report.session_id;
  Alcotest.(check bool) "summary non-empty" true (List.length report.summary > 0);
  Alcotest.(check bool) "markdown non-empty" true (String.length report.markdown > 0);
  (* Verify participant with None summary shows n/a *)
  Alcotest.(check bool) "contains n/a" true
    (Util.string_contains ~needle:"n/a" report.markdown)

let test_build_report_empty () =
  let session = mk_session ~phase:Completed () in
  let report = Runtime_projection.build_report session [] in
  Alcotest.(check string) "session_id" "s-001" report.session_id

(* ── build_proof ──────────────────────────────────────── *)

let test_build_proof_passing () =
  let p = mk_participant ~name:"a" ~state:Done () in
  let session = mk_session ~phase:Completed ~participants:[p] ~last_seq:4
    ~artifacts:[{
      artifact_id = "a1"; name = "f"; kind = "k"; mime_type = "t";
      path = Some "/p"; inline_content = None; size_bytes = 0; created_at = 0.0;
    }] () in
  let events = [
    mk_event ~seq:1 ~ts:10.0 (Session_started { goal = "g"; participants = [] });
    mk_event ~seq:2 ~ts:20.0 (Turn_recorded { actor = None; message = "m" });
    mk_event ~seq:3 ~ts:30.0 (Agent_completed {
      participant_name = "a"; summary = None; provider = None; model = None; error = None;
    });
    mk_event ~seq:4 ~ts:40.0 (Session_completed { outcome = Some "ok" });
  ] in
  let proof = Runtime_projection.build_proof session events in
  Alcotest.(check bool) "proof ok" true proof.ok;
  Alcotest.(check bool) "all checks passed" true
    (List.for_all (fun (c : Runtime.proof_check) -> c.passed) proof.checks)

let test_build_proof_failing () =
  let session = mk_session ~phase:Running () in
  let events = [] in
  let proof = Runtime_projection.build_proof session events in
  Alcotest.(check bool) "proof not ok" false proof.ok;
  (* seq_contiguous should fail for empty events *)
  let seq_check = List.find (fun (c : Runtime.proof_check) -> c.name = "seq_contiguous") proof.checks in
  Alcotest.(check bool) "seq_contiguous fails" false seq_check.passed

let test_build_proof_duplicate_artifact_ids () =
  let art1 : Runtime.artifact = {
    artifact_id = "dup"; name = "a"; kind = "k"; mime_type = "m";
    path = Some "/p"; inline_content = None; size_bytes = 0; created_at = 0.0;
  } in
  let art2 = { art1 with name = "b" } in
  let session = mk_session ~phase:Completed ~artifacts:[art1; art2] () in
  let events = [
    mk_event ~seq:1 (Session_started { goal = "g"; participants = [] });
    mk_event ~seq:2 (Session_completed { outcome = None });
  ] in
  let proof = Runtime_projection.build_proof session events in
  let artifact_check = List.find (fun (c : Runtime.proof_check) -> c.name = "artifact_ids_unique") proof.checks in
  Alcotest.(check bool) "artifact_ids_unique fails" false artifact_check.passed

let test_build_proof_non_contiguous_seq () =
  let session = mk_session ~phase:Completed () in
  let events = [
    mk_event ~seq:1 (Session_started { goal = "g"; participants = [] });
    mk_event ~seq:3 (Session_completed { outcome = None });
  ] in
  let proof = Runtime_projection.build_proof session events in
  let seq_check = List.find (fun (c : Runtime.proof_check) -> c.name = "seq_contiguous") proof.checks in
  Alcotest.(check bool) "seq_contiguous fails" false seq_check.passed

(* ── apply multiple events ────────────────────────────── *)

let test_apply_event_sequence () =
  let req = mk_start_request ~goal:"full flow" ~participants:["alice"] () in
  let session = Runtime_projection.initial_session req in
  let events = [
    mk_event ~seq:1 ~ts:10.0 (Session_started { goal = "full flow"; participants = ["alice"] });
    mk_event ~seq:2 ~ts:20.0 (Turn_recorded { actor = Some "user"; message = "do it" });
    mk_event ~seq:3 ~ts:30.0 (Agent_spawn_requested {
      participant_name = "alice"; role = Some "exec"; prompt = "go";
      provider = None; model = None; permission_mode = None;
    });
    mk_event ~seq:4 ~ts:40.0 (Agent_became_live {
      participant_name = "alice"; summary = None;
      provider = Some "anthropic"; model = Some "sonnet"; error = None;
    });
    mk_event ~seq:5 ~ts:50.0 (Agent_completed {
      participant_name = "alice"; summary = Some "completed";
      provider = None; model = None; error = None;
    });
    mk_event ~seq:6 ~ts:60.0 (Session_completed { outcome = Some "success" });
  ] in
  let result = List.fold_left (fun session event ->
    match session with
    | Error e -> Error e
    | Ok s -> Runtime_projection.apply_event s event
  ) (Ok session) events in
  match result with
  | Ok final ->
    Alcotest.(check bool) "Completed" true (final.phase = Completed);
    Alcotest.(check int) "turn_count" 1 final.turn_count;
    Alcotest.(check (option string)) "outcome" (Some "success") final.outcome;
    let alice = List.find (fun (p : Runtime.participant) -> p.name = "alice") final.participants in
    Alcotest.(check bool) "alice Done" true (alice.state = Done)
  | Error e -> Alcotest.fail (Error.to_string e)

(* ── Test runner ───────────────────────────────────────── *)

let () =
  Alcotest.run "Runtime_projection_cov2" [
    "initial_session", [
      Alcotest.test_case "basic" `Quick test_initial_session_basic;
      Alcotest.test_case "defaults" `Quick test_initial_session_defaults;
      Alcotest.test_case "explicit id" `Quick test_initial_session_explicit_id;
    ];
    "ensure_active_phase", [
      Alcotest.test_case "Running ok" `Quick test_turn_on_running_ok;
      Alcotest.test_case "Bootstrapping ok" `Quick test_turn_on_bootstrapping_ok;
      Alcotest.test_case "Waiting ok" `Quick test_turn_on_waiting_ok;
      Alcotest.test_case "Finalizing ok" `Quick test_turn_on_finalizing_ok;
      Alcotest.test_case "Completed fails" `Quick test_turn_on_completed_fails;
      Alcotest.test_case "Failed fails" `Quick test_turn_on_failed_fails;
      Alcotest.test_case "Cancelled fails" `Quick test_turn_on_cancelled_fails;
    ];
    "apply_event", [
      Alcotest.test_case "Session_started" `Quick test_apply_session_started;
      Alcotest.test_case "Settings_updated" `Quick test_apply_settings_updated;
      Alcotest.test_case "Turn_recorded" `Quick test_apply_turn_recorded;
      Alcotest.test_case "Turn terminal fails" `Quick test_apply_turn_recorded_terminal_fails;
      Alcotest.test_case "Spawn_requested" `Quick test_apply_agent_spawn;
      Alcotest.test_case "Became_live" `Quick test_apply_agent_became_live;
      Alcotest.test_case "Output_delta" `Quick test_apply_agent_output_delta;
      Alcotest.test_case "Completed" `Quick test_apply_agent_completed;
      Alcotest.test_case "Failed" `Quick test_apply_agent_failed;
      Alcotest.test_case "Artifact_attached" `Quick test_apply_artifact_attached;
      Alcotest.test_case "Vote_recorded" `Quick test_apply_vote_recorded;
      Alcotest.test_case "Checkpoint_saved" `Quick test_apply_checkpoint_saved;
      Alcotest.test_case "Finalize_requested" `Quick test_apply_finalize_requested;
      Alcotest.test_case "Session_completed" `Quick test_apply_session_completed;
      Alcotest.test_case "Session_failed" `Quick test_apply_session_failed;
      Alcotest.test_case "new participant" `Quick test_update_participant_new;
    ];
    "count_by_state", [
      Alcotest.test_case "via report" `Quick test_count_by_state_via_report;
    ];
    "build_report", [
      Alcotest.test_case "basic" `Quick test_build_report_basic;
      Alcotest.test_case "empty" `Quick test_build_report_empty;
    ];
    "build_proof", [
      Alcotest.test_case "passing" `Quick test_build_proof_passing;
      Alcotest.test_case "failing" `Quick test_build_proof_failing;
      Alcotest.test_case "duplicate artifact ids" `Quick test_build_proof_duplicate_artifact_ids;
      Alcotest.test_case "non-contiguous seq" `Quick test_build_proof_non_contiguous_seq;
    ];
    "integration", [
      Alcotest.test_case "full event sequence" `Quick test_apply_event_sequence;
    ];
  ]
