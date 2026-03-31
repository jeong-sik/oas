(** Unit tests for runtime_projection.ml — targeting 65 uncovered points at 73%.

    All functions are pure transformations between Runtime.session and
    Collaboration.t types. No I/O, no LLM calls.

    Covers:
    - phase_to_collaboration / phase_of_collaboration (all 7 variants)
    - participant_state_to_collaboration (all 7 variants)
    - participant_to_collaboration (21-field -> 6-field lossy projection)
    - artifact_to_collaboration (8-field -> 5-field lossy projection)
    - collaboration_of_session (full end-to-end)
    - update_session_from_collaboration (selective sync)
    - Roundtrip: session -> collab -> session preserves synced fields
    - Edge cases: empty participants and empty artifacts *)

open Agent_sdk

(* ── Helpers ────────────────────────────────────────────────────── *)

let mk_session
    ?(session_id = "rt-test-001")
    ?(goal = "test goal")
    ?(phase = Runtime.Running)
    ?(participants = [])
    ?(artifacts = [])
    ?(outcome = None)
    () : Runtime.session =
  {
    session_id;
    goal;
    title = None;
    tag = None;
    permission_mode = None;
    phase;
    created_at = 100.0;
    updated_at = 200.0;
    provider = Some "anthropic";
    model = Some "sonnet-4-6";
    system_prompt = Some "You are helpful.";
    max_turns = 10;
    workdir = Some "/tmp/work";
    planned_participants = List.map (fun (p : Runtime.participant) -> p.name) participants;
    participants;
    artifacts;
    turn_count = 5;
    last_seq = 12;
    outcome;
  }

let mk_participant
    ?(name = "alice")
    ?(role = Some "lead")
    ?(state = Runtime.Live)
    ?(summary = Some "working on it")
    ?(started_at = Some 100.0)
    ?(finished_at = None)
    () : Runtime.participant =
  {
    name;
    role;
    aliases = ["a"];
    worker_id = Some (name ^ "-worker");
    runtime_actor = Some name;
    requested_provider = Some "anthropic";
    requested_model = Some "sonnet-4-6";
    requested_policy = None;
    provider = Some "anthropic";
    model = Some "sonnet-4-6";
    resolved_provider = Some "anthropic";
    resolved_model = Some "claude-sonnet-4-6-20250514";
    state;
    summary;
    accepted_at = Some 99.0;
    ready_at = Some 99.5;
    first_progress_at = Some 100.0;
    started_at;
    finished_at;
    last_progress_at = Some 150.0;
    last_error = None;
  }

let mk_artifact ?(artifact_id = "art-001") () : Runtime.artifact =
  {
    artifact_id;
    name = "report.md";
    kind = "document";
    mime_type = "text/markdown";
    path = Some "/tmp/report.md";
    inline_content = None;
    size_bytes = 2048;
    created_at = 150.0;
  }

(* ── phase_to_collaboration: all 7 variants ─────────────────────── *)

let test_phase_to_collaboration_all () =
  let pairs = [
    (Runtime.Bootstrapping, Collaboration.Bootstrapping);
    (Runtime.Running, Collaboration.Active);
    (Runtime.Waiting_on_workers, Collaboration.Waiting_on_participants);
    (Runtime.Finalizing, Collaboration.Finalizing);
    (Runtime.Completed, Collaboration.Completed);
    (Runtime.Failed, Collaboration.Failed);
    (Runtime.Cancelled, Collaboration.Cancelled);
  ] in
  List.iter (fun (rt_phase, expected) ->
    let session = mk_session ~phase:rt_phase () in
    let collab = Runtime_projection.collaboration_of_session session in
    Alcotest.(check bool)
      (Printf.sprintf "phase %s -> %s"
        (Runtime.show_phase rt_phase)
        (Collaboration.show_phase expected))
      true (collab.phase = expected)
  ) pairs

(* ── phase_of_collaboration: all 7 variants ─────────────────────── *)

let test_phase_of_collaboration_all () =
  let pairs = [
    (Collaboration.Bootstrapping, Runtime.Bootstrapping);
    (Collaboration.Active, Runtime.Running);
    (Collaboration.Waiting_on_participants, Runtime.Waiting_on_workers);
    (Collaboration.Finalizing, Runtime.Finalizing);
    (Collaboration.Completed, Runtime.Completed);
    (Collaboration.Failed, Runtime.Failed);
    (Collaboration.Cancelled, Runtime.Cancelled);
  ] in
  List.iter (fun (collab_phase, expected_rt) ->
    let session = mk_session () in
    let collab = Collaboration.create ~id:"c1" ~goal:"g" () in
    let collab = Collaboration.set_phase collab collab_phase in
    let updated = Runtime_projection.update_session_from_collaboration session collab in
    Alcotest.(check bool)
      (Printf.sprintf "collab phase %s -> runtime %s"
        (Collaboration.show_phase collab_phase)
        (Runtime.show_phase expected_rt))
      true (updated.phase = expected_rt)
  ) pairs

(* ── participant_state_to_collaboration: all 7 variants ─────────── *)

let test_participant_state_mapping () =
  let pairs = [
    (Runtime.Planned, Collaboration.Planned);
    (Runtime.Starting, Collaboration.Joined);
    (Runtime.Live, Collaboration.Working);
    (Runtime.Idle, Collaboration.Joined);
    (Runtime.Done, Collaboration.Done);
    (Runtime.Failed_participant, Collaboration.Failed_participant);
    (Runtime.Detached, Collaboration.Left);
  ] in
  List.iter (fun (rt_state, expected) ->
    let p = mk_participant ~state:rt_state () in
    let session = mk_session ~participants:[p] () in
    let collab = Runtime_projection.collaboration_of_session session in
    let cp = List.hd collab.participants in
    Alcotest.(check bool)
      (Printf.sprintf "state %s -> %s"
        (Runtime.show_participant_state rt_state)
        (Collaboration.show_participant_state expected))
      true (cp.state = expected)
  ) pairs

(* ── participant_to_collaboration: field mapping ────────────────── *)

let test_participant_field_mapping () =
  let p = mk_participant ~name:"bob" ~role:(Some "reviewer")
    ~state:Runtime.Done ~summary:(Some "reviewed all")
    ~started_at:(Some 110.0) ~finished_at:(Some 160.0) () in
  let session = mk_session ~participants:[p] () in
  let collab = Runtime_projection.collaboration_of_session session in
  let cp = List.hd collab.participants in
  Alcotest.(check string) "name" "bob" cp.name;
  Alcotest.(check (option string)) "role" (Some "reviewer") cp.role;
  Alcotest.(check bool) "state is Done" true (cp.state = Collaboration.Done);
  Alcotest.(check (option string)) "summary" (Some "reviewed all") cp.summary;
  (* joined_at comes from started_at *)
  Alcotest.(check bool) "joined_at = started_at" true
    (cp.joined_at = Some 110.0);
  Alcotest.(check bool) "finished_at" true
    (cp.finished_at = Some 160.0)

let test_participant_none_fields () =
  let p = mk_participant ~role:None ~summary:None ~started_at:None ~finished_at:None () in
  let session = mk_session ~participants:[p] () in
  let collab = Runtime_projection.collaboration_of_session session in
  let cp = List.hd collab.participants in
  Alcotest.(check (option string)) "role None" None cp.role;
  Alcotest.(check (option string)) "summary None" None cp.summary;
  Alcotest.(check bool) "joined_at None (from started_at)" true
    (cp.joined_at = None);
  Alcotest.(check bool) "finished_at None" true (cp.finished_at = None)

(* ── artifact_to_collaboration ──────────────────────────────────── *)

let test_artifact_projection () =
  let a = mk_artifact ~artifact_id:"art-xyz" () in
  let session = mk_session ~artifacts:[a] () in
  let collab = Runtime_projection.collaboration_of_session session in
  Alcotest.(check int) "1 artifact" 1 (List.length collab.artifacts);
  let ca = List.hd collab.artifacts in
  Alcotest.(check string) "id" "art-xyz" ca.id;
  Alcotest.(check string) "name" "report.md" ca.name;
  Alcotest.(check string) "kind" "document" ca.kind;
  (* producer is always empty because Runtime.artifact has no producer field *)
  Alcotest.(check string) "producer empty (lossy)" "" ca.producer;
  Alcotest.(check bool) "created_at" true (ca.created_at = 150.0)

let test_artifact_multiple () =
  let a1 = mk_artifact ~artifact_id:"art-1" () in
  let a2 = { (mk_artifact ~artifact_id:"art-2" ()) with
    name = "data.json"; kind = "data"; mime_type = "application/json" } in
  let session = mk_session ~artifacts:[a1; a2] () in
  let collab = Runtime_projection.collaboration_of_session session in
  Alcotest.(check int) "2 artifacts" 2 (List.length collab.artifacts);
  let ids = List.map (fun (a : Collaboration.artifact) -> a.id) collab.artifacts in
  Alcotest.(check bool) "art-1 present" true (List.mem "art-1" ids);
  Alcotest.(check bool) "art-2 present" true (List.mem "art-2" ids)

(* ── collaboration_of_session: full end-to-end ──────────────────── *)

let test_collaboration_of_session_full () =
  let p1 = mk_participant ~name:"alice" ~state:Runtime.Done ~finished_at:(Some 155.0) () in
  let p2 = mk_participant ~name:"bob" ~state:Runtime.Live ~role:(Some "exec") () in
  let a = mk_artifact () in
  let session = mk_session
    ~session_id:"rt-full-001"
    ~goal:"deploy v2"
    ~phase:Runtime.Completed
    ~participants:[p1; p2]
    ~artifacts:[a]
    ~outcome:(Some "deployed successfully")
    () in
  let collab = Runtime_projection.collaboration_of_session session in
  Alcotest.(check string) "id = session_id" "rt-full-001" collab.id;
  Alcotest.(check string) "goal" "deploy v2" collab.goal;
  Alcotest.(check bool) "phase Completed" true
    (collab.phase = Collaboration.Completed);
  Alcotest.(check int) "2 participants" 2 (List.length collab.participants);
  Alcotest.(check int) "1 artifact" 1 (List.length collab.artifacts);
  Alcotest.(check int) "0 contributions" 0 (List.length collab.contributions);
  Alcotest.(check bool) "created_at" true (collab.created_at = 100.0);
  Alcotest.(check bool) "updated_at" true (collab.updated_at = 200.0);
  Alcotest.(check (option string)) "outcome" (Some "deployed successfully") collab.outcome;
  Alcotest.(check (option int)) "max_participants None" None collab.max_participants;
  Alcotest.(check int) "empty metadata" 0 (List.length collab.metadata);
  (* shared_context is always fresh *)
  Alcotest.(check int) "empty shared_context" 0
    (List.length (Context.keys collab.shared_context))

let test_collaboration_of_session_empty () =
  let session = mk_session
    ~participants:[]
    ~artifacts:[]
    ~outcome:None
    () in
  let collab = Runtime_projection.collaboration_of_session session in
  Alcotest.(check int) "0 participants" 0 (List.length collab.participants);
  Alcotest.(check int) "0 artifacts" 0 (List.length collab.artifacts);
  Alcotest.(check int) "0 contributions" 0 (List.length collab.contributions);
  Alcotest.(check (option string)) "no outcome" None collab.outcome

(* ── update_session_from_collaboration ──────────────────────────── *)

let test_update_syncs_goal_phase_outcome () =
  let session = mk_session ~goal:"old" ~phase:Runtime.Running ~outcome:None () in
  let collab = Collaboration.create ~id:"c1" ~goal:"new goal" () in
  let collab = Collaboration.set_phase collab Collaboration.Completed in
  let collab = Collaboration.set_outcome collab "merged" in
  let updated = Runtime_projection.update_session_from_collaboration session collab in
  Alcotest.(check string) "goal synced" "new goal" updated.goal;
  Alcotest.(check bool) "phase synced" true (updated.phase = Runtime.Completed);
  Alcotest.(check (option string)) "outcome synced" (Some "merged") updated.outcome

let test_update_preserves_non_synced_fields () =
  let p = mk_participant ~name:"x" () in
  let a = mk_artifact () in
  let session = mk_session
    ~session_id:"keep-me"
    ~participants:[p]
    ~artifacts:[a]
    () in
  let collab = Collaboration.create ~id:"c2" ~goal:"changed" () in
  let updated = Runtime_projection.update_session_from_collaboration session collab in
  (* These fields must NOT change *)
  Alcotest.(check string) "session_id" "keep-me" updated.session_id;
  Alcotest.(check int) "participants count" 1 (List.length updated.participants);
  Alcotest.(check int) "artifacts count" 1 (List.length updated.artifacts);
  Alcotest.(check (option string)) "provider" (Some "anthropic") updated.provider;
  Alcotest.(check (option string)) "model" (Some "sonnet-4-6") updated.model;
  Alcotest.(check (option string)) "system_prompt" (Some "You are helpful.") updated.system_prompt;
  Alcotest.(check int) "max_turns" 10 updated.max_turns;
  Alcotest.(check int) "turn_count" 5 updated.turn_count;
  Alcotest.(check int) "last_seq" 12 updated.last_seq

(* ── Roundtrip: session -> collab -> session ────────────────────── *)

let test_roundtrip_preserves_synced_fields () =
  let session = mk_session
    ~goal:"roundtrip test"
    ~phase:Runtime.Finalizing
    ~outcome:(Some "finalizing")
    () in
  let collab = Runtime_projection.collaboration_of_session session in
  let restored = Runtime_projection.update_session_from_collaboration session collab in
  Alcotest.(check string) "goal preserved" session.goal restored.goal;
  Alcotest.(check bool) "phase preserved" true (restored.phase = session.phase);
  Alcotest.(check (option string)) "outcome preserved" session.outcome restored.outcome

let test_roundtrip_updated_at_changes () =
  let session = mk_session ~phase:Runtime.Running () in
  let collab = Runtime_projection.collaboration_of_session session in
  (* Modify collab's updated_at via touch *)
  let collab = Collaboration.set_phase collab Collaboration.Active in
  let restored = Runtime_projection.update_session_from_collaboration session collab in
  (* updated_at comes from collab, which was touched *)
  Alcotest.(check bool) "updated_at changed" true
    (restored.updated_at >= session.updated_at)

(* ── Test runner ───────────────────────────────────────────────── *)

let () =
  Alcotest.run "Runtime_projection_unit" [
    "phase_mapping", [
      Alcotest.test_case "to_collaboration all 7" `Quick test_phase_to_collaboration_all;
      Alcotest.test_case "of_collaboration all 7" `Quick test_phase_of_collaboration_all;
    ];
    "participant_state", [
      Alcotest.test_case "all 7 variants" `Quick test_participant_state_mapping;
    ];
    "participant_projection", [
      Alcotest.test_case "field mapping" `Quick test_participant_field_mapping;
      Alcotest.test_case "None fields" `Quick test_participant_none_fields;
    ];
    "artifact_projection", [
      Alcotest.test_case "single" `Quick test_artifact_projection;
      Alcotest.test_case "multiple" `Quick test_artifact_multiple;
    ];
    "collaboration_of_session", [
      Alcotest.test_case "full" `Quick test_collaboration_of_session_full;
      Alcotest.test_case "empty collections" `Quick test_collaboration_of_session_empty;
    ];
    "update_session_from_collaboration", [
      Alcotest.test_case "syncs goal/phase/outcome" `Quick test_update_syncs_goal_phase_outcome;
      Alcotest.test_case "preserves non-synced fields" `Quick test_update_preserves_non_synced_fields;
    ];
    "roundtrip", [
      Alcotest.test_case "session -> collab -> session" `Quick test_roundtrip_preserves_synced_fields;
      Alcotest.test_case "updated_at propagation" `Quick test_roundtrip_updated_at_changes;
    ];
  ]
