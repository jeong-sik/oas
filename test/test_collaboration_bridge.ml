open Agent_sdk

(** Tests for Runtime.session ↔ Collaboration.t projection functions.
    Verifies the lossy mapping between the 18-field wire protocol type
    and the 12-field domain type. *)

let () =
  let open Alcotest in
  run "Collaboration_bridge" [

    "phase_mapping", [
      test_case "all 7 Runtime phases map to Collaboration phases" `Quick (fun () ->
        let open Runtime in
        let pairs = [
          (Bootstrapping, Collaboration.Bootstrapping);
          (Running, Collaboration.Active);
          (Waiting_on_workers, Collaboration.Waiting_on_participants);
          (Finalizing, Collaboration.Finalizing);
          (Completed, Collaboration.Completed);
          (Failed, Collaboration.Failed);
          (Cancelled, Collaboration.Cancelled);
        ] in
        List.iter (fun (rt, expected) ->
          let session = {
            session_id = "s1"; goal = "g"; title = None; tag = None;
            permission_mode = None; phase = rt; created_at = 1.0;
            updated_at = 2.0; provider = None; model = None;
            system_prompt = None; max_turns = 8; workdir = None;
            planned_participants = []; participants = [];
            artifacts = []; turn_count = 0;
            last_seq = 0; outcome = None;
          } in
          let collab = Runtime_projection.collaboration_of_session session in
          check bool
            (Printf.sprintf "phase %s" (Runtime.show_phase rt))
            true (collab.phase = expected)
        ) pairs);

      test_case "round-trip phase through update_session_from_collaboration" `Quick (fun () ->
        let session = {
          Runtime.session_id = "s2"; goal = "test"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = []; participants = [];
          artifacts = []; turn_count = 0;
          last_seq = 0; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        let collab = Collaboration.set_phase collab Completed in
        let collab = Collaboration.set_outcome collab "done" in
        let session2 =
          Runtime_projection.update_session_from_collaboration session collab in
        check bool "phase is Completed" true
          (session2.phase = Runtime.Completed);
        check (option string) "outcome" (Some "done") session2.outcome);
    ];

    "participant_projection", [
      test_case "Runtime.participant (21 fields) → Collaboration.participant (6 fields)" `Quick (fun () ->
        let rt_participant : Runtime.participant = {
          name = "alice"; role = Some "lead";
          aliases = ["a"]; worker_id = Some "w1";
          runtime_actor = Some "alice";
          requested_provider = Some "anthropic";
          requested_model = Some "claude-3";
          requested_policy = None;
          provider = Some "anthropic"; model = Some "claude-3";
          resolved_provider = Some "anthropic";
          resolved_model = Some "claude-3";
          state = Live; summary = Some "working on it";
          accepted_at = Some 1.0; ready_at = Some 1.1;
          first_progress_at = Some 1.2; started_at = Some 1.0;
          finished_at = None; last_progress_at = Some 2.0;
          last_error = None;
        } in
        let session = {
          Runtime.session_id = "s3"; goal = "build"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = ["alice"];
          participants = [rt_participant];
          artifacts = []; turn_count = 0;
          last_seq = 0; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        check int "1 participant" 1 (List.length collab.participants);
        let p = List.hd collab.participants in
        check string "name" "alice" p.name;
        check (option string) "role" (Some "lead") p.role;
        check bool "state is Working (Live→Working)" true
          (p.state = Collaboration.Working);
        check (option string) "summary" (Some "working on it") p.summary;
        check bool "joined_at from started_at" true
          (p.joined_at = Some 1.0));

      test_case "participant_state mapping coverage" `Quick (fun () ->
        let mk_session state = {
          Runtime.session_id = "s4"; goal = "g"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = ["x"];
          participants = [{
            name = "x"; role = None; aliases = [];
            worker_id = None; runtime_actor = None;
            requested_provider = None; requested_model = None;
            requested_policy = None; provider = None; model = None;
            resolved_provider = None; resolved_model = None;
            state; summary = None; accepted_at = None;
            ready_at = None; first_progress_at = None;
            started_at = None; finished_at = None;
            last_progress_at = None; last_error = None;
          }];
          artifacts = []; turn_count = 0;
          last_seq = 0; outcome = None;
        } in
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
          let collab =
            Runtime_projection.collaboration_of_session (mk_session rt_state) in
          let p = List.hd collab.participants in
          check bool
            (Printf.sprintf "state %s"
              (Runtime.show_participant_state rt_state))
            true (p.state = expected)
        ) pairs);
    ];

    "artifact_contribution_projection", [
      test_case "artifact projection" `Quick (fun () ->
        let session = {
          Runtime.session_id = "s5"; goal = "g"; title = None;
          tag = None; permission_mode = None; phase = Completed;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = [];
          participants = [];
          artifacts = [{
            artifact_id = "art-1"; name = "report.md";
            kind = "document"; mime_type = "text/markdown";
            path = Some "/tmp/report.md"; inline_content = None;
            size_bytes = 1024; created_at = 1.5;
          }];
          turn_count = 0;
          last_seq = 0; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        check int "1 artifact" 1 (List.length collab.artifacts);
        let a = List.hd collab.artifacts in
        check string "id" "art-1" a.id;
        check string "name" "report.md" a.name;
        check string "kind" "document" a.kind;
        check string "producer is empty (lossy)" "" a.producer);

      test_case "collaboration omits runtime vote state" `Quick (fun () ->
        let session = {
          Runtime.session_id = "s6"; goal = "g"; title = None;
          tag = None; permission_mode = None; phase = Completed;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = [];
          participants = []; artifacts = [];
          turn_count = 0; last_seq = 0; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        check int "no contributions" 0 (List.length collab.contributions));
    ];

    "collaboration_of_session", [
      test_case "session_id becomes collaboration id" `Quick (fun () ->
        let session = {
          Runtime.session_id = "rt-abc123"; goal = "deploy"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 100.0; updated_at = 200.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = [];
          participants = []; artifacts = [];
          turn_count = 5; last_seq = 10; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        check string "id" "rt-abc123" collab.id;
        check string "goal" "deploy" collab.goal;
        check bool "timestamps" true
          (collab.created_at = 100.0 && collab.updated_at = 200.0);
        check (option string) "outcome is None" None collab.outcome);

      test_case "shared_context is fresh empty" `Quick (fun () ->
        let session = {
          Runtime.session_id = "s8"; goal = "g"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = [];
          participants = []; artifacts = [];
          turn_count = 0; last_seq = 0; outcome = None;
        } in
        let collab = Runtime_projection.collaboration_of_session session in
        check int "empty context" 0
          (List.length (Context.keys collab.shared_context)));
    ];

    "update_session_from_collaboration", [
      test_case "syncs goal, phase, outcome, updated_at" `Quick (fun () ->
        let session = {
          Runtime.session_id = "s9"; goal = "old goal"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = Some "p";
          model = Some "m"; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = [];
          participants = []; artifacts = [];
          turn_count = 3; last_seq = 5; outcome = None;
        } in
        let collab =
          Collaboration.create ~id:"s9" ~goal:"new goal" () in
        let collab = Collaboration.set_phase collab Completed in
        let collab = Collaboration.set_outcome collab "merged" in
        let session2 =
          Runtime_projection.update_session_from_collaboration session collab in
        check string "goal updated" "new goal" session2.goal;
        check bool "phase is Completed" true
          (session2.phase = Runtime.Completed);
        check (option string) "outcome" (Some "merged") session2.outcome;
        check (option string) "provider preserved" (Some "p") session2.provider;
        check int "turn_count preserved" 3 session2.turn_count);

      test_case "does not touch participants (lossy direction)" `Quick (fun () ->
        let rt_participant : Runtime.participant = {
          name = "x"; role = None; aliases = ["alias"];
          worker_id = Some "w1"; runtime_actor = None;
          requested_provider = None; requested_model = None;
          requested_policy = None; provider = None; model = None;
          resolved_provider = None; resolved_model = None;
          state = Live; summary = None; accepted_at = None;
          ready_at = None; first_progress_at = None;
          started_at = None; finished_at = None;
          last_progress_at = None; last_error = None;
        } in
        let session = {
          Runtime.session_id = "s10"; goal = "g"; title = None;
          tag = None; permission_mode = None; phase = Running;
          created_at = 1.0; updated_at = 2.0; provider = None;
          model = None; system_prompt = None; max_turns = 8;
          workdir = None; planned_participants = ["x"];
          participants = [rt_participant];
          artifacts = [];
          turn_count = 0; last_seq = 0; outcome = None;
        } in
        let collab = Collaboration.create ~id:"s10" ~goal:"g" () in
        let session2 =
          Runtime_projection.update_session_from_collaboration session collab in
        check int "participants untouched" 1
          (List.length session2.participants);
        let p = List.hd session2.participants in
        check bool "still Live (not overwritten)" true
          (p.state = Runtime.Live);
        check bool "aliases preserved" true
          (p.aliases = ["alias"]));
    ];

  ]
