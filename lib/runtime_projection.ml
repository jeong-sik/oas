open Runtime

let ( let* ) = Result.bind

let now () = Unix.gettimeofday ()

let make_session_id () =
  let base = int_of_float (now () *. 1000.0) in
  let salt = Random.int 0xFFFF in
  Printf.sprintf "rt-%08x-%04x" base salt

let first_some = Util.first_some

let make_planned_participant name =
  {
    name;
    role = None;
    aliases = [];
    worker_id = None;
    runtime_actor = Some name;
    requested_provider = None;
    requested_model = None;
    requested_policy = None;
    provider = None;
    model = None;
    resolved_provider = None;
    resolved_model = None;
    state = Planned;
    summary = None;
    accepted_at = None;
    ready_at = None;
    first_progress_at = None;
    started_at = None;
    finished_at = None;
    last_progress_at = None;
    last_error = None;
  }

let initial_session (request : start_request) =
  let ts = now () in
  {
    session_id =
      Option.value request.session_id ~default:(make_session_id ());
    goal = request.goal;
    title = None;
    tag = None;
    permission_mode = request.permission_mode;
    phase = Bootstrapping;
    created_at = ts;
    updated_at = ts;
    provider = request.provider;
    model = request.model;
    system_prompt = request.system_prompt;
    max_turns = Option.value request.max_turns ~default:8;
    workdir = request.workdir;
    planned_participants = request.participants;
    participants = List.map make_planned_participant request.participants;
    artifacts = [];
    votes = [];
    turn_count = 0;
    last_seq = 0;
    outcome = None;
  }

let update_participant (session : session) name f =
  let rec loop found acc = function
    | [] ->
        if found then List.rev acc
        else
          List.rev
            (f
               ({
                 name;
                 role = None;
                 aliases = [];
                 worker_id = None;
                 runtime_actor = Some name;
                 requested_provider = None;
                 requested_model = None;
                 requested_policy = None;
                 provider = None;
                 model = None;
                 resolved_provider = None;
                 resolved_model = None;
                 state = Planned;
                 summary = None;
                 accepted_at = None;
                 ready_at = None;
                 first_progress_at = None;
                 started_at = None;
                 finished_at = None;
                 last_progress_at = None;
                 last_error = None;
               } : participant)
             :: acc)
    | (participant : participant) :: rest when String.equal participant.name name ->
        loop true (f participant :: acc) rest
    | (participant : participant) :: rest -> loop found (participant :: acc) rest
  in
  { session with participants = loop false [] session.participants }

let update_session_meta session event =
  { session with updated_at = event.ts; last_seq = event.seq }

let ensure_active_phase session =
  match session.phase with
  | Completed | Failed | Cancelled ->
      Error
        (Error.Internal
           (Printf.sprintf "Session %s is terminal" session.session_id))
  | Bootstrapping | Running | Waiting_on_workers | Finalizing -> Ok session

let apply_event (session : session) (event : event) =
  let session = update_session_meta session event in
  match event.kind with
  | Session_started _ ->
      Ok { session with phase = Running }
  | Session_settings_updated detail ->
      Ok
        {
          session with
          model = detail.model;
          permission_mode = detail.permission_mode;
        }
  | Turn_recorded _ ->
      let* session = ensure_active_phase session in
      Ok { session with turn_count = session.turn_count + 1 }
  | Agent_spawn_requested detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               role = detail.role;
               requested_provider = detail.provider;
               requested_model = detail.model;
               requested_policy = detail.permission_mode;
               provider = first_some detail.provider session.provider;
               model = first_some detail.model session.model;
               resolved_provider = first_some detail.provider session.provider;
               resolved_model = first_some detail.model session.model;
               state = Starting;
               summary = None;
               worker_id = first_some participant.worker_id (Some detail.participant_name);
               runtime_actor =
                 first_some participant.runtime_actor (Some detail.participant_name);
               accepted_at = Some event.ts;
               ready_at = None;
               first_progress_at = None;
               started_at = Some event.ts;
               finished_at = None;
               last_progress_at = Some event.ts;
               last_error = None;
             }))
  | Agent_became_live detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               provider = first_some detail.provider participant.provider;
               model = first_some detail.model participant.model;
               resolved_provider =
                 first_some detail.provider participant.resolved_provider;
               resolved_model =
                 first_some detail.model participant.resolved_model;
               state = Live;
               summary = detail.summary;
               worker_id =
                 first_some participant.worker_id (Some detail.participant_name);
               runtime_actor =
                 first_some participant.runtime_actor (Some detail.participant_name);
               accepted_at =
                 first_some participant.accepted_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               ready_at =
                 first_some participant.ready_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               first_progress_at = first_some participant.first_progress_at (Some event.ts);
               started_at = Some (Option.value participant.started_at ~default:event.ts);
               last_progress_at = Some event.ts;
               last_error = None;
             }))
  | Agent_output_delta detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               state = Live;
               worker_id =
                 first_some participant.worker_id (Some detail.participant_name);
               runtime_actor =
                 first_some participant.runtime_actor (Some detail.participant_name);
               accepted_at =
                 first_some participant.accepted_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               ready_at =
                 first_some participant.ready_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               first_progress_at = first_some participant.first_progress_at (Some event.ts);
               last_progress_at = Some event.ts;
             }))
  | Agent_completed detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               provider = first_some detail.provider participant.provider;
               model = first_some detail.model participant.model;
               resolved_provider =
                 first_some detail.provider participant.resolved_provider;
               resolved_model =
                 first_some detail.model participant.resolved_model;
               state = Done;
               summary = detail.summary;
               worker_id =
                 first_some participant.worker_id (Some detail.participant_name);
               runtime_actor =
                 first_some participant.runtime_actor (Some detail.participant_name);
               accepted_at =
                 first_some participant.accepted_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               ready_at =
                 first_some participant.ready_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               first_progress_at = first_some participant.first_progress_at (Some event.ts);
               finished_at = Some event.ts;
               last_progress_at = Some event.ts;
               last_error = None;
             }))
  | Agent_failed detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               provider = first_some detail.provider participant.provider;
               model = first_some detail.model participant.model;
               resolved_provider =
                 first_some detail.provider participant.resolved_provider;
               resolved_model =
                 first_some detail.model participant.resolved_model;
               state = Failed_participant;
               summary = detail.summary;
               worker_id =
                 first_some participant.worker_id (Some detail.participant_name);
               runtime_actor =
                 first_some participant.runtime_actor (Some detail.participant_name);
               accepted_at =
                 first_some participant.accepted_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               ready_at =
                 first_some participant.ready_at
                   (Some (Option.value participant.started_at ~default:event.ts));
               first_progress_at = first_some participant.first_progress_at (Some event.ts);
               finished_at = Some event.ts;
               last_progress_at = Some event.ts;
               last_error = detail.error;
             }))
  | Artifact_attached detail ->
      let artifact =
        {
          artifact_id = detail.artifact_id;
          name = detail.name;
          kind = detail.kind;
          mime_type = detail.mime_type;
          path = Some detail.path;
          inline_content = None;
          size_bytes = detail.size_bytes;
          created_at = event.ts;
        }
      in
      Ok { session with artifacts = Util.snoc session.artifacts artifact }
  | Vote_recorded vote ->
      let* session = ensure_active_phase session in
      Ok { session with votes = Util.snoc session.votes vote }
  | Checkpoint_saved _ ->
      let* session = ensure_active_phase session in
      Ok session
  | Finalize_requested detail ->
      let* session = ensure_active_phase session in
      Ok { session with phase = Finalizing; outcome = detail.reason }
  | Session_completed detail ->
      Ok
        {
          session with
          phase = Completed;
          outcome = detail.outcome;
          updated_at = event.ts;
        }
  | Session_failed detail ->
      Ok
        {
          session with
          phase = Failed;
          outcome = detail.outcome;
          updated_at = event.ts;
        }

let count_by_state target (session : session) =
  session.participants
  |> List.filter (fun participant -> participant.state = target)
  |> List.length

let build_report (session : session) (events : event list) =
  let generated_at = now () in
  let summary =
    [
      Printf.sprintf "Goal: %s" session.goal;
      Printf.sprintf "Phase: %s" (show_phase session.phase);
      Printf.sprintf "Turns: %d" session.turn_count;
      Printf.sprintf "Participants done: %d" (count_by_state Done session);
      Printf.sprintf "Participants failed: %d"
        (count_by_state Failed_participant session);
      Printf.sprintf "Artifacts: %d" (List.length session.artifacts);
    ]
  in
  let participant_lines =
    session.participants
    |> List.map (fun (participant : participant) ->
           let summary =
             match participant.summary with
             | Some text when String.trim text <> "" -> text
             | _ -> "n/a"
           in
           Printf.sprintf "- %s [%s] %s" participant.name
             (show_participant_state participant.state)
             summary)
    |> String.concat "\n"
  in
  let event_lines =
    events
    |> List.map (fun event ->
           Printf.sprintf "- #%d %s %s" event.seq
             (show_event_kind event.kind) (Printf.sprintf "@ %.3f" event.ts))
    |> String.concat "\n"
  in
  {
    session_id = session.session_id;
    summary;
    generated_at;
    markdown =
      String.concat "\n"
        [
          "# Runtime Session Report";
          "";
          Printf.sprintf "- Session ID: %s" session.session_id;
          Printf.sprintf "- Goal: %s" session.goal;
          Printf.sprintf "- Phase: %s" (show_phase session.phase);
          Printf.sprintf "- Outcome: %s"
            (Option.value session.outcome ~default:"n/a");
          "";
          "## Summary";
          String.concat "\n" (List.map (fun line -> "- " ^ line) summary);
          "";
          "## Participants";
          participant_lines;
          "";
          "## Event Trace";
          event_lines;
          "";
        ];
  }

let build_proof (session : session) (events : event list) =
  let generated_at = now () in
  let ordered_events =
    List.sort (fun (a : event) (b : event) -> Int.compare a.seq b.seq) events
  in
  let has_turn = List.exists (function { kind = Turn_recorded _; _ } -> true | _ -> false) events in
  let has_spawn =
    List.exists
      (function { kind = Agent_completed _ | Agent_failed _; _ } -> true | _ -> false)
      events
  in
  let finalized =
    match session.phase with
    | Completed | Failed | Cancelled -> true
    | Bootstrapping | Running | Waiting_on_workers | Finalizing -> false
  in
  let has_session_started =
    List.exists (function { kind = Session_started _; _ } -> true | _ -> false) events
  in
  let has_terminal_event =
    List.exists
      (function
        | { kind = Session_completed _ | Session_failed _; _ } -> true
        | _ -> false)
      events
  in
  let seq_contiguous =
    let rec loop expected = function
      | [] -> true
      | (event : event) :: rest ->
          event.seq = expected && loop (expected + 1) rest
    in
    match ordered_events with
    | [] -> false
    | first :: _ -> loop first.seq ordered_events
  in
  let no_duplicate_artifact_ids =
    let ids =
      session.artifacts |> List.map (fun (artifact : artifact) -> artifact.artifact_id)
    in
    let uniq = List.sort_uniq String.compare ids in
    List.length uniq = List.length ids
  in
  let checks =
    [
      { name = "session_started"; passed = has_session_started };
      { name = "turn_recorded"; passed = has_turn };
      { name = "participant_outcome"; passed = has_spawn };
      { name = "terminal_phase"; passed = finalized };
      { name = "terminal_event"; passed = has_terminal_event };
      { name = "seq_contiguous"; passed = seq_contiguous };
      { name = "artifact_ids_unique"; passed = no_duplicate_artifact_ids };
    ]
  in
  let ok = List.for_all (fun check -> check.passed) checks in
  let evidence =
    [
      Printf.sprintf "last_seq=%d" session.last_seq;
      Printf.sprintf "turn_count=%d" session.turn_count;
      Printf.sprintf "participants=%d" (List.length session.participants);
      Printf.sprintf "artifacts=%d" (List.length session.artifacts);
      Printf.sprintf "phase=%s" (show_phase session.phase);
    ]
  in
  {
    session_id = session.session_id;
    ok;
    checks;
    evidence;
    generated_at;
  }

(* ── Collaboration bridge ────────────────────────────────────────
   Lossy projection: Runtime.session (18 fields) → Collaboration.t (12 fields).
   Runtime.participant has 21 fields; Collaboration.participant has 6.
   The reverse direction fills defaults — not a true round-trip.

   New code should operate on Collaboration.t directly.
   Existing code can call [collaboration_of_session] as a migration step. *)

let phase_to_collaboration : phase -> Collaboration.phase = function
  | Bootstrapping -> Bootstrapping
  | Running -> Active
  | Waiting_on_workers -> Waiting_on_participants
  | Finalizing -> Finalizing
  | Completed -> Completed
  | Failed -> Failed
  | Cancelled -> Cancelled

let phase_of_collaboration : Collaboration.phase -> phase = function
  | Bootstrapping -> Bootstrapping
  | Active -> Running
  | Waiting_on_participants -> Waiting_on_workers
  | Finalizing -> Finalizing
  | Completed -> Completed
  | Failed -> Failed
  | Cancelled -> Cancelled

let participant_state_to_collaboration
    : participant_state -> Collaboration.participant_state = function
  | Planned -> Planned
  | Starting -> Joined
  | Live -> Working
  | Idle -> Joined
  | Done -> Done
  | Failed_participant -> Failed_participant
  | Detached -> Left

let participant_to_collaboration (p : participant) : Collaboration.participant =
  {
    name = p.name;
    role = p.role;
    state = participant_state_to_collaboration p.state;
    joined_at = p.started_at;
    finished_at = p.finished_at;
    summary = p.summary;
  }

let artifact_to_collaboration (a : artifact) : Collaboration.artifact =
  {
    id = a.artifact_id;
    name = a.name;
    kind = a.kind;
    producer = "";  (* Runtime.artifact has no producer field *)
    created_at = a.created_at;
  }

let vote_to_collaboration (v : vote) : Collaboration.vote =
  {
    topic = v.topic;
    choice = v.choice;
    voter = Option.value v.actor ~default:"anonymous";
    cast_at = v.created_at;
  }

(** Extract a {!Collaboration.t} from a {!Runtime.session}.

    This is a lossy projection: Runtime.participant (21 fields) is
    compressed to Collaboration.participant (6 fields).
    [shared_context] is set to a fresh empty context since
    Runtime.session does not carry one. *)
let collaboration_of_session (session : session) : Collaboration.t =
  {
    id = session.session_id;
    goal = session.goal;
    phase = phase_to_collaboration session.phase;
    participants =
      List.map participant_to_collaboration session.participants;
    artifacts = List.map artifact_to_collaboration session.artifacts;
    votes = List.map vote_to_collaboration session.votes;
    shared_context = Context.create ();
    created_at = session.created_at;
    updated_at = session.updated_at;
    outcome = session.outcome;
    max_participants = None;
    metadata = [];
  }

(** Sync collaboration-owned fields back into a {!Runtime.session}.

    Only updates: [goal], [phase], [outcome], [updated_at].
    Does {b not} touch [participants], [artifacts], or [votes]
    because Runtime versions of these carry richer data (21-field
    participant vs 6-field).  Use per-field update functions for those. *)
let update_session_from_collaboration
    (session : session) (collab : Collaboration.t) : session =
  {
    session with
    goal = collab.goal;
    phase = phase_of_collaboration collab.phase;
    outcome = collab.outcome;
    updated_at = collab.updated_at;
  }
