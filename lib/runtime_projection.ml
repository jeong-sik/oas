open Runtime

let ( let* ) = Result.bind

let now () = Unix.gettimeofday ()

let make_session_id () =
  let base = int_of_float (now () *. 1000.0) in
  let salt = Random.int 0xFFFF in
  Printf.sprintf "rt-%08x-%04x" base salt

let make_planned_participant name =
  {
    name;
    role = None;
    state = Planned;
    summary = None;
    started_at = None;
    finished_at = None;
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
                 state = Planned;
                 summary = None;
                 started_at = None;
                 finished_at = None;
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
               state = Starting;
               summary = None;
               started_at = Some event.ts;
               finished_at = None;
               last_error = None;
             }))
  | Agent_became_live detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               state = Live;
               summary = detail.summary;
               started_at = Some (Option.value participant.started_at ~default:event.ts);
               last_error = None;
             }))
  | Agent_output_delta _ ->
      let* session = ensure_active_phase session in
      Ok session
  | Agent_completed detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               state = Done;
               summary = detail.summary;
               finished_at = Some event.ts;
               last_error = None;
             }))
  | Agent_failed detail ->
      let* session = ensure_active_phase session in
      Ok
        (update_participant session detail.participant_name (fun participant ->
             {
               participant with
               state = Failed_participant;
               summary = detail.summary;
               finished_at = Some event.ts;
               last_error = detail.error;
             }))
  | Artifact_attached detail ->
      let* session = ensure_active_phase session in
      let artifact =
        {
          name = detail.name;
          kind = detail.kind;
          path = Some detail.path;
          inline_content = None;
          created_at = event.ts;
        }
      in
      Ok { session with artifacts = session.artifacts @ [ artifact ] }
  | Vote_recorded vote ->
      let* session = ensure_active_phase session in
      Ok { session with votes = session.votes @ [ vote ] }
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
  let checks =
    [
      { name = "session_started"; passed = session.last_seq >= 1 };
      { name = "turn_recorded"; passed = has_turn };
      { name = "participant_outcome"; passed = has_spawn };
      { name = "terminal_phase"; passed = finalized };
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
