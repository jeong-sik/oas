open Runtime

let ( let* ) = Result.bind
let now () = Unix.gettimeofday ()

let make_session_id () =
  let base = int_of_float (now () *. 1000.0) in
  let salt = Random.int 0xFFFF in
  Printf.sprintf "rt-%08x-%04x" base salt
;;

let first_some = Util.first_some

let make_planned_participant name =
  { name
  ; role = None
  ; aliases = []
  ; worker_id = None
  ; runtime_actor = Some name
  ; requested_provider = None
  ; requested_model = None
  ; requested_policy = None
  ; provider = None
  ; model = None
  ; resolved_provider = None
  ; resolved_model = None
  ; state = Planned
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

let initial_session (request : start_request) =
  let ts = now () in
  { session_id = Option.value request.session_id ~default:(make_session_id ())
  ; goal = request.goal
  ; title = None
  ; tag = None
  ; permission_mode = request.permission_mode
  ; phase = Bootstrapping
  ; created_at = ts
  ; updated_at = ts
  ; provider = request.provider
  ; model = request.model
  ; system_prompt = request.system_prompt
  ; max_turns = Option.value request.max_turns ~default:8
  ; workdir = request.workdir
  ; planned_participants = request.participants
  ; participants = List.map make_planned_participant request.participants
  ; artifacts = []
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let update_participant (session : session) name f =
  let rec loop found acc = function
    | [] ->
      if found
      then List.rev acc
      else
        List.rev
          (f
             ({ name
              ; role = None
              ; aliases = []
              ; worker_id = None
              ; runtime_actor = Some name
              ; requested_provider = None
              ; requested_model = None
              ; requested_policy = None
              ; provider = None
              ; model = None
              ; resolved_provider = None
              ; resolved_model = None
              ; state = Planned
              ; summary = None
              ; accepted_at = None
              ; ready_at = None
              ; first_progress_at = None
              ; started_at = None
              ; finished_at = None
              ; last_progress_at = None
              ; last_error = None
              }
              : participant)
           :: acc)
    | (participant : participant) :: rest when String.equal participant.name name ->
      loop true (f participant :: acc) rest
    | (participant : participant) :: rest -> loop found (participant :: acc) rest
  in
  { session with participants = loop false [] session.participants }
;;

let update_session_meta session event =
  { session with updated_at = event.ts; last_seq = event.seq }
;;

let ensure_active_phase session =
  match session.phase with
  | Completed | Failed | Cancelled ->
    Error (Error.Internal (Printf.sprintf "Session %s is terminal" session.session_id))
  | Bootstrapping | Running | Waiting_on_workers | Finalizing -> Ok session
;;

let participant_presence_status_of_state = function
  | Planned -> Presence_idle
  | Starting -> Presence_busy
  | Live -> Presence_active
  | Idle -> Presence_idle
  | Done -> Presence_offline
  | Failed_participant -> Presence_error
  | Detached -> Presence_offline
;;

let collaboration_metadata_of_channel channel =
  match channel with
  | Presence_channel ->
    { channel; persistence = Ephemeral; qos = Coalesced { max_hz = 30 } }
  | Activity_channel -> { channel; persistence = Append_only; qos = Ordered }
  | System_channel -> { channel; persistence = Aggregated_snapshot; qos = Best_effort }
;;

let presence_event ?summary ?subject participant_name status =
  { metadata = collaboration_metadata_of_channel Presence_channel
  ; payload = Participant_presence_updated { participant_name; status; summary; subject }
  }
;;

let activity_event ?actor ?summary ?subject ~category ~severity ~title () =
  { metadata = collaboration_metadata_of_channel Activity_channel
  ; payload = Activity_feed_item { actor; category; severity; title; summary; subject }
  }
;;

let system_event ?summary ~severity ~status component =
  { metadata = collaboration_metadata_of_channel System_channel
  ; payload = System_health_updated { component; severity; status; summary }
  }
;;

let collaboration_events_of_event (event : event) =
  match event.kind with
  | Session_started detail ->
    [ system_event
        ~severity:Severity_normal
        ~status:"running"
        ~summary:detail.goal
        "runtime_session"
    ; activity_event
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"session started"
        ~summary:detail.goal
        ()
    ]
  | Session_settings_updated _ ->
    [ activity_event
        ~category:Activity_system
        ~severity:Severity_low
        ~title:"session settings updated"
        ()
    ]
  | Turn_recorded detail ->
    [ activity_event
        ?actor:detail.actor
        ~category:Activity_work
        ~severity:Severity_low
        ~title:"turn recorded"
        ~summary:detail.message
        ()
    ]
  | Agent_spawn_requested detail ->
    [ presence_event detail.participant_name Presence_busy
    ; activity_event
        ~actor:detail.participant_name
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"participant spawn requested"
        ?summary:detail.role
        ~subject:detail.participant_name
        ()
    ]
  | Agent_became_live detail ->
    [ presence_event ?summary:detail.summary detail.participant_name Presence_active
    ; activity_event
        ~actor:detail.participant_name
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"participant live"
        ?summary:detail.summary
        ~subject:detail.participant_name
        ()
    ]
  | Agent_output_delta detail ->
    [ presence_event detail.participant_name Presence_active ]
  | Agent_completed detail ->
    [ presence_event ?summary:detail.summary detail.participant_name Presence_offline
    ; activity_event
        ~actor:detail.participant_name
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"participant completed"
        ?summary:detail.summary
        ~subject:detail.participant_name
        ()
    ]
  | Agent_failed detail ->
    [ presence_event
        ?summary:detail.summary
        ?subject:detail.error
        detail.participant_name
        Presence_error
    ; activity_event
        ~actor:detail.participant_name
        ~category:Activity_lifecycle
        ~severity:Severity_high
        ~title:"participant failed"
        ?summary:(first_some detail.error detail.summary)
        ~subject:detail.participant_name
        ()
    ]
  | Artifact_attached detail ->
    [ activity_event
        ~category:Activity_artifact
        ~severity:Severity_normal
        ~title:"artifact attached"
        ~summary:detail.name
        ~subject:detail.artifact_id
        ()
    ]
  | Checkpoint_saved detail ->
    [ activity_event
        ~category:Activity_system
        ~severity:Severity_low
        ~title:"checkpoint saved"
        ?summary:detail.label
        ~subject:detail.path
        ()
    ]
  | Finalize_requested detail ->
    [ system_event
        ~severity:Severity_normal
        ~status:"finalizing"
        ?summary:detail.reason
        "runtime_session"
    ; activity_event
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"finalize requested"
        ?summary:detail.reason
        ()
    ]
  | Session_completed detail ->
    [ system_event
        ~severity:Severity_normal
        ~status:"completed"
        ?summary:detail.outcome
        "runtime_session"
    ; activity_event
        ~category:Activity_lifecycle
        ~severity:Severity_normal
        ~title:"session completed"
        ?summary:detail.outcome
        ()
    ]
  | Session_failed detail ->
    [ system_event
        ~severity:Severity_critical
        ~status:"failed"
        ?summary:detail.outcome
        "runtime_session"
    ; activity_event
        ~category:Activity_lifecycle
        ~severity:Severity_critical
        ~title:"session failed"
        ?summary:detail.outcome
        ()
    ]
;;

let failure_cause_message = function
  | Runtime.Execution_error detail -> detail
  | Runtime.Persistence_failure { phase; detail } -> Printf.sprintf "%s: %s" phase detail
;;

let apply_event (session : session) (event : event) =
  let session = update_session_meta session event in
  match event.kind with
  | Session_started _ -> Ok { session with phase = Running }
  | Session_settings_updated detail ->
    Ok { session with model = detail.model; permission_mode = detail.permission_mode }
  | Turn_recorded _ ->
    let* session = ensure_active_phase session in
    Ok { session with turn_count = session.turn_count + 1 }
  | Agent_spawn_requested detail ->
    let* session = ensure_active_phase session in
    Ok
      (update_participant session detail.participant_name (fun participant ->
         { participant with
           role = detail.role
         ; requested_provider = detail.provider
         ; requested_model = detail.model
         ; requested_policy = detail.permission_mode
         ; provider = first_some detail.provider session.provider
         ; model = first_some detail.model session.model
         ; resolved_provider = first_some detail.provider session.provider
         ; resolved_model = first_some detail.model session.model
         ; state = Starting
         ; summary = None
         ; worker_id = first_some participant.worker_id (Some detail.participant_name)
         ; runtime_actor =
             first_some participant.runtime_actor (Some detail.participant_name)
         ; accepted_at = Some event.ts
         ; ready_at = None
         ; first_progress_at = None
         ; started_at = Some event.ts
         ; finished_at = None
         ; last_progress_at = Some event.ts
         ; last_error = None
         }))
  | Agent_became_live detail ->
    let* session = ensure_active_phase session in
    Ok
      (update_participant session detail.participant_name (fun participant ->
         { participant with
           provider = first_some detail.provider participant.provider
         ; model = first_some detail.model participant.model
         ; resolved_provider = first_some detail.provider participant.resolved_provider
         ; resolved_model = first_some detail.model participant.resolved_model
         ; state = Live
         ; summary = detail.summary
         ; worker_id = first_some participant.worker_id (Some detail.participant_name)
         ; runtime_actor =
             first_some participant.runtime_actor (Some detail.participant_name)
         ; accepted_at =
             first_some
               participant.accepted_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; ready_at =
             first_some
               participant.ready_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; first_progress_at = first_some participant.first_progress_at (Some event.ts)
         ; started_at = Some (Option.value participant.started_at ~default:event.ts)
         ; last_progress_at = Some event.ts
         ; last_error = None
         }))
  | Agent_output_delta detail ->
    let* session = ensure_active_phase session in
    Ok
      (update_participant session detail.participant_name (fun participant ->
         { participant with
           state = Live
         ; worker_id = first_some participant.worker_id (Some detail.participant_name)
         ; runtime_actor =
             first_some participant.runtime_actor (Some detail.participant_name)
         ; accepted_at =
             first_some
               participant.accepted_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; ready_at =
             first_some
               participant.ready_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; first_progress_at = first_some participant.first_progress_at (Some event.ts)
         ; last_progress_at = Some event.ts
         }))
  | Agent_completed detail ->
    let* session = ensure_active_phase session in
    Ok
      (update_participant session detail.participant_name (fun participant ->
         { participant with
           provider = first_some detail.provider participant.provider
         ; model = first_some detail.model participant.model
         ; resolved_provider = first_some detail.provider participant.resolved_provider
         ; resolved_model = first_some detail.model participant.resolved_model
         ; state = Done
         ; summary = detail.summary
         ; worker_id = first_some participant.worker_id (Some detail.participant_name)
         ; runtime_actor =
             first_some participant.runtime_actor (Some detail.participant_name)
         ; accepted_at =
             first_some
               participant.accepted_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; ready_at =
             first_some
               participant.ready_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; first_progress_at = first_some participant.first_progress_at (Some event.ts)
         ; finished_at = Some event.ts
         ; last_progress_at = Some event.ts
         ; last_error = None
         }))
  | Agent_failed detail ->
    let* session = ensure_active_phase session in
    Ok
      (update_participant session detail.participant_name (fun participant ->
         { participant with
           provider = first_some detail.provider participant.provider
         ; model = first_some detail.model participant.model
         ; resolved_provider = first_some detail.provider participant.resolved_provider
         ; resolved_model = first_some detail.model participant.resolved_model
         ; state = Failed_participant
         ; summary = detail.summary
         ; worker_id = first_some participant.worker_id (Some detail.participant_name)
         ; runtime_actor =
             first_some participant.runtime_actor (Some detail.participant_name)
         ; accepted_at =
             first_some
               participant.accepted_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; ready_at =
             first_some
               participant.ready_at
               (Some (Option.value participant.started_at ~default:event.ts))
         ; first_progress_at = first_some participant.first_progress_at (Some event.ts)
         ; finished_at = Some event.ts
         ; last_progress_at = Some event.ts
         ; last_error =
             first_some
               detail.error
               (Option.map failure_cause_message detail.failure_cause)
         }))
  | Artifact_attached detail ->
    let artifact =
      { artifact_id = detail.artifact_id
      ; name = detail.name
      ; kind = detail.kind
      ; mime_type = detail.mime_type
      ; path = Some detail.path
      ; inline_content = None
      ; size_bytes = detail.size_bytes
      ; created_at = event.ts
      }
    in
    Ok { session with artifacts = Util.snoc session.artifacts artifact }
  | Checkpoint_saved _ ->
    let* session = ensure_active_phase session in
    Ok session
  | Finalize_requested detail ->
    let* session = ensure_active_phase session in
    Ok { session with phase = Finalizing; outcome = detail.reason }
  | Session_completed detail ->
    Ok { session with phase = Completed; outcome = detail.outcome; updated_at = event.ts }
  | Session_failed detail ->
    Ok { session with phase = Failed; outcome = detail.outcome; updated_at = event.ts }
;;

let count_by_state target (session : session) =
  session.participants
  |> List.filter (fun participant -> participant.state = target)
  |> List.length
;;

let build_report (session : session) (events : event list) =
  let generated_at = now () in
  let summary =
    [ Printf.sprintf "Goal: %s" session.goal
    ; Printf.sprintf "Phase: %s" (show_phase session.phase)
    ; Printf.sprintf "Turns: %d" session.turn_count
    ; Printf.sprintf "Participants done: %d" (count_by_state Done session)
    ; Printf.sprintf "Participants failed: %d" (count_by_state Failed_participant session)
    ; Printf.sprintf "Artifacts: %d" (List.length session.artifacts)
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
      Printf.sprintf
        "- %s [%s] %s"
        participant.name
        (show_participant_state participant.state)
        summary)
    |> String.concat "\n"
  in
  let event_lines =
    events
    |> List.map (fun event ->
      Printf.sprintf
        "- #%d %s %s"
        event.seq
        (show_event_kind event.kind)
        (Printf.sprintf "@ %.3f" event.ts))
    |> String.concat "\n"
  in
  { session_id = session.session_id
  ; summary
  ; generated_at
  ; markdown =
      String.concat
        "\n"
        [ "# Runtime Session Report"
        ; ""
        ; Printf.sprintf "- Session ID: %s" session.session_id
        ; Printf.sprintf "- Goal: %s" session.goal
        ; Printf.sprintf "- Phase: %s" (show_phase session.phase)
        ; Printf.sprintf "- Outcome: %s" (Option.value session.outcome ~default:"n/a")
        ; ""
        ; "## Summary"
        ; String.concat "\n" (List.map (fun line -> "- " ^ line) summary)
        ; ""
        ; "## Participants"
        ; participant_lines
        ; ""
        ; "## Event Trace"
        ; event_lines
        ; ""
        ]
  }
;;

let build_proof (session : session) (events : event list) =
  let generated_at = now () in
  let ordered_events =
    List.sort (fun (a : event) (b : event) -> Int.compare a.seq b.seq) events
  in
  let has_turn =
    List.exists
      (function
        | { kind = Turn_recorded _; _ } -> true
        | _ -> false)
      events
  in
  let has_spawn =
    List.exists
      (function
        | { kind = Agent_completed _ | Agent_failed _; _ } -> true
        | _ -> false)
      events
  in
  let finalized =
    match session.phase with
    | Completed | Failed | Cancelled -> true
    | Bootstrapping | Running | Waiting_on_workers | Finalizing -> false
  in
  let has_session_started =
    List.exists
      (function
        | { kind = Session_started _; _ } -> true
        | _ -> false)
      events
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
      | (event : event) :: rest -> event.seq = expected && loop (expected + 1) rest
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
    [ { name = "session_started"; passed = has_session_started }
    ; { name = "turn_recorded"; passed = has_turn }
    ; { name = "participant_outcome"; passed = has_spawn }
    ; { name = "terminal_phase"; passed = finalized }
    ; { name = "terminal_event"; passed = has_terminal_event }
    ; { name = "seq_contiguous"; passed = seq_contiguous }
    ; { name = "artifact_ids_unique"; passed = no_duplicate_artifact_ids }
    ]
  in
  let ok = List.for_all (fun check -> check.passed) checks in
  let evidence =
    [ Printf.sprintf "last_seq=%d" session.last_seq
    ; Printf.sprintf "turn_count=%d" session.turn_count
    ; Printf.sprintf "participants=%d" (List.length session.participants)
    ; Printf.sprintf "artifacts=%d" (List.length session.artifacts)
    ; Printf.sprintf "phase=%s" (show_phase session.phase)
    ]
  in
  { session_id = session.session_id; ok; checks; evidence; generated_at }
;;
